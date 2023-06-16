/* #define _GNU_SOURCE */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "config.h"
#include "log.h"
#include "mustach.h"
#include "mustache_cjson.h"
#ifdef DEVBUILD
#include "ansi_colors.h"
#endif

struct tstack_s {
    int predicate;
    bool lambda;
	cJSON null;
	cJSON *root;
	cJSON *selection;
	int depth;
    struct {
        cJSON *ctx;
        cJSON *obj;
        cJSON *next;
        int is_objiter;
        size_t index;    /* so we can detect first item */
        int predicate;         /*  */
        bool lambda;            /*  */
        int  workbuf_idx;
    } stack[MUSTACH_MAX_DEPTH]; //FIXME: rename 'frame'
};

char fmt_str[512];

static void dump_stack(struct tstack_s *stack);

static int start(struct tstack_s *stack)
{
    TRACE_ENTRY(start);
    /* struct tstack_s *e = closure; */
    stack->depth = 0;
    memset(&stack->null, 0, sizeof stack->null);
    stack->null.type = cJSON_NULL;
    stack->selection = &stack->null;
    stack->stack[0].ctx = NULL;
    stack->stack[0].obj = stack->root;
    stack->stack[0].index = 0;
    return MUSTACH_OK;
}

static int compare(void *closure, const char *value)
{
	struct tstack_s *e = closure;
	cJSON *o = e->selection;
	double d;

	if (cJSON_IsNumber(o)) {
		d = o->valuedouble - atof(value);
		return d < 0 ? -1 : d > 0 ? 1 : 0;
	} else if (cJSON_IsString(o)) {
		return strcmp(o->valuestring, value);
	} else if (cJSON_IsTrue(o)) {
		return strcmp("true", value);
	} else if (cJSON_IsFalse(o)) {
		return strcmp("false", value);
	} else if (cJSON_IsNull(o)) {
		return strcmp("null", value);
	} else {
		return 1;
	}
}

/* maybe copies value for key to stack->selection */
static int sel(void *closure, const char *key)
{
    TRACE_ENTRY(sel);
    struct tstack_s *stack = closure;
#ifdef DEVBUILD
    log_trace("key: %s", key);
    log_trace("stack->depth: %d", stack->depth);
    log_trace("stack->stack[stack->depth].obj %p", stack->stack[stack->depth].obj);
    log_trace("predicate: %d", stack->predicate);
    /* DUMP_CLOSURE(e, stack->depth); */
#endif

    cJSON *selection;
    int i, r;

    /* dump_stack(stack); */

    if (key == NULL) {
        /* log_debug("stack->predicate: %d", stack->predicate); */
        if (stack->predicate) {
            switch(stack->predicate) {
            case FIRST_P:
#ifdef DEVBUILD
                log_debug("case: FIRST_P");
#endif
                if (stack->stack[stack->depth].index == 0) {
#ifdef DEVBUILD
                    log_debug("predicate is truthy!");
#endif
                    selection = cJSON_CreateTrue(); //FIXME
                } else {
#ifdef DEVBUILD
                    log_debug("predicate is false!");
#endif
                    selection = cJSON_CreateFalse(); //FIXME
                }
                r = 1;
                break;
            case LAST_P:
#ifdef DEVBUILD
                log_debug("case: LAST_P");
#endif
                stack->stack[stack->depth].predicate = stack->predicate;
                /* log_debug("next: %p", stack->stack[stack->depth].next); */
                if (stack->stack[stack->depth].next == NULL) {
#ifdef DEVBUILD
                    log_debug("predicate is truthy!");
#endif
                    selection = cJSON_CreateTrue(); //FIXME
                } else {
#ifdef DEVBUILD
                    log_debug("predicate is false!");
#endif
                    selection = cJSON_CreateFalse(); //FIXME
                }
                r = 1;
                break;
            case BUTLAST_P:
#ifdef DEVBUILD
                log_debug("case: BUTLAST_P");
#endif
                stack->stack[stack->depth].predicate = stack->predicate;
                // true for all but last
                if (stack->stack[stack->depth].next != NULL) {
                    /* s7_pointer tmp = s7_vector_ref(s7, stack->stack[stack->depth].ctx, stack->stack[stack->depth].index); */
                    /* (void)tmp; */
#ifdef DEVBUILD
                    log_debug("predicate {{?}} is truthy!");
#endif
                    selection = cJSON_CreateTrue(); //FIXME
                } else {
#ifdef DEVBUILD
                    log_debug("pred {{?}} is false!");
#endif
                    selection = cJSON_CreateFalse(); //FIXME
                }
                r = 1;
                break;
            default:
                selection = cJSON_CreateFalse(); //FIXME
                r = 1;
            }
            // key null but not stack->predicate
        } else {
            TRACE_LOG_DEBUG("NULL key", "");
            selection = stack->stack[stack->depth].obj;
            r = 1;
        }
    } else {
        i = stack->depth;
        /* log_debug("searching stack for key %s", key); */

        /*
          SPECIAL CASE: root table objiter: {{#^.*}}...{{/^.*}}
         */
        if ((strncmp(key, "^", 1) == 0) && strlen(key) == 1) {
            selection = stack->stack[i].obj;
        } else {
            while (i >= 0) {
                selection = cJSON_GetObjectItemCaseSensitive(stack->stack[i].obj, key);
                if  (selection) {
                    /* log_debug("selecting for key '%s' val: '%s'", */
                    /*           key, cJSON_PrintUnformatted(selection)); */
                    break;
                } else {
                    i--;
                }
            }
        }
        if (i >= 0)
            r = 1;
        else {
            selection = &stack->null;
            r = 0;
        }
    }
    stack->selection = selection;
    return r;
}

static int subsel(void *closure, const char *name)
{
    TRACE_ENTRY(subsel);
    TRACE_LOG_DEBUG("key: %s", name);
    struct tstack_s *stack = closure;
    cJSON *o;
    /* int r; */

    o = cJSON_GetObjectItemCaseSensitive(stack->selection, name);
    if (o) {
        TRACE_LOG_DEBUG("setting selection to val for key %s", name);
        stack->selection = o;
        return 1;
    } else {
        return 0;
    }
    /* r = o != NULL; */
    /* if (r) */
    /*     stack->selection = o; */
    /* return r; */
}

static int enter(void *closure, int objiter)
{
	struct tstack_s *e = closure;
	cJSON *o;

	if (++e->depth >= MUSTACH_MAX_DEPTH)
		return MUSTACH_ERROR_TOO_DEEP;

	o = e->selection;
	e->stack[e->depth].is_objiter = 0;
	if (objiter) {
		if (! cJSON_IsObject(o))
			goto not_entering;
		if (o->child == NULL)
			goto not_entering;
		e->stack[e->depth].obj = o->child;
		e->stack[e->depth].next = o->child->next;
		e->stack[e->depth].index = 0;
		e->stack[e->depth].ctx = o;
		e->stack[e->depth].is_objiter = 1;
	} else if (cJSON_IsArray(o)) {
		if (o->child == NULL)
			goto not_entering;
		e->stack[e->depth].obj = o->child;
		e->stack[e->depth].next = o->child->next;
		e->stack[e->depth].index = 0;
		e->stack[e->depth].ctx = o;
	} else if ((cJSON_IsObject(o) && o->child == NULL) || (! cJSON_IsFalse(o) && ! cJSON_IsNull(o))) {
		e->stack[e->depth].obj = o;
		e->stack[e->depth].ctx = NULL;
		e->stack[e->depth].next = NULL;
		e->stack[e->depth].index = 0;
	} else
		goto not_entering;
	return 1;

not_entering:
	e->depth--;
	return 0;
}

static int next(void *closure)
{
    struct tstack_s *e = closure;
    cJSON *o;

    if (e->depth <= 0) {
        fprintf(stderr, "ERR: next\n");
        return MUSTACH_ERROR_CLOSING;
    }
    o = e->stack[e->depth].next;
    if (o == NULL)
        return 0;

    e->stack[e->depth].obj = o;
    e->stack[e->depth].next = o->next;
    e->stack[e->depth].index++;
    return 1;
}

static int leave(void *closure, struct mustach_sbuf *sbuf)
/* static int leave(void *closure) */
{
    (void)sbuf;                 /* future: lambda support */
    struct tstack_s *e = closure;

    if (e->depth <= 0) {
        fprintf(stderr, "ERR: leave\n");
        return MUSTACH_ERROR_CLOSING;
    }
    e->depth--;
    return 0;
}

/* writes formatted value of stack->selection to sbuf.s */
static int format(struct tstack_s *stack, const char *fmt,
                  struct mustach_sbuf *sbuf, int key)
//(void *closure, struct mustach_sbuf *sbuf, int key)
{
    TRACE_ENTRY(format);
    TRACE_LOG_DEBUG("key: %d", key);
    TRACE_LOG_DEBUG("fmt: %s", fmt);
    const char *s = NULL, *dec = NULL;
    char work[512];
    int len;

    if (key) {
        s = stack->stack[stack->depth].is_objiter
            ? stack->stack[stack->depth].obj->string
            : "";
    } else {
        if (cJSON_IsString(stack->selection)) {
            /* s = stack->selection->valuestring; */
            if (fmt) {
                TRACE_LOG_DEBUG("FORMATTING nbr:: %s", stack->selection->valuestring);
                len = snprintf(NULL, 0, fmt, stack->selection->valuestring);
                /* log_debug("LEN: %d", len); */
                snprintf(work, len+1, fmt, stack->selection->valuestring);
                TRACE_LOG_DEBUG("formatted str: %s", work);
                s = strndup(work, len+1);
                sbuf->freecb = free;
            } else {
                s = stack->selection->valuestring;
            }
            if (s == NULL)
                return MUSTACH_ERROR_SYSTEM;
            /* log_debug("string '%s'", s); */
        }
        else if (cJSON_IsObject(stack->selection)) {
            s = cJSON_PrintUnformatted(stack->selection);
            log_debug("Formatting JSON object: %s", s);
            if (strncmp(s, "{}", 2) == 0) {
                free((void*)s);
                s = "";
            }
        }
        else if (cJSON_IsArray(stack->selection)) {
            log_debug("Formatting JSON array");
            if (cJSON_GetArraySize(stack->selection) == 0) {
                s = "";
            } else {
                s = cJSON_PrintUnformatted(stack->selection);
            }
        } else if (cJSON_IsNull(stack->selection))
            s = "";
        else if (cJSON_IsNumber(stack->selection)) {
            if (fmt) {
                /* s = cJSON_Print(stack->selection); */
                // cJSON_Print won't work, we need to format
                //PITA: cJSON makes no distinction between ints and floats.
                //workaround: user should use '%g' for ints, not '%d" or '%i'
                //fix: print to work with %g, detect decimal pt,
                // then print using type coercion (e.g. ((int)n)
                double n = cJSON_GetNumberValue(stack->selection);
                /* log_debug("FORMATTING nbr:: %f", n); */
                len = snprintf(NULL, 0, "%g", n);
                /* log_debug("LEN: %d", len); */
                snprintf(work, len+1, "%g", n);
                dec = strchr(work, '.');
                if (dec) {
                    /* log_debug("float"); */
                    len = snprintf(NULL, 0, fmt, n);
                    /* log_debug("LEN2: %d", len); */
                    snprintf(work, len+1, fmt, n);
                } else {
                    /* log_debug("int, fmt: %s", fmt); */
                    long i = (long)n;
                    len = snprintf(NULL, 0, fmt, i);
                    /* log_debug("LEN2: %d", len); */
                    snprintf(work, len+1, fmt, i);
                }
                TRACE_LOG_DEBUG("formatted nbr: %s", work);
                s = strndup(work, len+1);
            } else {
                s = cJSON_PrintUnformatted(stack->selection);
            }
            if (s == NULL)
                return MUSTACH_ERROR_SYSTEM;
            sbuf->freecb = free;
        }
        else {
            s = cJSON_PrintUnformatted(stack->selection);
            if (s == NULL)
                return MUSTACH_ERROR_SYSTEM;
            sbuf->freecb = free;
        }
    }
    sbuf->value = s;
    return 1;
}

/* **************************************************************** */
/* static void _dump_obj(char *msg, struct cJSON *item) */
/* { */
/*     log_debug(msg); */
/*     if (item) { */
/*         if (cJSON_IsBool(item)) { */
/*             log_debug("\t  bool: %s", cJSON_PrintUnformatted(item)); */
/*             /\* if (cJSON_IsTrue(item)) *\/ */
/*             /\*     log_debug("\t  bool: true"); *\/ */
/*             /\* else *\/ */
/*             /\*     log_debug("\t  bool: false"); *\/ */
/*         } */
/*         else if (cJSON_IsNull(item)) { */
/*             log_debug("\t  null: %s", cJSON_PrintUnformatted(item)); */
/*             /\* log_debug("\t  null: null"); *\/ */
/*         } */
/*         else if (cJSON_IsNumber(item)) { */
/*             /\* double n = cJSON_GetNumberValue(item); //FIXME: free? *\/ */
/*             /\* int len = snprintf(NULL, 0, "%f", n); *\/ */
/*             log_debug("\t  number: %s", */
/*                       cJSON_PrintUnformatted(item)); */
/*         } */
/*         else if (cJSON_IsString(item)) { */
/*             /\* char *s = cJSON_GetStringValue(item); //FIXME: free? *\/ */
/*             log_debug("\t  string: %s", */
/*                       cJSON_PrintUnformatted(item)); */
/*         } */
/*         else if (cJSON_IsArray(item)) { */
/*             /\* log_debug("\t  array: %s", "todo..."); *\/ */
/*             log_debug("\t  array: %s", cJSON_PrintUnformatted(item)); */
/*         } */
/*         else if (cJSON_IsObject(item)) { */
/*             log_debug("\t  object: %s", cJSON_PrintUnformatted(item)); */
/*             /\* char *s = cJSON_PrintUnformatted(item); *\/ */
/*             /\* log_debug("\t  object: %s", s); *\/ */
/*         } */
/*         else { */
/*             log_warn("\t UNKNOWN DATUM TYPE"); */
/*         } */
/*     } else */
/*         log_debug("\t  NULL"); */
/* } */

static void dump_stack(struct tstack_s *stack)
{
    /* log_debug("stack ptr: %p", stack); */
    /* struct tstack_s *e = (struct tstack_s*)stack; */
    int d = stack->depth;
    log_debug("DUMP_STACK");
    log_debug("\tpredicate: %d", stack->predicate);
    log_debug("\tlambda: %d", stack->lambda);
    log_debug("\tdepth: %d", d);

    /* _dump_obj("\troot:", stack->root); */
    log_debug("\troot     : %s", cJSON_PrintUnformatted(stack->root));
    log_debug("\tselection: %s", cJSON_PrintUnformatted(stack->selection));
    /* _dump_obj("\tselection:", stack->selection); */
    for (int i = 0; i <= stack->depth; i++) {
        log_debug("stackframe: %d", i);
        /* _dump_obj("\tctx:", stack->stack[i].ctx); */
        if (stack->stack[i].ctx) {
            log_debug("\tstack[%d].ctx: %s", i,
                      cJSON_PrintUnformatted(stack->stack[i].ctx));
        } else {
            log_debug("\tstack[%d].ctx: NULL", i);
        }
        /* _dump_obj("\tobj", stack->stack[i].obj); */
        if (stack->stack[i].obj) {
            log_debug("\tstack[%d].obj: %s", i,
                      cJSON_PrintUnformatted(stack->stack[i].obj));
        } else {
            log_debug("\tstack[%d].obj: NULL", i);
        }
        if (stack->stack[i].obj->child) {
                log_debug("\tstack[%d].obj->child: %s", i,
                          cJSON_PrintUnformatted(stack->stack[i].obj->child));
        } else {
            log_debug("\tstack[%d].obj->child: NULL", i);
        }
        if (stack->stack[i].obj->next) {
            log_debug("\tstack[%d].obj->next: %s", i,
                      cJSON_PrintUnformatted(stack->stack[i].obj->next));
        } else {
            log_debug("\tstack[%d].obj->next: NULL", i);
        }
        log_debug("\tstack[%d].index: %d", i, stack->stack[i].index);
        log_debug("\tstack[%d].lambda: %d", i, stack->stack[i].lambda);
        log_debug("\tstack[%d].predicate: 0x%04X", i, stack->stack[i].predicate);
    }
    log_debug("end stack");
    fflush(NULL);
}

/* static void dump_stack(struct tstack_s *stack) */
/* { */
/*     /\* struct tstack_s *e = closure; *\/ */
/*     int d = stack->depth; */
/*     log_debug("DUMP_STACK"); */
/*     log_debug("\tpredicate: %d", stack->predicate); */
/*     log_debug("\tlambda: %d", stack->lambda); */
/*     log_debug("\tdepth: %d", d); */
/*     TRACE_CJSON_DUMP("\troot", stack->root); */
/*     TRACE_CJSON_DUMP("\tselection", stack->selection); */
/*     if (stack->stack[d].ctx) */
/*         TRACE_CJSON_DUMP("\tstack->stack[%d].ctx", stack->stack[d].ctx); */
/*     else */
/*         log_debug("ctx: ?"); */
/*     if (stack->stack[d].obj) */
/*         TRACE_CJSON_DUMP("\tstack->stack[%d].obj", stack->stack[d].obj); */
/*     else */
/*         log_debug("obj: ?"); */
/*     log_debug("\tstack->stack[%d].count: %d", d, stack->stack[d].count); */
/*     log_debug("\tstack->stack[%d].index: %d", d, stack->stack[d].index); */
/*     log_debug("\tstack->stack[%d].lambda: %d", d, stack->stack[d].lambda); */
/*     log_debug("\tstack->stack[%d].predicate: %d", d, stack->stack[d].predicate); */
/*     log_debug("end stack"); */
/*     fflush(NULL); */
/* } */

/* const struct mustach_wrap_itf mustach_wrap_itf_json = { */
const struct mustach_ds_methods_s cjson_methods = {
    .start = (int (*)(void*))start,
    .stop = NULL,
    .compare = compare,
    .sel = sel,
    .subsel = subsel,
    .enter = enter,
    .next = next,
    .leave = leave,
    .format = (int (*)(void*, const char*, struct mustach_sbuf*, int))format,
    .dump_stack = (void (*)(void*))dump_stack
};

/* void *mustach_encode_cjson(char *json_str, size_t len) */
/* { */
/*     cJSON *json_data = cJSON_ParseWithLength(json_str, len); */
/*     return json_data; */
/* } */

void mustach_free(void *json_c)
{
    cJSON_Delete((cJSON*)json_c);
}

/* ****************************************************************
 * PUBLIC RENDER API
 **************************************************************** */
const char *mustache_json_render(const char *template,
                                 size_t template_sz,
                                 cJSON *root,
                                 int _flags)
{
    TRACE_ENTRY(mustache_json_render);
    //FIXME: client responsible for flags
    int flags = Mustach_With_All_JSON_Extensions;
    /* flags &= ~Mustach_With_JsonPointer; */
    (void)_flags;
    //FIXME: _flags arg is for opting out of default 'all'

    struct tstack_s stack;
    memset(&stack, 0, sizeof(struct tstack_s));
    stack.root = (cJSON*)root;
    char *result = mustach_json_render_to_string(template, template_sz,
                                                 &cjson_methods,
                                                 &stack,
                                                 flags);
    if (!result) {
        log_error("mustach_json_mem failure");
        return NULL;
    } else {
        return result;
    }
}

int mustache_json_frender(FILE * restrict file,
                          const char *template,
                          size_t template_sz,
                          cJSON *root,
                          int _flags)
{
    TRACE_ENTRY(mustache_json_frender);
    //FIXME: client responsible for flags
    int flags = Mustach_With_AllExtensions;
    flags &= ~Mustach_With_JsonPointer;
    (void)_flags;

    struct tstack_s stack;
    stack.root = (cJSON*)root;
    return mustach_json_file(template, template_sz,
                             &cjson_methods,
                             &stack,
                             flags,
                             file);
}

int mustache_json_fdrender(int fd,
                          const char *template,
                          size_t template_sz,
                          cJSON *root,
                          int _flags)
{
    TRACE_ENTRY(mustache_json_fdrender);
    //FIXME: client responsible for flags
    int flags = Mustach_With_AllExtensions;
    flags &= ~Mustach_With_JsonPointer;
    (void)_flags;
    (void)flags;
    struct tstack_s stack;
    stack.root = (cJSON*)root;
    (void)stack;
    (void)fd;
    (void)template;
    (void)template_sz;
    return -1;
}


/* int mustach_fprintf(FILE * restrict file, */
/*                     const char * restrict template, size_t tlength, */
/*                     void *json_root, */
/*                     int data_schema, */
/*                     int flags) */
/* { */
/*     (void)data_schema; */
/*     struct tstack_s e; */

/*     e.root = (cJSON*)json_root; */
/*     return mustach_json_file(template, tlength, &cjson_methods, &e, flags, file); */
/* } */

/* int mustach_cJSON_file(const char *template, size_t length, cJSON *root, int flags, FILE *file) */
/* { */
/* 	struct tstack_s e; */
/* 	e.root = root; */
/* 	return mustach_json_file(template, length, &cjson_methods, &e, flags, file); */
/* } */

/* int mustach_cJSON_fd(const char *template, size_t tlength, */
/*                      cJSON *root, int flags, int fd) */
/* { */
/* 	struct tstack_s e; */
/* 	e.root = root; */
/* 	return mustach_json_fd(template, tlength, &cjson_methods, &e, flags, fd); */
/* } */

/* size_t mustach_asprintf(char **ret, */
/*                     const char *template, size_t tlength, */
/*                     void *json_root, */
/*                     int data_schema, // JSON or SCHEME */
/*                     int flags) */
/* { */
/*     (void)data_schema; */
/*     struct tstack_s e; */
/*     e.root = (cJSON*)json_root; */
/*     size_t size; */
/*     int rc = mustach_json_mem(template, tlength, */
/*                               &cjson_methods, &e, */
/*                               flags? flags */
/*                               : Mustach_With_AllExtensions, */
/*                               ret, */
/*                               &size); */
/*     if (rc < 0) { */
/*         log_error("mustach_json_mem failure: %s", strerror(errno)); */
/*         return rc; */
/*     } else { */
/*         return size; */
/*     } */
/* } */

/* int mustach_cJSON_mem(const char *template, size_t length, cJSON *root, int flags, char **result, size_t *size) */
/* { */
/* 	struct tstack_s e; */
/* 	e.root = root; */
/* 	return mustach_json_mem(template, length, &cjson_methods, &e, flags, result, size); */
/* } */

/* int mustach_cJSON_write(const char *template, size_t length, cJSON *root, int flags, mustach_write_cb_t *writecb, void *closure) */
/* { */
/* 	struct tstack_s e; */
/* 	e.root = root; */
/* 	return mustach_json_write(template, length, &cjson_methods, &e, flags, writecb, closure); */
/* } */

/* int mustach_cJSON_emit(const char *template, size_t length, cJSON *root, int flags, mustach_emit_cb_t *emitcb, void *closure) */
/* { */
/* 	struct tstack_s e; */
/* 	e.root = root; */
/* 	return mustach_json_emit(template, length, &cjson_methods, &e, flags, emitcb, closure); */
/* } */

