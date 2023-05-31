/* #define _GNU_SOURCE */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "config.h"
#include "log.h"
#include "mustach.h"
#include "mustach_cjson.h"
#ifdef DEBUGGING
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
		cJSON *cont;
		cJSON *obj;
		cJSON *next;
		int is_objiter;
            size_t index, count;    /* not for cjson? */
        int predicate;         /*  */
        bool lambda;            /*  */
        int  workbuf_idx;
	} stack[MUSTACH_MAX_DEPTH];
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
    stack->stack[0].cont = NULL;
    stack->stack[0].obj = stack->root;
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
static int sel(void *closure, const char *name)
{
    TRACE_ENTRY(sel);
    struct tstack_s *stack = closure;
#ifdef DEBUGGING
    log_trace("key: %s", name);
    log_trace("stack->depth: %d", stack->depth);
    log_trace("stack->stack[stack->depth].obj %p", stack->stack[stack->depth].obj);
    log_trace("predicate: %d", stack->predicate);
    /* DUMP_CLOSURE(e, stack->depth); */
#endif
    cJSON *o;
    int i, r;

    if (name == NULL) {
        o = stack->stack[stack->depth].obj;
        r = 1;
    } else {
        i = stack->depth;
        while (i >= 0 && !(o = cJSON_GetObjectItemCaseSensitive(stack->stack[i].obj, name)))
            i--;
        if (i >= 0)
            r = 1;
        else {
            o = &stack->null;
            r = 0;
        }
    }
    stack->selection = o;
    return r;
}

static int subsel(void *closure, const char *name)
{
	struct tstack_s *stack = closure;
	cJSON *o;
	int r;

	o = cJSON_GetObjectItemCaseSensitive(stack->selection, name);
	r = o != NULL;
	if (r)
		stack->selection = o;
	return r;
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
		e->stack[e->depth].cont = o;
		e->stack[e->depth].is_objiter = 1;
	} else if (cJSON_IsArray(o)) {
		if (o->child == NULL)
			goto not_entering;
		e->stack[e->depth].obj = o->child;
		e->stack[e->depth].next = o->child->next;
		e->stack[e->depth].cont = o;
	} else if ((cJSON_IsObject(o) && o->child == NULL) || (! cJSON_IsFalse(o) && ! cJSON_IsNull(o))) {
		e->stack[e->depth].obj = o;
		e->stack[e->depth].cont = NULL;
		e->stack[e->depth].next = NULL;
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
    const char *s;

    if (key) {
        s = stack->stack[stack->depth].is_objiter
            ? stack->stack[stack->depth].obj->string
            : "";
    }
    else if (cJSON_IsString(stack->selection))
        s = stack->selection->valuestring;
    else if (cJSON_IsNumber(stack->selection)) {
        if (fmt) {
            //FIXME: format
        } else {
        }
        s = cJSON_PrintUnformatted(stack->selection);
        if (s == NULL)
            return MUSTACH_ERROR_SYSTEM;
        sbuf->freecb = free;
    }
    else if (cJSON_IsNull(stack->selection))
        s = "";
    else {
        s = cJSON_PrintUnformatted(stack->selection);
        if (s == NULL)
            return MUSTACH_ERROR_SYSTEM;
        sbuf->freecb = free;
    }
    sbuf->value = s;
    return 1;
}

/* **************************************************************** */
static void dump_stack(struct tstack_s *stack)
{
    /* struct tstack_s *e = closure; */
    int d = stack->depth;
    log_debug("DUMP_CLOSURE");
    log_debug("\tpredicate: %d", stack->predicate);
    log_debug("\tlambda: %d", stack->lambda);
    log_debug("\tdepth: %d", d);
    TRACE_CJSON_DUMP("\troot", stack->root);
    TRACE_CJSON_DUMP("\tselection", stack->selection);
    if (stack->stack[d].cont)
        TRACE_CJSON_DUMP("\tstack->stack[%d].cont", stack->stack[d].cont);
    else
        log_debug("ctx: ?");
    if (stack->stack[d].obj)
        TRACE_CJSON_DUMP("\tstack->stack[%d].obj", stack->stack[d].obj);
    else
        log_debug("obj: ?");
    log_debug("\tstack->stack[%d].count: %d", d, stack->stack[d].count);
    log_debug("\tstack->stack[%d].index: %d", d, stack->stack[d].index);
    log_debug("\tstack->stack[%d].lambda: %d", d, stack->stack[d].lambda);
    log_debug("\tstack->stack[%d].predicate: %d", d, stack->stack[d].predicate);
    log_debug("end closure");
    fflush(NULL);
}

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

/* ****************************************************************
 * PUBLIC RENDER API
 **************************************************************** */
const char *mustache_cjson_render(const char *template,
                                 size_t template_sz,
                                 cJSON *root,
                                 int _flags)
{

    (void)template;
    (void)template_sz;
    (void)root;
    (void)_flags;
    return NULL;
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

void *mustach_encode_cjson(char *json_str, size_t len)
{
    cJSON *json_data = cJSON_ParseWithLength(json_str, len);
    return json_data;
}

void mustach_free(void *json_c)
{
    cJSON_Delete((cJSON*)json_c);
}

int mustach_cJSON_file(const char *template, size_t length, cJSON *root, int flags, FILE *file)
{
	struct tstack_s e;
	e.root = root;
	return mustach_json_file(template, length, &cjson_methods, &e, flags, file);
}

int mustach_cJSON_fd(const char *template, size_t tlength,
                     cJSON *root, int flags, int fd)
{
	struct tstack_s e;
	e.root = root;
	return mustach_json_fd(template, tlength, &cjson_methods, &e, flags, fd);
}

size_t mustach_asprintf(char **ret,
                    const char *template, size_t tlength,
                    void *json_root,
                    int data_schema, // JSON or SCHEME
                    int flags)
{
    (void)data_schema;
    struct tstack_s e;
    e.root = (cJSON*)json_root;
    size_t size;
    int rc = mustach_json_mem(template, tlength,
                              &cjson_methods, &e,
                              flags? flags
                              : Mustach_With_AllExtensions,
                              ret,
                              &size);
    if (rc < 0) {
        log_error("mustach_json_mem failure: %s", strerror(errno));
        return rc;
    } else {
        return size;
    }
}

int mustach_cJSON_mem(const char *template, size_t length, cJSON *root, int flags, char **result, size_t *size)
{
	struct tstack_s e;
	e.root = root;
	return mustach_json_mem(template, length, &cjson_methods, &e, flags, result, size);
}

int mustach_cJSON_write(const char *template, size_t length, cJSON *root, int flags, mustach_write_cb_t *writecb, void *closure)
{
	struct tstack_s e;
	e.root = root;
	return mustach_json_write(template, length, &cjson_methods, &e, flags, writecb, closure);
}

int mustach_cJSON_emit(const char *template, size_t length, cJSON *root, int flags, mustach_emit_cb_t *emitcb, void *closure)
{
	struct tstack_s e;
	e.root = root;
	return mustach_json_emit(template, length, &cjson_methods, &e, flags, emitcb, closure);
}

