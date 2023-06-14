/* #define _GNU_SOURCE */

#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/errno.h>

#include "config.h"
#include "log.h"

#include "mustach.h"
#include "mustache_tomlc99.h"
#ifdef DEVBUILD
#include "ansi_colors.h"
#endif

/*
  tomlc99 data: toml_datum_t, toml_table_t, or toml_array_t
  json impls use a single struct as a ∪nion of types, and
  call a function to get the type.
  with tomlc99 we cannot do that - toml_datum_t is a ∪ without
  a type indicator.  so we need a union a type tag?
 */

/* stack of template sections? */
struct tstack_s {
    int predicate;
    bool lambda;
    struct tomlx_item_s *root;
    struct tomlx_item_s *selection;
    int depth;
    struct {
        struct tomlx_item_s *ctx;
        struct tomlx_item_s *obj; /* bound value of selected item? */

        /* map iteration: same as iterating over array, except for
           each idx, we use use toml_key_in(tbl, idx) to obtain the
           key, and we need to store it since tomlc99 does not provide
           a mapitem (kv pair).
        */
        size_t index, count;
        /* struct tomlx_item_s *key; */
        const char *key;
        struct tomlx_item_s *val;
        int iterating;

        int predicate;         /*  */
        bool lambda;            /*  */
        int  workbuf_idx;
    } stack[MUSTACH_MAX_DEPTH];
};

char fmt_str[512];

static void dump_stack(struct tstack_s *stack);

void mustache_toml_free(void *datum)
{
    toml_free((toml_table_t*)datum);
}

static int start(struct tstack_s *stack)
{
    TRACE_ENTRY(start);
    /* struct tstack_s *stack = closure; */
    stack->depth = 0;
    stack->selection = NULL;
    stack->stack[0].ctx = NULL;
    stack->stack[0].obj = stack->root;
    stack->stack[0].index = 0;
    stack->stack[0].count = 1;
    return MUSTACH_OK;
}

static int compare(void *closure, const char *value)
{
    TRACE_ENTRY(compare);
    struct tstack_s *e = closure;
    struct tomlx_item_s *o = e->selection;
    double d;

    switch(o->type) {
    case TOML_STRING:
        return strcmp(o->u.s, value);
        break;
    case TOML_INT:
        //FIXME: use strtol?
        d = o->u.i - atoi(value);
        return d < 0 ? -1 : d > 0 ? 1 : 0;
        break;
    case TOML_DOUBLE:
        //FIXME: use strtod?
        d = o->u.d - atof(value);
        return d < 0 ? -1 : d > 0 ? 1 : 0;
        break;
    case TOML_BOOL:
        //???
        break;
    case TOML_TIMESTAMP:
        //??
        break;

        /* case TOML_TABLE: */
        /*     break; */
        /* case TOML_ARRAY: */
        /*     break; */
    default:
        ;
    }
    return 0; //FIXME
}

/* maybe copies value for key to stack->selection */
static int sel(void *closure, const char *key)
{
    TRACE_ENTRY(sel);
    struct tstack_s *stack = (struct tstack_s*)closure;
#ifdef DEVBUILD
    log_trace("key: %s", key);
    log_trace("stack->depth: %d", stack->depth);
    /* log_trace("stack->stack[stack->depth].obj type: %d", */
    /*           stack->stack[stack->depth].obj->type); */
    log_trace("predicate: %d", stack->predicate);
    /* DUMP_CLOSURE(e, stack->depth); */
#endif

    struct tomlx_item_s *selection;
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
                stack->stack[stack->depth].predicate = stack->predicate;
                if (stack->stack[stack->depth].index == 0) {
#ifdef DEVBUILD
                    log_debug("predicate is truthy!");
#endif
                    bool b = true;
                    struct tomlx_item_s *titem = tomlx_make_item(&b, TOML_BOOL);

                    selection = titem;
                } else {
#ifdef DEVBUILD
                    log_debug("predicate is false!");
#endif
                    bool b = false;
                    struct tomlx_item_s *titem = tomlx_make_item(&b, TOML_BOOL);

                    selection = titem;
                }
                r = 1;
                break;
            case LAST_P:
#ifdef DEVBUILD
                log_debug("case: LAST_P");
#endif
                stack->stack[stack->depth].predicate = stack->predicate;
                if (stack->stack[stack->depth].index + 1 < stack->stack[stack->depth].count) {
#ifdef DEVBUILD
                    log_debug("predicate is false!");
#endif
                    bool b = false;
                    struct tomlx_item_s *titem = tomlx_make_item(&b, TOML_BOOL);
                    selection = titem;
                } else {
#ifdef DEVBUILD
                    log_debug("predicate is truthy!");
#endif
                    bool b = true;
                    struct tomlx_item_s *titem = tomlx_make_item(&b, TOML_BOOL);
                    selection = titem;
                }
                r = 1;
                break;
            case BUTLAST_P:
#ifdef DEVBUILD
                log_debug("case: BUTLAST_P");
#endif
                stack->stack[stack->depth].predicate = stack->predicate;
                // true for all but last
                if (stack->stack[stack->depth].index + 1 < stack->stack[stack->depth].count) {
                    /* s7_pointer tmp = s7_vector_ref(s7, stack->stack[stack->depth].cont, stack->stack[stack->depth].index); */
                    /* (void)tmp; */
#ifdef DEVBUILD
                    log_debug("predicate {{?}} is truthy!");
#endif
                    bool b = true;
                    struct tomlx_item_s *titem = tomlx_make_item(&b, TOML_BOOL);
                    selection = titem;
                } else {
#ifdef DEVBUILD
                    log_debug("pred {{?}} is false!");
#endif
                    bool b = false;
                    struct tomlx_item_s *titem = tomlx_make_item(&b, TOML_BOOL);
                    selection = titem;
                }
                r = 1;
                break;
            default:
                {bool b = false;
                struct tomlx_item_s *titem = tomlx_make_item(&b, TOML_BOOL);
                selection = titem;
                r = 1;
                }
            }
        }
        else {
            TRACE_LOG_DEBUG("NULL key", "");
            // if this is {{#.}}, select entire vec/map
            // if  its {{.}} and we're in {{#.}}, select (k v)
            // otherwise select v
            selection = stack->stack[stack->depth].obj;
            r = 1;
        }
    } else {
        i = stack->depth;
        TRACE_LOG_DEBUG("searching stack for key %s", key);
        /*
          SPECIAL CASE: root table objiter: {{#^.*}}...{{/^.*}}
         */
        if ((strncmp(key, "^", 1) == 0) && strlen(key) == 1) {
            selection = stack->stack[i].obj;
        } else {
            while (i >= 0) {
                TRACE_LOG_DEBUG("stackframe: %d", i);
                if (stack->stack[i].obj->type == TOML_TABLE) {
                    selection = tomlx_table_ref(stack->stack[i].obj->u.t, key);
                    if (selection) {
                        TRACE_LOG_DEBUG("selected table item at: %d", i);
                        /* if (selection->type == TOML_BOOL) */
                        /*     log_debug("bool: %d", selection->u.b); */
                        break;
                    }
                    else {
                        /* log_debug("miss"); */
                        /* log_debug("\tkey: %s", stack->stack[i].key); */
                        /* dump_stack(stack); */
                        /* if (stack->stack[i].key) { */
                        /*     if (strncmp(key, stack->stack[i].key, */
                        /*                 strlen(key))) { */
                        /*         selection = stack->stack[i].val; */
                        /*         break; */
                        /*     } */
                        /* } */
                    }
                }
                i--;
            }
        }
        if (i >= 0)
            r = 1;
        else {
            TRACE_LOG_DEBUG("NOT found", "");
            selection = NULL;
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
    struct tomlx_item_s *o;
    int r;
    (void)r; //FIXME

    /* dump_stack(e); */
    if (stack->selection->type == TOML_TABLE) {
        o = tomlx_table_ref(stack->selection->u.t, name);
        if (o) {
            TRACE_LOG_DEBUG("setting selection to val for key %s", name);
            stack->selection = o;
            return 1;
        } else
            return 0;
    } else {
        log_error("SUBSEL selection is not a table!");
        log_debug("type: %d", stack->selection->type);
        return 0;
    }
}

static int enter(void *closure, int objiter)
{
    TRACE_ENTRY(enter);
    TRACE_LOG_DEBUG("objiter: %d", objiter);
    struct tstack_s *e = closure;
    /* struct tomlx_item_s *o; */

    if (++e->depth >= MUSTACH_MAX_DEPTH)
        return MUSTACH_ERROR_TOO_DEEP;
    /* log_debug("depth: %d", e->depth); */
    /* o = e->selection; */
    e->stack[e->depth].iterating = 0;
    if (objiter) {
        if (e->selection->type != TOML_TABLE)
            goto not_entering;
        size_t fld_ct = tomlx_table_length(e->selection->u.t);
        if (fld_ct == 0)
            goto not_entering;
        e->stack[e->depth].count = fld_ct;
        e->stack[e->depth].index = 0;
        // obtain first mapitem
        const char *key = toml_key_in(e->selection->u.t, 0);
        e->stack[e->depth].key = key;
        struct tomlx_item_s *item = tomlx_table_ref(e->selection->u.t, key);
        e->stack[e->depth].obj = item;
        e->stack[e->depth].ctx = e->selection;
        e->stack[e->depth].iterating = 1;
    } else if (e->selection->type == TOML_ARRAY) {
        TRACE_LOG_DEBUG("OPENING ARRAY, sz: %d", toml_array_nelem(e->selection->u.a));
        e->stack[e->depth].count = toml_array_nelem(e->selection->u.a);
        if (e->stack[e->depth].count == 0) {
            /* log_debug("EMPTY ARRAY: NOT OPENING"); */
            goto not_entering;
        }
        e->stack[e->depth].ctx = e->selection;
        struct tomlx_item_s *x = tomlx_array_ref(e->selection->u.a, 0);
        /* log_debug("ARRAY[%d] type: %d", e->depth, x->type); */
        e->stack[e->depth].obj = x;
        //NB: helper_toml_array_ref must create new toml_item_t
        e->stack[e->depth].index = 0;
        /* e->stack[e->depth].key = tomlx_make_item(&(e->stack[e->depth].index), */
        /*                                          TOML_INT); */
        /* e->stack[e->depth].val = x; */
        /* log_debug("stack p: %p", e); */
        /* log_debug("stack[%d] ct: %d", e->depth, e->stack[e->depth].count); */
        /* dump_stack(e); */
    } else if (e->selection->type == TOML_TABLE) {
               /* && tomlx_table_is_empty(e->selection->u.t)) { */
        TRACE_LOG_DEBUG("OPENING TABLE", "");
        /* && ! cJSON_IsNull(e->selection))) { */
        int len = tomlx_table_length(e->selection->u.t);
        e->stack[e->depth].count = len;
        const char *k = toml_key_in(e->selection->u.t, 0);
        struct tomlx_item_s *val = tomlx_table_ref(e->selection->u.t, k);
        e->stack[e->depth].key = k;
        e->stack[e->depth].val = val;
        e->stack[e->depth].obj = e->selection; // val;
        e->stack[e->depth].ctx = e->selection;
        e->stack[e->depth].index = 0;
    } else if ( !((e->selection->type == TOML_BOOL) && (e->selection->u.b == false))) {
        TRACE_LOG_DEBUG("OPENING NON-FALSE, depth: %d", e->depth);
        /* && ! cJSON_IsNull(e->selection))) { */
        e->stack[e->depth].count = 1;
        e->stack[e->depth].index = 0;
        e->stack[e->depth].obj = e->selection;
        /* e->stack[e->depth].key = tomlx_make_item(&(e->stack[e->depth].index), */
        /*                                          TOML_INT); */
        /* e->stack[e->depth].val = e->selection; */
        e->stack[e->depth].ctx = NULL;
    } else {
        goto not_entering;
    }
    /* log_debug("returniing: 1"); */
    /* log_debug("stack ct: %d", e->stack[e->depth].count); */
    return 1;

 not_entering:
    /* log_debug("NOT ENTERING"); */
    /* free(e->stack[e->depth].obj); */
    /* free(e->stack[e->depth].key); */
    /* free(e->stack[e->depth].val); */
    /* free(e->stack[e->depth].ctx); */

    e->depth--;
    return 0;
}

static int next(void *closure)
{
    TRACE_ENTRY(next);
    struct tstack_s *e = closure;
    struct tomlx_item_s *o = e->stack[e->depth].obj;
    struct tomlx_item_s *ctx = e->stack[e->depth].ctx;

    if (e->depth <= 0) {
        fprintf(stderr, "ERR: next\n");
        return MUSTACH_ERROR_CLOSING;
    }

    if (e->stack[e->depth].iterating) {
        // just like array, advance index
        if (ctx->type != TOML_TABLE) {
            log_error("Unexpected type for objiter: %d", o->type);
        }
        e->stack[e->depth].index++;
        if (e->stack[e->depth].index >= e->stack[e->depth].count)
            return 0;
        // set key, obj to next mapitem
        const char *key = toml_key_in(ctx->u.t, e->stack[e->depth].index);
        /* log_debug("Next key: %s", key); */
        e->stack[e->depth].key = key;
        struct tomlx_item_s *item = tomlx_table_ref(ctx->u.t, key);
        //FIXME: free current obj?
        e->stack[e->depth].obj = item;
        /* e->stack[e->depth].iter = json_object_iter_next(e->stack[e->depth].cont, e->stack[e->depth].iter); */
        /* if (e->stack[e->depth].iter == NULL) */
        /*     return 0; */
        /* e->stack[e->depth].obj = json_object_iter_value(e->stack[e->depth].iter); */
        return 1;
    }

    e->stack[e->depth].index++;
    if (e->stack[e->depth].index >= e->stack[e->depth].count)
        return 0;

    if (e->stack[e->depth].ctx->type == TOML_ARRAY) {
        e->stack[e->depth].obj
            = tomlx_array_ref(e->stack[e->depth].ctx->u.a,
                              e->stack[e->depth].index);
    }
    else if (e->stack[e->depth].ctx->type == TOML_TABLE) {
        // only if iterating???
        if (e->stack[e->depth].iterating) {
            const char *k = toml_key_in(e->stack[e->depth].ctx->u.t,
                                        e->stack[e->depth].index);
            e->stack[e->depth].key = k;
            e->stack[e->depth].val
                = tomlx_table_ref(e->stack[e->depth].ctx->u.t, k);
            e->stack[e->depth].obj = e->stack[e->depth].val;
        } else {
            return 0;
        }
    }
        /* = tomlx_array_datum_for_idx(e->stack[e->depth].ctx, */
        /*                             e->stack[e->depth].index); */
    return 1;
}

static int leave(void *closure, struct mustach_sbuf *sbuf)
/* static int leave(void *closure) */
{
    TRACE_ENTRY(leave);
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
{
    TRACE_ENTRY(format);
    TRACE_LOG_DEBUG("key: %d", key);
    TRACE_LOG_DEBUG("fmt: %s", fmt);
    TRACE_LOG_DEBUG("iterating?: %d", stack->stack[stack->depth].iterating);
    TRACE_LOG_DEBUG("stack->selection type: %d", stack->selection->type);
    const char *s = NULL;
    char *work;
    int len;

    if (key) {
        if (stack->stack[stack->depth].iterating) {
            // int key should be index of current mapitem?
            // or any non-zero?
            s = stack->stack[stack->depth].key;
        } else {
            s = stack->stack[stack->depth].key;
            /* s ="XFOO"; */
        }
        // jansson:
        /* s = e->stack[e->depth].iterating */
        /*     ? json_object_iter_key(e->stack[e->depth].iter) */
	/* 		: ""; */
    } else {
        struct tomlx_item_s *o = stack->selection;
        switch(o->type) {
        case TOML_STRING:
            TRACE_LOG_DEBUG("formatting string: %s", o->u.s);
            s = o->u.s; //FIXME: strdup?
            break;
        case TOML_TABLE:
            TRACE_LOG_DEBUG("formatting table", "");
            if (tomlx_table_is_empty(o->u.t))
                s = "";
            else
                s = tomlx_table_to_string(o->u.t, PRINT_SYNTAX_TOML_RAW);
            break;
        case TOML_ARRAY:
            TRACE_LOG_DEBUG("formatting array", "");
            // string elts should be "displayed", printed w/o quotes
            /* if (_array_is_empty(o-u.a)) { */
            /* } */
            if (toml_array_nelem(o->u.a) == 0)
                s = "";
            else
                s = tomlx_array_to_string(o->u.a, PRINT_SYNTAX_TOML_RAW);
            break;
            /* log_debug("array: %s", s); */
            break;
        case TOML_BOOL:
            if (o->u.b) {
                s = "true";
            } else {
                s = "false";
            }
            break;
        case TOML_INT:
            TRACE_LOG_DEBUG("formatting int: %lld", o->u.i);
            if (fmt) {
                len = snprintf(NULL, 0, fmt, o->u.i);
                TRACE_LOG_DEBUG("LEN: %d", len);
                work = malloc(len+1);
                snprintf(work, len+1, fmt, o->u.i);
                TRACE_LOG_DEBUG("formatted int: %s", work);
                s = strndup(work, len+1);
            } else {
                len = snprintf(NULL, 0, "%lld", o->u.i);
                char *work = malloc(len+1);
                snprintf(work, len+1, "%lld", o->u.i);
                s = strndup(work, len+1);
            }
            sbuf->freecb = free;
            break;
        case TOML_DOUBLE:
            TRACE_LOG_DEBUG("formatting double: %lf", o->u.d);
            if (fmt) {
                TRACE_LOG_DEBUG("format string: %s", fmt);
                len = snprintf(NULL, 0, fmt, o->u.d);
                TRACE_LOG_DEBUG("LEN: %d", len);
                work = malloc(len+1);
                snprintf(work, len+1, fmt, o->u.d);
                s = strndup(work, len+1);
            } else {
                len = snprintf(NULL, 0, "%#g", o->u.d);
                char *work = malloc(len+1);
                snprintf(work, len+1, "%g", o->u.d);
                s = strndup(work, len+1);
            }
            sbuf->freecb = free;
            break;
        case TOML_TIMESTAMP:
            TRACE_LOG_DEBUG("formatting ts: %p", o->u.ts);
            if (fmt) {
                TRACE_LOG_DEBUG("format string: %s", fmt);
                s = tomlx_format_datetime(o->u.ts, fmt);
                /* len = snprintf(NULL, 0, fmt, o->u.ts); */
                /* TRACE_LOG_DEBUG("LEN: %d", len); */
                /* work = malloc(len+1); */
                /* snprintf(work, len+1, fmt, o->u.ts); */
                /* s = strndup(work, len+1); */
            } else {
                s = tomlx_datetime_to_string(o->u.ts, PRINT_SYNTAX_TOML);
            }
            /* log_debug("ts: %s", s); */
            sbuf->freecb = free;
            break;
        default:
            log_error("Bad arg");
            s = "";
        }
    }
    //FIXME sbuf->freecb ??
    sbuf->value = s;
    return 1;
}

static void _dump_obj(char *msg, struct tomlx_item_s *item)
{
    char *s;
    log_debug(msg);
    if (item) {
        switch(item->type) {
        case TOML_BOOL:
            log_debug("\t  bool: %d", item->u.b);
            /* s = item->u.b? strdup("true") : strdup("false"); */
            break;
        case TOML_INT:
            log_debug("\t  int: %lld", item->u.i);
            break;
        case TOML_DOUBLE:
            log_debug("\t  double: %lld", item->u.d);
            break;
        case TOML_STRING:
            log_debug("\t  string: %s", item->u.s);
            break;
        case TOML_ARRAY:
            ; \
            s = tomlx_array_to_string(item->u.a, PRINT_SYNTAX_TOML);
            log_debug("\t    %s", s);
            free(s);
            break;
        case TOML_TABLE:
            ;
            s = tomlx_table_to_string(item->u.t, PRINT_SYNTAX_TOML);
            log_debug("\t    %s", s);
            free(s);
            break;
        default:
            log_debug("\t\ttype: %d", item->type);
        }
    } else
        log_debug("\t  NULL");
}

static void dump_stack(struct tstack_s *stack)
{
    /* log_debug("stack ptr: %p", stack); */
    int d = stack->depth;
    log_debug("DUMP_STACK");
    log_debug("\tpredicate: %d", stack->predicate);
    log_debug("\tlambda: %d", stack->lambda);
    log_debug("\tdepth: %d", d);

    _dump_obj("\troot:", stack->root);
    _dump_obj("\tselection:", stack->selection);
    for (int i = 0; i <= stack->depth; i++) {
        log_debug("stackframe: %d", i);
        _dump_obj("\tctx:", stack->stack[i].ctx);
        _dump_obj("\tobj", stack->stack[i].obj);
        log_debug("\tkey: %s", stack->stack[i].key);
        _dump_obj("\tval", stack->stack[i].val);
        log_debug("\tstack[%d].iterating: %d", i, stack->stack[i].iterating);
        log_debug("\tstack[%d].count: %d", i, stack->stack[i].count);
        log_debug("\tstack[%d].index: %d", i, stack->stack[i].index);
        log_debug("\tstack[%d].lambda: %d", i, stack->stack[i].lambda);
        log_debug("\tstack[%d].predicate: %d", i, stack->stack[i].predicate);
    }
    log_debug("end stack");
    fflush(NULL);
}

const struct mustach_ds_methods_s tomlc99_methods = {
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
/*
  mustach_toml_render   -  returns string
  mustach_toml_frender  - to stream (FILE*)
  mustach_toml_fdrender - to fd
  mustach_toml_snrender - to buffer

 */

/* called by application: */
const char *mustache_toml_render(const char *template,
                                 size_t template_sz,
                                 toml_table_t *root,
                                 int _flags)
{
    struct tomlx_item_s *titem = tomlx_make_item(root, TOML_TABLE);
    struct tstack_s stack;
    memset(&stack, 0, sizeof(struct tstack_s));
    stack.root = titem;

    //FIXME: client responsible for flags
    int flags = Mustach_With_All_TOML_Extensions;
    /* flags &= ~Mustach_With_JsonPointer; */
    (void)_flags;
    //FIXME: _flags arg is for opting out of default 'all'

    /* log_debug("Flags: 0x%02x", flags); */
    /* log_debug("MWAE: 0x%02x", Mustach_With_All_TOML_Extensions); */

    errno = 0;
    char *result = mustach_toml_render_to_string(template, template_sz,
                                                 &tomlc99_methods,
                                                 &stack,
                                                 flags);
    if (!result) {
        log_error("mustach_toml_render_to_string");
        switch (errno) {
        case MUSTACH_ERROR_UNEXPECTED_END:
            log_error("Unexpected end. Check your close tags to verify that you have not omitted a delimiter, and that section tags starting with '#', '~', etc. are matched by close tags starting with '/'. ");
            break;
        default:
            log_error("RC: %d", errno);
        }
        //FIXME handle error
        return NULL;
    }
    /* size_t ln = strlen(result); */
    /* if (ln != result_sz) { */
    /*     log_warn("reported result sz %d does not match strlen %d", */
    /*              result_sz, ln); */
    /* } */
    /* log_debug("render result: %s", result); */

    //FIXME: toml_item_free(titem);

    return result; // client must free
}

int mustache_toml_frender(FILE * restrict file,
                          const char *template,
                          size_t template_sz,
                          toml_table_t *root,
                          int _flags)
{
    TRACE_ENTRY(mustache_toml_frender);
    struct tomlx_item_s *titem = tomlx_make_item(root, TOML_TABLE);
    struct tstack_s stack;
    memset(&stack, 0, sizeof(struct tstack_s));
    stack.root = titem;

    //FIXME: client responsible for flags
    int flags = Mustach_With_AllExtensions;
    flags &= ~Mustach_With_JsonPointer;
    (void)_flags;

    return mustach_toml_file(template, template_sz,
                             &tomlc99_methods,
                             &stack,
                             flags,
                             file);
}

int mustache_toml_fdrender(int fd,
                          const char *template,
                          size_t template_sz,
                          toml_table_t *root,
                          int _flags)
{
    TRACE_ENTRY(mustache_toml_fdrender);
    struct tomlx_item_s *titem = tomlx_make_item(root, TOML_TABLE);
    struct tstack_s stack;
    memset(&stack, 0, sizeof(struct tstack_s));
    stack.root = titem;

    //FIXME: client responsible for flags
    int flags = Mustach_With_AllExtensions;
    flags &= ~Mustach_With_JsonPointer;
    (void)_flags;
    (void)flags;
    (void)fd;
    (void)template;
    (void)template_sz;
    return -1;
}

/* render to string buffer */
/* int mustache_toml_srender(char *buf, */
/*                           const char *template, size_t length, */
/*                           struct tomlx_item_s *root, int flags) */
/* { */
/* 	/\* struct tstack_s stack; *\/ */
/* 	/\* stack.root = root; *\/ */
/* 	/\* return mustach_toml_file(template, length, *\/ */
/*         /\*                          &tomlc99_methods, *\/ */
/*         /\*                          &stack, flags, *\/ */
/*         /\*                          file); *\/ */
/* } */

/* /\* render to file stream (FILE*) *\/ */
/* int mustache_toml_frender(FILE *file, */
/*                           const char *template, size_t length, */
/*                           struct tomlx_item_s *root, int flags) */
/* { */
/* 	struct tstack_s stack; */
/* 	stack.root = root; */
/* 	return mustach_toml_file(template, length, */
/*                                  &tomlc99_methods, */
/*                                  &stack, flags, */
/*                                  file); */
/* } */

/* /\* render to file descriptor *\/ */
/* int mustache_toml_fdrender(int fd, */
/*                            const char *template, size_t tlength, */
/*                            struct tomlx_item_s *root, int flags) */
/* { */
/* 	struct tstack_s stack; */
/* 	stack.root = root; */
/* 	return mustach_toml_fd(template, tlength, */
/*                                &tomlc99_methods, */
/*                                &stack, */
/*                                flags, fd); */
/* } */

/* int mustach_tomlc99_write(const char *template, size_t length, */
/*                           struct tomlx_item_s *root, int flags, */
/*                           mustach_write_cb_t *writecb, void *closure) */
/* { */
/* 	struct tstack_s e; */
/* 	e.root = root; */
/* 	return mustach_toml_write(template, length, &tomlc99_methods, &e, flags, writecb, closure); */
/* } */

/* int mustach_tomlc99_emit(const char *template, size_t length, */
/*                          struct tomlx_item_s *root, int flags, */
/*                          mustach_emit_cb_t *emitcb, void *closure) */
/* { */
/* 	struct tstack_s e; */
/* 	e.root = root; */
/* 	return mustach_toml_emit(template, length, &tomlc99_methods, */
/*                                  &e, flags, emitcb, closure); */
/* } */

/* **************************************************************** */
