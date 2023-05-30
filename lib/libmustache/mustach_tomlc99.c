/* #define _GNU_SOURCE */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "log.h"

#include "mustach.h"
#include "mustach_tomlc99.h"
#include "trace.h"
#ifdef DEBUGGING
#include "ansi_colors.h"
#endif

/*
  tomlc99 data: toml_datum_t, toml_table_t, or toml_array_t
  json impls use a single struct as a ∪nion of types, and
  call a function to get the type.
  with tomlc99 we cannot do that - toml_datum_t is a ∪ without
  a type indicator.  so we need a union a type tag?
 */
#define TOML_BOOL      1
#define TOML_STRING    2
#define TOML_INT       3
#define TOML_DOUBLE    4
#define TOML_TIMESTAMP 5
#define TOML_ARRAY     6
#define TOML_TABLE     7
struct toml_item_s {
    union {
        toml_table_t     *t;
        toml_array_t     *a;
        toml_timestamp_t *ts; /* ts must be freed after use */
        char             *s; /* string value. s must be freed after use */
        int               b; /* bool value */
        int64_t           i; /* int value */
        double            d; /* double value */
    } u;
    int  type; // TOML_BOOL etc.
}

struct tstack_s {
    int predicate;
    bool lambda;
    struct toml_item_s *root;
    struct toml_item_s *selection;
    int depth;
    struct {
        struct toml_item_s *ctx;
        struct toml_item_s *obj;
        struct toml_item_s *next;
        int is_objiter;
        size_t index, count;    /* not for cjson? */
        int predicate;         /*  */
        bool lambda;            /*  */
        int  workbuf_idx;
    } stack[MUSTACH_MAX_DEPTH];
};

static int start(void *closure)
{
	struct tstack_s *e = closure;
	e->depth = 0;
	e->selection = NULL;
	e->stack[0].ctx = NULL;
	e->stack[0].obj = e->root;
	e->stack[0].index = 0;
	e->stack[0].count = 1;
	return MUSTACH_OK;
}

static int compare(void *closure, const char *value)
{
	struct tstack_s *e = closure;
	struct toml_item_s *o = e->selection;
	double d;

        switch(o->type) {
        case TOML_STRING:
            return strcmp(o->s, value);
            break;
        case TOML_INT:
            //FIXME: use strtol?
            d = o->i - atoi(value);
            return d < 0 ? -1 : d > 0 ? 1 : 0;
            break;
        case TOML_DOUBLE:
            //FIXME: use strtod?
            d = o->d - atof(value);
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
}

static int sel(void *closure, const char *name)
{
    TRACE_ENTRY(sel);
    struct tstack_s *e = closure;
#ifdef DEBUGGING
    log_trace("key: %s", name);
    log_trace("e->depth: %d", e->depth);
    log_trace("e->stack[e->depth].obj %p", e->stack[e->depth].obj);
    log_trace("predicate: %d", e->predicate);
    /* DUMP_CLOSURE(e, e->depth); */
#endif
    toml_item_s *o;
    int i, r;

    if (name == NULL) {
        o = e->stack[e->depth].obj;
        r = 1;
    } else {
        i = e->depth;
        // e->stack[i].o must be a table?
        while (i >= 0 && !(o = helper_toml_table_ref(e->stack[i].obj, name)))
            i--;
        if (i >= 0)
            r = 1;
        else {
            o = &e->null;
            r = 0;
        }
    }
    e->selection = o;
    return r;
}

static int subsel(void *closure, const char *name)
{
	struct tstack_s *e = closure;
	toml_item_s *o;
	int r;

	o = helper_toml_table_ref(e->selection, name);
        if (o) {
            e->selection = o;
            return 1;
        } else
            return 0;
	/* r = o != NULL; */
	/* if (r) */
	/* 	e->selection = o; */
	/* return r; */
}

static int enter(void *closure, int objiter)
{
	struct tstack_s *e = closure;
	toml_item_s *o;

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
		e->stack[e->depth].ctx = o;
		e->stack[e->depth].is_objiter = 1;
	} else if (o->type == TOML_ARRAY) {
		e->stack[e->depth].count = toml_array_nelem(o->a);
		if (e->stack[e->depth].count == 0)
			goto not_entering;
		e->stack[e->depth].ctx = o;
		e->stack[e->depth].obj = helper_toml_array_ref(o->a, 0);
                //NB: helper_toml_array_ref must create new toml_item_t
		e->stack[e->depth].index = 0;
	} else if (((o->type == TOML_TABLE)
                    && o->child == NULL)
                   || ((o->type == TOML_BOOL) && (o->b == true))) {
                   /* && ! cJSON_IsNull(o))) { */
		e->stack[e->depth].count = 1;
		e->stack[e->depth].obj = o;
		e->stack[e->depth].ctx = NULL;
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
    toml_item_s *o;

    if (e->depth <= 0) {
        fprintf(stderr, "ERR: next\n");
        return MUSTACH_ERROR_CLOSING;
    }
    // tomlc99 stores kvpairs, tables and arrays separately.
    // TODO: implement an iterator

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

static int get(void *closure, struct mustach_sbuf *sbuf, int key)
{
    TRACE_ENTRY(get);
	struct tstack_s *e = closure;
	const char *s;

	if (key) {
		s = e->stack[e->depth].is_objiter
			? e->stack[e->depth].obj->string
			: "";
	}
	else if (cJSON_IsString(e->selection))
		s = e->selection->valuestring;
	else if (cJSON_IsNull(e->selection))
		s = "";
	else {
		s = cJSON_PrintUnformatted(e->selection);
		if (s == NULL)
			return MUSTACH_ERROR_SYSTEM;
		sbuf->freecb = free;
	}
	sbuf->value = s;
	return 1;
}

/* **************************************************************** */
static void dump_closure(void *closure)
{
    struct tstack_s *e = closure;

    (void)closure;
    int d = e->depth;
    log_debug("DUMP_CLOSURE");
    log_debug("\tpredicate: %d", e->predicate);
    log_debug("\tlambda: %d", e->lambda);
    log_debug("\tdepth: %d", d);
    TRACE_TOMLC99_DUMP("\troot", e->root);
    TRACE_TOMLC99_DUMP("\tselection", e->selection);
    if (e->stack[d].ctx)
        TRACE_TOMLC99_DUMP("\te->stack[%d].ctx", e->stack[d].ctx);
    else
        log_debug("ctx: ?");
    if (e->stack[d].obj)
        TRACE_TOMLC99_DUMP("\te->stack[%d].obj", e->stack[d].obj);
    else
        log_debug("obj: ?");
    log_debug("\te->stack[%d].count: %d", d, e->stack[d].count);
    log_debug("\te->stack[%d].index: %d", d, e->stack[d].index);
    log_debug("\te->stack[%d].lambda: %d", d, e->stack[d].lambda);
    log_debug("\te->stack[%d].predicate: %d", d, e->stack[d].predicate);
    log_debug("end closure");
    fflush(NULL);
}

const struct mustach_wrap_itf mustach_wrap_itf_toml = {
	.start = start,
	.stop = NULL,
	.compare = compare,
	.sel = sel,
	.subsel = subsel,
	.enter = enter,
	.next = next,
	.leave = leave,
	.get = get,
        .dump_closure = dump_closure
};

/* int mustach_fprintf(FILE * restrict file, */
/*                     const char * restrict template, size_t tlength, */
/*                     void *json_root, */
/*                     int data_schema, */
/*                     int flags) */
/* { */
/*     (void)data_schema; */
/*     struct tstack_s e; */

/*     e.root = (cJSON*)json_root; */
/*     return mustach_tomlc99_file(template, tlength, &mustach_wrap_itf_json, &e, flags, file); */
/* } */

void *mustach_encode_toml(char *json_str, size_t len)
{
    toml_item_s *json_data = cJSON_ParseWithLength(json_str, len);
    return json_data;
}

void mustach_free(void *json_c)
{
    cJSON_Delete((cJSON*)json_c);
}

int mustach_tomlc99_file(const char *template, size_t length, toml_item_s *root, int flags, FILE *file)
{
	struct tstack_s e;
	e.root = root;
	return mustach_toml_file(template, length, &mustach_wrap_itf_json, &e, flags, file);
}

int mustach_tomlc99_fd(const char *template, size_t tlength,
                     toml_item_s *root, int flags, int fd)
{
	struct tstack_s e;
	e.root = root;
	return mustach_toml_fd(template, tlength, &mustach_wrap_itf_json, &e, flags, fd);
}

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
/*     int rc = mustach_toml_mem(template, tlength, */
/*                               &mustach_wrap_itf_json, &e, */
/*                               flags? flags */
/*                               : Mustach_With_AllExtensions, */
/*                               ret, */
/*                               &size); */
/*     if (rc < 0) { */
/*         log_error("mustach_toml_mem failure: %s", strerror(errno)); */
/*         return rc; */
/*     } else { */
/*         return size; */
/*     } */
/* } */

int mustach_tomlc99_mem(const char *template, size_t length, toml_item_s *root, int flags, char **result, size_t *size)
{
	struct tstack_s e;
	e.root = root;
	return mustach_toml_mem(template, length, &mustach_wrap_itf_json, &e, flags, result, size);
}

int mustach_tomlc99_write(const char *template, size_t length, toml_item_s *root, int flags, mustach_write_cb_t *writecb, void *closure)
{
	struct tstack_s e;
	e.root = root;
	return mustach_toml_write(template, length, &mustach_wrap_itf_json, &e, flags, writecb, closure);
}

int mustach_tomlc99_emit(const char *template, size_t length, toml_item_s *root, int flags, mustach_emit_cb_t *emitcb, void *closure)
{
	struct tstack_s e;
	e.root = root;
	return mustach_toml_emit(template, length, &mustach_wrap_itf_json, &e, flags, emitcb, closure);
}

