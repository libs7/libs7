/* #define _GNU_SOURCE */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "log.h"

#include "mustach.h"
/* #include "mustach-json.h" */
#include "mustachios7_cjson.h"
#include "trace.h"
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

static int start(void *closure)
{
	struct tstack_s *e = closure;
	e->depth = 0;
	memset(&e->null, 0, sizeof e->null);
	e->null.type = cJSON_NULL;
	e->selection = &e->null;
	e->stack[0].cont = NULL;
	e->stack[0].obj = e->root;
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
    cJSON *o;
    int i, r;

    if (name == NULL) {
        o = e->stack[e->depth].obj;
        r = 1;
    } else {
        i = e->depth;
        while (i >= 0 && !(o = cJSON_GetObjectItemCaseSensitive(e->stack[i].obj, name)))
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
	cJSON *o;
	int r;

	o = cJSON_GetObjectItemCaseSensitive(e->selection, name);
	r = o != NULL;
	if (r)
		e->selection = o;
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
    TRACE_CJSON_DUMP("\troot", e->root);
    TRACE_CJSON_DUMP("\tselection", e->selection);
    if (e->stack[d].cont)
        TRACE_CJSON_DUMP("\te->stack[%d].cont", e->stack[d].cont);
    else
        log_debug("ctx: ?");
    if (e->stack[d].obj)
        TRACE_CJSON_DUMP("\te->stack[%d].obj", e->stack[d].obj);
    else
        log_debug("obj: ?");
    log_debug("\te->stack[%d].count: %d", d, e->stack[d].count);
    log_debug("\te->stack[%d].index: %d", d, e->stack[d].index);
    log_debug("\te->stack[%d].lambda: %d", d, e->stack[d].lambda);
    log_debug("\te->stack[%d].predicate: %d", d, e->stack[d].predicate);
    log_debug("end closure");
    fflush(NULL);
}

const struct mustach_wrap_itf mustach_wrap_itf_json = {
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

int mustach_fprintf(FILE * restrict file,
                    const char * restrict template, size_t tlength,
                    void *json_root,
                    int data_schema,
                    int flags)
{
    (void)data_schema;
    struct tstack_s e;

    e.root = (cJSON*)json_root;
    return mustach_json_file(template, tlength, &mustach_wrap_itf_json, &e, flags, file);
}

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
	return mustach_json_file(template, length, &mustach_wrap_itf_json, &e, flags, file);
}

int mustach_cJSON_fd(const char *template, size_t tlength,
                     cJSON *root, int flags, int fd)
{
	struct tstack_s e;
	e.root = root;
	return mustach_json_fd(template, tlength, &mustach_wrap_itf_json, &e, flags, fd);
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
                              &mustach_wrap_itf_json, &e,
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
	return mustach_json_mem(template, length, &mustach_wrap_itf_json, &e, flags, result, size);
}

int mustach_cJSON_write(const char *template, size_t length, cJSON *root, int flags, mustach_write_cb_t *writecb, void *closure)
{
	struct tstack_s e;
	e.root = root;
	return mustach_json_write(template, length, &mustach_wrap_itf_json, &e, flags, writecb, closure);
}

int mustach_cJSON_emit(const char *template, size_t length, cJSON *root, int flags, mustach_emit_cb_t *emitcb, void *closure)
{
	struct tstack_s e;
	e.root = root;
	return mustach_json_emit(template, length, &mustach_wrap_itf_json, &e, flags, emitcb, closure);
}

