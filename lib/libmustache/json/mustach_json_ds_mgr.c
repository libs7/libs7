/*
 Author: José Bollo <jobol@nonadev.net>

 https://gitlab.com/jobol/mustach

 SPDX-License-Identifier: ISC
*/

#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifdef _WIN32
#include <malloc.h>
#endif

#include "config.h"
#include "log.h"
#include "mustach.h"
#include "mustach_json_ds_mgr.h"

#ifdef DEBUGGING
#include "ansi_colors.h"
/* #include "s7.h" */
//#include "debug.h"
/* extern s7_scheme *s7; */
/* s7_pointer xx; */
#endif

/* #if !defined(INCLUDE_PARTIAL_EXTENSION) */
/* # define INCLUDE_PARTIAL_EXTENSION ".mustache" */
/* #endif */

/* /\* global hook for partials *\/ */
/* int (*mustach_wrap_get_partial)(const char *name, struct mustach_sbuf *sbuf) = NULL; */

/* /\* internal structure for wrapping *\/ */
// - renamed struct datasource_s, in mustach_wrap.h
/* struct wrap { */
/*     int predicate; /\* so mustach.c can signal a predicate metatag *\/ */

/* 	/\* original interface *\/ */
/* 	const struct mustach_wrap_itf *itf; */

/* 	/\* original closure *\/ */
/* 	void *closure; */

/* 	/\* flags *\/ */
/* 	int flags; */

/* 	/\* emiter callback *\/ */
/* 	mustach_emit_cb_t *emitcb; */

/* 	/\* write callback *\/ */
/* 	mustach_write_cb_t *writecb; */
/* }; */

/* /\* length given by masking with 3 *\/ */
/* enum comp {                     /\* 'comp' means relop *\/ */
/* 	C_no = 0, */
/* 	C_eq = 1, */
/* 	C_lt = 5, */
/* 	C_le = 6, */
/* 	C_gt = 9, */
/* 	C_ge = 10 */
/* }; */

/* enum sel { */
/* 	S_none = 0, */
/* 	S_ok = 1, */
/* 	S_objiter = 2, */
/* 	S_ok_or_objiter = S_ok | S_objiter */
/* }; */

/* extensions: comparison */
static enum comp getcomp(char *head, int sflags)
{
#ifdef DEBUGGING
    /* log_debug("getcomp, head: %s", head); */
#endif
	return (head[0] == '=' && (sflags & Mustach_With_Equal)) ? C_eq
		: (head[0] == '<' && (sflags & Mustach_With_Compare)) ? (head[1] == '=' ? C_le : C_lt)
		: (head[0] == '>' && (sflags & Mustach_With_Compare)) ? (head[1] == '=' ? C_ge : C_gt)
		: C_no;
}

/* checks for compare extension: {{key=val}}, {{key>=val}}, {{key<=val}}
   if head contains relop (=, >=, <=)
       a. terminates key part of head by writing \0
       b. sets comp to relop constant code (C_eq, C_le, C_lt, C_ge, C_gt, or C_no)
       c. returns val part of key
   else returns NULL
 */
static char *keyval(char *head, int sflags, enum comp *comp)
{
    TRACE_ENTRY(keyval);
#ifdef DEBUGGING
    log_debug("\thead: %s", head);
#endif
    char *w, car, escaped;
    enum comp k;

    k = C_no;
    w = head;
    car = *head;
    escaped = (sflags & Mustach_With_EscFirstCmp) && (getcomp(head, sflags) != C_no);
    while (car && (escaped || (k = getcomp(head, sflags)) == C_no)) {
        if (escaped)
            escaped = 0;
        else
            escaped = ((sflags & Mustach_With_JsonPointer) ? car == '~' : car == '\\')
                && (getcomp(head + 1, sflags) != C_no);
        if (!escaped)
            *w++ = car;
        head++;
        car = *head;
    }
    *w = 0;
    *comp = k;
    return k == C_no ? NULL : &head[k & 3];
}

/* WARNING: flag Mustach_With_JsonPointer not compatible with dotted segs (e.g. {{person.name}} */
static char *getkey(char **head, int sflags)
{
    TRACE_ENTRY(getkey);
#ifdef DEBUGGING
    log_debug("\thead: %s", *head);
#endif

    char *result, *iter, *write, car;

    car = *(iter = *head);
    if (!car)
        result = NULL;
    else {
        result = write = iter;
        if (sflags & Mustach_With_JsonPointer) {
            while (car && car != '/') {
                if (car == '~')
                    switch (iter[1]) {
                    case '1': car = '/'; /*@fallthrough@*/
                    case '0': iter++;
                    }
                *write++ = car;
                car = *++iter;
            }
            *write = 0;
            while (car == '/')
                car = *++iter;
        } else {
            while (car && car != '.') {
                if (car == '\\' && (iter[1] == '.' || iter[1] == '\\'))
                    car = *++iter;
                *write++ = car;
                car = *++iter;
            }
            *write = 0;
            while (car == '.')
                car = *++iter;
        }
        *head = iter;
    }
    return result;
}

/*  */
static enum sel sel(struct datasource_s *ds, // datasource
                    const char *name)
//(struct wrap *w, const char *name)
{
    TRACE_ENTRY(sel);
#ifdef DEBUGGING
    log_debug("\tname: '%s'", name);
    log_debug("\tstrlen(name): '%d'", strlen(name));
    log_debug("\tpred: %d", w->predicate);
#endif

    enum sel result;
    int i, j, sflags, scmp;
    char *key, *value;
    enum comp k;

    ((struct closure_hdr*)ds->stack)->predicate = ds->predicate;

    /* make a local writeable copy */
    size_t lenname = 1 + strlen(name);
    char buffer[lenname];
    char *copy = buffer;
    memcpy(copy, name, lenname);

    /* check if matches json pointer selection */
    sflags = ds->flags;
#ifdef JSON_ADAPTER
    if (sflags & Mustach_With_JsonPointer) {
        if (copy[0] == '/')
            copy++;
        else
            sflags ^= Mustach_With_JsonPointer;
    }
#endif

    /* extract the value, translate the key and get the comparator */
    if (sflags & (Mustach_With_Equal | Mustach_With_Compare))
        value = keyval(copy, sflags, &k);
    else {
        k = C_no;               /* C_no means no relop (=, <, >=, <, <=) */
        value = NULL;
    }
#ifdef DEBUGGING
    log_debug("relop (none == 0): %d", k);
    if (k) {
        log_debug("rel LHS: %s", copy);
        log_debug("rel RHS: %s", value);
    } else {
        log_debug("key: %s", copy);
    }
    log_debug("sflags: %d", sflags);
#endif

    switch(copy[0]) {
    case '.':
#ifdef DEBUGGING
    /* case of . alone if Mustach_With_SingleDot? */
        log_debug("CASE '.'");
#endif
        if (copy[1] == '\0'
            && (sflags & Mustach_With_SingleDot)) {
            /* yes, select current */
#ifdef DEBUGGING
            log_debug("SINGLEDOT");
#endif
            result = ds->methods->sel(ds->stack, NULL) ? S_ok : S_none;
        } else {
            log_debug("DOTTED NAME");
            // op is '.foo' - ??
            result = S_none;
        }
        break;
    case PREDOP_FIRST:          /* '^' */
#ifdef DEBUGGING
        log_debug("CASE PREDOP_FIRST");
        log_debug("pred: %d", ds->predicate);
#endif
        ((struct closure_hdr*)ds->stack)->predicate = FIRST_P;
        result = ds->methods->sel(ds->stack, NULL) ? S_ok : S_none;
        if (copy[1] == 0) {
            /* && (sflags & Mustach_With_Predicates)) { */
        } else {
            /* else if (copy[0] == PREDOP_FIRST) {  /\* extension *\/ */
            /*     /\* && (sflags & Mustach_With_Precates)) { *\/ */
#ifdef DEBUGGING
            log_debug("NAMED PREDOP_FIRST");
#endif
            char *ptmp = copy + 1;
            key = getkey(&ptmp, sflags);
            /* log_debug("KEY: %s", key); */
            // same as NOT SINGLEDOT below
            int rc = ds->methods->sel(ds->stack, key);
            if (rc) return S_ok; else return S_none;
        }
        break;

    case PREDOP_LAST:           /* '$' */
#ifdef DEBUGGING
        log_debug("CASE PREDOP_LAST");
        log_debug("pred: %d", ds->predicate);
#endif
        ((struct closure_hdr*)ds->stack)->predicate = LAST_P;
        result = ds->methods->sel(ds->stack, NULL) ? S_ok : S_none;
        if (copy[1] == 0) {
            /* && (sflags & Mustach_With_Precates)) { */
        } else {
            /* else if (copy[0] == PREDOP_BUTLAST) {  /\* extension *\/ */
            /* && (sflags & Mustach_With_Precates)) { */
            /* ((struct closure_hdr*)ds->stack)->nonfinal_predicate = true; */
            char *ptmp = copy + 1;
            key = getkey(&ptmp, sflags);
            /* log_debug("KEY: %s", key); */
            // same as NOT SINGLEDOT below
            int rc = ds->methods->sel(ds->stack, key);
            if (rc) return S_ok; else return S_none;
        }
        break;

    case PREDOP_BUTLAST:        /* '?' -> '~$' */
#ifdef DEBUGGING
        log_debug("CASE PREDOP_BUTLAST");
        log_debug("pred: %d", ds->predicate);
#endif
        ((struct closure_hdr*)ds->stack)->predicate = BUTLAST_P;
        result = ds->methods->sel(ds->stack, NULL) ? S_ok : S_none;
        if (copy[1] == 0) {
            /* && (sflags & Mustach_With_Precates)) { */
        } else {
            /* else if (copy[0] == PREDOP_BUTLAST) {  /\* extension *\/ */
            /* && (sflags & Mustach_With_Precates)) { */
#ifdef DEBUGGING
            log_debug("NAMED BUTLAST");
#endif
            /* ((struct closure_hdr*)ds->stack)->nonfinal_predicate = true; */
            char *ptmp = copy + 1;
            key = getkey(&ptmp, sflags);
            /* log_debug("KEY: %s", key); */
            // same as NOT SINGLEDOT below
            int rc = ds->methods->sel(ds->stack, key);
            if (rc) return S_ok; else return S_none;
        }
        break;
    default:
#ifdef DEBUGGING
        log_debug("CASE DEFAULT: no metachar ('.', '^', '$', '?')");
#endif
        /* not the single dot, extract the first key */
        key = getkey(&copy, sflags);
        if (key == NULL)
            return 0;
        /* select the root item */
#ifdef DEBUGGING
        log_debug("key: %s", key);
        log_debug("selecting root item");
        log_debug("w: %x", w);
        log_debug("ds->predicate: %d", ((struct closure_hdr*)w)->predicate);
        /* log_debug("ds->stack: %x", ds->stack); */
        /* log_debug("ds->stack->nonfinal_predicate: %x", */
        /*           ((struct closure_hdr*)ds->stack)->nonfinal_predicate); */
#endif
        if (ds->methods->sel(ds->stack, key))
            result = S_ok;
        else if (key[0] == '*'
                 && !key[1]
                 && !value
                 && !*copy
                 && (ds->flags & Mustach_With_ObjectIter)
                 && ds->methods->sel(ds->stack, NULL))
            result = S_ok_or_objiter;
        else
            result = S_none;
#ifdef DEBUGGING
        log_debug("app sel returned: %d", result);
        /* struct Xexpl *x = (struct Xexpl*)ds->stack; */
        /* DUMP_CLOSURE(x, 0); */
#endif
        if (result == S_ok) { /* S_ok == 1 */
            /* iterate the selection of sub items */
            key = getkey(&copy, sflags);
            while(result == S_ok && key) {
#ifdef DEBUGGING
                log_debug("SUBSELECTING subitem, key: '%s'", key);
#endif
                if (ds->methods->subsel(ds->stack, key))
                    /* nothing */;
                else if (key[0] == '*'
                         && !key[1]
                         && !value
                         && !*copy
                         && (ds->flags & Mustach_With_ObjectIter))
                    result = S_objiter;
                else
                    result = S_none;
                key = getkey(&copy, sflags);
            }
        }
    }

    /* should it be compared? */
    if (result == S_ok && value) {
#ifdef DEBUGGING
        log_debug("comparing? ");
#endif
        if (!ds->methods->compare)
            result = S_none;
        else {
            i = value[0] == '!';
            scmp = ds->methods->compare(ds->stack, &value[i]);
            switch (k) {
            case C_eq: j = scmp == 0; break;
            case C_lt: j = scmp < 0; break;
            case C_le: j = scmp <= 0; break;
            case C_gt: j = scmp > 0; break;
            case C_ge: j = scmp >= 0; break;
            default: j = i; break;
            }
            if (i == j)
                result = S_none;
        }
    }
#ifdef DEBUGGING
    log_debug("wrap sel returning: %d", result);
#endif
    return result;
}

static int start(struct datasource_s *ds) // (void *closure)
{
    TRACE_ENTRY(start)
    /* struct wrap *w = closure; */
    return ds->methods->start ? ds->methods->start(ds->stack) : MUSTACH_OK;
}

static void stop(struct datasource_s *ds, int status)
//(void *closure, int status)
{
#ifdef DEBUGGING
    log_debug("stop, status: %d", status);
#endif
	/* struct wrap *w = closure; */
    if (ds->methods->stop)
        ds->methods->stop(ds->stack, status);
}

static int _write(struct datasource_s *ds, const char *buffer, size_t size, FILE *file)
//(struct wrap *w, const char *buffer, size_t size, FILE *file)
{
    TRACE_ENTRY(_write);
#ifdef DEBUGGING
    log_debug("\tbuffer: %.15s", buffer);
    log_debug("\tsize: %d", size);
#endif

    int r;

    if (ds->writecb)
        r = ds->writecb(file, buffer, size);
    else {
#ifdef DEBUGGING
        log_debug("fwriting");
#endif
        r = fwrite(buffer, 1, size, file) == size ? MUSTACH_OK : MUSTACH_ERROR_SYSTEM;
#ifdef DEBUGGING
        log_debug("frwrite rc: %d", r);
#endif
        /* log_debug("wrote, buf: %s", buffer); */
    }
    return r;
}

static int emit(struct datasource_s *ds,
                const char *buffer, size_t size,
                int escape, FILE *file)
// (void *closure, const char *buffer, size_t size, int escape, FILE *file)
{
    TRACE_ENTRY(emit)
#ifdef DEBUGGING
    log_debug("\tbuffer: '%.30s ...'", buffer);
    log_debug("\tsize:   %d", size);
    log_debug("\tescape: %d", escape);
#endif

	/* struct wrap *w = closure; */
	int r;
	size_t s, i;
	char car;

	if (ds->emitcb)
		r = ds->emitcb(file, buffer, size, escape);
	else if (!escape)
		r = _write(ds, buffer, size, file);
	else {
		i = 0;
		r = MUSTACH_OK;
		while(i < size && r == MUSTACH_OK) {
			s = i;
			while (i < size && (car = buffer[i]) != '<' && car != '>' && car != '&' && car != '"')
				i++;
			if (i != s) {
#ifdef DEBUGGING
                            log_debug("call write: %s", &buffer[s]);
                            log_debug("call len: %d", i - s);
#endif
				r = _write(ds, &buffer[s], i - s, file);
                        }
			if (i < size && r == MUSTACH_OK) {
				switch(car) {
				case '<': r = _write(ds, "&lt;", 4, file); break;
				case '>': r = _write(ds, "&gt;", 4, file); break;
				case '&': r = _write(ds, "&amp;", 5, file); break;
				case '"': r = _write(ds, "&quot;", 6, file); break;
				}
				i++;
			}
		}
	}
	return r;
}

/* this is called by mustache.c, with 'closure' being iwrap->closure, which has type struct wrap*, which contains ptr to user struct expl */
static int enter(struct datasource_s *ds, const char *name)
// (void *closure, const char *name)
{
    TRACE_ENTRY(enter)
#ifdef DEBUGGING
    log_debug("pred: %d", ((struct closure_hdr*)closure)->predicate);
#endif

    // case iwrap->closure* to struct wrap*
    // the former is the app's closure struct
    // BUT there's a struct wrap* on the call stack
    /* struct wrap *w = (struct wrap*)closure; */
#ifdef DEBUGGING
    log_debug("enter calling sel");
#endif
    // call local sel w/arg 0 struct wrap
    // local sel calls ds->stack ???
    enum sel s = sel(ds, name);
#ifdef DEBUGGING
    log_debug("sel returned %d", s);
    /* S_none = 0, */
    /* S_ok = 1, */
    /* S_objiter = 2, */
    /* S_ok_or_objiter = S_ok | S_objiter */
    if (s != S_none) log_debug("calling ds->methods->enter");
#endif
    if (s == S_none)
        return 0;
    else
        return ds->methods->enter(ds->stack, s & S_objiter);
    /* return s == S_none ? 0 : ds->methods->enter(ds->stack, s & S_objiter); */
}

static int next(struct datasource_s *ds)
// (void *closure) // returns bool: has_next
{
#ifdef DEBUGGING
    log_debug("next");
#endif
    /* struct wrap *w = closure; */
    int rc = ds->methods->next(ds->stack);
#ifdef DEBUGGING
    log_debug("next rc: %d", rc);
#endif
    return rc;
}

static int leave(struct datasource_s *ds, struct mustach_sbuf *sbuf)
// (void *closure, struct mustach_sbuf *sbuf)
{
#ifdef DEBUGGING
    log_debug("leave");
#endif
    /* struct wrap *w = closure; */
    return ds->methods->leave(ds->stack, sbuf);
}

static int maybe_format(struct datasource_s *ds,
                        const char *name, const char *fmt,
                        struct mustach_sbuf *sbuf)
// (struct wrap *w, const char *name, struct mustach_sbuf *sbuf)
{
    TRACE_ENTRY(maybe_format);
#ifdef DEBUGGING
    log_debug("\tname: %s", name);
#endif
	enum sel s = sel(ds, name);
#ifdef DEBUGGING
        log_debug("maybe_format: sel returned %d", s);
	/* S_none = 0, */
	/* S_ok = 1, */
	/* S_objiter = 2, */
	/* S_ok_or_objiter = S_ok | S_objiter */
#endif
	if (!(s & S_ok))
		return 0;
#ifdef DEBUGGING
        log_debug("\tcalling ds->methods->format");
        log_debug("\tsbuf->value: %s", sbuf->value);
        log_debug("\tsbuf->length: %d", sbuf->length);
        log_debug("\tsbuf->releasecb: %x", sbuf->releasecb);
        /* log_debug("sbuf->closure: %d", sbuf->closure); */
#endif
	int rc = ds->methods->format(ds->stack, fmt, sbuf, s & S_objiter);
#ifdef DEBUGGING
        log_debug("maybe_format format rc: %d", rc);
        log_debug("maybe_format format result: sbuf->value: %s", sbuf->value);
        log_debug("maybe_format format result: sbuf->length: %d", sbuf->length);
        log_debug("\tsbuf->releasecb: %x", sbuf->releasecb);
#endif
        return rc;
}

static int format(struct datasource_s *ds,
                  const char *name, const char *fmt,
                  struct mustach_sbuf *sbuf)
// (void *closure, const char *name, struct mustach_sbuf *sbuf)
{
    TRACE_ENTRY(format);
#ifdef DEBUGGING
    log_debug("\tname='%s'", name);
    log_debug("\tsbuf->value='%s'", name, sbuf->value);
    /* log_debug("sbuf->releasecb: %x", sbuf->releasecb); */
#endif
    /* struct wrap *w = closure; */
    int rc = maybe_format(ds, name, fmt, sbuf); /* puts str val in sbuf.value */
#ifdef DEBUGGING
    log_debug("maybe_format rc: %d", rc);
    log_debug("sbuf->releasecb: %x", sbuf->releasecb);
#endif
    if (rc <= 0) {
        // template var w/o matching data fld
        if (ds->flags & Mustach_With_ErrorUndefined)
            return MUSTACH_ERROR_UNDEFINED_TAG;
        sbuf->value = "";
        return rc;
    }
    return MUSTACH_OK;
}

static int get_partial_from_file(const char *name, struct mustach_sbuf *sbuf)
{
    TRACE_ENTRY(get_partial_from_file);
	static char extension[] = INCLUDE_PARTIAL_EXTENSION;
	size_t s;
	long pos;
	FILE *file;
	char *path, *buffer;

	/* allocate path */
	s = strlen(name);
	path = malloc(s + sizeof extension);
	if (path == NULL)
		return MUSTACH_ERROR_SYSTEM;

	/* try without extension first */
	memcpy(path, name, s + 1);
	file = fopen(path, "r");
	if (file == NULL) {
		memcpy(&path[s], extension, sizeof extension);
		file = fopen(path, "r");
	}
	free(path);

	/* if file opened */
	if (file == NULL)
		return MUSTACH_ERROR_PARTIAL_NOT_FOUND;

	/* compute file size */
	if (fseek(file, 0, SEEK_END) >= 0
	 && (pos = ftell(file)) >= 0
	 && fseek(file, 0, SEEK_SET) >= 0) {
		/* allocate value */
		s = (size_t)pos;
		buffer = malloc(s + 1);
		if (buffer != NULL) {
			/* read value */
			if (1 == fread(buffer, s, 1, file)) {
				/* force zero at end */
				sbuf->value = buffer;
				buffer[s] = 0;
				sbuf->freecb = free;
				fclose(file);
				return MUSTACH_OK;
			}
			free(buffer);
		}
	}
	fclose(file);
	return MUSTACH_ERROR_SYSTEM;
}

static int partial(struct datasource_s *ds,
                   const char *name, const char *fmt,
                   struct mustach_sbuf *sbuf)
// (void *closure, const char *name, struct mustach_sbuf *sbuf)
{
    TRACE_ENTRY(partial);
    /* struct wrap *w = closure; */
    int rc;
    if (mustach_wrap_get_partial != NULL) {
        log_debug("1xxxxxxxxxxxxxxxx");
        rc = mustach_wrap_get_partial(name, sbuf);
    }
    else if (ds->flags & Mustach_With_PartialDataFirst) {
        if (maybe_format(ds, name, fmt, sbuf) > 0) {
            rc = MUSTACH_OK;
        } else {
            rc = get_partial_from_file(name, sbuf);
        }
    }
    else {
        rc = get_partial_from_file(name, sbuf);
        if (rc != MUSTACH_OK &&  maybe_format(ds, name, fmt, sbuf) > 0)
            rc = MUSTACH_OK;
    }
    if (rc != MUSTACH_OK)
        sbuf->value = "";
    return MUSTACH_OK;
}

static void dump_stack(void *closure)
{
    TRACE_ENTRY(dump_stack);
    struct datasource_s *ds= closure;
    ds->methods->dump_stack(ds->stack);
}

static const struct mustach_ds_mgr_methods_s json_ds_mgr_methods = {
    .start = (int (*)(void*))start,
    .put = (int (*)(void*, const char*, const char*, int, FILE*))NULL,
    .enter = (int (*)(void*, const char*))enter,
    .next = (int (*)(void*))next,
    .leave = (int (*)(void*, struct mustach_sbuf*))leave,
    .partial = (int (*)(void*, const char*, const char*, struct mustach_sbuf*))partial,
    .format = (int (*)(void*, const char*, const char*, struct mustach_sbuf*))format,
    .emit = (int (*)(void*, const char*, size_t, int, FILE *))emit,
    .stop = (void (*)(void*, int))stop,
    .dump_stack = (void (*)(void*))dump_stack
	/* .start = start, */
	/* .put = NULL, */
	/* .enter = enter, */
	/* .next = next, */
	/* .leave = leave, */
	/* .partial = partial, */
	/* .format = format, */
	/* .emit = emit, */
	/* .stop = stop, */
        /* .dump_closure = dump_closure */
};

/* static void json_init(struct wrap *wrap, const struct mustach_ds_mgr_methods_s_json *itf, void *closure, int flags, mustach_emit_cb_t *emitcb, mustach_write_cb_t *writecb) */
/* { */
/* 	if (flags & Mustach_With_Compare) */
/* 		flags |= Mustach_With_Equal; */
/* 	wrap->closure = closure; */
/* 	wrap->itf = itf; */
/* 	wrap->flags = flags; */
/* 	wrap->emitcb = emitcb; */
/* 	wrap->writecb = writecb; */
/* } */

int mustach_json_file(const char *template, size_t length,
                      const struct mustach_ds_methods_s *methods,
                      void *closure, int flags, FILE *file)
{
	/* struct wrap w; */
	struct datasource_s ds;
	datasource_init(&ds, methods, closure, flags, NULL, NULL);
	return mustach_file(template, length, &json_ds_mgr_methods, &ds, flags, file);
}

int mustach_json_fd(const char *template, size_t length,
                    const struct mustach_ds_methods_s *methods,
                    void *closure, int flags, int fd)
{
	struct datasource_s ds;
	datasource_init(&ds, methods, closure, flags, NULL, NULL);
	return mustach_fd(template, length, &json_ds_mgr_methods, &ds, flags, fd);
}

int mustach_json_mem(const char *template, size_t length,
                     const struct mustach_ds_methods_s *methods,
                     void *closure, int flags, char **result, size_t *size)
{
#ifdef DEBUGGING
    log_debug("mustach_json_mem");
#endif
	struct datasource_s ds;
	datasource_init(&ds, methods, closure, flags, NULL, NULL);
	return mustach_mem(template, length, &json_ds_mgr_methods, &ds, flags, result, size);
}

int mustach_json_write(const char *template, size_t length,
                       const struct mustach_ds_methods_s *methods,
                       void *closure, int flags,
                       mustach_write_cb_t *writecb, void *writeclosure)
{
	struct datasource_s ds;
	datasource_init(&ds, methods, closure, flags, NULL, writecb);
	return mustach_file(template, length, &json_ds_mgr_methods, &ds, flags, writeclosure);
}

int mustach_json_emit(const char *template, size_t length,
                      const struct mustach_ds_methods_s *methods,
                      void *closure, int flags,
                      mustach_emit_cb_t *emitcb, void *emitclosure)
{
	struct datasource_s ds;
	datasource_init(&ds, methods, closure, flags, emitcb, NULL);
	return mustach_file(template, length, &json_ds_mgr_methods, &ds, flags, emitclosure);
}

