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

#include "log.h"
#include "mustach.h"
#include "mustachios7_scm.h"

#ifdef DEBUG_TRACE
#include "ansi_colors.h"
/* #include "s7.h" */
//#include "debug.h"
/* extern s7_scheme *s7; */
/* s7_pointer xx; */
#endif
#include "trace.h"

/* extensions: comparison */
static enum comp getcomp(char *head, int sflags)
{
#ifdef DEBUG_TRACE
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
#ifdef DEBUG_TRACE
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
#ifdef DEBUG_TRACE
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
static enum sel sel(struct wrap *w, const char *name)
{
    TRACE_ENTRY(sel);
#ifdef DEBUG_TRACE
    log_debug("\tname: %s", name);
    log_debug("\tpred: %d", w->predicate);
#endif

    enum sel result;
    int i, j, sflags, scmp;
    char *key, *value;
    enum comp k;

    ((struct closure_hdr*)w->closure)->predicate = w->predicate;

    /* make a local writeable copy */
    size_t lenname = 1 + strlen(name);
    char buffer[lenname];
    char *copy = buffer;
    memcpy(copy, name, lenname);

    /* check if matches json pointer selection */
    sflags = w->flags;
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
#ifdef DEBUG_TRACE
    log_debug("relop (none == 0): %d", k);
    if (k) {
        log_debug("rel LHS: %s", copy);
        log_debug("rel RHS: %s", value);
    } else {
        log_debug("key: %s", copy);
    }
#endif

    switch(copy[0]) {
    case '.':
#ifdef DEBUG_TRACE
    /* case of . alone if Mustach_With_SingleDot? */
        log_debug("CASE '.'");
#endif
        if (copy[1] == '\0'
            && (sflags & Mustach_With_SingleDot)) {
            /* yes, select current */
#ifdef DEBUG_TRACE
            log_debug("SINGLEDOT");
#endif
            result = w->itf->sel(w->closure, NULL) ? S_ok : S_none;
        } else {
#ifdef DEBUG_TRACE
            log_debug("DOTTEDNAME");
#endif
            // op is '.foo' - ??
            result = S_none;
        }
        break;
    case PREDOP_FIRST:          /* '^' */
#ifdef DEBUG_TRACE
        log_debug("CASE PREDOP_FIRST");
        log_debug("pred: %d", w->predicate);
#endif
        ((struct closure_hdr*)w->closure)->predicate = FIRST_P;
        result = w->itf->sel(w->closure, NULL) ? S_ok : S_none;
        if (copy[1] == 0) {
            /* && (sflags & Mustach_With_Predicates)) { */
        } else {
            /* else if (copy[0] == PREDOP_FIRST) {  /\* extension *\/ */
            /*     /\* && (sflags & Mustach_With_Precates)) { *\/ */
#ifdef DEBUG_TRACE
            log_debug("NAMED PREDOP_FIRST");
#endif
            char *ptmp = copy + 1;
            key = getkey(&ptmp, sflags);
            /* log_debug("KEY: %s", key); */
            // same as NOT SINGLEDOT below
            int rc = w->itf->sel(w->closure, key);
            if (rc) return S_ok; else return S_none;
        }
        break;

    case PREDOP_LAST:           /* '$' */
#ifdef DEBUG_TRACE
        log_debug("CASE PREDOP_LAST");
        log_debug("pred: %d", w->predicate);
#endif
        ((struct closure_hdr*)w->closure)->predicate = LAST_P;
        result = w->itf->sel(w->closure, NULL) ? S_ok : S_none;
        if (copy[1] == 0) {
            /* && (sflags & Mustach_With_Precates)) { */
        } else {
            /* else if (copy[0] == PREDOP_BUTLAST) {  /\* extension *\/ */
            /* && (sflags & Mustach_With_Precates)) { */
            /* ((struct closure_hdr*)w->closure)->nonfinal_predicate = true; */
            char *ptmp = copy + 1;
            key = getkey(&ptmp, sflags);
            /* log_debug("KEY: %s", key); */
            // same as NOT SINGLEDOT below
            int rc = w->itf->sel(w->closure, key);
            if (rc) return S_ok; else return S_none;
        }
        break;

    case PREDOP_BUTLAST:        /* '?' -> '~$' */
#ifdef DEBUG_TRACE
        log_debug("CASE PREDOP_BUTLAST");
        log_debug("pred: %d", w->predicate);
#endif
        ((struct closure_hdr*)w->closure)->predicate = BUTLAST_P;
        result = w->itf->sel(w->closure, NULL) ? S_ok : S_none;
        if (copy[1] == 0) {
            /* && (sflags & Mustach_With_Precates)) { */
        } else {
            /* else if (copy[0] == PREDOP_BUTLAST) {  /\* extension *\/ */
            /* && (sflags & Mustach_With_Precates)) { */
#ifdef DEBUG_TRACE
            log_debug("NAMED BUTLAST");
#endif
            /* ((struct closure_hdr*)w->closure)->nonfinal_predicate = true; */
            char *ptmp = copy + 1;
            key = getkey(&ptmp, sflags);
            /* log_debug("KEY: %s", key); */
            // same as NOT SINGLEDOT below
            int rc = w->itf->sel(w->closure, key);
            if (rc) return S_ok; else return S_none;
        }
        break;
    default:
#ifdef DEBUG_TRACE
        log_debug("CASE DEFAULT: no metachar ('.', '^', '$', '?')");
#endif
        /* not the single dot, extract the first key */
        key = getkey(&copy, sflags);
        if (key == NULL)
            return 0;
        /* select the root item */
#ifdef DEBUG_TRACE
        log_debug("key: %s", key);
        log_debug("selecting root item");
        log_debug("w: %x", w);
        log_debug("w->predicate: %d", ((struct closure_hdr*)w)->predicate);
        /* log_debug("w->closure: %x", w->closure); */
        /* log_debug("w->closure->nonfinal_predicate: %x", */
        /*           ((struct closure_hdr*)w->closure)->nonfinal_predicate); */
#endif
        if (w->itf->sel(w->closure, key))
            result = S_ok;
        else if (key[0] == '*'
                 && !key[1]
                 && !value
                 && !*copy
                 && (w->flags & Mustach_With_ObjectIter)
                 && w->itf->sel(w->closure, NULL))
            result = S_ok_or_objiter;
        else
            result = S_none;
#ifdef DEBUG_TRACE
        log_debug("app sel returned: %d", result);
        /* struct Xexpl *x = (struct Xexpl*)w->closure; */
        /* DUMP_CLOSURE(x, 0); */
#endif
        if (result == S_ok) { /* S_ok == 1 */
            /* iterate the selection of sub items */
            key = getkey(&copy, sflags);
            while(result == S_ok && key) {
#ifdef DEBUG_TRACE
                log_debug("SUBSELECTING subitem, key: '%s'", key);
#endif
                if (w->itf->subsel(w->closure, key))
                    /* nothing */;
                else if (key[0] == '*'
                         && !key[1]
                         && !value
                         && !*copy
                         && (w->flags & Mustach_With_ObjectIter))
                    result = S_objiter;
                else
                    result = S_none;
                key = getkey(&copy, sflags);
            }
        }
    }

    /* should it be compared? */
    if (result == S_ok && value) {
#ifdef DEBUG_TRACE
        log_debug("comparing? ");
#endif
        if (!w->itf->compare)
            result = S_none;
        else {
            i = value[0] == '!';
            scmp = w->itf->compare(w->closure, &value[i]);
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
#ifdef DEBUG_TRACE
    log_debug("wrap sel returning: %d", result);
#endif
    return result;
}

static int start(void *closure)
{
    TRACE_ENTRY(start)
    struct wrap *w = closure;
    return w->itf->start ? w->itf->start(w->closure) : MUSTACH_OK;
}

static void stop(void *closure, int status)
{
#ifdef DEBUG_TRACE
    log_debug("stop, status: %d", status);
#endif
	struct wrap *w = closure;
	if (w->itf->stop)
		w->itf->stop(w->closure, status);
}

static int _write(struct wrap *w, const char *buffer, size_t size, FILE *file)
{
    TRACE_ENTRY(_write);
#ifdef DEBUG_TRACE
    log_debug("\tbuffer: %.15s", buffer);
    log_debug("\tsize: %d", size);
#endif

    int r;

    if (w->writecb)
        r = w->writecb(file, buffer, size);
    else {
#ifdef DEBUG_TRACE
        log_debug("fwriting");
#endif
        r = fwrite(buffer, 1, size, file) == size ? MUSTACH_OK : MUSTACH_ERROR_SYSTEM;
#ifdef DEBUG_TRACE
        log_debug("frwrite rc: %d", r);
#endif
        /* log_debug("wrote, buf: %s", buffer); */
    }
    return r;
}

static int emit(void *closure, const char *buffer, size_t size, int escape, FILE *file)
{
    TRACE_ENTRY(emit)
#ifdef DEBUG_TRACE
    log_debug("\tbuffer: '%.30s ...'", buffer);
    log_debug("\tsize:   %d", size);
    log_debug("\tescape: %d", escape);
#endif

	struct wrap *w = closure;
	int r;
	size_t s, i;
	char car;

	if (w->emitcb)
		r = w->emitcb(file, buffer, size, escape);
	else if (!escape)
		r = _write(w, buffer, size, file);
	else {
		i = 0;
		r = MUSTACH_OK;
		while(i < size && r == MUSTACH_OK) {
			s = i;
			while (i < size && (car = buffer[i]) != '<' && car != '>' && car != '&' && car != '"')
				i++;
			if (i != s) {
#ifdef DEBUG_TRACE
                            log_debug("call write: %s", &buffer[s]);
                            log_debug("call len: %d", i - s);
#endif
				r = _write(w, &buffer[s], i - s, file);
                        }
			if (i < size && r == MUSTACH_OK) {
				switch(car) {
				case '<': r = _write(w, "&lt;", 4, file); break;
				case '>': r = _write(w, "&gt;", 4, file); break;
				case '&': r = _write(w, "&amp;", 5, file); break;
				case '"': r = _write(w, "&quot;", 6, file); break;
				}
				i++;
			}
		}
	}
	return r;
}

/* this is called by mustache.c, with 'closure' being iwrap->closure, which has type struct wrap*, which contains ptr to user struct expl */
static int enter(void *closure, const char *name)
{
    TRACE_ENTRY(enter)
#ifdef DEBUG_TRACE
    log_debug("pred: %d", ((struct closure_hdr*)closure)->predicate);
#endif

    // case iwrap->closure* to struct wrap*
    // the former is the app's closure struct
    // BUT there's a struct wrap* on the call stack
    struct wrap *w = (struct wrap*)closure;
#ifdef DEBUG_TRACE
    log_debug("enter calling sel");
#endif
    // call local sel w/arg 0 struct wrap
    // local sel calls w->closure ???
    enum sel s = sel(w, name);
#ifdef DEBUG_TRACE
    log_debug("sel returned %d", s);
    /* S_none = 0, */
    /* S_ok = 1, */
    /* S_objiter = 2, */
    /* S_ok_or_objiter = S_ok | S_objiter */
    if (s != S_none) log_debug("calling w->itf->enter");
#endif
    if (s == S_none)
        return 0;
    else
        return w->itf->enter(w->closure, s & S_objiter);
    /* return s == S_none ? 0 : w->itf->enter(w->closure, s & S_objiter); */
}

static int next(void *closure) // returns bool: has_next
{
#ifdef DEBUG_TRACE
    log_debug("next");
#endif
    struct wrap *w = closure;
    int rc = w->itf->next(w->closure);
#ifdef DEBUG_TRACE
    log_debug("next rc: %d", rc);
#endif
    return rc;
}

static int leave(void *closure, struct mustach_sbuf *sbuf)
{
#ifdef DEBUG_TRACE
    log_debug("leave");
#endif
    struct wrap *w = closure;
    return w->itf->leave(w->closure, sbuf);
}

static int getoptional(struct wrap *w, const char *name, struct mustach_sbuf *sbuf)
{
    TRACE_ENTRY(getoptional);
#ifdef DEBUG_TRACE
    log_debug("\tname: %s", name);
#endif
	enum sel s = sel(w, name);
#ifdef DEBUG_TRACE
        log_debug("getoptional: sel returned %d", s);
	/* S_none = 0, */
	/* S_ok = 1, */
	/* S_objiter = 2, */
	/* S_ok_or_objiter = S_ok | S_objiter */
#endif
	if (!(s & S_ok))
		return 0;
#ifdef DEBUG_TRACE
        log_debug("\tcalling w->itf->get");
        log_debug("\tsbuf->value: %s", sbuf->value);
        log_debug("\tsbuf->length: %d", sbuf->length);
        log_debug("\tsbuf->releasecb: %x", sbuf->releasecb);
        /* log_debug("sbuf->closure: %d", sbuf->closure); */
#endif
	int rc = w->itf->get(w->closure, sbuf, s & S_objiter);
#ifdef DEBUG_TRACE
        log_debug("getoptional get rc: %d", rc);
        log_debug("getoptional get result: sbuf->value: %s", sbuf->value);
        log_debug("getoptional get result: sbuf->length: %d", sbuf->length);
        log_debug("\tsbuf->releasecb: %x", sbuf->releasecb);
#endif
        return rc;
}

static int get(void *closure, const char *name, struct mustach_sbuf *sbuf)
{
    TRACE_ENTRY(get);
#ifdef DEBUG_TRACE
    log_debug("\tname='%s'", name);
    log_debug("\tsbuf->value='%s'", name, sbuf->value);
    /* log_debug("sbuf->releasecb: %x", sbuf->releasecb); */
#endif
    struct wrap *w = closure;
    int rc = getoptional(w, name, sbuf); /* puts str val in sbuf.value */
#ifdef DEBUG_TRACE
    log_debug("getoptional rc: %d", rc);
    log_debug("sbuf->releasecb: %x", sbuf->releasecb);
#endif
    if (rc <= 0) {
        // template var w/o matching data fld
        if (w->flags & Mustach_With_ErrorUndefined)
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

static int partial(void *closure, const char *name, struct mustach_sbuf *sbuf)
{
    TRACE_ENTRY(partial);
    struct wrap *w = closure;
    int rc;
    if (mustach_wrap_get_partial != NULL) {
        log_debug("1xxxxxxxxxxxxxxxx");
        rc = mustach_wrap_get_partial(name, sbuf);
    }
    else if (w->flags & Mustach_With_PartialDataFirst) {
        if (getoptional(w, name, sbuf) > 0) {
            rc = MUSTACH_OK;
        } else {
            rc = get_partial_from_file(name, sbuf);
        }
    }
    else {
        rc = get_partial_from_file(name, sbuf);
        if (rc != MUSTACH_OK &&  getoptional(w, name, sbuf) > 0)
            rc = MUSTACH_OK;
    }
    if (rc != MUSTACH_OK)
        sbuf->value = "";
    return MUSTACH_OK;
}

static void dump_closure(void *closure)
{
    TRACE_ENTRY(dump_closure);
    struct wrap *w = closure;
    w->itf->dump_closure(w->closure);
}

static const struct mustach_itf mustach_wrap_itf = {
	.start = start,
	.put = NULL,
	.enter = enter,
	.next = next,
	.leave = leave,
	.partial = partial,
	.get = get,
	.emit = emit,
	.stop = stop,
        .dump_closure = dump_closure
};

/* static void wrap_init(struct wrap *wrap, const struct mustach_wrap_itf *itf, void *closure, int flags, mustach_emit_cb_t *emitcb, mustach_write_cb_t *writecb) */
/* { */
/* 	if (flags & Mustach_With_Compare) */
/* 		flags |= Mustach_With_Equal; */
/* 	wrap->closure = closure; */
/* 	wrap->itf = itf; */
/* 	wrap->flags = flags; */
/* 	wrap->emitcb = emitcb; */
/* 	wrap->writecb = writecb; */
/* } */

int mustach_wrap_file(const char *template, size_t length, const struct mustach_wrap_itf *itf, void *closure, int flags, FILE *file)
{
	struct wrap w;
	wrap_init(&w, itf, closure, flags, NULL, NULL);
	return mustach_file(template, length, &mustach_wrap_itf, &w, flags, file);
}

int mustach_wrap_fd(const char *template, size_t length, const struct mustach_wrap_itf *itf, void *closure, int flags, int fd)
{
	struct wrap w;
	wrap_init(&w, itf, closure, flags, NULL, NULL);
	return mustach_fd(template, length, &mustach_wrap_itf, &w, flags, fd);
}

int mustach_wrap_mem(const char *template, size_t length, const struct mustach_wrap_itf *itf, void *closure, int flags, char **result, size_t *size)
{
#ifdef DEBUG_TRACE
    log_debug("mustach_wrap_mem");
#endif
	struct wrap w;
	wrap_init(&w, itf, closure, flags, NULL, NULL);
	return mustach_mem(template, length, &mustach_wrap_itf, &w, flags, result, size);
}

int mustach_wrap_write(const char *template, size_t length, const struct mustach_wrap_itf *itf, void *closure, int flags, mustach_write_cb_t *writecb, void *writeclosure)
{
	struct wrap w;
	wrap_init(&w, itf, closure, flags, NULL, writecb);
	return mustach_file(template, length, &mustach_wrap_itf, &w, flags, writeclosure);
}

int mustach_wrap_emit(const char *template, size_t length, const struct mustach_wrap_itf *itf, void *closure, int flags, mustach_emit_cb_t *emitcb, void *emitclosure)
{
	struct wrap w;
	wrap_init(&w, itf, closure, flags, emitcb, NULL);
	return mustach_file(template, length, &mustach_wrap_itf, &w, flags, emitclosure);
}

