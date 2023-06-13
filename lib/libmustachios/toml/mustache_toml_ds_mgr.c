/*
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
#include "mustache_toml_ds_mgr.h"

#ifdef DEVBUILD
#include "ansi_colors.h"
#endif

/* #if !defined(INCLUDE_PARTIAL_EXTENSION) */
/* # define INCLUDE_PARTIAL_EXTENSION ".mustache" */
/* #endif */

/* /\* global hook for partials *\/ */
/* int (*mustach_wrap_get_partial)(const char *name, struct mustach_sbuf *sbuf) = NULL; */

// struct datasource_s is shared across data types (toml, json, scheme)
// so it is in mustachios7_wrap.h

/* /\* internal structure for wrapping *\/ */
/* struct datasource_s - see mustachios7_wrap.h{ */

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
#ifdef DEVBUILD
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
#ifdef DEVBUILD
    log_debug("\thead: %s", head);
#endif
    char *h, car, escaped;
    enum comp k;

    k = C_no;
    h = head;
    car = *head;
    escaped = (sflags & Mustach_With_EscFirstCmp) && (getcomp(head, sflags) != C_no);
    while (car && (escaped || (k = getcomp(head, sflags)) == C_no)) {
        if (escaped)
            escaped = 0;
        else
            escaped = ((sflags & Mustach_With_JsonPointer) ? car == '~' : car == '\\')
                && (getcomp(head + 1, sflags) != C_no);
        if (!escaped)
            *h++ = car;
        head++;
        car = *head;
    }
    *h = 0;
    *comp = k;
    return k == C_no ? NULL : &head[k & 3];
}

/* WARNING: flag Mustach_With_JsonPointer not compatible with dotted segs (e.g. {{person.name}} */
static char *getkey(char **head, int sflags)
{
    TRACE_ENTRY(getkey);
#ifdef DEVBUILD
    log_debug("\thead: %s", *head);
#endif

    char *result, *iter, *write, car;

    car = *(iter = *head);
    if (!car)
        result = NULL;
    else {
        result = write = iter;
        // no JsonPointer for toml
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

/* boolean: name selected or not?  */
static enum sel sel(struct datasource_s *ds, // datasource
                    const char *name)
{
    TRACE_ENTRY(sel);
#ifdef DEVBUILD
    log_debug("\tname: '%s'", name);
    log_debug("\tstrlen(name): %d", strlen(name));
    log_debug("\tpred: %d", ds->predicate);
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
#ifdef DEVBUILD
    log_debug("relop (none == 0): %d", k);
    if (k) {
        log_debug("rel LHS: %s", copy);
        log_debug("rel RHS: %s", value);
    } else {
        log_debug("key: %s", copy);
    }
    log_debug("sflags: 0x%02x", sflags);
#endif

    switch(copy[0]) {
    case '.':
#ifdef DEVBUILD
    /* case of . alone if Mustach_With_SingleDot? */
        log_debug("CASE '.'");
#endif
        if (copy[1] == '\0') { // Mustach_With_SingleDot always enabled
            /* && (sflags & Mustach_With_SingleDot)) { */
            /* yes, select current */
#ifdef DEVBUILD
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
#ifdef DEVBUILD
        log_debug("CASE PREDOP_FIRST");
        log_debug("pred: %d", ds->predicate);
#endif
        if (ds->predicate) {
            ((struct closure_hdr*)ds->stack)->predicate = FIRST_P;
            result = ds->methods->sel(ds->stack, NULL) ? S_ok : S_none;
            if (copy[1] == 0) {
                /* && (sflags & Mustach_With_Predicates)) { */
            } else {
                /* else if (copy[0] == PREDOP_FIRST) {  /\* extension *\/ */
                /*     /\* && (sflags & Mustach_With_Precates)) { *\/ */
#ifdef DEVBUILD
                log_debug("NAMED PREDOP_FIRST");
#endif
                char *ptmp = copy + 1;
                key = getkey(&ptmp, sflags);
                /* log_debug("KEY: %s", key); */
                // same as NOT SINGLEDOT below
                int rc = ds->methods->sel(ds->stack, key);
                if (rc) return S_ok; else return S_none;
            }
        } else {
            goto no_metachar;
        }
        break;
    case PREDOP_LAST:           /* '$' */
#ifdef DEVBUILD
        log_debug("CASE PREDOP_LAST");
        log_debug("pred: %d", ds->predicate);
#endif
        if (ds->predicate) {
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
        } else {
            goto no_metachar;
        }
        break;

    case PREDOP_BUTLAST:        /* '?' -> '~$' */
#ifdef DEVBUILD
        log_debug("CASE PREDOP_BUTLAST");
        log_debug("pred: %d", ds->predicate);
#endif
        if (ds->predicate) {
            ((struct closure_hdr*)ds->stack)->predicate = BUTLAST_P;
            result = ds->methods->sel(ds->stack, NULL) ? S_ok : S_none;
            if (copy[1] == 0) {
                /* && (sflags & Mustach_With_Precates)) { */
            } else {
                /* else if (copy[0] == PREDOP_BUTLAST) {  /\* extension *\/ */
                /* && (sflags & Mustach_With_Precates)) { */
#ifdef DEVBUILD
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
        } else {
            goto no_metachar;
        }
        break;
    default:

no_metachar:
#ifdef DEVBUILD
        log_debug("CASE DEFAULT: no metachar ('.', '^', '$', '?')");
        log_debug("calling getkey w %s, sflags: 0x%02x", (char*)copy, sflags);
#endif
        /* not the single dot, extract the first key */
        key = getkey(&copy, sflags);
        if (key == NULL)
            return 0;
        /* select the root item */
#ifdef DEVBUILD
        log_debug("key: %s", key);
        log_debug("selecting root item");
        log_debug("ds: %x", ds);
        log_debug("ds->predicate: %d", ((struct closure_hdr*)ds)->predicate);
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
                 && ds->methods->sel(ds->stack, NULL)) {
            /* log_debug("setting S_ok_or_objiter"); */
            result = S_ok_or_objiter;
        } else
            result = S_none;
#ifdef DEVBUILD
        log_debug("app sel returned: %d", result);
        /* struct Xexpl *x = (struct Xexpl*)ds->stack; */
        /* DUMP_CLOSURE(x, 0); */
#endif
        if (result == S_ok) { /* S_ok == 1 */
            /* iterate the selection of sub items */
            key = getkey(&copy, sflags);
            while(result == S_ok && key) {
#ifdef DEVBUILD
                log_debug("SUBSELECTING subitem, key: '%s'", key);
#endif
                int subsel = ds->methods->subsel(ds->stack, key);
                /* log_debug("subselected: %d, key: %s", subsel, key); */
                if (subsel) /* nothing */
                    ;
                else {
#ifdef DEVBUILD
                    log_debug("subsel fail");
                    log_debug("key[0]: %c", key[0]);
                    log_debug("key[1]: %c", key[1]);
                    log_debug("value: %s", value);
                    log_debug("*copy: %s", *copy);
                    log_debug("ds->flags: 0x%04X", ds->flags);
#endif
                    if (key[0] == '*'
                        && !key[1]
                        && !value
                        && !*copy) {
                        /* && (ds->flags & Mustach_With_ObjectIter)) { */
                        TRACE_LOG_DEBUG("setting result: objiter", "");
                        result = S_objiter;
                    } else {
                        TRACE_LOG_DEBUG("no objiter", "");
                        result = S_none;
                    }
                }
                key = getkey(&copy, sflags);
                /* log_debug("getkey res: %s", key); */
            }
        }
    }

    /* should it be compared? */
    if (result == S_ok && value) {
#ifdef DEVBUILD
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
#ifdef DEVBUILD
    log_debug("wrap sel returning: %d", result);
#endif
    return result;
}

/* static int start(void *closure) */
static int start(struct datasource_s *ds)
{
    TRACE_ENTRY(start)
    /* struct datasource_s *ds= closure; */
    return ds->methods->start ? ds->methods->start(ds->stack) : MUSTACH_OK;
}

static void stop(struct datasource_s *ds, int status)
{
#ifdef DEVBUILD
    log_debug("stop, status: %d", status);
#endif
	/* struct datasource_s *ds= closure; */
	if (ds->methods->stop)
		ds->methods->stop(ds->stack, status);
}

static int _write(struct datasource_s *ds, const char *buffer, size_t size, FILE *file)
{
    TRACE_ENTRY(_write);
#ifdef DEVBUILD
    log_debug("\tbuffer: %.15s", buffer);
    log_debug("\tsize: %d", size);
#endif

    int r;

    if (ds->writecb)
        r = ds->writecb(file, buffer, size);
    else {
#ifdef DEVBUILD
        log_debug("fwriting");
#endif
        r = fwrite(buffer, 1, size, file) == size ? MUSTACH_OK : MUSTACH_ERROR_SYSTEM;
#ifdef DEVBUILD
        log_debug("frwrite rc: %d", r);
#endif
        /* log_debug("wrote, buf: %s", buffer); */
    }
    return r;
}

static int emit(struct datasource_s *ds,
                const char *buffer, size_t size,
                int escape, FILE *file)
{
    TRACE_ENTRY(emit)
#ifdef DEVBUILD
    log_debug("\tbuffer: '%.30s ...'", buffer);
    log_debug("\tsize:   %d", size);
    log_debug("\tescape: %d", escape);
#endif

	/* struct datasource_s *ds= closure; */
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
#ifdef DEVBUILD
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

/* this is called by mustache.c, with 'closure' being iwrap->closure, which has type struct datasource_s*, which contains ptr to user struct stack */
/* static int enter(void *closure, const char *name) */
/* static int enter(struct datasource_s *ds, const char *name) */
// only call ds->enter if selected is true
static int enter(struct datasource_s *ds, const char *name)
{
    TRACE_ENTRY(enter)
#ifdef DEVBUILD
    log_debug("pred: %d", ((struct closure_hdr*)ds)->predicate);
#endif

    // case iwrap->closure* to struct datasource_s*
    // the former is the app's closure struct
    // BUT there's a struct datasource_s* on the call stack
    /* struct datasource_s *ds= (struct datasource_s*)closure; */
#ifdef DEVBUILD
    log_debug("enter calling sel");
#endif
    // call local sel w/arg 0 struct datasource_s
    // local sel calls ds->stack ???
    enum sel s = sel(ds, name);
#ifdef DEVBUILD
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

/* static int next(void *closure) // returns bool: has_next */
static int next(struct datasource_s *ds)
{
#ifdef DEVBUILD
    log_debug("next");
#endif
    /* struct datasource_s *ds= closure; */
    int rc = ds->methods->next(ds->stack);
#ifdef DEVBUILD
    log_debug("next rc: %d", rc);
#endif
    return rc;
}

/* static int leave(void *closure, struct mustach_sbuf *sbuf) */
static int leave(struct datasource_s *ds, struct mustach_sbuf *sbuf)
{
#ifdef DEVBUILD
    log_debug("leave");
#endif
    /* struct datasource_s *ds= closure; */
    return ds->methods->leave(ds->stack, sbuf);
}

/* was: getoptional
   formats value for 'name' if it is selected
*/
static int maybe_format(struct datasource_s *ds,
                        const char *name, const char *fmt,
                        struct mustach_sbuf *sbuf)
{
    TRACE_ENTRY(maybe_format);
#ifdef DEVBUILD
    log_debug("\tname: %s", name);
#endif
    enum sel s = sel(ds, name);
#ifdef DEVBUILD
    log_debug("maybe_format: sel returned 0x%02x", s);
    /* S_none = 0, */
    /* S_ok = 1, */
    /* S_objiter = 2, */
    /* S_ok_or_objiter = S_ok | S_objiter */
#endif
    if (!(s & S_ok))
        return 0;
#ifdef DEVBUILD
    log_debug("\tcalling ds->methods->format");
    log_debug("\tsbuf->value: %s", sbuf->value);
    log_debug("\tsbuf->length: %d", sbuf->length);
    log_debug("\tsbuf->releasecb: %x", sbuf->releasecb);
    /* log_debug("sbuf->closure: %d", sbuf->closure); */
#endif
    int rc = ds->methods->format(ds->stack, fmt, sbuf, s & S_objiter);
#ifdef DEVBUILD
    log_debug("maybe_format format rc: %d", rc);
    log_debug("maybe_format format result: sbuf->value: %s", sbuf->value);
    log_debug("maybe_format format result: sbuf->length: %d", sbuf->length);
    log_debug("\tsbuf->releasecb: %x", sbuf->releasecb);
#endif
    return rc;
}

/* calls maybe_format - why not call it directly? */
static int format(struct datasource_s *ds,
                  const char *name, const char *fmt,
                  struct mustach_sbuf *sbuf)
{
    TRACE_ENTRY(format);
#ifdef DEVBUILD
    log_debug("\tname='%s', fmt: %s", name, fmt);
    log_debug("\tsbuf->value='%s'", name, sbuf->value);
    /* log_debug("sbuf->releasecb: %x", sbuf->releasecb); */
#endif
    /* struct datasource_s *ds= closure; */
    int rc = maybe_format(ds, name, fmt, sbuf); /* puts str val in sbuf.value */
#ifdef DEVBUILD
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
        //FIXME: use fstat, not fseek/ftell
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

/* partial can be a file or a datum, in the latter case,
   may have a format string */
static int partial(struct datasource_s *ds,
                   const char *name, const char *fmt,
                   struct mustach_sbuf *sbuf)
{
    TRACE_ENTRY(partial);
    /* struct datasource_s *ds= closure; */
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

static void dump_stack(struct datasource_s *ds)
{
    TRACE_ENTRY(dump_stack);
    ds->methods->dump_stack(ds->stack);
}

static const struct mustach_ds_mgr_methods_s toml_ds_mgr_methods = {
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
};

/* ****************************************************************
 * PRIVATE RENDER API
 **************************************************************** */
char *mustach_toml_render_to_string(const char *template,
                                  size_t template_sz,
                                  const struct mustach_ds_methods_s *toml_methods,
                                  void *stack, // struct tstack_s*
                                  int flags)
{
#ifdef DEVBUILD
    log_debug("mustach_toml_render_to_string");
    log_debug("flags: 0x%02x", flags);
#endif
    struct datasource_s ds;

    char *result; // Must be released by call to free
    size_t result_sz;

    datasource_init(&ds, toml_methods, stack, flags, NULL, NULL);
    int rc = mustach_mem(template, template_sz,
                         &toml_ds_mgr_methods,
                         &ds,
                         flags,
                         &result, &result_sz);
    if (rc) {
        log_error("mustach_mem rc: %d", rc);
        return NULL;
    }
    size_t ln = strlen(result);
    if (ln != result_sz) {
        log_warn("reported result sz %d does not match strlen %d",
                 result_sz, ln);
    }
    return result; // client must free
}

int mustach_toml_file(const char *template, size_t length,
                      const struct mustach_ds_methods_s *methods,
                      void *closure,
                      int flags, FILE *file)
{
	struct datasource_s ds;
	datasource_init(&ds, methods, closure, flags, NULL, NULL);
	return mustach_file(template, length, &toml_ds_mgr_methods, &ds, flags, file);
}

int mustach_toml_fd(const char *template, size_t length, const struct mustach_ds_methods_s *methods, void *closure, int flags, int fd)
{
	struct datasource_s ds;
	datasource_init(&ds, methods, closure, flags, NULL, NULL);
	return mustach_fd(template, length, &toml_ds_mgr_methods, &ds, flags, fd);
}

int mustach_toml_write(const char *template, size_t length, const struct mustach_ds_methods_s *methods, void *closure, int flags, mustach_write_cb_t *writecb, void *writeclosure)
{
	struct datasource_s ds;
	datasource_init(&ds, methods, closure, flags, NULL, writecb);
	return mustach_file(template, length, &toml_ds_mgr_methods, &ds, flags, writeclosure);
}

int mustach_toml_emit(const char *template, size_t length, const struct mustach_ds_methods_s *methods, void *closure, int flags, mustach_emit_cb_t *emitcb, void *emitclosure)
{
	struct datasource_s ds;
	datasource_init(&ds, methods, closure, flags, emitcb, NULL);
	return mustach_file(template, length, &toml_ds_mgr_methods, &ds, flags, emitclosure);
}

