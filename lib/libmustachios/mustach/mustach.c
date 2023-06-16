/*
 Author: José Bollo <jobol@nonadev.net>

 https://gitlab.com/jobol/mustach

 SPDX-License-Identifier: ISC
*/

/* #define _GNU_SOURCE // set by build pgm*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#ifdef _WIN32
#include <malloc.h>
#endif

#include "log.h"
#include "config.h"
#ifdef DEVBUILD
#include "ansi_colors.h"
/* #include "logging.h" */
#endif

#include "memfile.h"
#include "mustach.h"

bool lambda_emitted_value = true;

// this matches mustach_ds_mgr_methods_s?
struct iwrap {
    int (*emit)(void *closure, const char *buffer, size_t size, int escape, FILE *file);
    void *closure; /* datasource for: enter, next, leave, emit, format */
    int (*put)(void *closure, const char *name, const char *fmt, int escape, FILE *file);
    void *closure_put; /* closure for put */
    int (*enter)(void *closure, const char *name);
    int (*next)(void *closure);
    int (*leave)(void *closure, struct mustach_sbuf *sbuf);
    int (*format)(void*closure, const char *name, const char *fmt, struct mustach_sbuf *sbuf);
    int (*partial)(void *closure, const char *name, const char *fmt, struct mustach_sbuf *sbuf);
    void *closure_partial; /* closure for partial */
    int flags;
    void (*dump_stack)(void *closure);
};

/* prefix means line-prefix: constant text preceding tag on same line as tag
   <sol>  foo {{bar}} ... => pfx = '  foo '
   <sol> {{foo}} => pfx = ' '
   <sol>start\nfoo{{bar}} ... => pfx = '', 'start' will be emitted, then pfx = 'foo'
   i.e. processing is line-oriented
 */
struct prefix_s {
	size_t len;
	const char *start;
	struct prefix_s *prefix;
};

static inline void sbuf_reset(struct mustach_sbuf *sbuf)
{
	sbuf->value = NULL;
	sbuf->freecb = NULL;
	sbuf->closure = NULL;   /* not used? */
	sbuf->length = 0;
        sbuf->lambda = false;
}

static inline void sbuf_release(struct mustach_sbuf *sbuf)
{
    (void)sbuf;
    if (sbuf->releasecb) {
        if (sbuf->closure) {
#ifdef DEVBUILD
            log_debug("calling releasecb");
#endif
            sbuf->releasecb(sbuf->value, sbuf->closure);
        } else {
#ifdef DEVBUILD
            log_debug("freeing sbuf->value: %.15s", sbuf->value);
            log_debug("free addr: %x", free);
            log_debug("freecb addr: %x", sbuf->freecb);
#endif
            sbuf->freecb((void*)sbuf->value);
        }
    }
}

static inline size_t sbuf_length(struct mustach_sbuf *sbuf)
{
	size_t length = sbuf->length;
	if (length == 0 && sbuf->value != NULL)
		length = strlen(sbuf->value);
	return length;
}

static int iwrap_emit(void *closure, const char *buffer, size_t size, int escape, FILE *file)
{
    TRACE_ENTRY(iwrap_emit)
	size_t i, j, r;

	(void)closure; /* unused */

	if (!escape)
		return fwrite(buffer, 1, size, file) != size ? MUSTACH_ERROR_SYSTEM : MUSTACH_OK;

	r = i = 0;
	while (i < size) {
		j = i;
		while (j < size && buffer[j] != '<' && buffer[j] != '>' && buffer[j] != '&' && buffer[j] != '"')
			j++;
		if (j != i && fwrite(&buffer[i], j - i, 1, file) != 1)
			return MUSTACH_ERROR_SYSTEM;
		if (j < size) {
			switch(buffer[j++]) {
			case '<':
				r = fwrite("&lt;", 4, 1, file);
				break;
			case '>':
				r = fwrite("&gt;", 4, 1, file);
				break;
			case '&':
				r = fwrite("&amp;", 5, 1, file);
				break;
			case '"':
				r = fwrite("&quot;", 6, 1, file);
				break;
			}
			if (r != 1)
				return MUSTACH_ERROR_SYSTEM;
		}
		i = j;
	}
	return MUSTACH_OK;
}

/* get value for selection & write to file */
/* FIXME: write to workbuf, so result can be passed to lambda */
static int iwrap_put(void *closure, const char *name, const char *fmt,
                     int escape, FILE *file)
{
#ifdef DEVBUILD
    log_debug("iwrap_put");
#endif
	struct iwrap *iwrap = closure;
	int rc;
	struct mustach_sbuf sbuf;
	size_t length;

	sbuf_reset(&sbuf);
#ifdef DEVBUILD
        log_debug("iwrap_put calling iwrap->format");
#endif
	rc = iwrap->format(iwrap->closure, name, fmt, &sbuf);
#ifdef DEVBUILD
        log_debug("iwrap->format rc: %d", rc);
        log_debug("iwrap->format sbuf.value: %s", sbuf.value);
        log_debug("iwrap->format sbuf.lambda: %d", sbuf.lambda);
#endif
        if (sbuf.value == NULL)
            lambda_emitted_value = false;
        else
            lambda_emitted_value = true;
	if (rc >= 0) {
		length = sbuf_length(&sbuf);
		if (length) {
#ifdef DEVBUILD
                    log_debug("iwrap_put: calling iwrap->emit: %d", length);
#endif
			rc = iwrap->emit(iwrap->closure, sbuf.value, length, escape, file);
                }
		sbuf_release(&sbuf);
	}
	return rc;
}

static int iwrap_partial(void *closure,
                         const char *name, const char *fmt,
                         struct mustach_sbuf *sbuf)
{
    struct iwrap *iwrap = closure;
    int rc;
    FILE *file;
    size_t size;
    char *result;

    result = NULL;
    file = memfile_open(&result, &size);
    if (file == NULL)
        rc = MUSTACH_ERROR_SYSTEM;
    else {
        rc = iwrap->put(iwrap->closure_put, name, fmt, 0, file);
        if (rc < 0)
            memfile_abort(file, &result, &size);
        else {
            rc = memfile_close(file, &result, &size);
            if (rc == 0) {
                sbuf->value = result;
                sbuf->freecb = free;
                sbuf->length = size;
            }
        }
    }
    return rc;
}

/* called even if prefix.len == 0, since prefix->prefix may have non-zero */
static int emitprefix(struct iwrap *iwrap, FILE *file, struct prefix_s *prefix)
{
    TRACE_ENTRY(emitprefix);
    DUMP_PREFIX(prefix);
    if (prefix->prefix) {
#ifdef DEVBUILD
        log_debug("recurring on emitprefix");
#endif
       int rc = emitprefix(iwrap, file, prefix->prefix);
        if (rc < 0)
            return rc;
    }
#ifdef DEVBUILD
    if (prefix->len)
        log_debug("emitprefix calling iwrap->emit");
#endif
    return prefix->len ? iwrap->emit(iwrap->closure, prefix->start, prefix->len, 0, file) : 0;
}

void dump_metastack(int idx, void *_ms);

const char *_parse_format(char *key)
{
    TRACE_ENTRY(_parse_format);
    (void)key;
    return "";
}

/* called by mustach_file */
static int process(const char *template, size_t template_length, struct iwrap *iwrap, FILE *file, struct prefix_s *prefix, int escape)
{
    TRACE_ENTRY(process);
#ifdef DEVBUILD
    log_debug("\ttemplate: %.15s", template);
    log_debug("\ttemplate len: %d", template_length);
    log_debug("PREFIX: %p", prefix);
    DUMP_PREFIX(prefix);
#endif
    struct mustach_sbuf sbuf;
    // opstr == tag_open_delim, clstr == tag_close_delim
    char tag_open_delim[MUSTACH_MAX_DELIM_LENGTH], tag_close_delim[MUSTACH_MAX_DELIM_LENGTH];
    size_t tag_open_delim_len, tag_close_delim_len; /* was: oplen, cllen */

    char key[MUSTACH_MAX_LENGTH + 1]; /* tag key */
    const char *fmt = NULL; // format string, with leading '%' dropped
    char *pct;
    char c;
    const char *bokey = NULL;        /* begin of tag key? */
    const char *botag_close;  /* beginning of tag_close_delim_str, e.g. "}}" */
    const char *template_curr_ptr;   /* beginning of template, i.e. current ptr to rest of template */
    const char *eotemplate;        /* end-of-template (constant ptr) */
    struct metastack_s {
        const char *key; /* tagkey */
        size_t keylen;
        const char *section_content_start;  /* template ptr, set at section begin, used to control iteration in sections (was: 'again') */
        /* bitfields */
        unsigned enabled:   1;  /* meaning? */
        unsigned entered:   1; /* means hashtag has truthy value? */
        unsigned lambda:    1;
        int predicate; //FIXME: rename to meta_flags
        signed int workbuf_idx; /* -1 if hashtag is not a lambda; index into workbuf_stack */
    } metastack[MUSTACH_MAX_DEPTH];
    size_t keylen = 0;             /* length of (tag)key in stackframe? */
    size_t worklen;                  /* ???????????????? */
    int depth, rc, enabled;
    /* stdalone tag: tag on line by itself (and maybe whitespace), that produces no output, e.g. \n{{#foo}}\n */
    /* 0 - false; 1 - start, 2 - end? */
    int stdalone;
    int did_enter = 0;
    /* bool enable_smartcommas = false; */

    /* INITIALIZATION */
    /* log_debug("PREFIX: %p", prefix); */
    struct prefix_s pref;
    memset(&pref, '\0', sizeof(struct prefix_s));
    /* log_debug("pref ptr: %p", &pref); */
    /* log_debug("pref.PREFIX: %p", pref.prefix); */
    pref.prefix = prefix;   /* levels? */
    /* log_debug("set pref.PREFIX: %p", pref.prefix); */
    template_curr_ptr = template;
    eotemplate = template + (template_length ? template_length : strlen(template));
    tag_open_delim[0] = tag_open_delim[1] = '{';
    tag_close_delim[0] = tag_close_delim[1] = '}';
    tag_open_delim_len = tag_close_delim_len = 2;
    stdalone = enabled = 1;
    depth = pref.len = 0;

    for (;;) {
#ifdef DEVBUILD
        TRACE_LOG_DEBUG(BLU "PROCESS ITER" CRESET, "");
        log_debug("\tbokey: '%.30s'", bokey);
        log_debug("\ttemplate_curr_ptr: '%.30s'", template_curr_ptr);
        log_debug("\teotemplate - template_curr_ptr: %d", eotemplate - template_curr_ptr);
        log_debug("\tdepth: %d", depth);
        log_debug("\tkeylen: %d", keylen);
        DUMP_PREFIX(&pref);
        /* log_debug("\tpref.len: %d", pref.len); */
        /* log_debug("\tpref.start: %d", pref.start); */
        log_debug("\tenabled: %d", enabled);
        log_debug("\tstdalone: %d", stdalone);
        /* log_debug("predicate?: %d", ((struct closure_hdr*)iwrap->closure)->predicate); */
        /* dump_stack(((struct Xwrap*)iwrap->closure)->closure); */
        if (iwrap->dump_stack) {
            /* log_debug("dump_stack"); */
            iwrap->dump_stack(iwrap->closure);
        }
        /* dump_metastack(depth, &metastack[depth]); */
        /* DUMP_PREFIX(&pref); */
#endif

#ifdef DEVBUILD
        log_debug("adjusting...");
        log_debug("template_curr_ptr: %s", template_curr_ptr);
#endif
        /* emit prefix and prev line text; set bokey to next tagkey */
        for (bokey = template_curr_ptr ; ; bokey++) {
            c = (bokey == eotemplate) ? '\n' : *bokey;
            /* log_debug("bokey: %s", bokey); */
            if (c == '\n') {
                /* means we've read a line of const text (no tags); now emit it */
                worklen = (bokey != eotemplate) + (size_t)(bokey - template_curr_ptr);
                if (stdalone != 2 && enabled) {
                    /*  */
                    if (bokey != template_curr_ptr) { /* don't prefix empty lines */
#ifdef DEVBUILD
                        log_debug("adjusting: call emitprefix");
                        // where does this pfx come from?
#endif
                        rc = emitprefix(iwrap, file, &pref);
                        if (rc < 0)
                            return rc;
                    }
#ifdef DEVBUILD
                    log_debug("adjusting: call iwrap->emit");
#endif
                    /* will print const text from previous line, e.g. 'Hello\n' in 'Hello\nx{{foo}}'? */
                    rc = iwrap->emit(iwrap->closure, template_curr_ptr, worklen, escape, file);
#ifdef DEVBUILD
                    log_debug("iwrap->emit rc: %d", rc);
#endif
                    if (rc < 0)
                        return rc;
                }
                if (bokey == eotemplate) { /* no more template */
#ifdef DEVBUILD
                    log_debug("no more template, returning depth: %d", depth);
#endif
                    if (depth > 0) {
                        log_error("Unexpected end; stack depth > 0: %d", depth);
                        return MUSTACH_ERROR_UNEXPECTED_END;
                    } else {
                        return MUSTACH_OK;
                    }
                    /* return depth ? MUSTACH_ERROR_UNEXPECTED_END : MUSTACH_OK; */
                }
                template_curr_ptr += worklen; /* advance past newline */
                stdalone = 1; /* reset to default value */
                pref.len = 0;
            }
            else if (!isspace(c)) {
                /* log_debug("XXXXXXXXXXXXXXXX"); */
                if (stdalone == 2 && enabled) {
                    /* stdalone 2 means a 'partial', where prefix is not discarded? */
                    /* see https://github.com/mustache/spec/blob/master/specs/partials.yml */
                    /* "Each line of the partial should be indented before rendering." */
#ifdef DEVBUILD
                    log_debug("stdalone==2 && enabled: calling emitprefix2: pref.len: %d, pfx: %s", pref.len, pref.start);
#endif
                    rc = emitprefix(iwrap, file, &pref);
                    if (rc < 0)
                        return rc;
                    pref.len = 0;
                    stdalone = 0; /* why not 1? */
                }
                /* log_debug("tag_open_delim_len: %d", tag_open_delim_len); */
                /* break if c pts to start of tag_open_delim */
                if (c == *tag_open_delim && eotemplate - bokey >= (ssize_t)tag_open_delim_len) {
                    /* log_debug("1 XXXXXXXXXXXXXXXX"); */
                    for (worklen = 1 ; worklen < tag_open_delim_len && bokey[worklen] == tag_open_delim[worklen] ; worklen++);
                    if (worklen == tag_open_delim_len) {
                        /* log_debug("1a XXXXXXXXXXXXXXXX"); */
                        break;
                    }
                }
                /* log_debug("2 XXXXXXXXXXXXXXXX"); */
                stdalone = 0; /* why? */
            }
        }
        /* log_debug("3 XXXXXXXXXXXXXXXX"); */
        /* log_debug("bokey: %s", bokey); */
        /* DUMP_PREFIX(&pref); */

        /* now bokey should pt to start of next tag */
        /* template_curr_ptr points to end of prev tag or start of curr line(?) */
        /* so new pfx is text between curr_ptr and bokey  */
        /* note that template_curr_ptr has been advanced past newlines, so */
        /* the new prefix has no internal newlines */
        pref.start = template_curr_ptr;
        pref.len = enabled ? (size_t)(bokey - template_curr_ptr) : 0;
        bokey += tag_open_delim_len;
        /* log_debug("bokey: %s", bokey); */
        /* log_debug("eotemplate: %s", eotemplate); */

        /* find botag_close and set template_curr_ptr ptr to one after eotag_close */
        for (botag_close = bokey ; ; botag_close++) {
            /* log_debug("botag_close: %s", botag_close); */
            if (botag_close == eotemplate) {
                log_error("unexpected end");
                return MUSTACH_ERROR_UNEXPECTED_END;
            }
            /* if (*botag_close == *tag_close_delim) && eotemplate - botag_close >= (ssize_t)tag_close_delim_len) { */
            if (*botag_close == *tag_close_delim) {
                if (eotemplate - botag_close >= (ssize_t)tag_close_delim_len) {
                    for (worklen = 1 ; worklen < tag_close_delim_len && botag_close[worklen] == tag_close_delim[worklen] ; worklen++);
                    if (worklen == tag_close_delim_len) {
                        break;
                    } else {
                        log_error("Closing tag missing final delim?");
                        return MUSTACH_ERROR_TAG_SYNTAX;
                    }
                } else {
                    log_error("Bad closing tag: missing final delim? %s", bokey);
                    return MUSTACH_ERROR_TAG_SYNTAX;
                }
            }
        }
        template_curr_ptr = botag_close + tag_close_delim_len;
        keylen = (size_t)(botag_close - bokey);
        c = *bokey;

#ifdef DEVBUILD
        log_debug("adjusted:");
        DUMP_PREFIX(&pref);
        log_debug("\tbokey: %.15s", bokey);
        log_debug("\ttemplate_curr_ptr: '%.30s'", template_curr_ptr);
        log_debug("\ttemplate_curr_ptr - bokey: %d", template_curr_ptr - bokey);
        /* log_debug("\tend: %.15s", eotemplate); */
        log_debug("\teotemplate - bokey: %d", eotemplate - bokey);
        log_debug("\tkey: %.15s", key);
        log_debug("\tkeylen: %d", keylen);
        log_debug("\tenabled: %d", enabled);
        log_debug(BLU "switch1" CRESET);
        log_debug("\tmetachar: %c", c);
        log_debug("\tbokey[0]: %c", *bokey);
        log_debug("\tlookahead: %.15s", bokey+1);
#endif
        /* get key for tag, deal with unescapes, setp stdalone */
        switch(c) { /* metachars: #, ^, /, &, {, >, !, =, and extensions :, ?, $ */
        case ':':       /* extension (JSON only?) */
            stdalone = 0;
            if (iwrap->flags & Mustach_With_Colon) {
                TRACE_LOG_DEBUG("metachar: COLON", "");
                goto exclude_first;
            }
            goto get_key;
        case '!':       /* comment */
        case '=':       /* delimiters */
            break;
        case '{':       /* unescape, e.g. {{{key}}} */
#ifdef DEVBUILD
            log_debug("unescape: %c", c);
#endif
            for (worklen = 0 ; worklen < tag_close_delim_len && tag_close_delim[worklen] == '}' ; worklen++);
            if (worklen < tag_close_delim_len) {
                if (!keylen || bokey[keylen-1] != '}')
                    return MUSTACH_ERROR_BAD_UNESCAPE_TAG;
                keylen--;
            } else {
                if (botag_close[worklen] != '}')
                    return MUSTACH_ERROR_BAD_UNESCAPE_TAG;
                template_curr_ptr++;
            }
            c = '&';
            /*@fallthrough@*/
        case '&':       /* unescape e.g. {{& key}} */
#ifdef DEVBUILD
            log_debug("unescape: %c", c);
#endif
            stdalone = 0;
            /*@fallthrough@*/
        case PREDOP_FIRST:    /* '^' */
        case PREDOP_LAST:     /* '$' */
        case PREDOP_BUTLAST:  /* '?' */
            /* log_debug("*BOKEY: %c", *bokey); */
            /* log_debug("*(BOKEY+1): %c", *(bokey+1)); */
            /* log_debug("tag_close_delim[0]): %c", tag_close_delim[0]); */
            /* stdalone = 0; */
            /* goto get_key; */
            if (*(bokey+1) == tag_close_delim[0]) { /* {{?}} */
                stdalone = 0;
                goto get_key;
            } else {
                goto exclude_first;
            }
        case NEGOP: /* inverted section, '^' or '~' */
        case '#':       /* section (data collection) */
        case '/':       /* close section, inverted section */
        case '>':       /* 'partials' (file inclusion)  */
            // FORMATTING: a partial could resolve to a file or a datum.
            // in the latter case it could be atomic (e.g. a float),
            // in which case the tag could have a format specifier,
            // e.g.  {{>amount%08.2f}}
        exclude_first:
            bokey++;
            keylen--;
            goto get_key;
        default:        /* not one of the std/extension metachars */
            stdalone = 0;

        get_key:                       /* LABEL GET_KEY */

#ifdef DEVBUILD
            log_debug(BLU "get_key:" CRESET " %.30s", bokey);
#endif
            // trim whitespace
            while (keylen && isspace(bokey[0])) { bokey++; keylen--; }
            while (keylen && isspace(bokey[keylen-1])) keylen--;
#ifdef DEVBUILD
            log_debug("bokey: %.15s", bokey);
            log_debug("keylen: %d", keylen);
#endif
            if (keylen == 0 && !(iwrap->flags & Mustach_With_EmptyTag))
                return MUSTACH_ERROR_EMPTY_TAG;
            if (keylen > MUSTACH_MAX_LENGTH)
                return MUSTACH_ERROR_TAG_TOO_LONG;
#ifdef DEVBUILD
            log_debug("memcpy bokey: %.30s", bokey);
#endif
            memcpy(key, bokey, keylen);
            key[keylen] = 0;
#ifdef DEVBUILD
            log_debug("key: %s", key);
#endif
            pct = strchr(key, '%');
            if (pct) {
                /* log_debug("FOUND PCT"); */
                if (*(pct+1) == '%') {
                    /* log_debug("Std fmt: %s", pct); */
                    // std format string, e.g. %%05.2f
                    *pct = '\0';
                    fmt = pct + 1;
                    keylen -= strlen(fmt) + 1;
                } else if (strchr(pct, '$')) {
                    *pct = '\0';
                    fmt = pct + 1;
                    /* log_debug("Datetime format string: %s", fmt); */
                    keylen -= strlen(fmt) + 1;
                } else {
                    log_warn("key \"%s\" contains a single '%c'; it will NOT be treated as start of a format string. If it should start a format string, it must be followed by another '%c' (standard format string) or '%c' (toml datetime format string)", key, '%', '%', '$');
                }
#ifdef DEVBUILD
                log_debug("k: %s", key);
                log_debug("klen: %d", keylen);
                log_debug("fmt: %s", fmt);
#endif
            } else {
                fmt = NULL;
            }

            break;
        }
#ifdef DEVBUILD
        log_debug(BLU "end switch1" CRESET);
        log_debug("\tc: %c", c);
        log_debug("\tenabled: %d", enabled);
        log_debug("\tstdalone: %d", stdalone);
        log_debug("\tkeylen: %d", keylen);
        DUMP_PREFIX(&pref);
#endif
        if (stdalone)
            stdalone = 2;
        else if (enabled) {
            // if prev output was not #<unspecified>
            /* PREDOP_BUTLAST predicate will be handled on next iteration */
            /* if (c != PREDOP_BUTLAST) { */
#ifdef DEVBUILD
            log_debug("enabled: call emitprefix, pref.len: %zu, pfx: '%.30s ...'", pref.len, pref.start);
#endif
            // emits prefix for next iteration?
            rc = emitprefix(iwrap, file, &pref);
            if (rc < 0)
                return rc;
            pref.len = 0; /* prevent re-emit */
            /*                 } else { */
            /* #ifdef DEVBUILD */
            /*                     log_debug("SKIP emitprefix3 for ?"); */
            /* #endif */
            /* } */
        }
#ifdef DEVBUILD
        log_debug("enabled: %d", enabled);
        log_debug("keylen: %d", keylen);
        log_debug("template_curr_ptr: %.30s", template_curr_ptr);
        /* log_debug("metastack[%d].section_content_start: %.30s", depth, metastack[depth].section_content_start); */

        log_debug(BLU "switch2 on metachar" CRESET);
        log_debug("\tswitch (%c)", c);
        log_debug("\tbokey: %.15s", bokey);
#endif
        ((struct closure_hdr*)iwrap->closure)->predicate = NULL_P;

        // switch2
        switch(c) { /* !, =, ^, #, /, > */
        case '!':
            /* comment */
            /* nothing to do */
            break;
        case '=':
            /* defines delimiters */
            if (keylen < 5 || bokey[keylen - 1] != '=')
                return MUSTACH_ERROR_BAD_SEPARATORS;
            bokey++;
            keylen -= 2;
            while (keylen && isspace(*bokey))
                bokey++, keylen--;
            while (keylen && isspace(bokey[keylen - 1]))
                keylen--;
            for (worklen = 0; worklen < keylen && !isspace(bokey[worklen]) ; worklen++);
            if (worklen == keylen || worklen > MUSTACH_MAX_DELIM_LENGTH)
                return MUSTACH_ERROR_BAD_SEPARATORS;
            tag_open_delim_len = worklen;
            memcpy(tag_open_delim, bokey, worklen);
            while (worklen < keylen && isspace(bokey[worklen])) worklen++;
            if (worklen == keylen || keylen - worklen > MUSTACH_MAX_DELIM_LENGTH)
                return MUSTACH_ERROR_BAD_SEPARATORS;
            tag_close_delim_len = keylen - worklen;
            memcpy(tag_close_delim, bokey + worklen, tag_close_delim_len);
            break;
        case PREDOP_FIRST:      /* '^' */
#ifdef DEVBUILD
            log_debug(RED "PREDOP_FIRST" );
#endif
            ((struct closure_hdr*)iwrap->closure)->predicate = FIRST_P;
            metastack[depth].predicate = true;

        case PREDOP_LAST:       /* '$' */
            if (c == PREDOP_LAST) {
#ifdef DEVBUILD
                log_debug(RED "PREDOP_LAST" );
#endif
                ((struct closure_hdr*)iwrap->closure)->predicate = LAST_P;
                metastack[depth].predicate = LAST_P;
            }

        case PREDOP_BUTLAST:    /* '?' */
            if (c == PREDOP_BUTLAST) {
#ifdef DEVBUILD
                log_debug(RED "PREDOP_BUTLAST" );
#endif
                // WARNING: iwrap->closure has type struct wrap*, which has ptr to app struct expl
                // insane but true
                /* log_debug("pred1: %d", ((struct closure_hdr*)iwrap->closure)->predicate); */
                ((struct closure_hdr*)iwrap->closure)->predicate = BUTLAST_P;
                metastack[depth].predicate = BUTLAST_P;
            }
        case NEGOP:  // '^' or '~'
#ifdef DEVBUILD
            if (c == NEGOP) log_debug(RED "inversion op ^");
#endif
        case '#':
#ifdef DEVBUILD
            if (c == '#') log_debug(RED "section op #");
            log_debug(RED "section start for key: " CRESET "%s", key);
            log_debug("META char: %c", c);
            log_debug("bokey: %.15s", bokey);
            log_debug("keylen: %d", keylen);
            log_debug("depth: %d", depth);
            log_debug("predicate: 0x%04X", ((struct closure_hdr*)iwrap->closure)->predicate);
#endif
            /* w/in section OR inverted section */
            if (depth == MUSTACH_MAX_DEPTH)
                return MUSTACH_ERROR_TOO_DEEP;
            /* rc = enabled; */
            /* did_enter = rc; */
            if (enabled) {
                /* if (rc) { */
#ifdef DEVBUILD
                log_debug("enabled: call enter for tagkey: %s", key);
#endif
                /* iwrap->closure type is struct wrap* */
                did_enter /*rc*/ = iwrap->enter(iwrap->closure, key);
#ifdef DEVBUILD
                log_debug("iwrap did_enter: %d", did_enter);
                iwrap->dump_stack(iwrap->closure);

                log_debug("c: %c", c);
                log_debug("depth: %d", depth);
                log_debug("((struct closure_hdr*)iwrap->closure)->predicate): %d", ((struct closure_hdr*)iwrap->closure)->predicate);
#endif
                if (did_enter /*rc*/ < 0) // error?
                    return did_enter /*rc*/;
            }
            /* dump_stack(((struct Xwrap*)iwrap->closure)->closure); */
            metastack[depth].key = bokey;
            metastack[depth].keylen = keylen;
            metastack[depth].section_content_start = template_curr_ptr;
            metastack[depth].enabled = enabled != 0; /* ??? enabled != 0 always == enabled,  no? */
            metastack[depth].entered = did_enter; // same as rc != 0 ???
            /* metastack[depth].entered = rc != 0; /\* rc (enter result): boolean, 1 means metastack[depth] is collection *\/ */
#ifdef DEVBUILD
            dump_metastack(depth, &metastack[depth]);
            /* log_debug("\tsection_content_start: %.30", metastack[depth].section_content_start); */
            /* log_debug("\tkey: %.15s",  metastack[depth].key); */
            /* log_debug("\tkeylen: %d",  metastack[depth].keylen); */
            /* log_debug("\tkeylen: %d",  metastack[depth].keylen); */
#endif

            // at this point c == '#' OR c == NEGOP OR c == PREDOP_BUTLAST
            // set enabled for next iteration (not for current metastack frame)
            // original: if ((c == '#') == (rc == 0)) enabled = 0;
            // where rc == 0 means did not enter
            // i.e. disable if either # and not entered,
            // or ^ and entered entered
            if (c == '#') {
                enabled = did_enter;
            }
            else if (c == NEGOP) {
                enabled = !did_enter;
            }
            else if (c == PREDOP_FIRST) {
                enabled = did_enter;
            }
            else if (c == PREDOP_LAST) {
                enabled = did_enter;
            }
            else if (c == PREDOP_BUTLAST) {
                enabled = did_enter;
            }
            else {
                log_error("BAD METACHAR: %c", c);
                exit(EXIT_FAILURE);
            }

            // done with section start - bump stack ptr for next iteration
#ifdef DEVBUILD
            log_debug("set enabled flag to: %d", enabled);
            log_debug("incrementing depth from %d", depth);
#endif
            if (((struct closure_hdr*)iwrap->closure)->predicate) {
                depth++; /* for next stack frame? */
            } else {
                depth++; /* for next stack frame? */
            }
            /* } */
#ifdef DEVBUILD
            log_debug(RED "end section start for key: " CRESET "%s", key);
#endif
            break;

        case '/': /* section end tag **************************************************************** */
#ifdef DEVBUILD
            log_debug(RED "section end tag for key: " CRESET "%s", key);
#endif
#ifdef DEVBUILD
            log_info("bokey: %.15s", bokey);
            log_info("key: %.15s", key);
            log_info("keylen: %d", keylen);
            log_info("depth: %d", depth);
#endif
            // if (depth-- == 0 || keylen != metastack[depth].keylen || memcmp(metastack[depth].key, key, keylen))
            if (depth < 1 /*depth-- == 0*/) {
                log_error("depth < 1");
                return MUSTACH_ERROR_CLOSING;
            } else {
#ifdef DEVBUILD
                dump_metastack(depth, &metastack[depth]);
                log_debug("decrementing depth from: %d", depth);
#endif
                depth--;    /* why? it was used for subnodes of
                               this section so we need to pop the
                               stack so depth is idx for this
                               closing tag. but stackframe depth
                               remains unchanged */
#ifdef DEVBUILD
                dump_metastack(depth, &metastack[depth]);
                /* log_info("metastack[%d].key: %s", depth, metastack[depth].key); */
                /* log_info("metastack[%d].keylen: %d", depth, metastack[depth].keylen); */
                /* log_info("metastack[%d].section_content_start: %s", depth, metastack[depth].section_content_start); */
                /* log_info("metastack[%d].enabled: %d", depth, metastack[depth].enabled); */
                /* log_info("metastack[%d].entered: %d", depth, metastack[depth].entered); */
#endif
                // metastack ToS should now match current node?
                if (keylen != metastack[depth].keylen) {
                    log_debug("keylen %d != metastach[%d].keylen %d",
                              keylen, depth, metastack[depth].keylen);
                    return MUSTACH_ERROR_CLOSING;
                } else if (memcmp(metastack[depth].key, key, keylen)) {
#ifdef DEVBUILD
                    log_info("metastack[%d].key: %s", depth, key);
#endif
                    log_debug("3xxxx");
                    return MUSTACH_ERROR_CLOSING;
                }
            }
#ifdef DEVBUILD
            log_debug("enabled: %d", enabled);
            log_debug("metastack[%d].enabled: %d", depth, metastack[depth].enabled);
            log_debug("metastack[%d].entered: %d", depth, metastack[depth].entered);
#endif

            int has_next = 0; // false;
            /* rc = enabled && metastack[depth].entered ? iwrap->next(iwrap->closure) : 0; */
            if (enabled) {
                if (metastack[depth].entered) {
#ifdef DEVBUILD
                    log_debug("section '%s' next, depth %d", key, depth);
                    iwrap->dump_stack(iwrap->closure);
#endif
                    has_next = iwrap->next(iwrap->closure);
                }
            }
#ifdef DEVBUILD
            log_debug("has_next: %d", has_next);
#endif

            if (has_next < 0) /* next_error? */
                return has_next; // rc;
            if (has_next == 1) {
#ifdef DEVBUILD
                log_debug("setting template_curr_ptr to metastack[%d].section_content_start: %.30s", depth, template_curr_ptr);
                log_debug("has_next; incrementing depth from %d", depth);
#endif
                template_curr_ptr = metastack[depth].section_content_start; /* reset ptr for iteration on #tag content*/
                depth++;    /* for next iteration */
            } else {
                // has_next == 0
                enabled = metastack[depth].enabled;
                if (enabled && metastack[depth].entered) { // meaning we were in a section, need to leave
                    /* FIXME: if we're in a lambda #tag, apply lambda to workbuf and emit */
                    if (metastack[depth].lambda) {
#ifdef DEVBUILD
                        log_debug("APPLY LAMBDA");
#endif
                    }
#ifdef DEVBUILD
                    // no next, if not enabled && entered not need to leave
                    /* enable_smartcommas = false; */
                    log_debug("section '%s' close tag; leaving", key);
#endif
                    /* depth--; */
                    struct mustach_sbuf sbuf;
                    sbuf_reset(&sbuf);
                    // lambda only applied at close tag, sbuf will hold arg
                    iwrap->leave(iwrap->closure, &sbuf);
#ifdef DEVBUILD
                    log_debug("iwrap->leave sbuf.value: %s", sbuf.value);
#endif
                    int length = sbuf_length(&sbuf);
                    int rc;
                    (void)rc;
                    if (length > 0)
                        rc = iwrap->emit(iwrap->closure, sbuf.value,
                                         length, 0, file);
                }
            }
#ifdef DEVBUILD
            log_debug("case '/' break");
#endif
            break;
        case '>': /* partials */
#ifdef DEVBUILD
            log_debug("case '<' (partial");
#endif
            if (enabled) {
                sbuf_reset(&sbuf);
                rc = iwrap->partial(iwrap->closure_partial, key, fmt, &sbuf);
                if (rc >= 0) {
                    /* log_debug("Calling process for partial"); */
                    /* log_debug("Prefix ptr: %p", &pref); */
                    /* DUMP_PREFIX(&pref); */
                    rc = process(sbuf.value, sbuf_length(&sbuf), iwrap, file, &pref, 0); // escape);
                    sbuf_release(&sbuf);
                }
                if (rc < 0)
                    return rc;
            }
            break;
        default: /* *************** default: thunk-tag **************** */
#ifdef DEVBUILD
            log_debug("switch default case: thunk-tag");
            log_debug("\tno metachar, bokey: %.15s", bokey);
            log_debug("\tkey: '%s'", key);
            log_debug("\tkeylen: %d", keylen);
            log_debug("\tkey[keylen]: '%c'", key[keylen]);
            log_debug("\tkey[keylen] == '\\0'?: '%d'",
                      key[keylen] == '\0');
            log_debug("\t*key+keylen: '%c'", *(key+keylen));
            log_debug("\t'\\0': '%c'", '\0');
            log_debug("FMT: %s", fmt);
#endif

/* #if defined(WITH_FORMATTING) */
            /* fmt = _parse_format(key); */
            /* (void)fmt; */
/* #endif */

            /* replacement */
            if (enabled) { // iwrap_put inlined
                struct iwrap *iwrapper = iwrap->closure_put;
                int rc;
                struct mustach_sbuf sbuf;
                size_t length;

                sbuf_reset(&sbuf);
#ifdef DEVBUILD
                log_debug("calling iwrapper->format");
                /* log_debug("sbuf.releasecb: %x", sbuf.releasecb); */
#endif
                rc = iwrapper->format(iwrapper->closure, key, fmt, &sbuf);
#ifdef DEVBUILD
                log_debug("iwrapper->format rc: %d", rc);
                log_debug("iwrapper->format sbuf.value: %s", sbuf.value);
                log_debug("iwrapper->format sbuf.lambda: %d", sbuf.lambda);
                log_debug("sbuf.releasecb: %x", sbuf.releasecb);
#endif
                if (rc >= 0) {
                    length = sbuf_length(&sbuf);
                    if (length) {

                        if (sbuf.lambda) {
#ifdef DEVBUILD
                            log_debug("processing lambda result: %.15s", sbuf.value);
                            log_debug("c: %c", c);
#endif
                            // copied from '>' (partial) handler:
                            rc = process(sbuf.value, sbuf_length(&sbuf), iwrapper, file, &pref, c != '&');
#ifdef DEVBUILD
                            log_debug("returned from process, rc: %d", rc);
#endif
                            sbuf_release(&sbuf);
                        } else {
#ifdef DEVBUILD
                            log_debug("iwrapper_put: calling iwrapper->emit: %d", length);
#endif
                            rc = iwrapper->emit(iwrapper->closure,
                                                sbuf.value, length,
                                                c != '&', /* escape? */
                                                file);
                        }
                    }
#ifdef DEVBUILD
                    log_debug("releasing sbuf");
#endif
                    sbuf_release(&sbuf);
                }
                if (rc < 0)
                    return rc;
                // end iwrap_put inlined
                // by default iwrap->put == iwrap_put
                /* rc = iwrap->put(iwrap->closure_put, key, c != '&', file); */
                /* if (rc < 0) */
                /*     return rc; */
            } else {
#ifdef DEVBUILD
                log_debug("disabled");
#endif
            }
            break;
        }
#ifdef DEVBUILD
        log_debug(BLU "end switch2;" CRESET " looping");
#endif
    }
}

/* closure arg type struct wrap* */
// called directly or indirectly by all render fns, e.g. mustach_mem
int mustach_file(const char *template, size_t length,
                 const struct mustach_ds_mgr_methods_s *ds_mgr_methods,
                 void *datasource, // struct datasource_s*
                 int flags, FILE *file)
{
#ifdef DEVBUILD
    log_debug("mustach_file");
    log_debug("flags: 0x%02x", flags);
#endif
	int rc;
	struct iwrap iwrap; // why do we need this wrapper?

	/* check validity */
	if (!ds_mgr_methods->enter
            || !ds_mgr_methods->next
            || !ds_mgr_methods->leave
            || (!ds_mgr_methods->put && !ds_mgr_methods->format))
		return MUSTACH_ERROR_INVALID_ITF;

	/* init wrap structure */
	iwrap.closure = datasource; // struct wrap,
	if (ds_mgr_methods->put) {
		iwrap.put = ds_mgr_methods->put;
		iwrap.closure_put = datasource;
	} else {
		iwrap.put = iwrap_put;
		iwrap.closure_put = &iwrap;
	}
	if (ds_mgr_methods->partial) {
		iwrap.partial = ds_mgr_methods->partial;
		iwrap.closure_partial = datasource;
	} else if (ds_mgr_methods->format) {
		iwrap.partial = ds_mgr_methods->format;
		iwrap.closure_partial = datasource;
	} else {
		iwrap.partial = iwrap_partial;
		iwrap.closure_partial = &iwrap;
	}
	iwrap.emit = ds_mgr_methods->emit ? ds_mgr_methods->emit : iwrap_emit;
	iwrap.enter = ds_mgr_methods->enter;
	iwrap.next = ds_mgr_methods->next;
	iwrap.leave = ds_mgr_methods->leave;
	iwrap.format = ds_mgr_methods->format;
	iwrap.flags = flags;
	iwrap.dump_stack = ds_mgr_methods->dump_stack;

	/* process */
	rc = ds_mgr_methods->start ? ds_mgr_methods->start(datasource) : 0;
	if (rc == 0) {
            rc = process(template, length, &iwrap, file,
                         NULL, // 0,     /* struct prefix_s* */
                         0);   /* escape */
                /* log_debug("Process rc: %d", rc); */
        } else {
            log_error("start failed, rc: %d", rc);
        }
	if (ds_mgr_methods->stop)
		ds_mgr_methods->stop(datasource, rc);
	return rc;
}

int mustach_fd(const char *template, size_t length, const struct mustach_ds_mgr_methods_s *itf, void *closure, int flags, int fd)
{
	int rc;
	FILE *file;

	file = fdopen(fd, "w");
	if (file == NULL) {
		rc = MUSTACH_ERROR_SYSTEM;
		errno = ENOMEM;
	} else {
		rc = mustach_file(template, length, itf, closure, flags, file);
		fclose(file);
	}
	return rc;
}

/* NB: the closure arg here has type struct wrap* */
// NB: result must be released by free
int mustach_mem(const char *template, size_t length,
                const struct mustach_ds_mgr_methods_s *ds_mgr_methods,
                void *closure, // struct datasource_s* (wrap.h)
                int flags, char **result, size_t *size)
{
#ifdef DEVBUILD
    log_debug("mustach_mem");
#endif
    int rc;
    FILE *file;
    size_t s;

    *result = NULL;
    if (size == NULL)
        size = &s;
    file = memfile_open(result, size); // returns tmpfile()
    if (file == NULL)
        rc = MUSTACH_ERROR_SYSTEM;
    else {
        // mustach_file writes to file stream
        rc = mustach_file(template, length, ds_mgr_methods, closure, flags, file);
        if (rc < 0)
            memfile_abort(file, result, size);
        else
            // memstream: adds terminal null, does not count it in sz
            rc = memfile_close(file, result, size);
    }
    return rc;
}

int fmustach(const char *template, const struct mustach_ds_mgr_methods_s *itf, void *closure, FILE *file)
{
	return mustach_file(template, 0, itf, closure, Mustach_With_AllExtensions, file);
}

int fdmustach(const char *template, const struct mustach_ds_mgr_methods_s *itf, void *closure, int fd)
{
	return mustach_fd(template, 0, itf, closure, Mustach_With_AllExtensions, fd);
}

int mustach(const char *template, const struct mustach_ds_mgr_methods_s *itf, void *closure, char **result, size_t *size)
{
	return mustach_mem(template, 0, itf, closure, Mustach_With_AllExtensions, result, size);
}

/* **************************************************************** */
#ifdef DEVBUILD
void dump_metastack(int idx, void *_ms)
{
    (void)idx;
    (void)_ms;
    /* struct metastack_s *ms = (struct metastack_s*)_ms; */
    /* log_debug("DUMP_METASTACK at %d", idx); */
    /* log_debug("\tkey: %.15s", ms->key); */
    /* log_debug("\tkeylen: %d", ms->keylen); */
    /* log_debug("\tsection_content_start: %.15s", ms->section_content_start); */
    /* log_debug("\tenabled: %d", ms->enabled); */
    /* log_debug("\tentered: %d", ms->entered); */
    /* log_debug("\tpredicate: %d", ms->predicate); */
    /* log_debug("\tlambda: %d", ms->lambda); */
    /* log_debug("\tworkbuf_idx: %d", ms->workbuf_idx); */
    /* log_debug("end metastack"); */
    fflush(NULL);
}
#endif
