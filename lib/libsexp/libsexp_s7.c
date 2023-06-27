/*
  protocol:
  (sexp:read)
    g_sexp_read
      _g_sexp_read
        _sexp_read_thunk

  read error:
  (sexp:read)
    g_sexp_read
      _g_sexp_read
        _sexp_read_thunk
        _sexp_read_catcher
          fix_sexpfile
            sexpfile_to_string
          _sexp_read_thunk
            returns to

 */

#include <assert.h>
#include <ctype.h>
#include <fcntl.h>
#include <libgen.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <unistd.h>

#if defined(DEVBUILD)
#include <execinfo.h>           /* backtrace */
#include <unistd.h>             /* write */
#endif

#include "config.h"
#include "utstring.h"
#include "libsexp_s7.h"
#include "error_handler_sexp.h"
#include "s7.h"

//extern FIXME
bool  verbose;

s7_pointer c_pointer_string, string_string, character_string, boolean_string, real_string, complex_string;
s7_pointer integer_string;
static s7_pointer int64_t__symbol, FILE__symbol;

s7_pointer result;

s7_pointer _inport_sym, _sexps_sym;

/* needed by read thunk and catcher */
/* const char *g_sexpfile; */
/* s7_pointer g_sexp_inport; */
/* s7_int gc_sexp_inport = -1; */
/* s7_pointer g_stanzas; */
/* s7_int gc_stanzas = -1; */

s7_pointer e7; // tmp var for error printing
const char *e; // tmp var for error printing

const char *errmsg;

s7_pointer _sexp_read_catcher_s7;
s7_int gc_sexp_read_catcher_s7 = -1;

const char *sexpfile_to_string(s7_scheme *s7, const char *sexpfile_name)
{
    TRACE_ENTRY(sexpfile_to_string);
#if defined(TRACING)
    log_trace("sexpfile: %s", sexpfile_name);
                  //utstring_body(sexpfile_name));
    s7_pointer cip = s7_current_input_port(s7);
    TRACE_S7_DUMP("cip", cip);
#endif
    /* core/sexp file size: 45572 */
    // 2K

    //FIXME: malloc
/* #define SEXP_BUFSZ 131072 */
/*     /\* static char inbuf[SEXP_BUFSZ]; *\/ */
/*     /\* memset(inbuf, '\0', SEXP_BUFSZ); *\/ */
/*     static char outbuf[SEXP_BUFSZ + 20]; */
/*     memset(outbuf, '\0', SEXP_BUFSZ); */

    size_t file_size;
    char *inbuf;
    struct stat stbuf;
    int fd;
    FILE *instream = NULL;

    errno = 0;
    fd = open(sexpfile_name, O_RDONLY);
    if (fd == -1) {
        /* Handle error */
        log_error("fd open error");
    log_trace("cwd: %s", getcwd(NULL, 0));
        s7_error(s7, s7_make_symbol(s7, "fd-open-error"),
                 s7_list(s7, 3,
                         s7_make_string(s7, "fd open error: ~A, ~A"),
                         s7_make_string(s7, sexpfile_name),
                         s7_make_string(s7, strerror(errno))));
    }

    if ((fstat(fd, &stbuf) != 0) || (!S_ISREG(stbuf.st_mode))) {
        /* Handle error */
        log_error("fstat error");
        goto cleanup;
    }

    file_size = stbuf.st_size;
#if defined(DEVBUILD)
    log_debug("filesize: %d", file_size);
#endif

    inbuf = (char*)calloc(file_size, sizeof(char));
    if (inbuf == NULL) {
        /* Handle error */
        log_error("malloc file_size fail");
        goto cleanup;
    }

    /* FIXME: what about e.g. unicode in string literals? */
    errno = 0;
    instream = fdopen(fd, "r");
    if (instream == NULL) {
        /* Handle error */
        log_error("fdopen failure: %s", sexpfile_name);
        /* printf(RED "ERROR" CRESET "fdopen failure: %s\n", */
        /*        sexpfile_name); */
               /* utstring_body(sexpfile_name)); */
        perror(NULL);
        close(fd);
        goto cleanup;
    } else {
#if defined(DEVBUILD)
        log_debug("fdopened %s",
                  sexpfile_name);
        /* utstring_body(sexpfile_name)); */
#endif
    }

    // now read the entire file
    size_t read_ct = fread(inbuf, 1, file_size, instream);
#if defined(DEVBUILD)
    log_debug("read_ct: %d", read_ct);
#endif
    if (read_ct != file_size) {
        if (ferror(instream) != 0) {
            /* printf(RED "ERROR" CRESET "fread error 2 for %s\n", */
            /*        sexpfile_name); */
            /* utstring_body(sexpfile_name)); */
            log_error("fread error 2 for %s\n",
                      sexpfile_name);
            /* utstring_body(sexpfile_name)); */
            exit(EXIT_FAILURE); //FIXME: exit gracefully
        } else {
            if (feof(instream) == 0) {
                /* printf(RED "ERROR" CRESET "fread error 3 for %s\n", */
                /*        sexpfile_name); */
                /* utstring_body(sexpfile_name)); */
                log_error("fread error 3 for %s\n",
                          sexpfile_name);
                /* utstring_body(sexpfile_name)); */
                exit(EXIT_FAILURE); //FIXME: exit gracefully
            } else {
                //FIXME
                log_error("WTF????????????????");
                goto cleanup;
            }
        }
    } else {
        close(fd);
        fclose(instream);
    }
    inbuf[read_ct + 1] = '\0';
    uint64_t outFileSizeCounter = file_size * 2;
    char *outbuf = calloc(outFileSizeCounter, sizeof(char));
    /* memset(outbuf, '\0', fileSize); */

    // FIXME: loop over the entire inbuf char by char, testing for
    // . or "\|
    char *inptr = (char*)inbuf;
    /* log_debug("INPTR str: %s", inptr); */
    char *outptr = (char*)outbuf;
    /* char *cursor = inptr; */

    bool eol_string = false;
    while (*inptr) {
        if (*inptr == '.') {
            if (*(inptr+1) == ')'){
                /* log_debug("FOUND DOT: %s", inptr); */
                *outptr++ = *inptr++;
                *outptr++ = '/';
                continue;
            }
        }
        if (*inptr == '"') {
            if (*(inptr+1) == '\\') {
                if (*(inptr+2) == '|') {
                    /* log_debug("FOUND EOL Q"); */
                    *outptr++ = *inptr++; // copy '"'
                    inptr += 2; // point to char after '"\|'
                    eol_string = true;
                    while (eol_string) {
                        if (*inptr == '\0') {
                            *outptr = *inptr;
                            eol_string = false;
                        }
                        if (*inptr == '\n') {
                            /* log_debug("hit eolstring newline"); */
                            // check to see if next line starts with "\|
                            char *tmp = inptr + 1;
                            while (isspace(*tmp)) {tmp++;}
                            /* log_debug("skipped to: %s", tmp); */
                            if (*(tmp) == '"') {
                                if (*(tmp+1) == '\\') {
                                    if (*(tmp+2) == '|') {
                                        // preserve \n
                                        *outptr++ = *inptr;
                                        /* *outptr++ = '\\'; */
                                        /* *outptr++ = 'n'; */
                                        inptr = tmp + 3;
                                        /* log_debug("resuming at %s", inptr); */
                                        continue;
                                    }
                                }
                            }
                            *outptr++ = '"';
                            inptr++; // omit \n
                            eol_string = false;
                        } else {
                            *outptr++ = *inptr++;
                        }
                    }
                }
            }
        }
        *outptr++ = *inptr++;
    }

/*     inptr = (char*)inbuf; */
/*     while (true) { */
/*         cursor = strstr(inptr, ".)"); */

/* /\* https://stackoverflow.com/questions/54592366/replacing-one-character-in-a-string-with-multiple-characters-in-c *\/ */

/*         if (cursor == NULL) { */
/* /\* #if defined(DEVBUILD) *\/ */
/* /\*             if (mibl_debug) log_debug("remainder: '%s'", inptr); *\/ */
/* /\* #endif *\/ */
/*             size_t ct = strlcpy(outptr, (const char*)inptr, file_size); // strlen(outptr)); */
/*             (void)ct;           /\* prevent -Wunused-variable *\/ */
/* /\* #if defined(DEVBUILD) *\/ */
/* /\*             if (mibl_debug) log_debug("concatenated: '%s'", outptr); *\/ */
/* /\* #endif *\/ */
/*             break; */
/*         } else { */
/* #if defined(DEVBUILD) */
/*             log_error("FOUND and fixing \".)\" at pos: %d", cursor - inbuf); */
/* #endif */
/*             size_t ct = strlcpy(outptr, (const char*)inptr, cursor - inptr); */
/* #if defined(DEVBUILD) */
/*             log_debug("copied %d chars", ct); */
/*             /\* log_debug("to buf: '%s'", outptr); *\/ */
/* #endif */
/*             /\* if (ct >= SEXP_BUFSZ) { *\/ */
/*             if (ct >= outFileSizeCounter) { */
/*                 printf("output string has been truncated!\n"); */
/*             } */
/*             outptr = outptr + (cursor - inptr) - 1; */
/*             outptr[cursor - inptr] = '\0'; */
/*             //FIXME: use memcpy */
/*             ct = strlcat(outptr, " ./", outFileSizeCounter); // SEXP_BUFSZ); */
/*             outptr += 3; */

/*             inptr = inptr + (cursor - inptr) + 1; */
/*             /\* printf(GRN "inptr:\n" CRESET " %s\n", inptr); *\/ */

/*             if (ct >= outFileSizeCounter) { // SEXP_BUFSZ) { */
/*                 log_error("write count exceeded output bufsz\n"); */
/*                 /\* printf(RED "ERROR" CRESET "write count exceeded output bufsz\n"); *\/ */
/*                 free(inbuf); */
/*                 exit(EXIT_FAILURE); */
/*                 // output string has been truncated */
/*             } */
/*         } */
/*     } */
    free(inbuf);
    return outbuf;

cleanup:
    //FIXME
    if (instream != NULL)
    {
        fclose(instream);
        close(fd);
    }
    return NULL;
}

static s7_pointer _sexp_read_input_port(s7_scheme*s7, s7_pointer port);
s7_pointer fix_sexpfile(s7_scheme *s7, const char *sexpfile_name)
{
    TRACE_ENTRY(fix_sexpfile);
    TRACE_LOG_DEBUG("sexpfile: %s", sexpfile_name);

    const char *sexpstring = sexpfile_to_string(s7, sexpfile_name);

    /* log_debug("read corrected string: %s", sexpstring); */

    /* now s7_read using string port */

    /* first config err handling. clears out prev. error */
    /* close_error_config(); */
    /* error_config(); */
    /* init_error_handling(); */

    s7_pointer _inport = s7_open_input_string(s7, sexpstring);
    if (!s7_is_input_port(s7, _inport)) { // g_sexp_inport)) {
        log_error("BAD INPUT PORT");
        s7_error(s7, s7_make_symbol(s7, "bad input port"),
                     s7_nil(s7));
    }
#if defined(DEVBUILD)
    log_debug("s7_open_input_string for error correction");
#endif

    result = _sexp_read_input_port(s7, _inport);
    return result;


    if (s7_curlet(s7) == s7_nil(s7)) {
        log_warn("curlet is '()");
    }
    TRACE_S7_DUMP("curlet", s7_curlet(s7));
    /* } else { */
    /* log_debug("varletting curlet..."); */
    /* s7_varlet(s7, s7_curlet(s7), */
    /*            s7_make_symbol(s7, "-sexp-inport"), */
    /*            _inport); */

    /* s7_varlet(s7, s7_curlet(s7), */
    /*               s7_make_symbol(s7, "-sexp-infile"), */
    /*               s7_make_string(s7, sexpfile_name)); */

    /* s7_varlet(s7, s7_curlet(s7), */
    /*               s7_make_symbol(s7, "-sexp-sexps"), */
    /*               s7_nil(s7)); */

    log_debug("SEXPFILE_NAME: %s", sexpfile_name);
    s7_pointer env = s7_inlet(s7,
                              s7_list(s7, 1,
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "-sexp-inport"),
                                              _inport),
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "-sexp-infile"),
                                              s7_make_string(s7, sexpfile_name)),

                                      s7_cons(s7,
                                              s7_make_symbol(s7, "-sexp-sexps"),
                                              s7_nil(s7))
                                      ));
    // WARNING: if the call to -g-sexp-read raises an error that gets
    // handled by the catcher, then the continuation is whatever
    // called this routine (i.e. the c stack), NOT the assignment to
    // result below.
    s7_pointer stanzas = s7_eval_c_string_with_environment(s7, "(apply -g-sexp-read (list -sexp-inport))", env);

    //FIXME: this should use s7_eval_c_string... in case sexpstring
    //has an error.

    // (with-let (inlet ...)
    //        (with-input-from-string sexpstring sexp:read))

    /* s7_pointer stanzas = s7_call_with_catch(s7, */
    /*                                 s7_t(s7),      /\* tag *\/ */
    /*                                 // _sexp_read_thunk_s7 */
    /*                                 s7_name_to_value(s7, "-sexp-read-thunk"), */
    /*                                 _sexp_read_catcher_s7 */
    /*                                 /\* s7_name_to_value(s7, "-sexp-read-thunk-catcher") *\/ */
    /*                                 ); */
    TRACE_S7_DUMP("fixed stanzas", stanzas);

    /* s7_close_input_port(s7, sport); */
    // g_sexp_inport will be closed by caller
    /* s7_gc_unprotect_at(s7, gc_baddot_loc); */
    /* close_error_config(); */

    /* leave error config as-is */
    free((void*)sexpstring);
    /* return s7_reverse(s7, stanzas); */
    return stanzas;
}

s7_pointer x_fix_sexpfile(s7_scheme *s7, const char *sexpfile_name)
{
    TRACE_ENTRY(fix_sexpfile);
    TRACE_LOG_DEBUG("sexpfile: %s", sexpfile_name);

    const char *sexpstring = sexpfile_to_string(s7, sexpfile_name);

    log_debug("readed string: %s", sexpstring);

    /* now s7_read using string port */

    /* first config err handling. clears out prev. error */
    /* close_error_config(); */
    /* error_config(); */
    /* init_error_handling(); */

    s7_pointer _inport = s7_open_input_string(s7, sexpstring);
    if (s7_curlet(s7) == s7_nil(s7)) {
        log_warn("curlet is '()");
    }
    TRACE_S7_DUMP("curlet", s7_curlet(s7));
    /* } else { */
    log_debug("varletting curlet...");
    s7_varlet(s7, s7_curlet(s7),
               s7_make_symbol(s7, "-sexp-inport"),
               _inport);

    s7_varlet(s7, s7_curlet(s7),
                  s7_make_symbol(s7, "-sexp-infile"),
                  s7_make_string(s7, sexpfile_name));

    s7_varlet(s7, s7_curlet(s7),
                  s7_make_symbol(s7, "-sexp-sexps"),
                  s7_nil(s7));
    /* } */
    /* g_sexp_inport = s7_open_input_string(s7, sexpstring); */
    /* s7_int gc_baddot_loc = s7_gc_protect(s7, g_sexp_inport); */

    /* s7_pointer _inport = s7_let_ref(s7, s7_curlet(s7), */
    /*                                 s7_make_symbol(s7, "-sexp-inport")); */
    /* TRACE_S7_DUMP("inport", _inport); */

    /* errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); */

    if (!s7_is_input_port(s7, _inport)) { // g_sexp_inport)) {
        log_error("BAD INPUT PORT");
        s7_error(s7, s7_make_symbol(s7, "bad input port"),
                     s7_nil(s7));
        /* errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); */
        /* if ((errmsg) && (*errmsg)) { */
        /*     log_error("[%s\n]", errmsg); */
        /*     /\* s7_shutdown(s7); *\/ */
        /*     /\* exit(EXIT_FAILURE); *\/ */
        /* } */
    }
#if defined(DEVBUILD)
    log_debug("s7_open_input_string for error correction");
#endif

    //FIXME: this should use s7_eval_c_string... in case sexpstring
    //has an error.

    // (with-let (inlet ...)
    //        (with-input-from-string sexpstring sexp:read))

    s7_pointer stanzas = s7_call_with_catch(s7,
                                    s7_t(s7),      /* tag */
                                    // _sexp_read_thunk_s7
                                    s7_name_to_value(s7, "-sexp-read-thunk"),
                                    _sexp_read_catcher_s7
                                    /* s7_name_to_value(s7, "-sexp-read-thunk-catcher") */
                                    );
    /* TRACE_S7_DUMP("fixed stanzas", stanzas); */

    /* s7_close_input_port(s7, sport); */
    // g_sexp_inport will be closed by caller
    /* s7_gc_unprotect_at(s7, gc_baddot_loc); */
    /* close_error_config(); */

    /* leave error config as-is */
    free((void*)sexpstring);
    /* return s7_reverse(s7, stanzas); */
    return stanzas;
}

#if defined(DEVBUILD)
/* https://stackoverflow.com/questions/6934659/how-to-make-backtrace-backtrace-symbols-print-the-function-names */
static void full_write(int fd, const char *buf, size_t len)
{
        while (len > 0) {
                ssize_t ret = write(fd, buf, len);

                if ((ret == -1) && (errno != EINTR))
                        break;

                buf += (size_t) ret;
                len -= (size_t) ret;
        }
}

void print_c_backtrace(void) // s7_scheme *s7)
{
    static const char s7_start[] = "S7 STACKTRACE ------------\n";
    static const char c_start[] = "C BACKTRACE ------------\n";
    static const char end[] = "----------------------\n";

    full_write(STDERR_FILENO, s7_start, strlen(s7_start));

    /* s7_show_stack(s7); */
    /* s7_stacktrace(s7); */
    /* s7_flush_output_port(s7, s7_current_error_port(s7)); */
    /* fflush(NULL); */

    full_write(STDERR_FILENO, end, strlen(end));

    void *bt[1024];
    int bt_size;
    char **bt_syms;
    int i;

    bt_size = backtrace(bt, 1024);
    bt_syms = backtrace_symbols(bt, bt_size);
    full_write(STDERR_FILENO, c_start, strlen(c_start));
    for (i = 1; i < bt_size; i++) {
        size_t len = strlen(bt_syms[i]);
        full_write(STDERR_FILENO, bt_syms[i], len);
        full_write(STDERR_FILENO, "\n", 1);
    }
    full_write(STDERR_FILENO, end, strlen(end));
    free(bt_syms);
    /* fflush(NULL); */
}
#endif

/*
 * precondition: env contains -inport, -sexps (accumulator)
 */
s7_pointer _sexp_read_thunk(s7_scheme *s7, s7_pointer args)
/* s7_pointer _sexp_read_port(s7_scheme *s7, s7_pointer inport) */
{
    TRACE_ENTRY(_sexp_read_thunk);
    (void)args;
#if defined(DEVBUILD)
    log_trace("cwd: %s", getcwd(NULL, 0));

    s7_pointer _curlet = s7_curlet(s7);
    char *tmp = s7_object_to_c_string(s7, _curlet);
    log_debug("CURLET: %s", tmp);
    free(tmp);
#endif
    /* TRACE_S7_DUMP("outlet", s7_outlet(s7, s7_curlet(s7))); */

    s7_pointer _inport = s7_let_ref(s7, s7_curlet(s7), _inport_sym);
    TRACE_S7_DUMP("inport", _inport);

    s7_pointer _sexps = s7_let_ref(s7, s7_curlet(s7), _sexps_sym);
    TRACE_S7_DUMP("_sexps", _sexps);

    // PROBLEM: if we hit a read-error after expanding an include,
    // then the corrected string will be passed as a string port, and
    // the original filename will be lost. But we need the dirname of
    // the original file because the include file is relative. So the
    // catcher puts -sexp-infile into the curlet.
    // No way to test if _inport is a string port, so we use brute force.
    const char *sexpfile = s7_port_filename(s7, _inport); // g_sexp_inport);
    if (sexpfile == NULL) {
        /* log_debug("no filename for inport"); */
        /* s7_pointer sexpfile7 = s7_let_ref(s7, s7_curlet(s7), */
        /*                        s7_make_symbol(s7, "-sexp-infile")); */
        /* TRACE_S7_DUMP("sexpfile7", sexpfile7); */
        /* sexpfile = s7_string(sexpfile7); */
    }
    /* TRACE_LOG_DEBUG("inport file: %s", sexpfile); */

    if (!s7_is_input_port(s7, _inport)) { // g_sexp_inport)) {
        errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
        if ((errmsg) && (*errmsg)) {
            log_error("[%s\n]", errmsg);
            //FIXME: throw error
            libs7_shutdown(s7);
            exit(EXIT_FAILURE);
        }
        //FIXME: throw error
    } else {
#if defined(DEVBUILD)
        /* s7_pointer port_filename = s7_call(s7, */
        /*                               s7_name_to_value(s7, "port-filename"), */
        /*                               s7_list(s7, 1, inport)); */
        /*     log_trace("reading sexpfile from input port: %s", */
        /*               TO_STR(port_filename)); */
#endif
    }

    /* g_stanzas = s7_list(s7, 0); // s7_nil(s7)); */
    /* gc_stanzas = s7_gc_protect(s7, g_stanzas); */

    // so read thunk can access port:
    /* g_sexpfile_port = inport; */
    /* gc_sexp_inport = s7_gc_protect(s7, g_sexpfile_port); */
    /* gc_sexp_inport = s7_gc_protect(s7, g_sexp_inport); */

#if defined(DEVBUILD)
    log_debug("reading stanzas");
    // from sexpfile: %s", sexpfile);
#endif

    //FIXME: error handling
    /* close_error_config(); */
    /* error_config(); */
    /* init_error_handling(); */

    /* s7_show_stack(s7); */
/* #if defined(DEVBUILD) */
/*     print_c_backtrace(); */
/* #endif */

    /* repeat until all objects read */
    while(true) {
#if defined(DEVBUILD)
        log_trace("reading next stanza");
#endif

        /* s7_show_stack(s7); */
        /* print_c_backtrace(); */
        s7_pointer stanza = s7_read(s7, _inport); // g_sexp_inport);
        if (stanza == s7_eof_object(s7)) {
            /* log_debug("EOF"); */
            break;
        }

/* #if defined(DEVBUILD) */
        TRACE_S7_DUMP("Readed stanza", stanza);
/* #endif */
        /* s7_show_stack(s7); */
        /* print_c_backtrace(); */
        /* errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); */
        /* log_error("errmsg: %s", errmsg); */


        /* close_error_config(); */
        /* init_error_handling(); */
        /* error_config(); */
        /* s7_gc_unprotect_at(s7, gc_sexp_inport); */

        if (stanza == s7_eof_object(s7)) {
#if defined(DEVBUILD)
            log_trace("readed eof");
#endif
            break;
        }

        /* LOG_S7_DEBUG("SEXP", stanza); */
        /* if (mibl_debug_traversal) */
        /*     LOG_S7_DEBUG("stanza", stanza); */


        // handle (include ...) stanzas
        if (s7_is_pair(stanza)) {
            if (s7_is_equal(s7, s7_car(stanza),
                            s7_make_symbol(s7, "include"))) {
                log_debug("FOUND (include ...)");
                /* we can't insert a comment, e.g. ;;(include ...)
                   instead we would have to put the included file in an
                   alist and add a :comment entry. but we needn't bother,
                   we're not going for roundtrippability.
                */

                s7_pointer inc_file = s7_cadr(stanza);
                TRACE_S7_DUMP("    including", inc_file);

                log_debug("sexpfile: %s", sexpfile);
                char *tmp = strdup(sexpfile);
                const char *dir = dirname(tmp);
                free((void*)tmp);

                UT_string *sexppath;
                utstring_new(sexppath);
                const char *tostr = s7_object_to_c_string(s7, inc_file);
                log_debug("INCFILE: %s", tostr);
                utstring_printf(sexppath,
                                "%s/%s",
                                //FIXME: dirname may mutate its arg
                                //dirname(path),
                                dir,
                                tostr);

                /* g_sexpfile_port = sexpfile_port; */
                /* /\* LOG_S7_DEBUG("nested", nested); *\/ */
                /* /\* LOG_S7_DEBUG("stanzas", stanzas); *\/ */

                if (s7_name_to_value(s7, "*sexp:expand-includes*")
                    == s7_t(s7)) {

                    s7_pointer env = s7_inlet(s7,
                                              s7_list(s7, 1,
                                                      s7_cons(s7,
                                      s7_make_symbol(s7, "datafile"),
                        s7_make_string(s7, utstring_body(sexppath)))));

                    log_debug("expanding: %s", utstring_body(sexppath));
                    //FIXME: use
                    // (with-let (inlet ...) (with-input-from-file ...))
                    s7_pointer expanded
                        = s7_eval_c_string_with_environment(s7,
                           "(with-input-from-file datafile sexp:read)",
                                                            env);
                    log_debug("expansion completed");
                /* s7_pointer expanded = s7_call_with_catch(s7, */
                /*                     s7_t(s7),      /\* tag *\/ */
                /*                     // _sexp_read_thunk_s7 */
                /*                     s7_name_to_value(s7, "-sexp-read-thunk"), */
                /*                     _sexp_read_catcher_s7 */
                /*                     /\* s7_name_to_value(s7, "-sexp-read-thunk-catcher") *\/ */
                /*                     ); */

                    /* _sexps = s7_append(s7, expanded, _sexps); */
                    _sexps = s7_append(s7, s7_reverse(s7, expanded), _sexps);
                    /* const char *x = s7_object_to_c_string(s7, _sexps); */
                    /* log_debug("_sexps w/inc: %s", x); */
                    /* free((void*)x); */

                    /* s7_pointer cmt = s7_cons(s7, */
                    /*                          s7_make_symbol(s7, "sexp:cmt"), */
                    /*                          s7_list(s7, 1, */
                    /*                                  stanza)); */
                    /* g_stanzas = s7_cons(s7, cmt, g_stanzas); */
                } else {
                    _sexps = s7_cons(s7, stanza, _sexps);
                }
            } else {
                _sexps = s7_cons(s7, stanza, _sexps);
                TRACE_S7_DUMP("_sexps", _sexps);

                /* g_stanzas = s7_cons(s7, stanza, g_stanzas); */
                /* TRACE_S7_DUMP("g_stanzas", g_stanzas); */
                /* if (s7_is_null(s7,stanzas)) { */
                /*     stanzas = s7_cons(s7, stanza, stanzas); */
                /* } else{ */
                /*     stanzas = s7_append(s7,stanzas, s7_list(s7, 1, stanza)); */
                /* } */
            }
        } else {
            /* stanza not a pair - automatically means corrupt sexpfile? */
            log_error("corrupt sexp file? %s\n",
                      // utstring_body(sexpfile_name)
                      "FIXME"
                      );
            s7_error(s7, s7_make_symbol(s7, "corrupt-sexp-file"),
                     s7_nil(s7));
        }
    }
    /* s7_gc_unprotect_at(s7, gc_sexp_inport); */
    /* fprintf(stderr, "s7_gc_unprotect_at gc_sexp_inport: %ld\n", (long)gc_sexp_inport); */
    /* s7_close_input_port(s7, inport); */
    // g_sexp_inport must be closed by caller (e.g. with-input-from-file)

#if defined(DEVBUILD)
    log_debug("finished reading sexpfile");
#endif

    /* return _sexps; */
    return s7_reverse(s7, _sexps);
    /* return g_stanzas; */
    /* s7_close_input_port(s7, sexpfile_port); */
    /* s7_gc_unprotect_at(s7, gc_loc); */
}

s7_pointer _sexp_read_thunk_s7; /* initialized by init fn */

/* impl of _sexp_read_thunk_s7 */
/* call by s7_call_with_catch as body arg*/
/* s7_pointer x_sexp_read_thunk(s7_scheme *s7, s7_pointer args) { */
/*     (void)args; */
/*     TRACE_ENTRY(_sexp_read_thunk); */
/* /\* #if defined(DEVBUILD) *\/ */
/* /\*     print_c_backtrace(); *\/ */
/* /\* #endif *\/ */
/*     /\* log_debug("reading sexpfile: %s", g_sexpfile); *\/ */

/*     return _sexp_read_port(s7, g_sexp_inport); */
/* } */

void _log_read_error(s7_scheme *s7)
{
    const char *s;
    s7_pointer ow_let = s7_call(s7, s7_name_to_value(s7, "owlet"), s7_nil(s7));
    s7_pointer edatum = s7_call(s7, ow_let,
                               s7_cons(s7,
                                       s7_make_symbol(s7, "error-type"),
                                       s7_nil(s7)));
    s = s7_symbol_name(edatum);
    log_warn("error-type: %s", s);

    edatum = s7_call(s7, ow_let,
                     s7_cons(s7,
                             s7_make_symbol(s7, "error-data"),
                             s7_nil(s7)));
    s = s7_object_to_c_string(s7, edatum);
    log_warn("error-data: %s", s);
    free((void*)s);

    edatum = s7_call(s7, ow_let,
                     s7_cons(s7,
                             s7_make_symbol(s7, "error-code"),
                             s7_nil(s7)));
    s = s7_object_to_c_string(s7, edatum);
    log_warn("error-code: %s", s);
    free((void*)s);

    edatum = s7_call(s7, ow_let,
                     s7_cons(s7,
                             s7_make_symbol(s7, "error-file"),
                             s7_nil(s7)));
    s = s7_object_to_c_string(s7, edatum);
    log_warn("error-file: %s", s);
    free((void*)s);
}

// s7_pointer _sexp_read_catcher_s7; /* initialized by init fn */

/* call by s7_call_with_catch as error_handler arg
   arg0: err symbol, e.g.'read-error
   arg1: msg, e.g. ("unexpected close paren: ...
   WARNING: we do not correct the error here, just return a sym so
   that caller can decide on corrective action.
   WARNING: printing s7_stacktrace clobbers globals, esp. g_sexpfile!!!
 */
static s7_pointer _sexp_read_catcher(s7_scheme *s7, s7_pointer args)
{
    (void)s7;
    (void)args;
    TRACE_ENTRY(_sexp_read_catcher);
    TRACE_S7_DUMP("args", args);

#if defined(DEVBUILD)
    s7_pointer owlet7 = s7_eval_c_string(s7,  "(owlet)");
    const char *owlet = s7_object_to_c_string(s7, owlet7);
    log_debug("owlet: %s", owlet);
    free((void*)owlet);
#endif

    //FIXME: what if error is in a string instead of a file?
    s7_pointer errfile7 = s7_eval_c_string(s7,  "((owlet) 'error-file)");
    const char *errfile = s7_string(errfile7);
    /* log_debug("errfile: %s", errfile); */
    /* free((void*)errfile); */

    s7_pointer errtype7 = s7_eval_c_string(s7,  "((owlet) 'error-type)");
    const char *errtype = s7_object_to_c_string(s7, errtype7);
    /* log_debug("errtype: %s", errtype); */
    if (errtype7 == s7_make_symbol(s7, "io-error")) {
        log_error("io-error: %s", errtype);
        s7_pointer errdata7 = s7_eval_c_string(s7,  "((owlet) 'error-data)");
        log_error("cwd: %s", getcwd(NULL, 0));
        s7_error(s7, s7_make_symbol(s7, "io-error"), errdata7);
                 /* s7_cons(s7, errdata7, s7_nil(s7))); */
    }
    free((void*)errtype);

#if defined(DEVBUILD)
    // WARNING: (curlet) may be empty
    s7_pointer _curlet = s7_curlet(s7);
    TRACE_S7_DUMP("catch curlet", _curlet);
    /* TRACE_S7_DUMP("outlet", s7_outlet(s7, s7_curlet(s7))); */

    // debugging msgs only
    s7_pointer _inport = NULL;
    s7_pointer _sexps  = NULL;
    if (_curlet == s7_nil(s7)) {
        log_error("catch curlet is empty");
        /* s7_error(s7, s7_make_symbol(s7, "empty curlet"), s7_nil(s7)); */
    } else {
        _inport = s7_let_ref(s7, s7_curlet(s7),
                             s7_make_symbol(s7, "-sexp-inport"));
        TRACE_S7_DUMP("catch curlet inport", _inport);

        _sexps = s7_let_ref(s7, s7_curlet(s7),
                                       s7_make_symbol(s7, "-sexp-sexps"));
        TRACE_S7_DUMP("catch sexps", _sexps);
    }
#endif

    if (verbose) {
        log_warn("Error reading sexpfile: %s", errfile);
        e7 = s7_eval_c_string(s7,  "((owlet) 'error-data)");
        e = s7_object_to_c_string(s7, e7);
        log_warn("error-data: %s", e);
        free((void*)e);
        e7 = s7_eval_c_string(s7,  "((owlet) 'error-position)");
        e = s7_object_to_c_string(s7, e7);
        log_warn("error-position: %s", e);
        free((void*)e);
        e7 = s7_eval_c_string(s7,  "((owlet) 'error-line)");
        e = s7_object_to_c_string(s7, e7);
        log_warn("error-line: %s", e);
        free((void*)e);
    }
/* #if defined(DEVBUILD) */
/*     print_c_backtrace(); */
/* #endif */

    /* if (verbose) { */
    /*     log_info("fixing sexpfile: %s", errfile); */
    /* } */

    /* s7_pointer err_sym = s7_car(args); */

    // we need the error msg, we can get it from either args
    // or the owlet (key 'error-data).
    /* s7_pointer err_msg = s7_cadr(args); */

/* #if defined(DEVBUILD) */
/*     TRACE_S7_DUMP("s7_read_catcher err sym", err_sym); */
/*     TRACE_S7_DUMP("s7_read_catcher err msg", err_msg); */
/* #endif */

    /* _log_read_error(s7); */

    s7_pointer fixed = fix_sexpfile(s7, errfile);
    return fixed;

    /* const char *s = s7_object_to_c_string(s7, err_msg); */
    /* log_warn("error-data: %s", s); */
    /* if (strstr(s, "(\"unexpected close paren:") != NULL) { */
    /*     free((void*)s); */
    /*     return s7_make_symbol(s7, "sexp-baddot-error"); */
    /* } */
    /* else if (strstr(s, */
    /*                 "(\"end of input encountered while in a string") != NULL) { */
    /*     free((void*)s); */
    /*     return s7_make_symbol(s7, "sexp-eol-string-error"); */
    /* } */

    /* return s7_make_symbol(s7, "sexp-read-error"); */
}

/* ****************************************************************
   internal sexp:read impl
   uses s7_call_with_catch
 */
static s7_pointer _g_sexp_read(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(_g_sexp_read);
    s7_pointer src;
    TRACE_S7_DUMP("args", args);

    TRACE_S7_DUMP("curlet", s7_curlet(s7));

/* #if defined(DEVBUILD) */
/*     print_c_backtrace(); */
/* #endif */

    /* s7_gc_on(s7, false); */

    /* src = s7_car(args); */
    /* inc = s7_cadr(args); */
    /* g_expand_includes = s7_boolean(s7, inc); */
    /* g_expand_includes = true; */
    if (args == s7_nil(s7)) {
        TRACE_LOG_DEBUG("SOURCE: current-input-port", "");
        /* result = _sexp_read_port(s7, s7_current_input_port(s7)); */

        /* g_sexp_inport = s7_current_input_port(s7); */
        /* gc_sexp_inport = s7_gc_protect(s7, g_sexp_inport); */

        s7_pointer _curlet = s7_curlet(s7);
        /* TRACE_S7_DUMP("g_sexp_read curlet", _curlet); */

        s7_pointer _sexp_inport_sym = s7_make_symbol(s7, "-sexp-inport");

        s7_pointer _inport = s7_current_input_port(s7);
        if (s7_let_ref(s7, _curlet, _sexp_inport_sym)) {
            log_debug("-sexp-inport in curlet");
            s7_let_set(s7, _curlet, _sexp_inport_sym, _inport);
        } else {
            log_debug("adding -sexp-inport to curlet");
            s7_varlet(s7, _curlet, _sexp_inport_sym, _inport);
        }
        // get sexpfile before calling read thunk, since
        // read errors may close the port
        const char *sexpfile = s7_port_filename(s7, _inport);
        s7_varlet(s7, s7_curlet(s7),
                  s7_make_symbol(s7, "-sexp-infile"),
                  s7_make_string(s7, sexpfile));

        s7_varlet(s7, s7_curlet(s7),
                  s7_make_symbol(s7, "-sexp-sexps"),
                  s7_nil(s7));

/* #if defined(DEVBUILD) */
/*         const char *sexpfile = s7_port_filename(s7, g_sexp_inport); */
/*         log_debug("reading sexpfile: %s", sexpfile); */
/* #endif */

        result = s7_call_with_catch(s7,
                                    s7_t(s7),      /* tag */
                                    s7_name_to_value(s7, "-sexp-read-thunk"),
                                    _sexp_read_catcher_s7
                                    /* s7_name_to_value(s7, "-sexp-read-thunk-catcher") */
                                    );

        if (s7_is_symbol(result)) {
            log_info("read error; correcting...");
            if (result == s7_make_symbol(s7, "sexp-baddot-error")) {
                log_debug("fixing baddot error for %s", sexpfile);
                TRACE_S7_DUMP("_inport", _inport);
                /* s7_gc_unprotect_at(s7, gc_stanzas); */
                /* s7_gc_unprotect_at(s7, gc_sexp_inport); */
                s7_gc_unprotect_at(s7, gc_sexp_read_catcher_s7);

#if defined(DEVBUILD)
                s7_pointer _curlet = s7_curlet(s7);
                TRACE_S7_DUMP("fixing, curlet", _curlet);
                s7_pointer _outlet = s7_outlet(s7, _curlet);
                TRACE_S7_DUMP("fixing, outlet", _outlet);
#endif
                s7_pointer fixed = fix_sexpfile(s7, sexpfile);
                return fixed;
            }
            else if (result == s7_make_symbol(s7, "sexp-eol-string-error")) {
                /* const char *sexpfile = s7_port_filename(s7, g_sexp_inport); */
                /* log_debug("fixing eol-string error for %s", sexpfile); */
                /* TRACE_S7_DUMP("stanzas readed so far", g_stanzas); */
                /* s7_gc_unprotect_at(s7, gc_stanzas); */
                s7_gc_unprotect_at(s7, gc_sexp_read_catcher_s7);
                /* s7_gc_unprotect_at(s7, gc_sexp_inport); */
                /* s7_close_input_port(s7, g_sexp_inport); */

                /* s7_close_input_port(s7, _inport); */

                s7_pointer fixed = fix_sexpfile(s7, sexpfile);
                return fixed;
            }
        }
            /* s7_gc_unprotect_at(s7, gc_sexp_inport); */
            /* s7_gc_unprotect_at(s7, gc_sexp_read_catcher_s7); */
        TRACE_S7_DUMP("_g_sexp_read call/catch result", result);
        /* s7_flush_output_port(s7, s7_current_output_port(s7)); */
        return s7_reverse(s7, result);

    } else {

        src = s7_car(args);
        if (s7_is_input_port(s7, src)) {
            TRACE_LOG_DEBUG("SOURCE: input port", "");
            /* return _sexp_read_port(s7, src); */
            /* g_sexp_inport = src; */

        s7_pointer _curlet = s7_curlet(s7);
        /* TRACE_S7_DUMP("g_sexp_read curlet", _curlet); */

        s7_pointer _sexp_inport_sym = s7_make_symbol(s7, "-sexp-inport");

        /* s7_pointer _inport = s7_current_input_port(s7); */
            s7_pointer _inport = src;
            /* s7_varlet(s7, s7_curlet(s7), */
            /*           s7_make_symbol(s7, "-sexp-inport"), */
            /*           _inport); */
            /* s7_pointer x7 =  s7_let_ref(s7, _curlet, _sexp_inport_sym); */
            /* TRACE_S7_DUMP("x7", x7); */
            if (s7_let_ref(s7, _curlet, _sexp_inport_sym)
                == s7_undefined(s7)) {
                log_debug("adding -sexp-inport to curlet");
                s7_varlet(s7, _curlet, _sexp_inport_sym, _inport);
            } else {
                log_debug("-sexp-inport in curlet");
                s7_let_set(s7, _curlet, _sexp_inport_sym, _inport);
            }

            const char *sexpfile = s7_port_filename(s7, _inport);
            if (sexpfile) {
                s7_varlet(s7, s7_curlet(s7),
                          s7_make_symbol(s7, "-sexp-infile"),
                          s7_make_string(s7, sexpfile));
            } else {
            }
            s7_varlet(s7, s7_curlet(s7),
                      s7_make_symbol(s7, "-sexp-sexps"),
                      s7_nil(s7));

            result = s7_call_with_catch(s7,
                                        s7_t(s7),      /* tag */
                                        // _sexp_read_thunk_s7
                                        s7_name_to_value(s7, "-sexp-read-thunk"),
                                        _sexp_read_catcher_s7
                                        /* s7_name_to_value(s7, "-sexp-read-thunk-catcher") */
                                        );

            /* sexpfile = s7_port_filename(s7, _inport); */
            if (result == s7_make_symbol(s7, "sexp-baddot-error")) {
                log_debug("fixing baddot error for %s", sexpfile);
                TRACE_S7_DUMP("_inport", _inport);
                /* s7_gc_unprotect_at(s7, gc_stanzas); */
                /* s7_gc_unprotect_at(s7, gc_sexp_inport); */
                s7_gc_unprotect_at(s7, gc_sexp_read_catcher_s7);

#if defined(DEVBUILD)
                s7_pointer _curlet = s7_curlet(s7);
                TRACE_S7_DUMP("fixing, curlet", _curlet);
                s7_pointer _outlet = s7_outlet(s7, _curlet);
                TRACE_S7_DUMP("fixing, outlet", _outlet);
#endif
                s7_pointer fixed = fix_sexpfile(s7, sexpfile);
                return fixed;
            }
            else if (result == s7_make_symbol(s7, "sexp-eol-string-error")) {
                /* const char *sexpfile = s7_port_filename(s7, g_sexp_inport); */
                /* log_debug("fixing eol-string error for %s", sexpfile); */
                /* TRACE_S7_DUMP("stanzas readed so far", g_stanzas); */
                /* s7_gc_unprotect_at(s7, gc_stanzas); */
                s7_gc_unprotect_at(s7, gc_sexp_read_catcher_s7);
                /* s7_gc_unprotect_at(s7, gc_sexp_inport); */
                /* s7_close_input_port(s7, g_sexp_inport); */

                /* s7_close_input_port(s7, _inport); */

                s7_pointer fixed = fix_sexpfile(s7, sexpfile);
                return fixed;
            }

            /* s7_gc_unprotect_at(s7, gc_sexp_inport); */
            /* s7_gc_unprotect_at(s7, gc_sexp_read_catcher_s7); */
            TRACE_S7_DUMP("_g_sexp_read call/catch result", result);
            /* s7_flush_output_port(s7, s7_current_output_port(s7)); */
            return s7_reverse(s7, result);

            /* TRACE_LOG_DEBUG("read_thunk result", result); */
            /* return result; */
        }
        else if (s7_is_string(src)) {
            TRACE_LOG_DEBUG("SOURCE: string", "");
            /* sexp_str = (char*)s7_string(src); */
        }
        else {
            return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "sexp:read", 10), 1, src, string_string));
        }
    }
    return s7_f(s7);
}

/* **************************************************************** */
static s7_pointer _sexp_read_current_input_port(s7_scheme*s7)
{
    TRACE_ENTRY(_sexp_read_current_input_port);

    s7_pointer _inport = s7_current_input_port(s7);

    /* s7_pointer port_filename = s7_call(s7, */
    /*                                    s7_name_to_value(s7, "port-filename"), */
    /*                                    s7_list(s7, 1, _inport)); */
    /* TRACE_S7_DUMP("cip fname", port_filename); */
    //FIXME: need we add fname to readlet?

    s7_pointer readlet
        = s7_inlet(s7,
                   s7_list(s7, 2,
                           s7_cons(s7, _inport_sym, _inport),
                           s7_cons(s7, _sexps_sym,  s7_nil(s7))));

    const char *sexp = "(catch #t -sexp-read-thunk -sexp-read-catcher)";
    // same as (with-let (inlet ...) (catch...)) ???
    /* log_debug("evaluating c string %s", sexp); */
    s7_pointer stanzas
        = s7_eval_c_string_with_environment(s7, sexp, readlet);

    TRACE_S7_DUMP("cip readed stanazas", stanzas);

    return stanzas;
    /* return s7_make_string(s7,  "testing _sexp_read_current_input_port"); */
}

static s7_pointer _sexp_read_input_port(s7_scheme*s7, s7_pointer inport)
{
    TRACE_ENTRY(_sexp_read_input_port);
    (void)inport;

    /* s7_pointer port_filename = s7_call(s7, */
    /*                                    s7_name_to_value(s7, "port-filename"), */
    /*                                    s7_list(s7, 1, inport)); */
    /* TRACE_S7_DUMP("port fname", port_filename); */

    s7_pointer readlet
        = s7_inlet(s7,
                   s7_list(s7, 2,
                           s7_cons(s7, _inport_sym, inport),
                           s7_cons(s7, _sexps_sym,  s7_nil(s7))));

    const char *sexp = "(catch #t -sexp-read-thunk -sexp-read-catcher)";
    // same as (with-let (inlet ...) (catch...)) ???
    /* log_debug("evaluating c string %s", sexp); */
    s7_pointer stanzas
        = s7_eval_c_string_with_environment(s7, sexp, readlet);
    /* log_debug("XXXXXXXXXXXXXXXX"); */
    /* TRACE_S7_DUMP("ip readed stanazas", stanzas); */

    return stanzas;
}

/* ********************************************************* */
static s7_pointer _sexp_read_string(s7_scheme*s7, s7_pointer s)
{
    TRACE_ENTRY(_sexp_read_string);

    s7_pointer _inport = s7_open_input_string(s7, s7_string(s));
    if (!s7_is_input_port(s7, _inport)) { // g_sexp_inport)) {
        log_error("BAD INPUT PORT - _sexp_read_string %s", s);
        s7_error(s7, s7_make_symbol(s7, "_sexp_read_string bad input port"),
                     s7_nil(s7));
    }

    s7_pointer readlet
        = s7_inlet(s7,
                   s7_list(s7, 2,
                           s7_cons(s7, _inport_sym, _inport),
                           s7_cons(s7, _sexps_sym,  s7_nil(s7))));

    const char *sexp = "(catch #t -sexp-read-thunk -sexp-read-catcher)";
    // same as (with-let (inlet ...) (catch...)) ???
    log_debug("evaluating c string %s", sexp);
    s7_pointer stanzas
        = s7_eval_c_string_with_environment(s7, sexp, readlet);

    TRACE_S7_DUMP("readed stanazas", stanzas);

    return stanzas;
}

/*
  (sexp:read) - read current-input-port
  (sexp:read str) - read string str
  (sexp:read p) - read port p
  calls internal _g_sexp_read with local env
  global *sexp:expand_includes*
 */
static s7_pointer g_sexp_read(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_sexp_read);
    /* s7_pointer p, arg; */
    TRACE_S7_DUMP("args", args);
#if defined(DEVBUILD)
    s7_pointer _curlet = s7_curlet(s7);
    char *tmp = s7_object_to_c_string(s7, _curlet);
    log_debug("CURLET: %s", tmp);
    free(tmp);
#endif

    s7_pointer result;

    /* #if defined(DEVBUILD) */
    /*     print_c_backtrace(); */
    /* #endif */

    if (args == s7_nil(s7)) {
        TRACE_LOG_DEBUG("SOURCE: current-input-port", "");
        result = _sexp_read_current_input_port(s7);
        return result;
    } else {
        if (s7_list_length(s7, args) != 1) {
            s7_wrong_number_of_args_error(s7, "sexp:read takes zero or 1 arg: ~S", args);
        }
        s7_pointer src = s7_car(args);
        if (s7_is_input_port(s7, src)) {
            TRACE_LOG_DEBUG("SOURCE: input port", "");
            result = _sexp_read_input_port(s7, src);
            TRACE_S7_DUMP("_sexp_read_input_port res", result);
            return result;
        }
        else if (s7_is_string(src)) {
            TRACE_LOG_DEBUG("SOURCE: string", "");
            result = _sexp_read_string(s7, src);
            return result;
        }
        else {
            return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "sexp:read", 10), 1, src, string_string));
        }
    }


    // internal impl - must be exported to scheme (s7_define_function)
    // so it can be called with eval_c_string so that call_with_catch
    // will work.
    s7_define_function(s7, "-g-sexp-read",
                       _g_sexp_read,
                       0,
                       1, // string or port
                       false,
                       "internal sexp:read");

    //TODO: to support recursion we need to use a local env rather
    //than global, to hold inport, stanzas, etc. so we alway use
    //eval_c_string_with_environment to call sexp:read, and inlet
    // to create new local env.
    // but then what is the env for the catcher?

    /* s7_int gc_sexp_read_s7 = s7_gc_protect(s7, _g_sexp_read_s7); */
    /* s7_pointer result = _g_sexp_read(s7, args); */
    /* s7_pointer result = s7_call(s7, _g_sexp_read_s7, args); */
    s7_pointer env = s7_inlet(s7,
                              s7_list(s7, 1,
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "xargs"),
                                              args)));
    // WARNING: if the call to -g-sexp-read raises an error that gets
    // handled by the catcher, then the continuation is whatever
    // called this routine (i.e. the c stack), NOT the assignment to
    // result below.
    result = s7_eval_c_string_with_environment(s7, "(apply -g-sexp-read xargs)", env);
    /* s7_gc_unprotect_at(s7, gc_sexp_read_s7); */
    TRACE_S7_DUMP("read result:", result);
    /* s7_gc_unprotect_at(s7, gc_stanzas); */
    return result;
}

s7_pointer pl_tx, pl_xx, pl_xxs,pl_sx, pl_sxi, pl_ix, pl_iis, pl_isix, pl_bxs;

s7_pointer libsexp_s7_init(s7_scheme *s7);
s7_pointer libsexp_s7_init(s7_scheme *s7)
{
    TRACE_ENTRY(libsexp_s7_init);
  s7_pointer cur_env;
  /* s7_pointer pl_tx, pl_xxs,pl_sx, pl_sxi, pl_ix, pl_iis, pl_isix, pl_bxs; */
  //  pl_xxsi, pl_ixs
  {
      s7_pointer t, x, b, s, i;

      t = s7_t(s7);
      x = s7_make_symbol(s7, "c-pointer?");
      b = s7_make_symbol(s7, "boolean?");
      s = s7_make_symbol(s7, "string?");
      i = s7_make_symbol(s7, "integer?");

      pl_tx = s7_make_signature(s7, 2, t, x);
      pl_xx = s7_make_signature(s7, 2, x, x);
      pl_xxs = s7_make_signature(s7, 3, x, x, s);
      /* pl_xxsi = s7_make_signature(s7, 4, x, x, s, i); */
      pl_sx = s7_make_signature(s7, 2, s, x);
      pl_sxi = s7_make_signature(s7, 3, s, x, i);
      pl_ix = s7_make_signature(s7, 2, i, x);
      pl_iis = s7_make_signature(s7, 3, i, i, s);
      pl_bxs = s7_make_signature(s7, 3, b, x, s);
      /* pl_ixs = s7_make_signature(s7, 3, i, x, s); */
      pl_isix = s7_make_signature(s7, 4, i, s, i, x);
  }

  string_string = s7_make_semipermanent_string(s7, "a string");
  c_pointer_string = s7_make_semipermanent_string(s7, "a c-pointer");
  character_string = s7_make_semipermanent_string(s7, "a character");
  boolean_string = s7_make_semipermanent_string(s7, "a boolean");
  real_string = s7_make_semipermanent_string(s7, "a real");
  complex_string = s7_make_semipermanent_string(s7, "a complex number");
  integer_string = s7_make_semipermanent_string(s7, "an integer");
  cur_env = s7_inlet(s7, s7_nil(s7));
  s7_pointer old_shadow = s7_set_shadow_rootlet(s7, cur_env);

  /* sexp_table_init(s7, cur_env); */
  /* sexp_array_init(s7, cur_env); */
  /* sexp_datetime_init(s7, cur_env); */

  int64_t__symbol = s7_make_symbol(s7, "int64_t*");
  /* sexp_datum_t__symbol = s7_make_symbol(s7, "sexp_datum_t*"); */
  /* sexp_array_t__symbol = s7_make_symbol(s7, "sexp_array_t*"); */
  /* sexp_table_t__symbol = s7_make_symbol(s7, "sexp_table_t*"); */
  FILE__symbol = s7_make_symbol(s7, "FILE*");

  _inport_sym = s7_make_symbol(s7, "-inport");
  _sexps_sym = s7_make_symbol(s7, "-sexps");

  /* s7_define_constant(s7, "sexp:version", s7_make_string(s7, "1.0-beta")); */

  /* s7_define(s7, cur_env, */
  /*           s7_make_symbol(s7, "sexp:free"), */
  /*           s7_make_typed_function(s7, "sexp:free", */
  /*                                  g_sexp_free, */
  /*                                  1, 0, false, */
  /*                                  "(sexp:free t) free table t", pl_tx)); */

  /* public api */
  s7_define_variable(s7, "*sexp:expand-includes*", s7_t(s7));

  s7_define(s7, cur_env,
            s7_make_symbol(s7, "sexp:read"),
            s7_make_typed_function(s7, "sexp:read",
                                   g_sexp_read,
                                   0, // 0 args: read from current inport
                                   // (for with-input-from-string or -file)
                                   1, // optional: string or port
                                   false,
                                   "(sexp:read) read sexpfile from current-input-port; (sexp:read src) read sexpfile from string or port",
                                   NULL)); //sig

  /* private */
  /* _sexp_read_thunk_s7 = s7_make_function(s7, "-sexp-read-thunk", */
  /*                                        _sexp_read_thunk, */
  /*                                        0, 0, false, ""); */
  /* _sexp_read_thunk_s7 = */
  s7_define_function(s7, "-sexp-read-thunk",
                     _sexp_read_thunk,
                     0, 0, false, "");

  /* _sexp_read_catcher_s7 = */
  s7_define_function(s7, "-sexp-read-catcher",
                   _sexp_read_catcher,
                   2, // catcher must take 2
                   0, false,
                   "handle read error");

    /* gc_sexp_read_catcher_s7 = s7_gc_protect(s7, _sexp_read_catcher_s7); */

  /* _sexp_read_catcher_s7 = s7_define_function(s7, "-sexp-read-thunk-catcher", */
  /*                                                _sexp_read_catcher, */
  /*                                                2, // catcher must take 2 */
  /*                                                0, false, ""); */

  s7_set_shadow_rootlet(s7, old_shadow);

  return(cur_env);
}
