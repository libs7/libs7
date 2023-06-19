#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <libgen.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

#include "config.h"

#include "log.h"
/* #include "s7.h" */

/* #include "trace.h" */
#include "utarray.h"
#include "utstring.h"
/* #endif */

/* #if defined(TRACING) */
/* #include "debug.h" */
/* #endif */


#include "s7.h"

s7_pointer g_mibl_read_thunk(s7_scheme *s7, s7_pointer args);

/* #include "dune_readers.h" */

#if LOCAL_INTERFACE
#include <string.h>
#endif

s7_pointer g_dunefile_port;

s7_int dune_gc_loc = -1;

extern const UT_icd ut_str_icd;

extern s7_scheme *s7;
extern UT_string *opam_switch_bin;
extern bool verbose;

#if defined(DEVBUILD)
extern bool mibl_debug;
extern bool mibl_debug_traversal;
extern bool debug;
extern char *tostr1;
extern char *tostr2;
#endif

#if defined(TRACING)
extern bool trace;
#endif

#define ERRSEXP "(with-let (owlet) " \
    "(format #t \"file: ~A, line ~A\n\" error-file error-line))"

/* TODO: validate dunefile_port */
/* s7 defined in s7_config.c */
s7_pointer g_dune_read_thunk(s7_scheme *s7, s7_pointer args) {
    (void)args;
    /* printf("g_dune_read_thunk: %s\n", */
    /*        s7_port_filename(s7, g_dunefile_port)); */
    return s7_read(s7, g_dunefile_port);
}

s7_pointer dune_read_thunk;
s7_pointer mibl_read_thunk;

void init_dune_readers(void)
{
    /* log_debug("init_dune_readers"); */
    // calls s7_read; not registered in env
    dune_read_thunk = s7_make_function(s7, "s7-read-thunk",
                                     g_dune_read_thunk,
                                     0, 0, false, "");

    // uses call-with-catch to execute dune_read_thunk
    // called by read_dunefile
    mibl_read_thunk = s7_make_function(s7, "mibl-read-thunk",
                                       g_mibl_read_thunk,
                                       0, 0, false, "");
}

s7_pointer g_dune_read_thunk_catcher(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_dune_read_thunk_catcher);
#if defined(DEVBUILD)
    LOG_S7_DEBUG("s7_read_thunk_catcher args", args);
#endif
    /* log_info("s7_read_thunk_catcher arg0: %s", TO_STR(s7_car(args))); */
    /* log_info("s7_read_thunk_catcher arg1: %s", TO_STR(s7_cadr(args))); */
    /* s7_show_stack(s7); */

    const char *dunefile = s7_port_filename(s7, g_dunefile_port);
    log_error("reading dunefile: %s", dunefile);

    /* if arg0 == 'read-error and */

    char *s;
#define TO_STR(x) s7_object_to_c_string(s7, x)
    s = TO_STR(s7_cadr(args));
    if (strstr(s, "(\"unexpected close paren:") != NULL) {

        /* printf("XXXXXXXXXXXXXXXX\n"); */
        /* if (strstr(errmsg, "BADDOT") != NULL) { */
#ifdef TRACING
        log_info(RED "fixing baddot in %s" CRESET, dunefile);
                 /* utstring_body(dunefile_name)); */
#endif
        s7_close_input_port(s7, g_dunefile_port);

        //FIXME FIXME
        /* s7_gc_unprotect_at(s7, dune_gc_loc); */

        /* s7_show_stack(s7); */
        /* clear out old error */
        /* s7_flush_output_port(s7, s7_current_error_port(s7)); */
        /* close_error_config(); */
        /* error_config(); */
        /* init_error_handlers(); */

        // FIXME: test case: 'include' after baddot
        s7_pointer fixed = fix_baddot(dunefile); //_name);
        /* s7_pointer fixed = s7_eval_c_string(s7, "'(foob)"); */
#if defined(DEVBUILD)
        LOG_S7_DEBUG("FIXED", fixed);
        /* if (mibl_debug) log_debug(RED "FIXED:" CRESET " %s", */
        /*                      TO_STR(fixed)); */
        /* s7_show_stack(s7); */
        /* print_backtrace(s7); */
#endif
        /* close_error_config(); */
        /* error_config(); */
        // FIXING baddot always re-reads entire dunefile
        /* stanzas = fixed; */
        /* if (s7_is_null(s7,stanzas)) { */
        /*     // fixed is a list of stanzas */
        /*     stanzas = fixed; */
        /* } else{ */
        /*     stanzas = s7_append(s7, stanzas, fixed); */
        /* } */
        /* } */

        return fixed;
    } else {
        /* char *args = TO_STR(s7_cadr(args)); */
        fprintf(stdout, RED "Read Error:" CRESET " %s\n", s);
        /* free(args); */

        s7_pointer st = s7_eval_c_string(s7, "(debug-print-stacktrace)");
        (void)st;
        /* fprintf(stdout, "STACKTRACE:\n%s\n", "TO_STR(st)"); */
        s7_flush_output_port(s7, s7_current_output_port(s7));
        fflush(NULL);

        fprintf(stdout, RED "[begin error context]\n");
        s7_eval_c_string(s7, ERRSEXP);
        char *sexp = "(do ((e (outlet (owlet)) (outlet e))) "
            "((eq? e (rootlet))) "
            "(format () \"~{~A ~}~%\" e)) ";
        s7_eval_c_string(s7, sexp);
        s7_write(s7,
                 /* s7_make_string(s7, s7_car(args)), */
                 // s7_string(s7_car(args)),
                 /* TO_STR(s7_car(args)), */
                 s7_car(args),
                 s7_current_error_port(s7));
        fprintf(stdout, "[end error context]" CRESET "\n");

        close_error_config();
        error_config();
        /* init_error_handlers(); */
        /* s7_quit(s7); */
        /* exit(EXIT_FAILURE); */
        return NULL;
    }
    free(s);
}

s7_pointer s7_read_thunk_catcher;

void init_error_handlers_dune(void)
{
    /* log_debug("init_error_handlers_dune"); */
    s7_read_thunk_catcher = s7_define_function(s7, "s7-read-thunk-catcher", g_dune_read_thunk_catcher,
                                               2, // required args
                                               0, // optional args
                                               false, // rest_arg
                                               "read-thunk error handler"); // docstring

    /* log_debug("s7_read_thunk_catcher: %d\n", s7_is_defined(s7, "s7-read-thunk-catcher")); */

    s7_define_function(s7, "error-handler", _s7_error_handler,
                       1, 0, false, "our error handler");

    s7_eval_c_string(s7, "(set! (hook-functions *error-hook*) \n\
                            (list (lambda (hook) \n\
                                    (error-handler \n\
                                      (apply format #f (hook 'data))) \n\
                                    (set! (hook 'result) 'our-error))))");

    /* read-error-hook evidently only catches problems with # names
       and \ escapes, not general read errors. */
    s7_define_function(s7, "read-error-handler", _s7_read_error_handler,
                       1, 0, false, "our read-error handler");

    s7_eval_c_string(s7, "(set! (hook-functions *read-error-hook*) \n\
                            (list (lambda (hook) \n\
                                    (format #t \"HOOK data: ~A\n\" (hook 'data)) \n\
                                    (set! (hook 'result) (hook 'data)))))");
                                      /* (apply format #f (hook 'data))) \n \ */

}

s7_pointer g_mibl_read_thunk(s7_scheme *s7, s7_pointer args)
{
    /* fprintf(stderr, "g_mibl_read_thunk\n"); */
    /* fprintf(stderr, "s7_read_thunk_catcher: %d\n", s7_is_defined(s7, "s7-read-thunk-catcher")); */
    /* s7_pointer body = s7_eval_c_string(s7, "(lambda () (+ #f 2))"); */
    /* s7_pointer err = s7_eval_c_string(s7, "(lambda (type info) 'oops)"); */

    /* if ( ! s7_is_defined(s7, "s7-read-thunk-catcher") ) { */
    /*     log_error("s7-read-thunk-catcher not defined"); */
    /*     exit(1); */
    /* } */
    /* char *rtc = TO_STR(s7_read_thunk_catcher); */
    /* log_error("s7_read_thunk_catcher: %s", rtc); */

    s7_pointer err_catcher = s7_name_to_value(s7, "s7-read-thunk-catcher");
    /* s7_pointer t = s7_type_of(s7, x); */
    /* /\* log_debug("s7-read-thunk-catcher type-of: %s", TO_STR(t)); *\/ */
    /* s7_pointer arity = s7_arity(s7, s7_read_thunk_catcher); */
    /* /\* log_debug("s7-read-thunk-catcher arity: %s", TO_STR(arity)); *\/ */

    /* s7_flush_input_port(s7, s7_current_input_port(s7)); */
    s7_flush_output_port(s7, s7_current_error_port(s7));
    s7_flush_output_port(s7, s7_current_error_port(s7));
    s7_pointer result = s7_call_with_catch(s7,
                                           s7_t(s7),      /* tag */
                                           dune_read_thunk, /* body */
                                           //s7_read_thunk_catcher /* body */
                                           err_catcher // s7_read_thunk_catcher /* err */
                                           );
    /* log_trace("RETURNED: dune_read_thunk"); */
    return result;
}

/* LOCAL s7_pointer mibl_read_thunk; */

//s7_pointer
EXPORT UT_array *get_pkg_executables(void *_stanzas)
/* UT_string *dune_pkg_file) */
{
    TRACE_ENTRY(get_pkg_executables);

    s7_pointer stanzas = (s7_pointer) _stanzas;
    UT_string *outpath;
    UT_string *opam_bin;
    utstring_new(outpath);
    utstring_new(opam_bin);

    UT_array *bins;
    utarray_new(bins, &ut_str_icd);

    /* s7_pointer stanzas = read_dune_package(dune_pkg_file); */

    s7_pointer iter, binfile;

    s7_pointer e = s7_inlet(s7,
                            s7_list(s7, 1,
                                    s7_cons(s7,
                                            s7_make_symbol(s7, "stanzas"),
                                            stanzas)));

    char * exec_sexp =
        "(let ((files (assoc 'files (cdr stanzas))))"
        "  (if files"
        "      (let ((bin (assoc 'bin (cdr files))))"
        "          (if bin (cadr bin)))))";

    s7_pointer executables = s7_eval_c_string_with_environment(s7, exec_sexp, e);

    if (executables == s7_unspecified(s7))
        return bins;

#if defined(DEVBUILD)
    if (mibl_debug) {
        /* log_debug("Pkg: %s", utstring_body(dune_pkg_file)); */
        LOG_S7_DEBUG("executables", executables);
    }
#endif

    /* /\* result is list of executables installed in $PREFIX/bin *\/ */
    /* if (s7_is_list(s7, executables)) { */
    /*     if (verbose) { */
    /*     } */
    /* } */
    iter = s7_make_iterator(s7, executables);
        //gc_loc = s7_gc_protect(s7, iter);
    if (!s7_is_iterator(iter)) {
        log_error("s7_make_iterator failed");
#if defined(DEVBUILD)
        LOG_S7_DEBUG("not an iterator", iter);
#endif
    }
    if (s7_iterator_is_at_end(s7, iter)) {
        log_error("s7_iterator_is_at_end prematurely");
#if defined(DEVBUILD)
        LOG_S7_DEBUG("iterator prematurely done", iter);
#endif
    }
    char *f;
    while (true) {
        binfile = s7_iterate(s7, iter);
        if (s7_iterator_is_at_end(s7, iter)) break;
#if defined(DEVBUILD)
        LOG_S7_DEBUG("binfile", binfile);
#endif
        f = TO_STR(binfile);
        utarray_push_back(bins, &f);
        free(f);
    }
        /* utstring_renew(opam_bin); */
        /* utstring_printf(opam_bin, "%s/%s", */
        /*                 utstring_body(opam_switch_bin), */
        /*                 TO_STR(binfile)); */

        /* utstring_renew(outpath); */
        /* utstring_printf(outpath, "%s/%s/bin/%s", */
        /*                 obazl, pkg, TO_STR(binfile)); */
        /* rc = symlink(utstring_body(opam_bin), */
        /*              utstring_body(outpath)); */
        /* if (rc != 0) { */
        /*     if (errno != EEXIST) { */
        /*         perror(NULL); */
        /*         fprintf(stderr, "exiting\n"); */
        /*         exit(EXIT_FAILURE); */
        /*     } */
        /* } */
        /* if (!emitted_bootstrapper) */
        /*     emit_local_repo_decl(bootstrap_FILE, pkg); */

        /* fprintf(ostream, "exports_files([\"%s\"])\n", TO_STR(binfile)); */
        /* fprintf(ostream, "## src: %s\n", utstring_body(opam_bin)); */
        /* fprintf(ostream, "## dst: %s\n", utstring_body(outpath)); */
    /* } */
    return bins;
}

EXPORT UT_array *get_pkg_stublibs(char *pkg, void *_stanzas)
/* UT_string *dune_pkg_file) */
{
    TRACE_ENTRY(get_pkg_stublibs);

    s7_pointer stanzas = (s7_pointer) _stanzas;
/* #if defined(DEVBUILD) */
/*     log_debug("stanzas: %s", TO_STR(stanzas)); */
/* #endif */

    UT_string *outpath;
    UT_string *opam_bin;
    utstring_new(outpath);
    utstring_new(opam_bin);

    UT_array *stubs;
    utarray_new(stubs, &ut_str_icd);

    /* s7_pointer stanzas = read_dune_package(dune_pkg_file); */

    s7_pointer iter, stublib_file;

    s7_pointer e = s7_inlet(s7,
                            s7_list(s7, 1,
                                    s7_cons(s7,
                                            s7_make_symbol(s7, "stanzas"),
                                            stanzas)));

    char * stublibs_sexp =
        "(let ((files (assoc 'files (cdr stanzas))))"
        "  (if files"
        "      (let ((bin (assoc 'stublibs (cdr files))))"
        "          (if bin (cadr bin)))))";

    s7_pointer stublibs = s7_eval_c_string_with_environment(s7, stublibs_sexp, e);

    if (stublibs == s7_unspecified(s7))
        return stubs;

#if defined(DEVBUILD)
    if (mibl_debug) {
        /* log_debug("Pkg: %s", utstring_body(dune_pkg_file)); */
        LOG_S7_DEBUG(RED "STUBLIBS" CRESET, stublibs);
    }
#endif

    /* result is list of stublibs installed in $PREFIX/bin */
    /* if (s7_is_list(s7, stublibs)) { */
    /*     if (verbose) { */
    /*         log_info(GRN "%s stublibs:" CRESET " %s", */
    /*                  pkg, */
    /*                  /\* " for %s: %s\n", *\/ */
    /*                  /\* utstring_body(dune_pkg_file), *\/ */
    /*                  TO_STR(stublibs)); */
    /*     } */
    /* } */
    iter = s7_make_iterator(s7, stublibs);
        //gc_loc = s7_gc_protect(s7, iter);
    if (!s7_is_iterator(iter)) {
        log_error("s7_is_iterator fail");
#if defined(DEVBUILD)
        LOG_S7_DEBUG("not an iterator", iter);
#endif
    }
    if (s7_iterator_is_at_end(s7, iter)) {
        log_error("s7_iterator_is_at_end prematurely");
#if defined(DEVBUILD)
        LOG_S7_DEBUG("iterator prematurely done", iter);
#endif
    }
    char *f;
    while (true) {
        stublib_file = s7_iterate(s7, iter);
        if (s7_iterator_is_at_end(s7, iter)) break;
#if defined(DEVBUILD)
        LOG_S7_DEBUG("stublib_file", stublib_file);
#endif
        f = TO_STR(stublib_file);
        utarray_push_back(stubs, &f);
        free(f);
    }
    return stubs;
}

char *dunefile_to_string(const char *dunefile_name)
{
    TRACE_ENTRY(dunefile_to_string);
#if defined(TRACING)
    log_trace("dunefile: %s", dunefile_name);
                  //utstring_body(dunefile_name));
#endif
    /* core/dune file size: 45572 */
    // 2K

#define DUNE_BUFSZ 131072
    /* static char inbuf[DUNE_BUFSZ]; */
    /* memset(inbuf, '\0', DUNE_BUFSZ); */
    static char outbuf[DUNE_BUFSZ + 20];
    memset(outbuf, '\0', DUNE_BUFSZ);

    off_t file_size;
    char *inbuf;
    struct stat stbuf;
    int fd;
    FILE *instream = NULL;

    fd = open(dunefile_name, O_RDONLY);
    if (fd == -1) {
        /* Handle error */
        log_error("fd open error");
        goto cleanup;
    }

    if ((fstat(fd, &stbuf) != 0) || (!S_ISREG(stbuf.st_mode))) {
        /* Handle error */
        log_error("fstat error");
        goto cleanup;
    }

    file_size = stbuf.st_size;
#if defined(DEVBUILD)
    if (mibl_debug) log_debug("filesize: %d", file_size);
#endif

    inbuf = (char*)malloc(file_size);
    if (inbuf == NULL) {
        /* Handle error */
        log_error("malloc file_size fail");
        goto cleanup;
    }

    /* FIXME: what about e.g. unicode in string literals? */
    errno = 0;
    /* FILE *instream = fopen(dunefile_name, "r"); */
    instream = fdopen(fd, "r");
    if (instream == NULL) {
        /* Handle error */
        printf(RED "ERROR" CRESET "fdopen failure: %s\n",
               dunefile_name);
               /* utstring_body(dunefile_name)); */
        perror(NULL);
        close(fd);
        goto cleanup;
    } else {
#if defined(DEVBUILD)
        if (mibl_debug) log_debug("fdopened %s",
                                  dunefile_name);
                                  /* utstring_body(dunefile_name)); */
#endif
    }

    // now read the entire file
    size_t read_ct = fread(inbuf, 1, file_size, instream);
#if defined(DEVBUILD)
    if (mibl_debug) log_debug("read_ct: %d", read_ct);
#endif
    if (read_ct != file_size) {
        if (ferror(instream) != 0) {
            printf(RED "ERROR" CRESET "fread error 2 for %s\n",
                   dunefile_name);
            /* utstring_body(dunefile_name)); */
            log_error("fread error 2 for %s\n",
                      dunefile_name);
            /* utstring_body(dunefile_name)); */
            exit(EXIT_FAILURE); //FIXME: exit gracefully
        } else {
            if (feof(instream) == 0) {
                printf(RED "ERROR" CRESET "fread error 3 for %s\n",
                       dunefile_name);
                /* utstring_body(dunefile_name)); */
                log_error("fread error 3 for %s\n",
                          dunefile_name);
                /* utstring_body(dunefile_name)); */
                exit(EXIT_FAILURE); //FIXME: exit gracefully
            } else {
                log_error("WTF????????????????");
                goto cleanup;
            }
        }
    } else {
        close(fd);
        fclose(instream);
    }


/*     if (instream == NULL) { */
/*         printf(RED "ERROR" CRESET "fopen failure: %s\n", */
/*                dunefile_name); */
/*                /\* utstring_body(dunefile_name)); *\/ */
/*         perror(NULL); */
/*         exit(EXIT_FAILURE); */
/*     } else { */
/* #if defined(DEVBUILD) */
/*         if (mibl_debug) log_debug("fopened %s", */
/*                                   dunefile_name); */
/*                                   /\* utstring_body(dunefile_name)); *\/ */
/* #endif */
/*     } */
    //FIXME: do not use fseek/ftell to get file size, use fstat
    // https://wiki.sei.cmu.edu/confluence/display/c/FIO19-C.+Do+not+use+fseek%28%29+and+ftell%28%29+to+compute+the+size+of+a+regular+file
    /* fseek(instream, 0, SEEK_END); */
    /* if (fseek(instream, 0, SEEK_END) != 0) { */
    /*     goto cleanup; */
    /* } */
    /* uint64_t fileSize = ftell(instream); */
/* #if defined(DEVBUILD) */
/*     if (mibl_debug) log_debug("filesize: %d", fileSize); */
/* #endif */

    /* if (fileSize > DUNE_BUFSZ) { */
    /*     printf(RED "ERROR:" CRESET */
    /*            " dune file '%s' size (%" PRIu64 " > DUNE_BUFSZ (%d)\n", */
    /*            dunefile_name, // utstring_body(dunefile_name), */
    /*            fileSize, DUNE_BUFSZ); */
    /*     log_error("dune file size (%d) > DUNE_BUFSZ (%d)", fileSize, DUNE_BUFSZ); */
    /*     exit(EXIT_FAILURE);     /\* FIXME: exit gracefully *\/ */
    /* } */
    /* rewind(instream); */

    /* char *outbuf = malloc(fileSize + 1); */
    /* memset(outbuf, '\0', fileSize); */

/*     uint64_t outFileSizeCounter = fileSize; */

/*     /\* we fread() bytes from instream in COPY_BUFFER_MAXSIZE increments, */
/*        until there is nothing left to fread() *\/ */
/*     int read_ct = 0; */
/*     do { */
/*         /\* printf("reading...\n"); *\/ */
/*         if (outFileSizeCounter > DUNE_BUFSZ) { */
/*             /\* probably won't see a 16K dune file *\/ */
/*             read_ct = fread(inbuf, 1, (size_t) DUNE_BUFSZ, instream); */
/*             if (read_ct != DUNE_BUFSZ) { */
/*                 if (ferror(instream) != 0) { */
/*                     printf(RED "ERROR" CRESET " fread error 1 for %s\n", */
/*                            dunefile_name); */
/*                               /\* utstring_body(dunefile_name)); *\/ */
/*                     log_error("fread error 1 for %s\n", */
/*                               dunefile_name); */
/*                               /\* utstring_body(dunefile_name)); *\/ */
/*                     exit(EXIT_FAILURE); //FIXME: exit gracefully */
/*                 } else { */
/*                     // readed < DUNE_BUFSZ? */
/*                 } */
/*             } else { */
/*                 // readed DUNE_BUFSZ bytes? */
/*             } */
/*             /\* log_debug("writing"); *\/ */
/*             outFileSizeCounter -= DUNE_BUFSZ; */
/*         } */
/*         else { */
/*             read_ct = fread(inbuf, 1, (size_t) outFileSizeCounter, instream); */
/* #if defined(DEVBUILD) */
/*             if (mibl_debug) log_debug("read_ct: %d", read_ct); */
/* #endif */
/*             if (read_ct != outFileSizeCounter) { */
/*                 if (ferror(instream) != 0) { */
/*                     printf(RED "ERROR" CRESET "fread error 2 for %s\n", */
/*                            dunefile_name); */
/*                               /\* utstring_body(dunefile_name)); *\/ */
/*                     log_error("fread error 2 for %s\n", */
/*                               dunefile_name); */
/*                               /\* utstring_body(dunefile_name)); *\/ */
/*                     exit(EXIT_FAILURE); //FIXME: exit gracefully */
/*                 } else { */
/*                     if (feof(instream) == 0) { */
/*                         printf(RED "ERROR" CRESET "fread error 3 for %s\n", */
/*                                dunefile_name); */
/*                               /\* utstring_body(dunefile_name)); *\/ */
/*                         log_error("fread error 3 for %s\n", */
/*                                   dunefile_name); */
/*                                   /\* utstring_body(dunefile_name)); *\/ */
/*                         exit(EXIT_FAILURE); //FIXME: exit gracefully */
/*                     } else { */
/*                         /\* printf("bbbbbbbbbbbbbbbb\n"); *\/ */
/*                     } */
/*                 } */
/*             } */
/*             outFileSizeCounter = 0ULL; */
/*         } */
/*     } while (outFileSizeCounter > 0); */
/* #if defined(DEVBUILD) */
/*     if (mibl_debug) { */
/*         log_debug(RED "readed" CRESET " %d bytes", read_ct); */
/*         /\* log_debug(RED "readed string:" CRESET " '%s'", inbuf); *\/ */
/*     } */
/* #endif */
    /* fclose(instream); */

    // FIXME: loop over the entire inbuf
    char *inptr = (char*)inbuf;
    char *outptr = (char*)outbuf;
    char *cursor = inptr;

    while (true) {
        cursor = strstr(inptr, ".)");

/* https://stackoverflow.com/questions/54592366/replacing-one-character-in-a-string-with-multiple-characters-in-c */

        if (cursor == NULL) {
/* #if defined(DEVBUILD) */
/*             if (mibl_debug) log_debug("remainder: '%s'", inptr); */
/* #endif */
            size_t ct = strlcpy(outptr, (const char*)inptr, file_size); // strlen(outptr));
            (void)ct;           /* prevent -Wunused-variable */
/* #if defined(DEVBUILD) */
/*             if (mibl_debug) log_debug("concatenated: '%s'", outptr); */
/* #endif */
            break;
        } else {
#if defined(DEVBUILD)
            if (mibl_debug) log_error("FOUND and fixing \".)\" at pos: %d", cursor - inbuf);
#endif
            size_t ct = strlcpy(outptr, (const char*)inptr, cursor - inptr);
#if defined(DEVBUILD)
            if (mibl_debug) {
                log_debug("copied %d chars", ct);
                /* log_debug("to buf: '%s'", outptr); */
            }
#endif
            if (ct >= DUNE_BUFSZ) {
                printf("output string has been truncated!\n");
            }
            outptr = outptr + (cursor - inptr) - 1;
            outptr[cursor - inptr] = '\0';
            ct = strlcat(outptr, " ./", DUNE_BUFSZ);
            outptr += 3;

            inptr = inptr + (cursor - inptr) + 1;
            /* printf(GRN "inptr:\n" CRESET " %s\n", inptr); */

            if (ct >= DUNE_BUFSZ) {
                printf(RED "ERROR" CRESET "write count exceeded output bufsz\n");
                exit(EXIT_FAILURE);
                // output string has been truncated
            }
        }
    }
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

/* s7_pointer */
void *read_dune_package(UT_string *dunefile_name)
{
    TRACE_ENTRY(read_dune_package);
    //FIXME: this duplicates the code in read_dunefile
#if defined(TRACING)
    log_trace("read_dune_package: %s", utstring_body(dunefile_name));
#endif

    char *dunestring = dunefile_to_string(utstring_body(dunefile_name));
/* #if defined(DEVBUILD) */
/*     if (mibl_debug) log_debug("readed str: %s", dunestring); */
/* #endif */

    /* stanza accumulator */
    s7_pointer stanzas = s7_list(s7, 0);

    s7_pointer sport = s7_open_input_string(s7, dunestring);

    const char *errmsg;
    if (!s7_is_input_port(s7, sport)) {
        errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
        if ((errmsg) && (*errmsg)) {
            printf(RED "ERROR" CRESET "s7_open_input_string failed\n");
            log_error("[%s\n]", errmsg);
            s7_quit(s7);
            exit(EXIT_FAILURE);
        }
    }

#if defined(TRACING)
    /* if (mibl_debug) */
    log_debug("s7 reading stanzas");
#endif

    /* read all stanzas in dunefile */
    while(true) {
/* #if defined(TRACING) */
/*         if (mibl_debug) log_debug("iter"); */
/* #endif */
        s7_pointer stanza = s7_read(s7, sport);
        /* FIXME: error checks */
        /* errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); */
        /* if ((errmsg) && (*errmsg)) { */
        /*     if (mibl_debug) log_error("[%s\n]", errmsg); */
        /*     s7_close_input_port(s7, sport); */
        /*     s7_quit(s7); */
        /*     exit(EXIT_FAILURE); */
        /*     break; */
        /* } */
        if (stanza == s7_eof_object(s7)) break;
        if (s7_is_null(s7,stanzas)) {
            stanzas = s7_list(s7, 1, stanza);
        } else{
            stanzas = s7_append(s7,stanzas, s7_list(s7, 1, stanza));
        }
    }
    s7_close_input_port(s7, sport);
#if defined(TRACING)
    /* if (mibl_debug) */
        log_debug("finished reading");
#endif

    /* s7_gc_unprotect_at(s7, baddot_gc_loc); */
    /* close_error_config(); */

    /* leave error config as-is */
    /* free(dunestring); */
    return stanzas;
}

s7_pointer read_dunefile(char *path)
{
    TRACE_ENTRY(read_dunefile);
#if defined(DEVBUILD)
    log_debug("read_dunefile %s", path); //, fname);
#endif

    /* read dunefile */
    utstring_renew(dunefile_name);
    utstring_printf(dunefile_name, "%s", path); //, fname);
    /* log_debug("reading dunefile: %s", utstring_body(dunefile_name)); */

    s7_pointer dunefile_port = s7_open_input_file(s7,
                                                  utstring_body(dunefile_name),
                                                  "r");
    if (!s7_is_input_port(s7, dunefile_port)) {
        errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
        if ((errmsg) && (*errmsg)) {
            log_error("[%s\n]", errmsg);
            s7_shutdown(s7);
            exit(EXIT_FAILURE);
        }
    } else {
#if defined(DEVBUILD)
        if (mibl_debug)
            log_trace("opened input dunefile_port for %s",
                      utstring_body(dunefile_name));
#endif
    }

    g_dunefile_port = dunefile_port;

    /* dune_read_thunk = s7_make_function(s7, "s7-read-thunk", */
    /*                                  dune_read_thunk, */
    /*                                  0, 0, false, ""); */
    /* mibl_read_thunk = s7_make_function(s7, "mibl-read-thunk", */
    /*                                    _mibl_read_thunk, */
    /*                                    0, 0, false, ""); */

    dune_gc_loc = s7_gc_protect(s7, g_dunefile_port);

    s7_pointer stanzas = s7_list(s7, 0); // s7_nil(s7));
    /* s7_int stanzas_gc_loc = s7_gc_protect(s7, stanzas); */

    close_error_config();
    error_config();
    /* init_error_handling(); */

#if defined(DEBUggING)
    if (mibl_debug)
        log_trace("reading stanzas of %s", utstring_body(dunefile_name));
#endif

    /* repeat until all objects read */
    while(true) {
#if defined(DEVBUILD)
        if (mibl_debug_traversal) log_trace("reading stanza");
#endif

        /* s7_show_stack(s7); */
        /* print_backtrace(s7); */
        s7_pointer stanza = s7_call(s7, mibl_read_thunk, s7_nil(s7)); //list(s7, 0));
        /* log_debug("RETURNED: mibl_read_thunk"); */
        if (stanza == s7_eof_object(s7)) {
            /* log_debug("EOF"); */
            break;
        }
        /* s7_pointer stanza = s7_read(s7, dunefile_port); */

#if defined(DEVBUILD)
        if (mibl_debug_traversal) {
            LOG_S7_DEBUG("Readed stanza", stanza);
        }
#endif
        /* s7_show_stack(s7); */
        /* print_backtrace(s7); */
        /* errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); */
        /* log_error("errmsg: %s", errmsg); */

        if ((errmsg) && (*errmsg)) {
#if defined(DEVBUILD)
            if (mibl_debug_traversal)
                log_error(RED "[%s]" CRESET, errmsg);
#endif
            /* s7_close_input_port(s7, dunefile_port); */
            /* s7_gc_unprotect_at(s7, dune_gc_loc); */
            //if ".)", read file into buffer, convert to "\.)", then
            // read with the scheme reader

            if (strstr(errmsg,
                       ";read-error (\"unexpected close paren:") != NULL) {
            /* if (strstr(errmsg, "BADDOT") != NULL) { */
                log_info(RED "fixing baddot in %s" CRESET,
                         utstring_body(dunefile_name));
                s7_close_input_port(s7, dunefile_port);
                /* fprintf(stderr, "s7_gc_unprotect_at: %ld", (long)dune_gc_loc); */
                s7_gc_unprotect_at(s7, dune_gc_loc);

                s7_show_stack(s7);
                /* clear out old error */
                /* s7_flush_output_port(s7, s7_current_error_port(s7)); */
                /* close_error_config(); */
                /* error_config(); */
                /* init_error_handlers(); */

                // FIXME: test case: 'include' after baddot
                s7_pointer fixed = fix_baddot(utstring_body(dunefile_name));
                /* s7_pointer fixed = s7_eval_c_string(s7, "'(foob)"); */
#if defined(DEVBUILD)
                if (mibl_debug_traversal)
                    LOG_S7_DEBUG("FIXED", fixed);
#endif


                /* close_error_config(); */
                /* error_config(); */
                // FIXING baddot always re-reads entire dunefile
                stanzas = fixed;
                     /* if (s7_is_null(s7,stanzas)) { */
                /*     // fixed is a list of stanzas */
                /*     stanzas = fixed; */
                /* } else{ */
                /*     stanzas = s7_append(s7, stanzas, fixed); */
                /* } */
            /* } */
            } else {
                close_error_config();
                error_config();
                /* init_error_handlers(); */
                s7_quit(s7);
                exit(EXIT_FAILURE);
            }
            break;
        }
        /* printf("readed stanza\n"); */

        /* close_error_config(); */
        /* init_error_handling(); */
        /* error_config(); */
        /* s7_gc_unprotect_at(s7, dune_gc_loc); */

        if (stanza == s7_eof_object(s7)) {
#if defined(DEVBUILD)
            if (mibl_debug) log_trace("readed eof");
#endif
            break;
        }

        /* LOG_S7_DEBUG("SEXP", stanza); */
        /* if (mibl_debug_traversal) */
        /*     LOG_S7_DEBUG("stanza", stanza); */

        if (s7_is_pair(stanza)) {
            if (s7_is_equal(s7, s7_car(stanza),
                            s7_make_symbol(s7, "include"))) {
                /* log_debug("FOUND (include ...)"); */
                /* we can't insert a comment, e.g. ;;(include ...)
                   instead we would have to put the included file in an
                   alist and add a :comment entry. but we needn't bother,
                   we're not going for roundtrippability.
                */

                s7_pointer inc_file = s7_cadr(stanza);
                /* LOG_S7_DEBUG("    including", inc_file); */
                UT_string *dunepath;
                utstring_new(dunepath);
                char *tostr = TO_STR(inc_file);
                utstring_printf(dunepath, "%s/%s",
                                //FIXME: dirname may mutate its arg
                                dirname(path), tostr);
                free(tostr);
                s7_pointer nested = read_dunefile(utstring_body(dunepath));
                g_dunefile_port = dunefile_port;
                /* LOG_S7_DEBUG("nested", nested); */
                /* LOG_S7_DEBUG("stanzas", stanzas); */
                stanzas = s7_append(s7,stanzas, nested);
                /* alt: (:include "(include dune.inc)" (included ...)) */
            } else {
                if (s7_is_null(s7,stanzas)) {
                    stanzas = s7_cons(s7, stanza, stanzas);
                } else{
                    stanzas = s7_append(s7,stanzas, s7_list(s7, 1, stanza));
                }
            }
        } else {
            /* stanza not a pair - automatically means corrupt dunefile? */
            log_error("corrupt dune file? %s\n", utstring_body(dunefile_name));
            /* if exit-on-error */
            exit(EXIT_FAILURE);
        }
    }
    s7_gc_unprotect_at(s7, dune_gc_loc);
    /* fprintf(stderr, "s7_gc_unprotect_at dune_gc_loc: %ld\n", (long)dune_gc_loc); */
    s7_close_input_port(s7, dunefile_port);

#if defined(DEVBUILD)
    if (mibl_debug_traversal)
        log_debug("finished reading dunefile: %s",
                  utstring_body(dunefile_name));
    if (mibl_debug) LOG_S7_DEBUG("readed stanzas", stanzas);
#endif

    return stanzas;
    /* s7_close_input_port(s7, dunefile_port); */
    /* s7_gc_unprotect_at(s7, gc_loc); */
}
