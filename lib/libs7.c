#include <errno.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <libgen.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

/* #include "liblogc.h" */
/* #include "librunfiles.h" */

#include "utarray.h"
#if INTERFACE
#include "s7.h"
#endif

#include "libs7_internal.h"

const char *libs7_version = LIBS7_VERSION;

#define TRACE_FLAG libs7_trace
bool    TRACE_FLAG          = false;
#define DEBUG_LEVEL libs7_debug
int     DEBUG_LEVEL         = 0;
#if defined(PROFILE_fastbuild)
bool libs7_debug_runfiles = false;
#endif

int  libs7_verbosity      = 0;

UT_array *dlopened; /* list of libs loaded dynamically */

void fs_api_init(s7_scheme *sc);

static char buf[512]; // max len of <libname>_init or lib/shared/<libname><ext>

static int _strsort(const void *_a, const void *_b)
{
    const char *a = *(const char* const *)_a;
    const char *b = *(const char* const *)_b;
    return strcmp(a,b);
}

/* **************************************************************** */
EXPORT char *libs7_read_file(char *fname)
{
    off_t file_size;
    char *buffer;
    struct stat stbuf;
    int fd;

    fd = open(fname, O_RDONLY);
    if (fd == -1) {
        /* Handle error */
        fprintf(stderr, "%s:%d open(%s) error\n",
                __FILE__, __LINE__, fname);
    }

    if ((fstat(fd, &stbuf) != 0) || (!S_ISREG(stbuf.st_mode))) {
        /* Handle error */
        fprintf(stderr, "%s:%d fstat error",
                __FILE__, __LINE__);
    }

    file_size = stbuf.st_size;
#if defined(PROFILE_fastbuild)
    LOG_DEBUG(0, "filesize: %d", file_size);
#endif

    buffer = (char*)malloc(file_size);
    if (buffer == NULL) {
        /* Handle error */
        fprintf(stderr, "%s:%d malloc file_size fail",
                __FILE__, __LINE__);
    }

    /* FIXME: what about e.g. unicode in string literals? */
    errno = 0;
    /* FILE *instream = fopen(dunefile_name, "r"); */
    FILE *instream = fdopen(fd, "r");
    if (instream == NULL) {
        /* Handle error */
        fprintf(stderr, "%s:%d fdopen error\n",
                __FILE__, __LINE__);
        /* printf(RED "ERROR" CRESET "fdopen failure: %s\n", */
        /*        dunefile_name); */
        /*        /\* utstring_body(dunefile_name)); *\/ */
        perror(NULL);
        exit(EXIT_FAILURE);
    } else {
        LOG_DEBUG(0, "fdopened %s", fname);
    }

    // now read the entire file
    size_t read_ct = fread(buffer, 1, file_size, instream);
    LOG_DEBUG(0, "read_ct: %d", read_ct);

    if (read_ct != (size_t)file_size) {
        if (ferror(instream) != 0) {
            fprintf(stderr,
                    RED "ERROR" CRESET
                    " %s:%d fread error for %s\n",
                    __FILE__, __LINE__, fname);
            exit(EXIT_FAILURE); //FIXME: exit gracefully
        } else {
            if (feof(instream) == 0) {
                fprintf(stderr,
                        RED "ERROR" CRESET
                        " %s:%d fread error for %s\n",
                        __FILE__, __LINE__, fname);
                /* utstring_body(dunefile_name)); */
                exit(EXIT_FAILURE); //FIXME: exit gracefully
            } else {
                fprintf(stderr,
                        RED "ERROR" CRESET
                        " %s:%d fread error for %s\n",
                        __FILE__, __LINE__, fname);
            }
        }
    } else {
        close(fd);
        fclose(instream);
    }
    return buffer;

    // cleanup:
    if (instream != NULL)
    {
        fclose(instream);
        close(fd);
    }

    return NULL;
}

/* **************************************************************** */
static s7_pointer _dlopen_clib(s7_scheme *s7, char *lib, char *init_fn_name)
{
    TRACE_ENTRY;
    TRACE_LOG(" LIB: %s", lib);

    char *libname = strdup(lib);

    // have we already loaded it?
    const char **p = NULL;
/*     while ( (p=(const char**)utarray_next(strs,p)) != NULL ) { */
/*         s = *p; */
/*         printf("finding %s\n",s); */
/* #ifdef __cplusplus */

    bool already_loaded = false;
    (void)already_loaded;

    LOG_DEBUG(0, "dlopened ct: %d", utarray_len(dlopened));
    if (utarray_len(dlopened) > 0) {
        LOG_DEBUG(0, "searching dlopened list for: %s", lib);
        utarray_sort(dlopened, _strsort);
        LOG_DEBUG(0, "sorted", "");
        p = NULL;
        const char *s;
        int i = 0;
        (void)i; // set-but-unused
        while ( (p=(const char**)utarray_next(dlopened,p)) != NULL ) {
            s = *p;
            LOG_DEBUG(0, "item %d: %s", i, s);
            if (strncmp(s, libname, strlen(libname)) == 0) {
                LOG_DEBUG(0, "MATCH", "");
                already_loaded = true;
                break;
            } else {
                LOG_DEBUG(0, "MISMATCH", "");
                i++;
            }
            /* p = (const char**)utarray_find(dlopened, lib, strsort); */
            /* p = utarray_find(dlopened, &libname, _strsort); */
            /* log_debug("LOADED %s? %s", lib, *p); */
            /* free(needle); */
        }
    }

    /* if (already_loaded) */

    /* int len = strlen(libname); */
    sprintf(buf, "lib%s_s7_init", lib);
    LOG_DEBUG(0, "init fn: %s", init_fn_name);
    s7_pointer init_sym    = s7_make_symbol(s7, "init_func");
    s7_pointer init_fn_sym = s7_make_symbol(s7, init_fn_name); // buf);
    s7_pointer init_list = s7_list(s7, 2, init_sym, init_fn_sym);

    s7_pointer e = s7_inlet(s7, init_list);
    s7_int gc_loc = s7_gc_protect(s7, e);
    s7_pointer old_e = s7_set_curlet(s7, e);

    char *ws = getenv("TEST_WORKSPACE");
//#if defined(PROFILE_opt) || defined(PROFILE_fastbuild)
    fprintf(stdout, "BAZEL_TEST: %s\n", getenv("BAZEL_TEST"));
    // BAZEL_TEST true if tgt is cc_test (either bazel test or  run cmd)

    // bazel test: The initial working directory shall be
    // $TEST_SRCDIR/$TEST_WORKSPACE.

    // TEST_SRCDIR: absolute path to the base of the runfiles tree. required
    fprintf(stderr, "BAZEL_CURRENT_REPO: %s\n",
              getenv("BAZEL_CURRENT_REPOSITORY"));
    fprintf(stderr, "TEST_SRCDIR: %s\n", getenv("TEST_SRCDIR"));
    fprintf(stderr, "TEST_WORKSPACE: %s\n", ws);
//#endif
    char *fmt;
    (void)fmt;
    if (ws) {
        if ( (strncmp("libs7", ws, 5) == 0)
             && strlen(ws) == 5 ) {
            fmt = "lib/lib%s/lib%s_s7%s";
        } else {
            fmt = "external/libs7/lib/lib%s/lib%s_s7%s";
        }
    } else {
        //FIXME: use BAZEL_CURRENT_REPOSITORY for runfiles
        ws = getenv("BUILD_WORKSPACE_DIRECTORY");
        /* log_debug("dlopen ns build ws: %s", ws); */
        if (strncmp(basename(ws), "libs7", 5) == 0)
            fmt = "lib/lib%s/lib%s_s7%s";
        else
            fmt = "external/libs7/lib/lib%s/lib%s_s7%s";
    }
    /* snprintf(buf, */
    /*          512, // 20 + 18 + len + strlen(dso_ext), */
    /*          fmt, */
    /*          lib, */
    /*          lib, DSO_EXT); */
    snprintf(buf,
             512, // 20 + 18 + len + strlen(dso_ext),
             "lib/lib%s_s7%s",
             /* lib, */
             lib, DSO_EXT);
    LOG_DEBUG(0, "dso: %s", buf);
    s7_pointer lp = s7_load_path(s7);
    char *s = s7_object_to_c_string(s7, lp);
    LOG_DEBUG(0, "load-path: %s", s);
    free(s);
    LOG_DEBUG(0, "CWD: %s", getcwd(NULL,0));

    s7_pointer val = s7_load_with_environment(s7, buf, e);

    if (val) {
        if (libs7_verbosity > 0)
            printf("loaded %s\n", buf);
        utarray_push_back(dlopened, &libname); // add to dlopened list
        s7_pointer libs = s7_slot(s7, s7_make_symbol(s7, "*libraries*"));
        snprintf(buf, strlen(lib) + 3, "*%s*", lib);
        s7_define(s7, s7_nil(s7), s7_make_symbol(s7, buf), e);
        snprintf(buf, strlen(lib) + 5, "%s.scm", lib);
        s7_slot_set_value(s7, libs,
              s7_cons(s7,
                s7_cons(s7, s7_make_semipermanent_string(s7, buf), e),
                            s7_slot_value(libs)));
    } else {
        fprintf(stderr,
                "%s:%d "
                RED "ERROR" CRESET
                " s7_load_with_environment %s\n",
                __FILE__, __LINE__, buf);
    }

    s7_set_curlet(s7, old_e);       /* restore incoming (curlet) */
    s7_gc_unprotect_at(s7, gc_loc);
    if (!val) {
        LOG_ERROR(0, "load fail: %s", buf);
        return s7_values(s7, s7_nil(s7));
    }
    return val;
}

EXPORT s7_pointer libs7_load_plugin(s7_scheme *s7, char *lib)
{
    TRACE_ENTRY;
    TRACE_LOG("load_plugin: %s", lib);

    static char init_fn_name[512]; // max len of <libname>_init or lib/shared/<libname><ext>

    init_fn_name[0] = '\0';
    sprintf(init_fn_name, "lib%s_s7_init", lib);
    LOG_DEBUG(0, "init_fn_name: %s", init_fn_name);

#if defined(__APPLE__)
#define HNDL RTLD_MAIN_ONLY
#else
#define HNDL RTLD_DEFAULT
#endif

    s7_pointer (*init_fn_ptr)(s7_scheme*);
    init_fn_ptr = (s7_pointer (*)(s7_scheme*))dlsym(HNDL,
                                                    init_fn_name);
    if (init_fn_ptr == NULL) {
/* #if defined(PROFILE_fastbuild) */
        LOG_DEBUG(0, "%s not statically linked, trying dlopen", init_fn_name);
/* #endif */
        s7_pointer res = _dlopen_clib(s7, lib, init_fn_name);
        return res;
    } else {
        LOG_DEBUG(0, "dlsym init_fn_ptr: %x", init_fn_ptr);
    }
    // we found the init fn, so unload, then reload

    s7_pointer e = s7_inlet(s7, s7_nil(s7)); // empty env
    s7_int gc_loc = s7_gc_protect(s7, e);
    s7_pointer old_e = s7_set_curlet(s7, e);
    s7_pointer old_shadow = s7_set_shadow_rootlet(s7, e);

    s7_pointer clib_let = init_fn_ptr(s7);

    /* s7_varlet(s7, s7_rootlet(s7), clib_let); */

    s7_pointer libs = s7_slot(s7, s7_make_symbol(s7, "*libraries*"));
    snprintf(buf, strlen(lib) + 3, "*%s*", lib);
    s7_define(s7, s7_nil(s7), s7_make_symbol(s7, buf), clib_let); // e);
    snprintf(buf, strlen(lib) + 5, "%s.scm", lib);
    s7_slot_set_value(s7, libs,
                      s7_cons(s7,
                              s7_cons(s7, s7_make_semipermanent_string(s7, buf), clib_let), // e),
                              s7_slot_value(libs)));
    s7_set_curlet(s7, old_e);       /* restore incoming (curlet) */
    s7_gc_unprotect_at(s7, gc_loc);

    s7_set_shadow_rootlet(s7, old_shadow);
    return clib_let;
}

static s7_pointer g_libs7_load_plugin(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY;
    s7_pointer p, arg;
    char* lib;
    p = args;
    arg = s7_car(p);
    if (s7_is_string(arg)) {
        lib = (char*)s7_string(arg);
    } else {
        if (s7_is_symbol(arg)) {
            lib = (char*)s7_symbol_name(arg);
        }
        else return(s7_wrong_type_arg_error(s7, __func__, 0, arg, "string"));
    }
    return libs7_load_plugin(s7, lib);
}

#define TO_STR(x) s7_object_to_c_string(s7, x)
/* #define TO_S7_INT(x) s7_make_integer(s7, x) */

/*
 * It is permissible to let nil be an element of an a-list in place of
 * a pair. Such an element is not considered to be a pair but is
 * simply passed over when the a-list is searched by assoc.
 * (https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node153.html)
 */
bool libs7_is_alist(s7_scheme *s7, s7_pointer arg)
{
    /* TRACE_ENTRY; */
    if (s7_is_list(s7, arg)) {
        if (arg == s7_nil(s7)) // '() is an alist
            return true;


        s7_pointer x;
        s7_pointer iter = s7_make_iterator(s7, arg);
        s7_int gc = s7_gc_protect(s7, iter);
        if (!s7_is_iterator(iter)) {
            /* FIXME: fprintf */
            LOG_ERROR(0, "s7_make_iterator failed on %s", TO_STR(arg));
            s7_gc_unprotect_at(s7, gc);
            return false;
        }

        if (s7_iterator_is_at_end(s7, iter)) {
            /* FIXME: fprintf */
            LOG_ERROR(0, "iterator %s is prematurely done on %s", TO_STR(arg));
            s7_gc_unprotect_at(s7, gc);
            return false;
        }

        x = s7_iterate(s7, iter);
        while (true) {
            if ((x == s7_eof_object(s7))
                || (s7_iterator_is_at_end(s7, iter))) {
                s7_gc_unprotect_at(s7, gc);
                return true;
            }
            if (s7_is_null(s7, x)) { // s7_cdr(x))) {
                // '() not a pair, but allowed as mbr of alist
                ;
            }
            else if (!s7_is_pair(x)) {
                s7_gc_unprotect_at(s7, gc);
                return false;
            }
            x = s7_iterate(s7, iter);
        }
    } else
        return false;
}

/*
 * WARNING: this is for mustache processing, it probably should not
 * live here.  returns true of arg is an alist whose keys are keywords.
 */
bool libs7_is_kw_alist(s7_scheme *s7, s7_pointer arg)
{
    if (s7_is_list(s7, arg)) {
        if (arg == s7_nil(s7)) // '() is an alist
            return true;


        s7_pointer x;
        s7_pointer iter = s7_make_iterator(s7, arg);
        s7_int gc = s7_gc_protect(s7, iter);
        if (!s7_is_iterator(iter)) {
            /* FIXME: fprintf */
            LOG_ERROR(0, "s7_make_iterator failed on %s", TO_STR(arg));
            s7_gc_unprotect_at(s7, gc);
            return false;
        }

        if (s7_iterator_is_at_end(s7, iter)) {
            /* FIXME: fprintf */
            LOG_ERROR(0, "iterator %s is prematurely done on %s", TO_STR(arg));
            s7_gc_unprotect_at(s7, gc);
            return false;
        }

        x = s7_iterate(s7, iter);
        while (true) {
            /* log_debug("XXXXXXXXXXXXXXXX iter: %s", TO_STR(x)); */
            if ((x == s7_eof_object(s7))
                || (s7_iterator_is_at_end(s7, iter))) {
                s7_gc_unprotect_at(s7, gc);
                return true;
            }
            if (s7_is_null(s7, x)) { // s7_cdr(x))) {
                // '() not a pair, but allowed as mbr of alist
                ;
            }
            else if (s7_is_pair(x)) {
                if ( !s7_is_keyword(s7_car(x)) ) {
                    return false;
                }
            } else {
                s7_gc_unprotect_at(s7, gc);
                return false;
            }
            x = s7_iterate(s7, iter);
        }
    } else
        return false;
}

bool libs7_is_empty_alist(s7_scheme *s7, s7_pointer arg)
{
    /* log_debug("libs7_is_empty_alist"); */
    if (s7_is_list(s7, arg)) {
        if (arg == s7_nil(s7)) // '() is an empty alist
            return true;

        // treat '(()) as empty alist
        if ((s7_car(arg) == s7_nil(s7))
            && (s7_cdr(arg) == s7_nil(s7)))
            return true;
        else
            return false;
    } else {
        /* log_debug("not list"); */
        return false;
    }
}

static s7_pointer g_is_alist(s7_scheme *s7, s7_pointer args)
{
    /* log_debug("g_is_alist: %s", TO_STR(arg)); */
    //NB: args is list of one arg
    if (libs7_is_alist(s7, s7_car(args)))
        return s7_t(s7);
    else
        return s7_f(s7);
}

/* static char *scm_runfiles_dirs[] = { */
/*     /\* this seems to work when libs7 is external dep *\/ */
/*     /\* "../libs7/scm", *\/ */
/*     /\* when run from libs7 repo itself:  *\/ */
/*     "libs7~0.1.0/scm", */
/*     NULL /\* do not remove terminating null *\/ */
/* }; */

/* char **scm_dir; */

static void _runfiles_init(s7_scheme *s7)
{
    TRACE_ENTRY;
    /* s7_pointer tmp_load_path = s7_list(s7, 0); */
/* #if defined(PROFILE_fastbuild) */
/* #ifdef BAZEL_CURRENT_REPOSITORY */
/*     if (libs7_debug) */
/*         log_debug("bazel_current_repo: " BAZEL_CURRENT_REPOSITORY); */
/* #endif */
/* #endif */

    if (libs7_verbosity > 0) {
        LOG_INFO(0, "libs7 BAZEL_CURRENT_REPOSITORY: %s", BAZEL_CURRENT_REPOSITORY);

        LOG_INFO(0, "TEST_WORKSPACE: %s", getenv("TEST_WORKSPACE"));
        LOG_INFO(0, "BAZEL_TEST:  %s", getenv("BAZEL_TEST"));
        LOG_INFO(0, "RUNFILES_DIR:  %s", getenv("RUNFILES_DIR"));
        LOG_INFO(0, "CWD:  %s", getcwd(NULL,0));
    }
    //TODO: is this use of BAZEL_CURRENT_REPOSITORY reliable? or do we
    // need to read _repo_mapping file to get canonical name for libs7?
    /* char linkbuf[512]; */

    char *libs7_repo;
    if (strlen(BAZEL_CURRENT_REPOSITORY) == 0) {
        libs7_repo = realpath("_main/scm", NULL);
    } else {
        libs7_repo = realpath("external/" BAZEL_CURRENT_REPOSITORY "/scm", NULL);
    }
    /* ssize_t linksz = readlink(libs7_repo, (char*)linkbuf, 512); */
    /* linkbuf[linksz + 1] = '\0'; */
    if (libs7_verbosity > 0)
        LOG_INFO(0, "libs7_repo: %s", libs7_repo);
    /* log_debug("readlink libs7_repo: %s", linkbuf); */
    s7_add_to_load_path(s7, libs7_repo);
/*     if (getenv("BAZEL_TEST")) { */
/*         s7_add_to_load_path(s7, libs7_repo); */
/*     } else { */
/*         scm_dir = scm_runfiles_dirs; */
/*         char *scmdir; */
/*         while (*scm_dir) { */
/* #if defined(PROFILE_fastbuild) */
/*             if (libs7_debug_runfiles) */
/*                 log_debug(" runfile: %s", *scm_dir); */
/* #endif */
/*             scmdir = realpath(*scm_dir, NULL); */
/*             if (scmdir == NULL) { */
/*                 log_error("Runfile not found: %s", *scm_dir); */
/*                 /\* exit(EXIT_FAILURE); *\/ */
/*             } */
/* #if defined(PROFILE_fastbuild) */
/*             if (libs7_debug_runfiles) */
/*                 log_debug("runfile realpath: %s", scmdir); */
/* #endif */
/*             s7_add_to_load_path(s7, scmdir); */
/*             free(scmdir); */
/*             (void)*scm_dir++; */
/*         } */
/*     } */
}

void libs7_shutdown(s7_scheme *s7)
{
    /* close_error_config(); */
    s7_quit(s7);
}

EXPORT s7_scheme *libs7_init(void)
/* WARNING: dlopen logic assumes file path <libns>/<libname><dso_ext> */
{
    s7_scheme *s7 = s7_init();

    //FIXME: initialize error handlers

    fs_api_init(s7); //FIXME: eliminate

    utarray_new(dlopened, &ut_str_icd);

    /* s7_pointer lp = s7_load_path(s7); */
    /* char *s = s7_object_to_c_string(s7, lp); */
    /* log_debug("load-path: %s", s); */
    /* free(s); */

    _runfiles_init(s7);

    /* lp = s7_load_path(s7); */
    /* s = s7_object_to_c_string(s7, lp); */
    /* log_debug("load-path: %s", s); */
    /* free(s); */

    s7_define_function(s7, "load-plugin", g_libs7_load_plugin,
                       1,         /* required: 1 arg, libname */
                       0,         /* optional: 0 */
                       false,     /* rest args: none */
                       "(load-clib sym) initializes statically linked clib archives and dsos, "
                       "and dlopens and initializes dynamically linked dsos. "
                       "libsym may be symbol or string; it should not include 'lib' prefix and '_s7' suffix.");

    /* two macros commonly used in srfis: receive and :optional */

    /* receive: https://srfi.schemers.org/srfi-8/srfi-8.html */
    /* (define-macro (receive formals expression . body) */
    /*   `(call-with-values (lambda () ,expression) */
    /*                     (lambda ,formals ,@body))) */

    /* (define-macro (?optional val default-value) `(if (null? ,val) ,default-value (car ,val))) */

    s7_define_function(s7, "alist?", g_is_alist,
                       1,         /* required: 1 arg */
                       0,         /* optional: 0 */
                       false,     /* rest args: none */
                       "(alist? lst)");

    /* install #; comment reader */
    s7_eval_c_string(s7,
                     "(set! *#readers* (cons (cons #\\; (lambda (str) (if (string=? str \";\") (read)) (values))) *#readers*))");

    /* #m(): proper map (hash-table) constructor
       - duplicate keys disallowed
       - map-entry pairs implicit
    */
    const char *ht_sexp = ""
        "(set! *#readers* "
        "      (cons (cons #\\m (lambda (str) "
        "			(and (string=? str \"m\") "
        "                      (let ((h (apply hash-table (read)))) "
        "                         (if (> (*s7* 'safety) 1) (immutable! h) h))))) "
        "	    *#readers*)) "
        ;
    s7_pointer ht_ctor = s7_eval_c_string(s7, ht_sexp);
    (void)ht_ctor;

    /* #M(): multimap (alist) constructor - duplicate keys allowed,
       map-entry pairs explicit. Just a notational convenience to make the
       contruction more conspicuous; it doesn't really do anything, just
       produces an alist.  #M((:a 1)) == '((:a 1)).

       Can we have it validate that its argument is an alist?
    */
    /* const char *mmap_sexp = "" */
    /*     "(set! *#readers* " */
    /*     "      (cons (cons #\\M (lambda (str) " */
    /*     "			(and (string=? str \"M\") " */
    /*     "                    (let ((mmap (read))) " */
    /*     "                        `(quote ,mmap)))))" */
    /*     "	    *#readers*)) " */
    /*     ; */
    /*     /\* "                       (if (> (*s7* 'safety) 1) " *\/ */
    /*     /\* "                          (immutable! m) " *\/ */
    /*     /\* "                           (quote m)))))) " *\/ */

    /* s7_pointer mmap_ctor = s7_eval_c_string(s7, mmap_sexp); */
    /* (void)mmap_ctor; */

    return s7;
}
