/* #include <wordexp.h> */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "gopt.h"
#include "log.h"
#include "libs7.h"


#if ! defined(CLIBS_LINK_RUNTIME)
#include "libc_s7.h"
#include "libm_s7.h"
#include "libcwalk_s7.h"
#include "libdl_s7.h"
#endif

extern bool libs7_debug;
extern bool libs7_trace;
bool verbose = false;
bool quiet   = false;

void s7_repl(s7_scheme *s7)
{
    /* log_debug("s7_repl starting"); */

    /* s7_pointer lp = s7_load_path(s7); */
    /* char *s = s7_object_to_c_string(s7, lp); */
    /* log_debug("load-path: %s", s); */
    /* free(s); */


  bool repl_loaded = false;

#if defined(CLIBS_LINK_RUNTIME)
    clib_dload_ns(s7, "libc_s7", "libc", DSO_EXT);
    /* clib_dload_ns(s7, "libdl_s7", "libdl", DSO_EXT); */
    /* clib_dload_global(s7, "libm_s7", "libm.scm", DSO_EXT); */
    /* clib_dload_global(s7, "libcwalk_s7", "libcwalk.scm", DSO_EXT); */
#else  /* link:shared? */
    clib_sinit(s7, libc_s7_init, "libc");
    /* clib_sinit(s7, libdl_s7_init, "libdl"); */
    /* clib_sinit(s7, libm_s7_init, "libm"); */
    /* clib_sinit(s7, libcwalk_s7_init, "libcwalk"); */
#endif

#if S7_DEBUGGING
      s7_autoload(s7, make_symbol(s7, "compare-calls", 13), s7_make_string(s7, "compare-calls.scm"));
      s7_autoload(s7, make_symbol(s7, "get-overheads", 13), s7_make_string(s7, "compare-calls.scm"));
#endif

      char *script = "repl/repl.scm";
      /* char *script = "external/libs7/repl/repl.scm"; */

      /* s7_provide(s7, "libc.scm"); */
      if (!repl_loaded) {
          /* log_debug("loading repl.scm"); */
          if (!s7_load(s7, script)) {
              log_error("failed: load repl.scm");
          }
      }
      s7_eval_c_string(s7, "((*repl* 'run))");
    /* } */
}

void _print_version(void) {
    printf("FIXME: version id\n");
}

void _print_usage(void) {
    printf("Usage:\t$ bazel run @libs7//repl [flags, options]\n");

    printf("Option:\n");
    printf("\t--load-path\t\tDirectory to add to s7 *load-path*.\n");

    printf("Flags:\n");
    printf("\t-d, --debug\t\tEnable all debugging flags.\n");
    printf("\t-h, --help\t\tPrint help screen.\n");
    printf("\t-t, --trace\t\tEnable trace flags.\n");
    printf("\t-v, --verbose\t\tEnable verbosity. Repeatable.\n");
    printf("\t-q, --quiet\t\tSuppress msgs to stdout/stderr.\n");
    printf("\t--version\t\tShow version Id.\n");
    /* printf("\n"); */
    /* printf("INI file: $XDG_CONFIG_HOME/miblrc\n"); */

    printf("\n");
}

enum OPTS {
    FLAG_DEBUG,
    FLAG_DEBUG_S7,
    FLAG_HELP,
    FLAG_TRACE,
    FLAG_VERBOSE,
    FLAG_QUIET,
    FLAG_VERSION,
    OPT_LOAD_PATH,
    LAST
};

static struct option options[] = {
    /* 0 */
    [FLAG_DEBUG] = {.long_name="debug",.short_name='d',
                    .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_DEBUG_S7] = {.long_name="debug-s7",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    /* [FLAG_DEBUG_S7_ALL] = {.long_name="debug-all", */
    /*                    .flags=GOPT_ARGUMENT_FORBIDDEN}, */
    /* [FLAG_DEBUG_S7_LOADS] = {.long_name="debug-s7-loads", */
    /*                    .flags=GOPT_ARGUMENT_FORBIDDEN}, */
    [FLAG_HELP] = {.long_name="help",.short_name='h',
                   .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_TRACE] = {.long_name="trace",.short_name='t',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERBOSE] = {.long_name="verbose",.short_name='v',
                      .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_QUIET] = {.long_name="quiet",.short_name='q',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERSION] = {.long_name="version",
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [OPT_LOAD_PATH] = {.long_name="load-path",
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [LAST] = {.flags = GOPT_LAST}
};

void _set_options(struct option options[])
{
    if (options[FLAG_HELP].count) {
        /* if (options[FLAG_HELP].count > 1) */
        /*     _print_debug_usage(); */
        /* else */
            _print_usage();
        exit(EXIT_SUCCESS);
    }

    if (options[FLAG_VERSION].count) {
        _print_version();
        exit(EXIT_SUCCESS);
    }

    if (options[FLAG_QUIET].count)   { quiet   = true; }
    if (options[FLAG_VERBOSE].count) { verbose = true; }

    if (options[FLAG_DEBUG].count) {
#if defined(DEBUG_TRACE)
        libs7_debug = true;
#endif
    }

    if (options[FLAG_DEBUG_S7].count) {
#if defined(DEBUG_TRACE)
        libs7_debug = true;
#endif
    }

    if (options[FLAG_TRACE].count) {
#if defined(DEBUG_TRACE)
        libs7_trace = true;
#endif
    }
}

// WITH_MAIN && ! WITH_NOTCURSES && ! WITH_C_LOADER
// ! USE_SND

// WITH_C_LOADER means: s7 'load' can handle .so files using dlopen.
// no C_LOADER means: s7.c does not include 'load_shared_object()'.
// Loads will only load scm source files, using fopen. So (load
// "foo.so") will not work?

/* void libc_s7_init(s7_scheme *sc); */

int main(int argc, char **argv) // , char **envp)
{
    /* for (char **env = envp; *env != 0; env++) */
    /*  { */
    /*    char *thisEnv = *env; */
    /*    printf("%s\n", thisEnv); */
    /*  } */
    /* char *pwd = getcwd(NULL, 0); */
    /* fprintf(stderr, "PWD: %s\n", pwd); */
    /* free(pwd); */

    /* log_info("argc: %d", argc); */
    /* for (int i = 0; i < argc; i++) { */
    /*     log_debug("argv %d: %s", i, argv[i]); */
    /* } */

    int gopt_argc = gopt(argv, options);
    (void)gopt_argc;

    gopt_errors(argv[0], options);

    _set_options(options);

    /* log_info("argc: %d", argc); */
    /* for (int i = 0; i < argc; i++) { */
    /*     log_debug("argv %d: %s", i, argv[i]); */
    /* } */


    s7_scheme *s7 = libs7_init();
    if (!quiet) {
        log_info("s7: %s", S7_DATE);
#if defined(CLIBS_LINK_STATIC)
        log_info("clib linkage: static");
#elif defined(CLIBS_LINK_RUNTIME)
        log_info("clib linkage: runtime");
#else
        log_info("clib linkage: shared");
#endif
    }

    if (options[OPT_LOAD_PATH].count) {
        s7_add_to_load_path(s7, options[OPT_LOAD_PATH].argument);
    }

    /* TODO: add scm libs from runfiles to *load-path* */

    /* log_info("argc: %d", argc); */
    if (argc > 2) { //FIXME: use name param, e.g. -s, --script
        // batch process
        fprintf(stderr, "load %s\n", argv[1]);
        if (!s7_load(s7, argv[1])) {
            fprintf(stderr, "can't load %s\n", argv[1]);
            return(2);
        }
    } else {
        /* s7_pointer lp = s7_load_path(s7); */
        /* char *s = s7_object_to_c_string(s7, lp); */
        /* log_debug("load-path: %s", s); */
        /* free(s); */

#if defined(CLIBS_LINK_RUNTIME)
        /* clib_dload_ns(s7, "libc_s7", "libc", DSO_EXT); */
        /* clib_dload_ns(s7, "libdl_s7", "libdl", DSO_EXT); */
        /* clib_dload_global(s7, "libm_s7", "libm.scm", DSO_EXT); */
        /* clib_dload_global(s7, "libcwalk_s7", "libcwalk.scm", DSO_EXT); */
#else  /* linkage: static or shared */
        clib_sinit(s7, libc_s7_init, "libc");
        clib_sinit(s7, libdl_s7_init, "libdl");
        clib_sinit(s7, libm_s7_init, "libm");
        clib_sinit(s7, libcwalk_s7_init, "libcwalk");
#endif

        char *script = "repl/repl.scm";
        /* char *script = "external/libs7/repl/repl.scm"; */
        if (!s7_load(s7, script)) log_error("failed: load %s", script);
        s7_eval_c_string(s7, "((*repl* 'run))");
/* #else */
/*         s7_repl(s7); */
/* #endif */
    }
    return(0);
}

