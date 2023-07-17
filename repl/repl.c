/* intro to termios: https://blog.nelhage.com/2009/12/a-brief-introduction-to-termios/ */

#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "gopt.h"
#include "log.h"
#include "libs7.h"

#if defined(DEVBUILD)
extern bool libs7_debug;
extern bool libs7_debug_runfiles;
extern bool libs7_trace;
#endif

bool verbose = false;
bool quiet   = false;

/* void s7_repl(s7_scheme *s7) */
/* { */
/*     log_debug("s7_repl starting"); */

/*     /\* s7_pointer lp = s7_load_path(s7); *\/ */
/*     /\* char *s = s7_object_to_c_string(s7, lp); *\/ */
/*     /\* log_debug("load-path: %s", s); *\/ */
/*     /\* free(s); *\/ */


/*   bool repl_loaded = false; */


/* #if S7_DEVBUILD */
/*       s7_autoload(s7, make_symbol(s7, "compare-calls", 13), s7_make_string(s7, "compare-calls.scm")); */
/*       s7_autoload(s7, make_symbol(s7, "get-overheads", 13), s7_make_string(s7, "compare-calls.scm")); */
/* #endif */

/*       char *script = "repl/repl.scm"; */
/*       /\* char *script = "external/libs7/repl/repl.scm"; *\/ */

/*       /\* s7_provide(s7, "libc.scm"); *\/ */
/*       if (!repl_loaded) { */
/*           /\* log_debug("loading repl.scm"); *\/ */
/*           if (!s7_load(s7, script)) { */
/*               log_error("failed: load repl.scm"); */
/*           } */
/*       } */
/*       s7_eval_c_string(s7, "((*repl* 'run))"); */
/*     /\* } *\/ */
/* } */

char *scm_runfiles_dirs[] = {
    /* this seems to work when pgm is run from libs7 repo or as external */
    "../libs7/scm",
    NULL /* do not remove terminating null */
};

char **scm_dir;

static void _runfiles_init(s7_scheme *s7)
{
    /* s7_pointer tmp_load_path = s7_list(s7, 0); */
#if defined(DEVBUILD)
#ifdef BAZEL_CURRENT_REPOSITORY
    if (libs7_debug)
        log_debug("bazel_current_repo: " BAZEL_CURRENT_REPOSITORY);
#endif
#endif
    scm_dir = scm_runfiles_dirs;
    char *scmdir;
    while (*scm_dir) {
#if defined(DEVBUILD)
        if (libs7_debug_runfiles)
            log_debug(" runfile: %s", *scm_dir);
#endif
        scmdir = realpath(*scm_dir, NULL);
        if (scmdir == NULL) {
            log_error("runfile not found: %s", *scm_dir);
            exit(EXIT_FAILURE);
        }
#if defined(DEVBUILD)
        if (libs7_debug_runfiles)
            log_debug("runfile realpath: %s", scmdir);
#endif
        s7_add_to_load_path(s7, scmdir);
        free(scmdir);
        (void)*scm_dir++;
    }
    //FIXME: uas s7_add_to_load_path!!!
    /* s7_define_variable(s7, "*load-path*", tmp_load_path); */
}

#if defined(DEVBUILD)
static void _print_debug_env(void)
{
    log_debug("getcwd: %s", getcwd(NULL, 0));
    log_debug("getenv(PWD): %s", getenv("PWD"));

    // $HOME - not reliable, use getpwuid() instead
    log_debug("getenv(HOME): %s", getenv("HOME"));
    struct passwd* pwd = getpwuid(getuid());
    log_debug("pwd->pw_dir: %s", pwd->pw_dir);

    // BAZEL_CURRENT_REPOSITORY: null when run from 'home' repo, 'libs7' when run as external repo
    log_debug("BAZEL_CURRENT_REPOSITORY (macro): '%s'", BAZEL_CURRENT_REPOSITORY);

    // TEST_WORKSPACE: always the root ws
    log_debug("TEST_WORKSPACE: '%s'", getenv("TEST_WORKSPACE"));

    // BAZEL_TEST: should always be true when this is compiled as cc_test
    log_debug("BAZEL_TEST: '%s'", getenv("BAZEL_TEST"));

    // BUILD_WORK* vars: null under 'bazel test'
    log_debug("BUILD_WORKSPACE_DIRECTORY: %s", getenv("BUILD_WORKSPACE_DIRECTORY"));
    log_debug("BUILD_WORKING_DIRECTORY: %s", getenv("BUILD_WORKING_DIRECTORY"));

    // TEST_SRCDIR - required for cc_test
    log_debug("GENDIR: %s", getenv("GENDIR"));
    log_debug("TEST_SRCDIR: %s", getenv("TEST_SRCDIR"));
    log_debug("BINDIR: %s", getenv("BINDIR"));

    /* RUNFILES_MANIFEST_FILE: null on macos. */
    log_debug("RUNFILES_MANIFEST_FILE: %s", getenv("RUNFILES_MANIFEST_FILE"));

    /* RUNFILES_MANIFEST_FILE: null on macos. */
    log_debug("RUNFILES_MANIFEST_ONLY: %s", getenv("RUNFILES_MANIFEST_ONLY"));

    /* RUNFILES_DIR: set on macos for both bazel test and bazel run. */
    log_debug("RUNFILES_DIR: %s", getenv("RUNFILES_DIR"));
}
#endif

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
    FLAG_DEBUG_RUNFILES,
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
    [FLAG_DEBUG_RUNFILES] = {.long_name="debug-runfiles",
                             .flags=GOPT_ARGUMENT_FORBIDDEN},
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
#if defined(DEVBUILD)
        libs7_debug = true;
#endif
    }

    if (options[FLAG_DEBUG_RUNFILES].count) {
#if defined(DEVBUILD)
        libs7_debug_runfiles = true;
#else
        log_error("--debug-runfiles requires debug build, -c dbg");
        exit(EXIT_FAILURE);
#endif
    }

    if (options[FLAG_DEBUG_S7].count) {
#if defined(DEVBUILD)
        libs7_debug = true;
#endif
    }

    if (options[FLAG_TRACE].count) {
#if defined(DEVBUILD)
        libs7_trace = true;
#endif
    }
}

/* static void dumb_repl(s7_scheme *sc) */
/* { */
/*   while (true) */
/*     { */
/*       char buffer[512]; */
/*       fprintf(stdout, "\n> "); */
/*       if (!fgets(buffer, 512, stdin)) break;  /\* error or ctrl-D *\/ */
/*       if (((buffer[0] != '\n') || (strlen(buffer) > 1))) */
/* 	{ */
/* 	  char response[1024]; */
/* 	  snprintf(response, 1024, "(write %s)", buffer); */
/* 	  s7_eval_c_string(sc, response); */
/* 	}} */
/*   fprintf(stdout, "\n"); */
/*   if (ferror(stdin)) */
/*     fprintf(stderr, "read error on stdin\n"); */
/* } */

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

#if defined(DEVBUILD)
    if (libs7_debug) _print_debug_env();
#endif

    /* log_info("argc: %d", argc); */
    /* for (int i = 0; i < argc; i++) { */
    /*     log_debug("argv %d: %s", i, argv[i]); */
    /* } */


    s7_scheme *s7 = libs7_init();
    if (!quiet) {
#if defined(DEVBUILD)
        log_debug("s7: %s", S7_DATE);
#if defined(CLIBS_LINK_STATIC)
        log_debug("clib linkage: static");
#elif defined(CLIBS_LINK_RUNTIME)
        log_debug("clib linkage: runtime");
#else
        log_debug("clib linkage: shared");
#endif
#endif
    }

    _runfiles_init(s7);

    if (options[OPT_LOAD_PATH].count) {
        s7_add_to_load_path(s7, options[OPT_LOAD_PATH].argument);
    }

    libs7_load_clib(s7, "c");
    libs7_load_clib(s7, "mustachios");
    libs7_load_clib(s7, "toml");
    libs7_load_clib(s7, "json");
    libs7_load_clib(s7, "sexp");

    // if under emacs: dumb_repl
    // else...

    // deal with bazel context
    char *script;
    if (strlen(BAZEL_CURRENT_REPOSITORY) == 0)
        script = "repl/repl.scm";
    else
        script = "external/libs7/repl/repl.scm";

    /* log_debug("script: %s", script); */

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

        if (!s7_load(s7, script)) log_error("failed: load %s", script);
        s7_eval_c_string(s7, "((*repl* 'run))");
/* #else */
/*         s7_repl(s7); */
/* #endif */
    }
    return(0);
}

