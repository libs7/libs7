#include <libgen.h>
#include <pwd.h>
#include <stdlib.h>             /* putenv */
#include <unistd.h>             /* getcwd */
#include <sys/errno.h>
#include <sys/types.h>

#include "liblogc.h"

#if EXPORT_INTERFACE
#include "gopt.h"
#include "libs7.h"
#endif

#include "s7plugin_test_config.h"

s7_scheme *s7;

#if defined(PROFILE_fastbuild)
bool s7plugin_trace;
int  s7plugin_debug;
extern bool libs7_trace;
extern int  libs7_debug;
extern bool libs7_debug_runfiles;
#endif

int  s7plugin_verbosity;
extern int  libs7_verbosity;

/* extern struct option options[]; */

/* char *sexp_input; */
/* char *sexp_expected; */

/* UT_string *setter; */
/* UT_string *sexp; */
/* s7_pointer actual; */
/* s7_pointer expected; */

void cleanup(void)
{
    /* https://wiki.sei.cmu.edu/confluence/display/c/FIO23-C.+Do+not+exit+with+unflushed+data+in+stdout+or+stderr */
    /* Do cleanup */
    /* printf("All cleaned up!\n"); */
    if (fflush(stdout) == EOF) {
        /* Handle error */
    }
}

static void print_plugin_usage(char *test) {
    printf("Usage:\t$ bazel test test:%s [-- flags]\n", test);
    /* printf("  Link strategies:\n"); */
    /* printf("\t--//config/clibs/link=static\tStatic link/load (static libs). Default.\n"); */
    /* printf("\t--//config/clibs/link=shared\tStatic link, dynamic load (shared libs)\n"); */
    /* printf("\t--//config/clibs/link=runtime\tLink/load at runtime (dload shared libs)\n"); */
    /* printf("  (prefix @libs7 if used as external repo), e.g.\n"); */
    /* printf("\t\t--@libs7//config/clibs/link=shared\n"); */

    printf("  Flags (repeatable)\n");
    printf("\t-d, --debug\t\tEnable plugin debug flags.\n");
    printf("\t    --libs7-debug\tEnable libs7 debug flags.\n");
    /* printf("\t--debug-config\t\tEnable all config debugging flags.\n"); */
    /* printf("\t--debug-scm\t\tEnable all scheme debugging flags.\n"); */
    printf("\t-t, --trace\t\tEnable plugin trace flags.\n");
    printf("\t    --libs7-trace\tEnable libs7 trace flags.\n");
    printf("\t-v, --verbose\t\tEnable verbosity. Repeatable.\n");

}

enum OPTS {
    FLAG_HELP,
#if defined(PROFILE_fastbuild)
    FLAG_DEBUG,
    FLAG_DEBUG_LIBS7,
    FLAG_DEBUG_CONFIG,
    FLAG_DEBUG_SCM,
    FLAG_DEBUG_SCM_LOADS,
    FLAG_TRACE,
    FLAG_TRACE_LIBS7,
#endif
    FLAG_SHOW_CONFIG,
    FLAG_VERBOSE,

    LAST
};

static struct option options[] = {
    /* 0 */
#if defined(PROFILE_fastbuild)
    [FLAG_DEBUG] = {.long_name="debug",
                    .short_name='d',
                    .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_DEBUG_LIBS7] = {.long_name="libs7-debug",
                    .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_DEBUG_CONFIG] = {.long_name="debug-config",
                           .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_SCM] = {.long_name="debug-scm", .short_name = 'D',
                        .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_SCM_LOADS] = {.long_name="debug-scm-loads",
                              .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_TRACE] = {.long_name="trace",.short_name='t',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_TRACE_LIBS7] = {.long_name="libs7-trace",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
#endif
    [FLAG_SHOW_CONFIG] = {.long_name="show-config",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERBOSE] = {.long_name="verbose",.short_name='v',
                      .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_HELP] = {.long_name="help",.short_name='h',
                   .flags=GOPT_ARGUMENT_FORBIDDEN},
    [LAST] = {.flags = GOPT_LAST}
};

static void set_options(char *test, struct option options[])
{
    /* log_trace("set_options"); */
    if (options[FLAG_HELP].count) {
        print_plugin_usage(test);
        exit(EXIT_SUCCESS);
    }
#if defined(PROFILE_fastbuild)
    if (options[FLAG_DEBUG].count) {
        s7plugin_debug = options[FLAG_DEBUG].count;
    }
    if (options[FLAG_DEBUG_LIBS7].count) {
        libs7_debug = options[FLAG_DEBUG_LIBS7].count;
    }
    if (options[FLAG_TRACE].count) {
        s7plugin_trace = true;
    }
    if (options[FLAG_TRACE_LIBS7].count) {
        libs7_trace = true;
    }
#endif
    if (options[FLAG_VERBOSE].count) {
        s7plugin_verbosity = options[FLAG_VERBOSE].count;
        if (s7plugin_verbosity > 1) {
            libs7_verbosity = s7plugin_verbosity;
        }
    }
}

void print_debug_env(void)
{
    log_debug("getcwd: %s", getcwd(NULL, 0));
    log_debug("getenv(PWD): %s", getenv("PWD"));

    // $HOME - not reliable, use getpwuid() instead
    log_debug("getenv(HOME): %s", getenv("HOME"));
    struct passwd* pwd = getpwuid(getuid());
    log_debug("pwd->pw_dir: %s", pwd->pw_dir);

    // LOCAL_REPO: null when run from 'home' repo, 'libs7' when run as external repo
    log_debug("LOCAL_REPO (macro): '%s'", LOCAL_REPO);

    // TEST_WORKSPACE: always the root ws
    log_debug("TEST_WORKSPACE: '%s'", getenv("TEST_WORKSPACE"));

    // BAZEL_TEST: should always be true when this is compiled as cc_test
    log_debug("BAZEL_TEST: '%s'", getenv("BAZEL_TEST"));

    // BUILD_WORK* vars: null under 'bazel test'
    log_debug("BUILD_WORKSPACE_DIRECTORY: %s", getenv("BUILD_WORKSPACE_DIRECTORY"));
    log_debug("BUILD_WORKING_DIRECTORY: %s", getenv("BUILD_WORKING_DIRECTORY"));

    // TEST_SRCDIR - required for cc_test
    log_debug("TEST_SRCDIR: %s", getenv("TEST_SRCDIR"));
    log_debug("BINDIR: %s", getenv("BINDIR"));

    /* RUNFILES_MANIFEST_FILE: null on macos. */
    log_debug("RUNFILES_MANIFEST_FILE: %s", getenv("RUNFILES_MANIFEST_FILE"));

    /* RUNFILES_MANIFEST_FILE: null on macos. */
    log_debug("RUNFILES_MANIFEST_ONLY: %s", getenv("RUNFILES_MANIFEST_ONLY"));

    /* RUNFILES_DIR: set on macos for both bazel test and bazel run. */
    log_debug("RUNFILES_DIR: %s", getenv("RUNFILES_DIR"));
}

s7_scheme *s7_plugin_initialize(char *test, int argc, char **argv)
{
    atexit(cleanup);

    if ( !getenv("BAZEL_TEST") ) {
        log_error("This test must be run in a Bazel environment: bazel test //path/to/test (or bazel run)" );
        exit(EXIT_FAILURE);
    }

    /* log_trace("WS: %s", getenv("TEST_WORKSPACE")); */
    /* log_debug("ARGV[0]: %s", argv[0]); */
    /* log_debug("CWD: %s", getcwd(NULL, 0)); */

    argc = gopt (argv, options);
    (void)argc;
    gopt_errors (argv[0], options);

    set_options(test, options);

#if defined(PROFILE_fastbuild)
    if (s7plugin_debug) print_debug_env();
#endif
    s7_scheme *s7 = libs7_init(argv[0]);

    /* libmustachios7_s7_init(s7); */
    return s7;
}
