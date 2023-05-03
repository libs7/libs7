#include <libgen.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

/* mac: getpwuuid, getpwuuid_r, which we do not use */
/* #include <uuid/uuid.h> */

#include "gopt.h"
#include "log.h"
#include "utstring.h"

#include "s7.h"

extern bool libs7_debug;
extern bool libs7_trace;
bool verbose = false;
bool quiet   = false;

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

void _print_version(void) {
    printf("FIXME: version id\n");
}

void _print_usage(void) {
    printf("Usage:\tclibgen -s <script> -p <prefix>\n");

    printf("Required options:\n");
    printf("\t-s, --script\t\tPath of script file.\n");
    printf("\t-p, --prefix\t\tPrefix for output path.\n");

    printf("Optional flags:\n");
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
    OPT_GENDIR,
    OPT_SCRIPT,
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
    [OPT_GENDIR] = {.long_name="gendir",.short_name='g',
                    .flags=GOPT_ARGUMENT_REQUIRED | GOPT_ARGUMENT_NO_HYPHEN},
    [OPT_SCRIPT] = {.long_name="script",.short_name='s',
                    .flags=GOPT_ARGUMENT_REQUIRED | GOPT_ARGUMENT_NO_HYPHEN},
    [LAST] = {.flags = GOPT_LAST}
};

void _set_options(struct option options[])
{
    if (options[FLAG_HELP].count) {
        _print_usage();
        exit(EXIT_SUCCESS);
    }

    if (options[FLAG_VERSION].count) {
        _print_version();
        exit(EXIT_SUCCESS);
    }

    if ( ! options[OPT_SCRIPT].count) {
        _print_usage();
        exit(EXIT_FAILURE);
    }

    if ( ! options[OPT_GENDIR].count) {
        _print_usage();
        exit(EXIT_FAILURE);
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

int main(int argc, char **argv)
{
    /* log_info("running clibgen"); */
    /* log_debug("CWD: %s", getcwd(NULL,0)); */
    /* log_debug("BUILD WS root: %s", getenv("BUILD_WORKSPACE_DIRECTORY")); */

    int gopt_argc = gopt(argv, options);
    (void)gopt_argc;

    gopt_errors(argv[0], options);

    _set_options(options);

    if (libs7_debug)
        _print_debug_env();

    /* log_debug("argv[0]: %s", argv[0]); */
    /* log_debug("argv[1]: %s", argv[1]); /\* the libfoo_clibgen.scm file *\/ */
    /* log_debug("argv[2]: %s", argv[2]); /\* Bazel $GENDIR -- FIXME: use --outdir *\/ */

    s7_scheme *s7 = s7_init();
    if (verbose)
        log_info("s7: %s", S7_DATE);

    /* log_debug("RUNFILES_MANIFEST_FILE: %s", getenv("RUNFILES_MANIFEST_FILE")); */

    /* /\* RUNFILES_MANIFEST_FILE: null on macos. *\/ */
    /* log_debug("RUNFILES_MANIFEST_ONLY: %s", getenv("RUNFILES_MANIFEST_ONLY")); */

    /* /\* RUNFILES_DIR: set on macos for both bazel test and bazel run. *\/ */
    /* log_debug("RUNFILES_DIR: %s", getenv("RUNFILES_DIR")); */

    if (argc > 1) {
        /* log_debug("argv[0]: %s", argv[0]); */
        /* log_debug("argv[1]: %s", argv[1]); */
        /* size_t len = strlen(argv[1]); */
        char *script = strdup(options[OPT_SCRIPT].argument);
        /* log_debug("script: %s", script); */
        /* char *script     = argv[1]; */
        char *script_dir = dirname(options[OPT_SCRIPT].argument);
        /* log_debug("script_dir: %s", script_dir); */

        /* log_debug("CWD: %s", getcwd(NULL,0)); */
        char *scmdir = realpath("../libs7/scm", NULL);
        s7_add_to_load_path(s7, scmdir);
        /* s7_pointer lp = s7_load_path(s7); */
        /* char *s = s7_object_to_c_string(s7, lp); */
        /* log_debug("LOAD-PATH: %s", s); */
        /* free(s); */
        if (!s7_load(s7, "string.scm")) {
            fprintf(stderr, "can't load string.scm\n");
        }

        /* (format #t "lp: ~A~%" *load-path*) */
        /*     (load "string.scm") ;; FIXME: use require */


        // deal with bazel context
        char *cload_dir_format;
        if (strlen(BAZEL_CURRENT_REPOSITORY) == 0) {
            cload_dir_format = "%s/%s";
            s7_add_to_load_path(s7, "lib"); //FIXME: hardcoded path
        } else {
            cload_dir_format = "%s/external/libs7/%s";
            s7_add_to_load_path(s7, "external/libs7");
            s7_add_to_load_path(s7, "external/libs7/lib");
        }
        /* s7_pointer lp = s7_load_path(s7); */
        /* char *s = s7_object_to_c_string(s7, lp); */
        /* log_debug("LOAD-PATH: %s", s); */
        /* free(s); */

        /* log_debug("script: %s", script); */

        /* s7_pointer lp = s7_load_path(s7); */
        /* char *s = s7_object_to_c_string(s7, lp); */
        /* log_debug("load-path: %s", s); */
        /* free(s); */

        /* log_debug("GENDIR: %s", options[OPT_GENDIR].argument); */

        UT_string *cload_dir;
        utstring_new(cload_dir);
        utstring_printf(cload_dir,
                        cload_dir_format,
                        options[OPT_GENDIR].argument,
                        /* argv[2], */
                        script_dir);

        s7_define_variable(s7, "*cload-directory*", s7_make_string(s7, utstring_body(cload_dir)));
        /* log_info("*cload-directory*: %s", utstring_body(cload_dir)); */

        utstring_free(cload_dir);

        /* if (!s7_load(s7, "clibgen.scm")) { */
        /*     fprintf(stderr, "can't load clibgen.scm\n"); */
        /*     return(2); */
        /* } */

        if (verbose)
            log_info("loading %s", script);

        if (!s7_load(s7, script)) {
            fprintf(stderr, "can't load %s\n", script);
            return(2);
        } else {
            /* log_debug("loaded script"); */
            /* free(script); */
            return(EXIT_SUCCESS);
       }
    } else {
        log_error("arg required");
    }
    return(EXIT_SUCCESS);
}

