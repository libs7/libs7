#include <libgen.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <uuid/uuid.h>

#include "utstring.h"
#include "log.h"
#include "s7.h"

bool verbose;

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

int main(int argc, char **argv)
{
    /* log_info("running clibgen"); */
    /* log_debug("CWD: %s", getcwd(NULL,0)); */
    /* log_debug("BUILD WS root: %s", getenv("BUILD_WORKSPACE_DIRECTORY")); */
    /* if (debug) */
    _print_debug_env();

    log_debug("argv[0]: %s", argv[0]);
    log_debug("argv[1]: %s", argv[1]); /* the libfoo_clibgen.scm file */
    log_debug("argv[2]: %s", argv[2]); /* Bazel $GENDIR -- FIXME: use --outdir */

    s7_scheme *s7 = s7_init();
    if (verbose)
        log_info("s7: %s", S7_DATE);

    if (argc > 1) {
        /* log_debug("argv[0]: %s", argv[0]); */
        /* log_debug("argv[1]: %s", argv[1]); */
        size_t len = strlen(argv[1]);
        char *script = strndup(argv[1], len);
        // put clibgen.scm on *load-path*
        if (strlen(BAZEL_CURRENT_REPOSITORY) == 0)
            s7_add_to_load_path(s7, "lib"); //FIXME: hardcoded path
        else
            s7_add_to_load_path(s7, "external/libs7/lib"); //FIXME
        /* s7_add_to_load_path(s7, dirname(argv[1])); */
        s7_add_to_load_path(s7, "lib/libc");
        /* log_debug("script: %s", script); */

        s7_pointer lp = s7_load_path(s7);
        char *s = s7_object_to_c_string(s7, lp);
        log_debug("load-path: %s", s);
        free(s);

        UT_string *pfx;
        utstring_new(pfx);
        utstring_printf(pfx, "%s/lib/libc", argv[2]);

        s7_define_variable(s7, "*cload-directory*", s7_make_string(s7, utstring_body(pfx)));

        utstring_free(pfx);

        if (verbose)
            log_info("loading %s", script);
        if (!s7_load(s7, script)) {
            fprintf(stderr, "can't load %s\n", argv[1]);
            return(2);
        } else {
            free(script);
        }
    } else {
        log_error("arg required");
    }
    return(0);
}

