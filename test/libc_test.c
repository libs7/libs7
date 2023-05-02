#include <libgen.h>
#include <pwd.h>
#include <stdlib.h>             /* putenv */
#include <unistd.h>             /* getcwd */
#include <sys/errno.h>
#include <sys/types.h>

#include "gopt.h"
#include "log.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"

#include "common.h"
#if ! defined(CLIBS_LINK_RUNTIME)
#include "libc_s7.h"
#include "libdl_s7.h"
#include "libgdbm_s7.h"
#include "libm_s7.h"
/* #include "libutf8proc_s7.h" */
#endif

#include "libs7.h"

s7_scheme *s7;

extern struct option options[];

char *sexp_input;
char *sexp_expected;

UT_string *setter;
UT_string *sexp;
s7_pointer actual;
s7_pointer expected;

bool verbose;
bool debug;

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

/* s7_flush_output_port(s7, s7_current_output_port(s7)); */
/* char *s = s7_object_to_c_string(s7, actual); */
/* log_debug("result: %s", s); */
/* free(s); */

void test_libc(void) {

    /* sexp_input = "((*libc* 'libc:fnmatch) \"*.c\" \"s7.c\" (*libc* 'libc:FNM_PATHNAME))"; */
    sexp_input = "(libc:fnmatch \"*.c\" \"s7.c\" libc:FNM_PATHNAME))";
    sexp_expected = "0";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
}

void test_wordexp(void) {
    sexp_input = ""
        "(let ((w (libc:wordexp.make))) "
        " (libc:wordexp \"~/foo/bar\" w libc:WRDE_NOCMD) "
        " (car (libc:wordexp.we_wordv w)))"
        ;
        /* ")) #t)))" */
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    struct passwd* pwd = getpwuid(getuid());
    char *h =  pwd->pw_dir;
    /* free(pwd); */
    /* log_debug("HOME: %s", h); */
    utstring_renew(sexp);
    utstring_printf(sexp, "%s/foo/bar", h);
    /* log_debug("exp: %s", utstring_body(sexp)); */
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, utstring_body(sexp))));
}

void test_gdbm(void) {
    s7_add_to_load_path(s7, "../libs7/scm");
    s7_load(s7, "string.scm");
    /* sexp_input = "(gdbm:version)"; */
    sexp_input = "(caddr (string-split (gdbm:version) #\\space))";
    // "GDBM version 1.23. 04/02/2022 (built Apr 30 2023 20:17:04)";
    sexp_expected = "1.23.";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, sexp_expected)));

    // gdbf = gdbm_open (argv[1], 0, GDBM_READER, 0, NULL);
    // (gdbm:open filename size flags mode func)

    sexp_input = ""
        "(let ((gfile (gdbm:open \"test.gdbm\" 1024 gdbm:GDBM_NEWDB #o664 "
        "                        (lambda (str) (format *stderr* \"str: ~S~%\" str))))) "
        " (gdbm:store gfile \"1\" \"1234\" gdbm:GDBM_REPLACE) "
	" (gdbm:fetch gfile \"1\") "
        " (gdbm:close gfile)) "
        ;
    sexp_expected = "#<unspecified>";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    /* s7_flush_output_port(s7, s7_current_output_port(s7)); */
    /* char *s = s7_object_to_c_string(s7, actual); */
    /* log_debug("result: %s", s); */
    /* free(s); */
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_unspecified(s7)));  // make_string(s7, sexp_expected)));
}

void test_math(void) {
    sexp_input = "(+ 2 3)";
    sexp_expected = "5";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
}

void test_libm(void) {
    sexp_input = "(libm:pow 2 3)";
    sexp_expected = "8.0";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    sexp_input = "(libm:ceil 2.3)";
    sexp_expected = "3.0";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    sexp_input = "(libm:floor 2.3)";
    sexp_expected = "2.0";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    sexp_input = "(libm:fmod 7 2)";
    sexp_expected = "1.0";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    sexp_input = "(libm:fmod 7 3)";
    sexp_expected = "1.0";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
}

void test_regex(void) {
    sexp_input = ""
        "(let* ((rg (libc:regex.make)) "
        "       (_ (libc:regcomp rg \"a.b\" 0))) "
        "   (let ((res (libc:regexec rg \"acb\" 0 0))) "
        "      (libc:regfree rg) "
        "      res))"
        ;
    sexp_expected = "0";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* capture - 1st match is whole thing, 2nd is subexpr */
#   define regex "\"a([0-9]+)b\""
    sexp_input = ""
        "(let* ((s \"a123b\") "
        "       (rg (libc:regex.make)) "
        "       (_ (libc:regcomp rg " regex " libc:REG_EXTENDED)) "
        "       (nmatch 2)) "
        "   (let* ((res (libc:regexec rg s nmatch 0)) "
        "          (match-so (int-vector-ref res 2)) "
        "          (match-eo (int-vector-ref res 3)) "
        "          (match (substring s match-so match-eo))) "
        "      (libc:regfree rg) "
        "      match))"
        ;
    sexp_expected = "\"123\"";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* from s7test.scm */
    /* not on macos: */
    /* sexp_input = "" */
    /*     "(let* ((rg (regex.make)) " */
    /*     "       (_ (regcomp rg \"colou\\?r\" 0))) " */
    /*     "   (let ((res (regexec rg \"The color green\" 1 0))) " */
    /*     "      (regfree rg) " */
    /*     "      res))" */
    /*     ; */
    /* sexp_expected = "#i(4 9)"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "%s", sexp_input); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */

#   define regex2 "\"(ba(na)*s |nefer(ti)* )*\""
    sexp_input = ""
        "(let* ((s \"bananas nefertiti\") "
        "       (rg (libc:regex.make)) "
        "       (_ (libc:regcomp rg " regex2 " libc:REG_EXTENDED)) "
        "       (nmatch 3)) "
        "   (let ((res (libc:regexec rg s nmatch 0))) "
        "      (libc:regfree rg) "
        "      res))"
        ;
    sexp_expected = "#i(0 8 0 8 4 6)";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
}

/* void _print_usage(void) { */
/*     printf("Usage:\t$ bazel test <tgt> [flags, options]\n"); */
/*     printf("Flags\n"); */
/*     printf("\t-d, --debug\t\tEnable all debugging flags.\n"); */
/*     printf("\t--debug-config\t\tEnable all config debugging flags.\n"); */
/*     printf("\t--debug-scm\t\tEnable all scheme debugging flags.\n"); */
/*     printf("\t-t, --trace\t\tEnable trace flags.\n"); */
/*     printf("\t-v, --verbose\t\tEnable verbosity. Repeatable.\n"); */

/*     printf("Options:\n"); */
/*     /\* printf("\t-D | -log <arg>\t\tLog <arg> (parsetree, mibl, or starlark}) to stdout.\n"); *\/ */

/*     printf("\t-r, --root <arg>" */
/*            "\tStart traversal at <arg> (path relative to cwd).\n"); */
/*     printf("\t-p, --pkg <arg>" */
/*            "\t\tProcess only <arg> (relative root path).\n"); */
/* } */

/* enum OPTS { */
/*     OPT_ROOT = 0, */
/*     OPT_PKG, */
/*     OPT_PACKAGE, */

/*     FLAG_HELP, */
/*     FLAG_DEBUG, */
/*     FLAG_DEBUG_CONFIG, */
/*     FLAG_DEBUG_MIBLRC, */
/*     FLAG_DEBUG_MIBL_CRAWL, */
/*     FLAG_DEBUG_SCM, */
/*     FLAG_DEBUG_SCM_LOADS, */

/*     FLAG_EMIT_PARSETREE,        /\* config load_project to emit PARSETREE.mibl *\/ */

/*     FLAG_SHOW_CONFIG, */
/*     FLAG_TRACE, */
/*     FLAG_VERBOSE, */

/*     LAST */
/* }; */

/* static struct option options[] = { */
/*     /\* 0 *\/ */
/*     [OPT_ROOT] = {.long_name="root",.short_name='r', */
/*                   .flags=GOPT_ARGUMENT_REQUIRED}, */
/*     [OPT_PKG] = {.long_name="pkg",.short_name='p', */
/*                  .flags=GOPT_ARGUMENT_REQUIRED */
/*     }, */
/*     [FLAG_DEBUG] = {.long_name="debug",.short_name='d', */
/*                     .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE}, */
/*     [FLAG_DEBUG_CONFIG] = {.long_name="debug-config", */
/*                            .flags=GOPT_ARGUMENT_FORBIDDEN}, */
/*     [FLAG_DEBUG_MIBLRC] = {.long_name="debug-miblrc", */
/*                            .flags=GOPT_ARGUMENT_FORBIDDEN}, */
/*     [FLAG_DEBUG_MIBL_CRAWL] = {.long_name="debug-mibl-crawl", */
/*                                .flags=GOPT_ARGUMENT_FORBIDDEN}, */
/*     [FLAG_DEBUG_SCM] = {.long_name="debug-scm", .short_name = 'D', */
/*                         .flags=GOPT_ARGUMENT_FORBIDDEN}, */
/*     [FLAG_DEBUG_SCM_LOADS] = {.long_name="debug-scm-loads", */
/*                               .flags=GOPT_ARGUMENT_FORBIDDEN}, */
/*     [FLAG_EMIT_PARSETREE] = {.long_name="emit-parsetree", */
/*                               .flags=GOPT_ARGUMENT_FORBIDDEN}, */
/*     [FLAG_SHOW_CONFIG] = {.long_name="show-config", */
/*                           .flags=GOPT_ARGUMENT_FORBIDDEN}, */
/*     [FLAG_TRACE] = {.long_name="trace",.short_name='t', */
/*                     .flags=GOPT_ARGUMENT_FORBIDDEN}, */
/*     [FLAG_VERBOSE] = {.long_name="verbose",.short_name='v', */
/*                       .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE}, */
/*     [FLAG_HELP] = {.long_name="help",.short_name='h', */
/*                    .flags=GOPT_ARGUMENT_FORBIDDEN}, */
/*     [LAST] = {.flags = GOPT_LAST} */
/* }; */

/* void _set_options(struct option options[]) */
/* { */
/*     if (options[FLAG_HELP].count) { */
/*         _print_usage(); */
/*         exit(EXIT_SUCCESS); */
/*     } */

/*     if (options[FLAG_DEBUG].count) { */
/*         debug = true; */
/*     } */
/*     if (options[FLAG_VERBOSE].count) { */
/*         log_info("verbose ct: %d", options[FLAG_VERBOSE].count); */
/*         verbose = true; */
/*     } */
/* } */

/* to print env vars:
 bazel run <tgt> -- -d
 bazel test <tgt> --test_arg=-d --test_output=all (or --config=show)
*/
/* static void _print_debug_env(void) */
/* { */
/*     log_debug("getcwd: %s", getcwd(NULL, 0)); */
/*     log_debug("getenv(PWD): %s", getenv("PWD")); */

/*     // $HOME - not reliable, use getpwuid() instead */
/*     log_debug("getenv(HOME): %s", getenv("HOME")); */
/*     struct passwd* pwd = getpwuid(getuid()); */
/*     log_debug("pwd->pw_dir: %s", pwd->pw_dir); */

/*     // BAZEL_CURRENT_REPOSITORY: null when run from 'home' repo, 'libs7' when run as external repo */
/*     log_debug("BAZEL_CURRENT_REPOSITORY (macro): '%s'", BAZEL_CURRENT_REPOSITORY); */

/*     // TEST_WORKSPACE: always the root ws */
/*     log_debug("TEST_WORKSPACE: '%s'", getenv("TEST_WORKSPACE")); */

/*     // BAZEL_TEST: should always be true when this is compiled as cc_test */
/*     log_debug("BAZEL_TEST: '%s'", getenv("BAZEL_TEST")); */

/*     // BUILD_WORK* vars: null under 'bazel test' */
/*     log_debug("BUILD_WORKSPACE_DIRECTORY: %s", getenv("BUILD_WORKSPACE_DIRECTORY")); */
/*     log_debug("BUILD_WORKING_DIRECTORY: %s", getenv("BUILD_WORKING_DIRECTORY")); */

/*     // TEST_SRCDIR - required for cc_test */
/*     log_debug("TEST_SRCDIR: %s", getenv("TEST_SRCDIR")); */
/*     log_debug("BINDIR: %s", getenv("BINDIR")); */

/*     /\* RUNFILES_MANIFEST_FILE: null on macos. *\/ */
/*     log_debug("RUNFILES_MANIFEST_FILE: %s", getenv("RUNFILES_MANIFEST_FILE")); */

/*     /\* RUNFILES_MANIFEST_FILE: null on macos. *\/ */
/*     log_debug("RUNFILES_MANIFEST_ONLY: %s", getenv("RUNFILES_MANIFEST_ONLY")); */

/*     /\* RUNFILES_DIR: set on macos for both bazel test and bazel run. *\/ */
/*     log_debug("RUNFILES_DIR: %s", getenv("RUNFILES_DIR")); */
/* } */

/* s7_scheme *libs7_init(void);    /\* libs7.h *\/ */

int main(int argc, char **argv)
{
    //FIXME: throw error if run outside of bazel
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

    set_options("libc", options);

    if (debug)
        print_debug_env();

    s7 = libs7_init();

#if defined(CLIBS_LINK_RUNTIME)
    clib_dload_ns(s7, "libc_s7", "libc", DSO_EXT);
    clib_dload_ns(s7, "libdl_s7", "libdl", DSO_EXT);
    clib_dload_ns(s7, "libgdbm_s7", "libgdbm", DSO_EXT);
    clib_dload_ns(s7, "libm_s7", "libm", DSO_EXT);
    /* clib_dload_ns(s7, "libutf8proc_s7", "libutf8proc", DSO_EXT); */
#else  /* link:static? or link:shared? */
    clib_sinit(s7, libc_s7_init, "libc");
    clib_sinit(s7, libdl_s7_init, "libdl");
    clib_sinit(s7, libgdbm_s7_init, "libgdbm");
    clib_sinit(s7, libm_s7_init, "libm");
    /* clib_sinit(s7, libutf8proc_s7_init, "libutf8proc"); */
#endif

    /* log_debug("INITIALIZED"); */

    char *script_dir = "./test";
    s7_pointer newpath;
    newpath =  s7_add_to_load_path(s7, script_dir);
    (void)newpath;

    /* debugging: */
    /* s7_pointer loadpath = s7_load_path(s7); */
    /* char *s = s7_object_to_c_string(s7, loadpath); */
    /* log_debug("load path: %s", s); */
    /* free(s); */

    utstring_new(sexp);

    UNITY_BEGIN();

    RUN_TEST(test_libc);
    RUN_TEST(test_wordexp);
    RUN_TEST(test_gdbm);
    RUN_TEST(test_math);
    RUN_TEST(test_libm);
    RUN_TEST(test_regex);
    /* RUN_TEST(test_utf8proc); */

    /* utstring_free(sexp); */
    return UNITY_END();
}
