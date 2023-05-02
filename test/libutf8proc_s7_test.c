/*
  test dsl:action->mibl, destructure-dsl-string, parse-pct-var
 */

#include <libgen.h>
#include <pwd.h>
#include <stdlib.h>             /* putenv */
#include <unistd.h>             /* getcwd */
#include <uuid/uuid.h>
#include <sys/errno.h>
#include <sys/types.h>

#include "gopt.h"
#include "log.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"

#if ! defined(CLIBS_LINK_RUNTIME)
#include "libc_s7.h"
#include "libutf8proc_s7.h"
#endif

#include "libs7.h"

s7_scheme *s7;

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

void test_misc(void) {
    actual = s7_eval_c_string(s7, "(utf8:version)");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, "2.8.0")));

    actual = s7_eval_c_string(s7, "(utf8:unicode_version)");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, "15.0.0")));

    actual = s7_eval_c_string(s7, "(utf8:codepoint_valid 66)");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_t(s7)));

    actual = s7_eval_c_string(s7, "(utf8:codepoint_valid (char->integer #\\A))");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_t(s7)));

    actual = s7_eval_c_string(s7, "(utf8:category_string (char->integer #\\A))");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, "Lu")));

    actual = s7_eval_c_string(s7, "(utf8:codepoint_valid #x0643)"); // Arabic Kaf
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_t(s7)));

    actual = s7_eval_c_string(s7, "(utf8:category_string #x0643)");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, "Lo")));

    actual = s7_eval_c_string(s7, "((utf8:get_property #x0643) 'category)");
    sexp_expected = "utf8:UTF8PROC_CATEGORY_LO";
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    actual = s7_eval_c_string(s7, "((utf8:get_property #x0643) 'bidi_class)");
    sexp_expected = "utf8:UTF8PROC_BIDI_CLASS_AL";
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
}

void test_case(void) {
    actual = s7_eval_c_string(s7, "(utf8:tolower (char->integer #\\A))");
    expected = s7_eval_c_string(s7, "(char->integer #\\a)");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    // À == 0x00C0,  à == 0x00E0
    actual = s7_eval_c_string(s7, "(utf8:tolower #x00C0)");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_integer(s7, 0x00E0)));

    actual = s7_eval_c_string(s7, "(utf8:toupper (char->integer #\\a))");
    expected = s7_eval_c_string(s7, "(char->integer #\\A)");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    actual = s7_eval_c_string(s7, "(utf8:toupper #x00E0)"); // à
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_integer(s7, 0x00C0)));
}

void test_categories(void) {
    actual = s7_eval_c_string(s7, "(utf8:category_string (char->integer #\\A))");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, "Lu")));
}

void test_composition(void) {
    actual = s7_eval_c_string(s7, "(let* ((s \"xxx\") (r (utf8:decompose_char #x00C0 s 3))) s)"); // À
}

void test_encoding(void) {
    /* utf8:encode_char does not work beyond ascii */

    // convert 0x00C0 (À) to utf-8
    actual = s7_eval_c_string(s7, "(utf8:encode_char (char->integer #\\A))");
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, "2.8.0"))); */

    // convert 0x00C0 (À) to utf-8: c3 80
/*     actual = s7_eval_c_string(s7, "(utf8:encode_char #x00C0)"); */
/* s7_flush_output_port(s7, s7_current_output_port(s7)); */
/* s = s7_object_to_c_string(s7, actual); */
/* log_debug("result: %s", s); */
/* free(s); */

/*  actual = s7_eval_c_string(s7, "(utf8:encode_char  #x0643)"); // Arabic Kaf */
/* s7_flush_output_port(s7, s7_current_output_port(s7)); */
/* s = s7_object_to_c_string(s7, actual); */
/* log_debug("result: %s", s); */
/* free(s); */

}

void test_iterate(void) {
    actual = s7_eval_c_string(s7, "(utf8:iterate \"αβγ\")");
    sexp_expected = "'(#x03B1 2)"; // Greek α
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
}

void test_normalization(void) {
    // s7_make_c_pointer_with_type(sc, (void *)s7_string(s7_car(args)), s7_make_symbol(sc, \"utf8proc_uint8_t*\"), s7_f(sc)));

/*     actual = s7_eval_c_string(s7, "(utf8:NFD (string->c-pointer \"αβγ\"))"); */
/* s7_flush_output_port(s7, s7_current_output_port(s7)); */
/* char *s = s7_object_to_c_string(s7, actual); */
/* log_debug("result: %s", s); */
/* free(s); */

    /* sexp_expected = "'(#x03B1 2)"; // Greek α */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */
}

void test_properties(void) {
    actual = s7_eval_c_string(s7, "((utf8:get_property #x0643) 'category)");
    sexp_expected = "utf8:UTF8PROC_CATEGORY_LO";
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    actual = s7_eval_c_string(s7, "((utf8:get_property #x0643) 'bidi_class)");
    sexp_expected = "utf8:UTF8PROC_BIDI_CLASS_AL";
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
}

void _print_usage(void) {
    printf("Usage:\t$ bazel test <tgt> [flags, options]\n");
    printf("Flags\n");
    printf("\t-d, --debug\t\tEnable all debugging flags.\n");
    printf("\t--debug-config\t\tEnable all config debugging flags.\n");
    printf("\t--debug-scm\t\tEnable all scheme debugging flags.\n");
    printf("\t-t, --trace\t\tEnable trace flags.\n");
    printf("\t-v, --verbose\t\tEnable verbosity. Repeatable.\n");

    printf("Options:\n");
    /* printf("\t-D | -log <arg>\t\tLog <arg> (parsetree, mibl, or starlark}) to stdout.\n"); */

    printf("\t-r, --root <arg>"
           "\tStart traversal at <arg> (path relative to cwd).\n");
    printf("\t-p, --pkg <arg>"
           "\t\tProcess only <arg> (relative root path).\n");
}

enum OPTS {
    OPT_ROOT = 0,
    OPT_PKG,
    OPT_PACKAGE,

    FLAG_HELP,
    FLAG_DEBUG,
    FLAG_DEBUG_CONFIG,
    FLAG_DEBUG_MIBLRC,
    FLAG_DEBUG_MIBL_CRAWL,
    FLAG_DEBUG_SCM,
    FLAG_DEBUG_SCM_LOADS,

    FLAG_EMIT_PARSETREE,        /* config load_project to emit PARSETREE.mibl */

    FLAG_SHOW_CONFIG,
    FLAG_TRACE,
    FLAG_VERBOSE,

    LAST
};

static struct option options[] = {
    [FLAG_DEBUG] = {.long_name="debug",.short_name='d',
                    .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_DEBUG_CONFIG] = {.long_name="debug-config",
                           .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_MIBLRC] = {.long_name="debug-miblrc",
                           .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_MIBL_CRAWL] = {.long_name="debug-mibl-crawl",
                               .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_SCM] = {.long_name="debug-scm", .short_name = 'D',
                        .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_SCM_LOADS] = {.long_name="debug-scm-loads",
                              .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_PARSETREE] = {.long_name="emit-parsetree",
                              .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_CONFIG] = {.long_name="show-config",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_TRACE] = {.long_name="trace",.short_name='t',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERBOSE] = {.long_name="verbose",.short_name='v',
                      .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_HELP] = {.long_name="help",.short_name='h',
                   .flags=GOPT_ARGUMENT_FORBIDDEN},
    [LAST] = {.flags = GOPT_LAST}
};

void _set_options(struct option options[])
{
    if (options[FLAG_HELP].count) {
        _print_usage();
        exit(EXIT_SUCCESS);
    }

    if (options[FLAG_DEBUG].count) {
        debug = true;
    }
    if (options[FLAG_VERBOSE].count) {
        log_info("verbose ct: %d", options[FLAG_VERBOSE].count);
        verbose = true;
    }
}

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

    _set_options(options);

    if (debug)
        _print_debug_env();

    s7 = libs7_init();

#if defined(CLIBS_LINK_RUNTIME)
    clib_dload_ns(s7, "libc_s7", "libc", DSO_EXT);
    clib_dload_global(s7, "libcwalk_s7", "libcwalk.scm", DSO_EXT);
#else  /* link:static? or link:shared? */
    clib_sinit(s7, libc_s7_init, "libc");
    clib_sinit(s7, libutf8proc_s7_init, "libutf8proc");
#endif

    /* log_debug("INITIALIZED"); */

    char *script_dir = "./test";
    s7_pointer newpath;
    newpath =  s7_add_to_load_path(s7, script_dir);
    (void)newpath;

    /* debugging: */
    /* s7_pointer loadpath = s7_load_path(s7); */
    /* char *s = s7_object_to_c_string(s7, loadpath); */
    /* printf("load path: %s\n", s); */
    /* free(s); */

    utstring_new(sexp);

    UNITY_BEGIN();

    RUN_TEST(test_misc);
    RUN_TEST(test_case);
    RUN_TEST(test_categories);
    RUN_TEST(test_composition);
    RUN_TEST(test_encoding);
    RUN_TEST(test_iterate);
    RUN_TEST(test_normalization);
    RUN_TEST(test_properties);

    /* utstring_free(sexp); */
    return UNITY_END();
}
