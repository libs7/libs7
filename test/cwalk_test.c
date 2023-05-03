#include "gopt.h"
#include "log.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"

#include "common.h"

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

void test_cwalk(void) {

    /* cwk_path_get_basename("/my/path.txt", &basename, &length); */
    /* sexp_input = "((*libcwalk* 'cwk_path_get_basename) \"/my/path.txt\")"; */
    sexp_input = "(cwk:path_get_basename \"/my/path.txt\")";
    sexp_expected = "\"path.txt\"";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* cwk_path_normalize("/var/log/weird/////path/.././..///", result, sizeof(result)); */
    /* sexp_input = "((*libcwalk* 'cwk:path_normalize) \"/var/log/weird/////path/.././..///\")"; */
    sexp_input = "(cwk:path_normalize \"/var/log/weird/////path/.././..///\")";
    sexp_expected = "\"/var/log\"";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    sexp_input = "(cwk:path_normalize \"~/foo/bar/../\")";
    sexp_expected = "\"~/foo\"";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* char *test_file; */
    /* if (strncmp(getenv("TEST_WORKSPACE"), "libs7", 5) == 0) */
    /*     test_file = "test/test.c"; */
    /* else */
    /*     test_file = "external/libs7/test/s7_test.c"; */

    /* /\* sexp_input = "(libc:realpath \"" testfile "\")"); *\/ */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(libc:realpath \"%s\")", test_file); */
    /* /\* log_debug("cwd: %s", getcwd(NULL,0)); *\/ */
    /* /\* log_debug("test file: %s", utstring_body(sexp)); *\/ */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* char *rp = realpath(test_file, NULL); */
    /* /\* log_debug("expected: %s", rp); *\/ */
    /* /\* free(s); *\/ */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, rp))); */
    /* free(rp); */

    sexp_input = "(->canonical-path \"/var/log/weird/path/../..\")";
    sexp_expected = "\"/var/log/weird/\"";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
}

int main(int argc, char **argv)
{
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

    set_options("cwalk", options);

    if (debug)
        print_debug_env();

    s7 = libs7_init();

    load_clib(s7, "cwalk"); //, libcwalk_s7_init);

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

    RUN_TEST(test_cwalk);

    utstring_free(sexp);
    return UNITY_END();
}
