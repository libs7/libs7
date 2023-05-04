#include "gopt.h"
#include "log.h"
#include "unity.h"
#include "utstring.h"

#include "common.h"

#include "libs7.h"

s7_scheme *s7;

extern struct option options[];

char *sexp_input;
char *sexp_expected;

/* UT_string *setter; */
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

    actual = s7_eval_c_string(s7, "(utf8:unicode-version)");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, "15.0.0")));

    actual = s7_eval_c_string(s7, "(utf8:codepoint-valid 66)");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_t(s7)));

    actual = s7_eval_c_string(s7, "(utf8:codepoint-valid (char->integer #\\A))");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_t(s7)));

    actual = s7_eval_c_string(s7, "(utf8:category-string (char->integer #\\A))");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, "Lu")));

    actual = s7_eval_c_string(s7, "(utf8:codepoint-valid #x0643)"); // Arabic Kaf
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_t(s7)));

    actual = s7_eval_c_string(s7, "(utf8:category-string #x0643)");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, "Lo")));

    actual = s7_eval_c_string(s7, "((utf8:get-property #x0643) 'category)");
    sexp_expected = "utf8:UTF8PROC_CATEGORY_LO";
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    actual = s7_eval_c_string(s7, "((utf8:get-property #x0643) 'bidi_class)");
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
    actual = s7_eval_c_string(s7, "(utf8:category-string (char->integer #\\A))");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, "Lu")));
}

void test_composition(void) {
    actual = s7_eval_c_string(s7, "(let* ((s \"xxx\") (r (utf8:decompose-char #x00C0 s 3))) s)"); // À
}

void test_encoding(void) {
    /* utf8:encode_char does not work beyond ascii */

    // convert 0x00C0 (À) to utf-8
    actual = s7_eval_c_string(s7, "(utf8:encode-char (char->integer #\\A))");
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, "2.8.0"))); */

    // convert 0x00C0 (À) to utf-8: c3 80
/*     actual = s7_eval_c_string(s7, "(utf8:encode-char #x00C0)"); */
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
    actual = s7_eval_c_string(s7, "((utf8:get-property #x0643) 'category)");
    sexp_expected = "utf8:UTF8PROC_CATEGORY_LO";
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    actual = s7_eval_c_string(s7, "((utf8:get-property #x0643) 'bidi_class)");
    sexp_expected = "utf8:UTF8PROC_BIDI_CLASS_AL";
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
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

    set_options("utf8proc", options);

    if (debug)
        print_debug_env();

    s7 = libs7_init();

    libs7_load_clib(s7, "utf8proc");

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

    utstring_free(sexp);
    return UNITY_END();
}
