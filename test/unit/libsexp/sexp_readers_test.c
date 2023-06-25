#include "config.h"
#include "unity.h"
#include "common.h"
#include "libs7.h"

s7_scheme *s7;

extern struct option options[];

s7_pointer sexp_read;
s7_pointer with_input_from_string;
s7_pointer call_with_input_string;
s7_pointer with_input_from_file;
s7_pointer call_with_input_file;

char *test_sexp_str = "(alias (name \"mwe\")) (alias (name \"foo\")) (alias (name \"bar\"))";
s7_pointer test_sexp_str7;
s7_pointer test_sexp_arg;

char *test_sexp_expected_str = "'((alias (name \"mwe\")) (alias (name \"foo\")) (alias (name \"bar\")))";
s7_pointer test_sexp_expected;

char *sexp_str;

s7_pointer flag, t, k, a, idx, res, actual, expected;
s7_pointer len, m;

bool flag_bool;
bool verbose;
bool debug;

char *cmd;

void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

void read_string(void) {
    actual = s7_call(s7, sexp_read, test_sexp_arg);
    TRACE_S7_DUMP("actual", actual);
    flag = APPLY_1("alist?", actual);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    flag = APPLY_2("equal?", actual, test_sexp_expected);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
}

/* (open-input-string s) */
void read_string_port(void) {
    s7_pointer inport = s7_open_input_string(s7, test_sexp_str);
    flag_bool = s7_is_input_port(s7, inport);
    TEST_ASSERT_TRUE(flag_bool);

    t = s7_call(s7, sexp_read, s7_list(s7, 1, inport));
    /* TRACE_S7_DUMP("read_string_port", t); */
    flag = APPLY_1("alist?", t);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
}

void with_input_from_string_test(void) {
    s7_pointer readlet
        = s7_inlet(s7,
                   s7_list(s7, 1,
                           s7_cons(s7,
                                   s7_make_symbol(s7, "sexp"),
                                   test_sexp_str7)
                           ));

    cmd = "(with-input-from-string sexp sexp:read)";
    actual = s7_eval_c_string_with_environment(s7, cmd, readlet);
    /* TRACE_S7_DUMP("actual", actual); */
    flag = APPLY_1("alist?", actual);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    /* TRACE_S7_DUMP("expected", test_sexp_expected); */
    flag = APPLY_2("equal?", actual, test_sexp_expected);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
}

void call_with_input_string_test(void) {
    s7_pointer readlet
        = s7_inlet(s7,
                   s7_list(s7, 1,
                           s7_cons(s7,
                                   s7_make_symbol(s7, "sexp"),
                                   test_sexp_str7)
                           ));

    cmd = "(call-with-input-string sexp sexp:read)";
    actual = s7_eval_c_string_with_environment(s7, cmd, readlet);
    TRACE_S7_DUMP("call-with-input-string", actual);
    res = APPLY_1("alist?", actual);
    TEST_ASSERT_TRUE(s7_boolean(s7, res));
}

void read_file_port(void) {
    s7_pointer inport = s7_open_input_file(s7, "test/data/sexp.scm", "r");
    flag_bool = s7_is_input_port(s7, inport);
    TEST_ASSERT_TRUE(flag_bool);
    t = s7_apply_function(s7, sexp_read, s7_list(s7, 1, inport));
    actual = APPLY_1("alist?", t);
    TEST_ASSERT_TRUE(s7_boolean(s7, actual));
}

void with_input_from_file_test(void) {
    s7_pointer fname7 = s7_make_string(s7, "test/data/sexp.scm");
    s7_pointer readlet
        = s7_inlet(s7,
                   s7_list(s7, 1,
                           s7_cons(s7,
                                   s7_make_symbol(s7, "fname"),
                                   fname7)));

    cmd = "(with-input-from-file fname sexp:read)";
    actual = s7_eval_c_string_with_environment(s7, cmd, readlet);
    TRACE_S7_DUMP("actual", actual);
    TRACE_S7_DUMP("expected", test_sexp_expected);
    flag = APPLY_2("equal?", actual, test_sexp_expected);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    flag = APPLY_1("alist?", actual);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
}

void baddot_test(void) {
    s7_pointer datafile7 = s7_make_string(s7,
                          "test/data/scm/baddot/case010/dune");
    s7_pointer expected7 = s7_make_string(s7,
                          "test/data/scm/baddot/case010/sexp.expected");
    s7_pointer readlet
        = s7_inlet(s7,
                   s7_list(s7, 2,
                           s7_cons(s7,
                                   s7_make_symbol(s7, "datafile"),
                                   datafile7),
                           s7_cons(s7,
                                   s7_make_symbol(s7, "expected"),
                                   expected7)));

    cmd = "(with-input-from-file datafile sexp:read)";
    actual = s7_eval_c_string_with_environment(s7, cmd, readlet);
    TRACE_S7_DUMP("actual", actual);

    // use read not sexp:read for expected
    cmd = "(with-input-from-file expected read)";
    expected = s7_eval_c_string_with_environment(s7, cmd, readlet);
    TRACE_S7_DUMP("expected", expected);
    flag = APPLY_2("equal?", actual, expected);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    flag = APPLY_1("alist?", actual);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
}

void call_with_input_file_test(void) {
    s7_pointer fname7 = s7_make_string(s7, "test/data/sexp.scm");
    s7_pointer readlet
        = s7_inlet(s7,
                   s7_list(s7, 1,
                           s7_cons(s7,
                                   s7_make_symbol(s7, "fname"),
                                   fname7)));

    cmd = "(call-with-input-file fname sexp:read)";
    actual = s7_eval_c_string_with_environment(s7, cmd, readlet);
    TRACE_S7_DUMP("call-with-input-file-test", actual);

    flag = APPLY_1("alist?", t);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    /* cmd = "" */
    /* "(call-with-input-file \"test/data/strings.sexp\" sexp:read)"; */
    /* t = s7_eval_c_string(s7, cmd); */
    /* TRACE_S7_DUMP("t", t); */
    /* actual = APPLY_1("sexp:map?", t); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */

    /* k = s7_make_string(s7, "str1"); */
    /* s7_pointer str1 = APPLY_2("sexp:map-ref", t, k); */
    /* flag = APPLY_1("string?", str1); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */
    /* TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.", */
    /*                          s7_string(str1)); */

    /* k = s7_make_string(s7, "str2"); */
    /* s7_pointer str2 = APPLY_2("sexp:map-ref", t, k); */
    /* flag = APPLY_1("string?", str1); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */
    /* TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.", */
    /*                          s7_string(str2)); */
    /* TEST_ASSERT_EQUAL_STRING(s7_string(str1), s7_string(str2)); */

    /* k = s7_make_string(s7, "str3"); */
    /* s7_pointer str3 = APPLY_2("sexp:map-ref", t, k); */
    /* flag = APPLY_1("string?", str3); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */
    /* TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.", */
    /*                          s7_string(str3)); */
    /* TEST_ASSERT_EQUAL_STRING(s7_string(str2), s7_string(str3)); */
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "sexp");

    sexp_read = s7_name_to_value(s7, "sexp:read");
    call_with_input_string = s7_name_to_value(s7, "call-with-input-string");
    with_input_from_file = s7_name_to_value(s7, "with-input-from-file");
    call_with_input_file = s7_name_to_value(s7, "call-with-input-file");

    test_sexp_str7 = s7_make_string(s7, test_sexp_str);
    test_sexp_arg = s7_list(s7, 1, test_sexp_str7);
    test_sexp_expected = s7_eval_c_string(s7, test_sexp_expected_str);


    UNITY_BEGIN();

    /* RUN_TEST(read_string); */

    /* RUN_TEST(read_string_port); */
    /* RUN_TEST(with_input_from_string_test); */
    /* RUN_TEST(call_with_input_string_test); */

    /* RUN_TEST(read_file_port); */
    /* RUN_TEST(with_input_from_file_test); */
    /* RUN_TEST(call_with_input_file_test); */

    RUN_TEST(baddot_test);

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
