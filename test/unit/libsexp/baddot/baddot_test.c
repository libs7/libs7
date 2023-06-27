#include "config.h"
#include "unity.h"
#include "common.h"
#include "libs7.h"

s7_scheme *s7;

extern struct option options[];

// fn syms, initialized by main
s7_pointer sexp_read;
s7_pointer libs7_read; //plain ol' read
s7_pointer with_input_from_file;
s7_pointer call_with_input_file;

char *sexp_str;

s7_pointer flag;
s7_pointer actual; // set by each test
s7_pointer expected; // initialized by main

char *cmd;

const char *data_fname_str;
s7_pointer data_fname7;
s7_pointer expected_fname7;

s7_pointer baddot_expected;

void setUp(void) {}

void tearDown(void) {}

void read_file_port(void) {
    s7_pointer inport = s7_open_input_file(s7, data_fname_str,  "r");
    TEST_ASSERT_TRUE(s7_is_input_port(s7, inport));
    actual = s7_apply_function(s7, sexp_read, s7_list(s7, 1, inport));
    flag = APPLY_1("alist?", actual);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    s7_close_input_port(s7, inport);

    /* TRACE_S7_DUMP("actual", actual); */
    /* TRACE_S7_DUMP("expected", baddot_expected); */

    flag = APPLY_2("equal?", actual, baddot_expected);
    TEST_ASSERT_TRUE_MESSAGE(s7_boolean(s7, flag), data_fname_str);
}

void with_input_from_file_test(void) {
    data_fname7 = s7_make_string(s7, data_fname_str);
    s7_pointer readlet
        = s7_inlet(s7,
                   s7_list(s7, 1,
                           s7_cons(s7,
                                   s7_make_symbol(s7, "datafile"),
                                   data_fname7)));

    cmd = "(with-input-from-file datafile sexp:read)";
    actual = s7_eval_c_string_with_environment(s7, cmd, readlet);
    flag = APPLY_1("alist?", actual);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    /* TRACE_S7_DUMP("actual", actual); */
    /* TRACE_S7_DUMP("expected", baddot_expected); */
    flag = APPLY_2("equal?", actual, baddot_expected);
    TEST_ASSERT_TRUE_MESSAGE(s7_boolean(s7, flag), data_fname_str);
}

void call_with_input_file_test(void) {
    data_fname7 = s7_make_string(s7, data_fname_str);
    s7_pointer readlet
        = s7_inlet(s7,
                   s7_list(s7, 1,
                           s7_cons(s7,
                                   s7_make_symbol(s7, "datafile"),
                                   data_fname7)));

    cmd = "(call-with-input-file datafile sexp:read)";
    actual = s7_eval_c_string_with_environment(s7, cmd, readlet);
    flag = APPLY_1("alist?", actual);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    /* TRACE_S7_DUMP("actual", actual); */
    /* TRACE_S7_DUMP("expected", baddot_expected); */
    flag = APPLY_2("equal?", actual, baddot_expected);
    TEST_ASSERT_TRUE_MESSAGE(s7_boolean(s7, flag), data_fname_str);
}

s7_pointer read_expected(char *fname) {
    s7_pointer fname7 = s7_make_string(s7, fname);
    s7_pointer readlet
        = s7_inlet(s7,
                   s7_list(s7, 1,
                           /* s7_cons(s7, */
                           /*         s7_make_symbol(s7, "datafile"), */
                           /*         baddot_010_data_fname7), */
                           s7_cons(s7,
                                   s7_make_symbol(s7, "expected"),
                                   fname7)));

    /* cmd = "(with-input-from-file datafile sexp:read)"; */
    /* actual = s7_eval_c_string_with_environment(s7, cmd, readlet); */
    /* TRACE_S7_DUMP("actual", actual); */

    // use read not sexp:read for expected
    cmd = "(with-input-from-file expected read)";
    return s7_eval_c_string_with_environment(s7, cmd, readlet);
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "sexp");

    sexp_read = s7_name_to_value(s7, "sexp:read");
    libs7_read = s7_name_to_value(s7, "read");

    with_input_from_file = s7_name_to_value(s7, "with-input-from-file");
    call_with_input_file = s7_name_to_value(s7, "call-with-input-file");

    s7_int gc_expected = -1;

    UNITY_BEGIN();

    data_fname_str = "test/unit/libsexp/baddot/case010/dune";
    baddot_expected = read_expected("test/unit/libsexp/baddot/case010/sexp.expected");
    gc_expected = s7_gc_protect(s7, baddot_expected);
    RUN_TEST(read_file_port);
    RUN_TEST(with_input_from_file_test);
    RUN_TEST(call_with_input_file_test);
    s7_gc_unprotect_at(s7, gc_expected);

    data_fname_str = "test/unit/libsexp/baddot/case020/dune";
    baddot_expected = read_expected("test/unit/libsexp/baddot/case020/sexp.expected");
    gc_expected = s7_gc_protect(s7, baddot_expected);
    RUN_TEST(read_file_port);
    RUN_TEST(with_input_from_file_test);
    RUN_TEST(call_with_input_file_test);
    s7_gc_unprotect_at(s7, gc_expected);

    data_fname_str = "test/unit/libsexp/baddot/case030/dune";
    baddot_expected = read_expected("test/unit/libsexp/baddot/case030/sexp.expected");
    gc_expected = s7_gc_protect(s7, baddot_expected);
    RUN_TEST(read_file_port);
    RUN_TEST(with_input_from_file_test);
    RUN_TEST(call_with_input_file_test);
    s7_gc_unprotect_at(s7, gc_expected);

    s7_quit(s7);
    s7_free(s7);
    return UNITY_END();
}
