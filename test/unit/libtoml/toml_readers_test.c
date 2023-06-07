#include "config.h"
/* #include "gopt.h" */
/* #include "log.h" */
#include "unity.h"
#include "utarray.h"
#include "utstring.h"
#include "trace.h"

#include "common.h"

#include "libs7.h"

s7_scheme *s7;

extern struct option options[];

s7_pointer toml_read;
char *toml_str;

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
    s7_pointer t = TOML_READ("m = { a = 0 }");
    flag = APPLY_1("toml:map?", t);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
}

/* (open-input-string s) */
void read_string_port(void) {
    toml_str = "t = { i = 1, s = \"Hello\" }";
    s7_pointer inport = s7_open_input_string(s7, toml_str);
    flag_bool = s7_is_input_port(s7, inport);
    TEST_ASSERT_TRUE(flag_bool);

    t = s7_apply_function(s7, toml_read, s7_list(s7, 1, inport));
    /* TRACE_S7_DUMP("read_string_port", t); */
    flag = APPLY_1("toml:map?", t);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
}

void with_input_from_string(void) {
    cmd = ""
    "(with-input-from-string \"t = { i = 1, s = \\\"Hello\\\" }\" toml:read)";
    actual = s7_eval_c_string(s7, cmd);
    TRACE_S7_DUMP("with-input-from-string", actual);
    res = APPLY_1("toml:map?", actual);
    TEST_ASSERT_EQUAL(res, s7_t(s7));
}

void call_with_input_string(void) {
    cmd = ""
    "(call-with-input-string \"t = { i = 1, s = \\\"Hello\\\" }\" toml:read)";
    actual = s7_eval_c_string(s7, cmd);
    TRACE_S7_DUMP("call-with-input-string", actual);
    res = APPLY_1("toml:map?", actual);
    TEST_ASSERT_EQUAL(res, s7_t(s7));
}

void read_file_port(void) {
    s7_pointer inport = s7_open_input_file(s7, "test/data/strings.toml", "r");
    flag_bool = s7_is_input_port(s7, inport);
    TEST_ASSERT_TRUE(flag_bool);
    t = s7_apply_function(s7, toml_read, s7_list(s7, 1, inport));
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
}

void with_input_from_file(void) {
    cmd = ""
    "(with-input-from-file \"test/data/strings.toml\" toml:read)";
    t = s7_eval_c_string(s7, cmd);
    flag = APPLY_1("toml:map?", t);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    k = s7_make_string(s7, "str1");
    s7_pointer str1 = APPLY_2("toml:map-ref", t, k);
    flag = APPLY_1("string?", str1);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.",
                             s7_string(str1));

    k = s7_make_string(s7, "str2");
    s7_pointer str2 = APPLY_2("toml:map-ref", t, k);
    flag = APPLY_1("string?", str1);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.",
                             s7_string(str2));
    TEST_ASSERT_EQUAL_STRING(s7_string(str1), s7_string(str2));

    k = s7_make_string(s7, "str3");
    s7_pointer str3 = APPLY_2("toml:map-ref", t, k);
    flag = APPLY_1("string?", str3);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.",
                             s7_string(str3));
    TEST_ASSERT_EQUAL_STRING(s7_string(str2), s7_string(str3));
}

void call_with_input_file(void) {
    cmd = ""
    "(call-with-input-file \"test/data/strings.toml\" toml:read)";
    t = s7_eval_c_string(s7, cmd);
    TRACE_S7_DUMP("t", t);
    /* s7_pointer is_input_port = s7_is_input_port(s7, inport); */
    /* TEST_ASSERT_EQUAL(true, is_input_port); */
    /* t = TOML_READ(inport); */
    /* t = s7_apply_function(s7, toml_read, s7_list(s7, 1, inport)); */
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "str1");
    s7_pointer str1 = APPLY_2("toml:map-ref", t, k);
    flag = APPLY_1("string?", str1);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.",
                             s7_string(str1));

    k = s7_make_string(s7, "str2");
    s7_pointer str2 = APPLY_2("toml:map-ref", t, k);
    flag = APPLY_1("string?", str1);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.",
                             s7_string(str2));
    TEST_ASSERT_EQUAL_STRING(s7_string(str1), s7_string(str2));

    k = s7_make_string(s7, "str3");
    s7_pointer str3 = APPLY_2("toml:map-ref", t, k);
    flag = APPLY_1("string?", str3);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.",
                             s7_string(str3));
    TEST_ASSERT_EQUAL_STRING(s7_string(str2), s7_string(str3));
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "toml");

    toml_read = s7_name_to_value(s7, "toml:read");

    UNITY_BEGIN();

    RUN_TEST(read_string);

    RUN_TEST(read_string_port);
    RUN_TEST(with_input_from_string);
    RUN_TEST(call_with_input_string);

    RUN_TEST(read_file_port);
    RUN_TEST(with_input_from_file);
    RUN_TEST(call_with_input_file);

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
