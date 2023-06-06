#include "gopt.h"
#include "log.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"
#include "trace.h"

#include "common.h"

#include "libs7.h"

s7_scheme *s7;

s7_pointer json_read;
s7_pointer json_str7;
char *json_str;
bool flag_bool;
s7_pointer json_obj;

extern struct option options[];

s7_pointer root, jo, k, keys, ja, idx;
s7_pointer flag, res, actual, expected;
s7_pointer len, m, map, v, vec;

char *expected_str;

bool verbose;
bool debug;

char *cmd;

void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

/*
  (json:read)   - read from current-input-port
  (json:read s) - read from string s
  (json:read p) - read from port p
 */

void read_string(void) {
    jo = JSON_READ("{ \"m\": { \"a\": 0 }}");
    actual = APPLY_1("json:map?", jo);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
}

/* (open-input-string s) */
void read_string_port(void) {
    json_str = "{\"t\": {\"i\": 1, \"s\": \"Hello\"}}";
    s7_pointer inport = s7_open_input_string(s7, json_str);
    flag_bool = s7_is_input_port(s7, inport);
    TEST_ASSERT_TRUE(flag_bool);

    json_obj = s7_apply_function(s7, json_read, s7_list(s7, 1, inport));
    flag = APPLY_1("json:map?", json_obj);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
}

void with_input_from_string(void) {
    s7_pointer with_input_from_string
        = s7_name_to_value(s7, "with-input-from-string");
    json_str = "{\"t\": {\"i\": 1, \"s\": \"Hello\"}}";
    json_str7 = s7_make_string(s7, json_str);
    actual = s7_eval(s7,
                     s7_cons(s7, with_input_from_string,
                             s7_cons(s7, json_str7,
                                     s7_cons(s7, json_read,
                                             s7_nil(s7)))),
                             s7_rootlet(s7));
    /* TRACE_S7_DUMP("with-input-from-string", actual); */
    res = APPLY_1("json:map?", actual);
    TEST_ASSERT_EQUAL(res, s7_t(s7));
}

void call_with_input_string(void) {
    s7_pointer call_with_input_string
        = s7_name_to_value(s7, "call-with-input-string");
    json_str = "{\"t\": {\"i\": 1, \"s\": \"Hello\"}}";
    json_str7 = s7_make_string(s7, json_str);
    actual = s7_eval(s7,
                     s7_cons(s7, call_with_input_string,
                             s7_cons(s7, json_str7,
                                     s7_cons(s7, json_read,
                                             s7_nil(s7)))),
                             s7_rootlet(s7));
    res = APPLY_1("json:map?", actual);
    TEST_ASSERT_EQUAL(res, s7_t(s7));
}

void read_file_port(void) {
    char *fpath = "test/data/strings.json";
    s7_pointer inport = s7_open_input_file(s7, fpath, "r");
    flag_bool = s7_is_input_port(s7, inport);
    TEST_ASSERT_TRUE(flag_bool);
    json_obj = s7_apply_function(s7, json_read, s7_list(s7, 1, inport));
    actual = APPLY_1("json:map?", json_obj);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
}

/* void with_input_from_file(void) { */
/*     cmd = "" */
/*     "(with-input-from-file \"test/lib/toml/data/strings.toml\" json:read)"; */
/*     t = s7_eval_c_string(s7, cmd); */
/*     flag = APPLY_1("json:map?", t); */
/*     TEST_ASSERT_TRUE(s7_boolean(s7, flag)); */

/*     k = s7_make_string(s7, "str1"); */
/*     s7_pointer str1 = APPLY_2("json:map-ref", t, k); */
/*     flag = APPLY_1("string?", str1); */
/*     TEST_ASSERT_TRUE(s7_boolean(s7, flag)); */
/*     TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.", */
/*                              s7_string(str1)); */

/*     k = s7_make_string(s7, "str2"); */
/*     s7_pointer str2 = APPLY_2("json:map-ref", t, k); */
/*     flag = APPLY_1("string?", str1); */
/*     TEST_ASSERT_EQUAL(s7_t(s7), flag); */
/*     TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.", */
/*                              s7_string(str2)); */
/*     TEST_ASSERT_EQUAL_STRING(s7_string(str1), s7_string(str2)); */

/*     k = s7_make_string(s7, "str3"); */
/*     s7_pointer str3 = APPLY_2("json:map-ref", t, k); */
/*     flag = APPLY_1("string?", str3); */
/*     TEST_ASSERT_EQUAL(s7_t(s7), flag); */
/*     TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.", */
/*                              s7_string(str3)); */
/*     TEST_ASSERT_EQUAL_STRING(s7_string(str2), s7_string(str3)); */
/* } */

/* void call_with_input_file(void) { */
/*     cmd = "" */
/*     "(call-with-input-file \"test/lib/toml/data/strings.toml\" json:read)"; */
/*     t = s7_eval_c_string(s7, cmd); */
/*     TRACE_S7_DUMP("t", t); */
/*     /\* s7_pointer is_input_port = s7_is_input_port(s7, inport); *\/ */
/*     /\* TEST_ASSERT_EQUAL(true, is_input_port); *\/ */
/*     /\* t = JSON_READ(inport); *\/ */
/*     /\* t = s7_apply_function(s7, json_read, s7_list(s7, 1, inport)); *\/ */
/*     actual = APPLY_1("json:map?", t); */
/*     TEST_ASSERT_EQUAL(actual, s7_t(s7)); */

/*     k = s7_make_string(s7, "str1"); */
/*     s7_pointer str1 = APPLY_2("json:map-ref", t, k); */
/*     flag = APPLY_1("string?", str1); */
/*     TEST_ASSERT_EQUAL(s7_t(s7), flag); */
/*     TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.", */
/*                              s7_string(str1)); */

/*     k = s7_make_string(s7, "str2"); */
/*     s7_pointer str2 = APPLY_2("json:map-ref", t, k); */
/*     flag = APPLY_1("string?", str1); */
/*     TEST_ASSERT_EQUAL(s7_t(s7), flag); */
/*     TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.", */
/*                              s7_string(str2)); */
/*     TEST_ASSERT_EQUAL_STRING(s7_string(str1), s7_string(str2)); */

/*     k = s7_make_string(s7, "str3"); */
/*     s7_pointer str3 = APPLY_2("json:map-ref", t, k); */
/*     flag = APPLY_1("string?", str3); */
/*     TEST_ASSERT_EQUAL(s7_t(s7), flag); */
/*     TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.", */
/*                              s7_string(str3)); */
/*     TEST_ASSERT_EQUAL_STRING(s7_string(str2), s7_string(str3)); */
/* } */

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "json");

    json_read = s7_name_to_value(s7, "json:read");

    UNITY_BEGIN();

    /* RUN_TEST(read_string); */
    /* RUN_TEST(read_string_port); */
    RUN_TEST(with_input_from_string);
    RUN_TEST(call_with_input_string);

    RUN_TEST(read_file_port);

    /* RUN_TEST(with_input_from_file); */
    /* RUN_TEST(call_with_input_file); */

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
