/* toml_serialization_test.c
   s7 format, display, object->string, etc
*/

#include "gopt.h"
#include "log.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"
#include "trace.h"

#include "common.h"

#include "libs7.h"

s7_scheme *s7;

extern struct option options[];

s7_pointer root;
s7_pointer b, t, k, a, idx, res, actual, expected;
s7_pointer dt, ts;
s7_pointer lst, alst, vec, ht, s1, s2;
s7_pointer len, m;
s7_pointer tmp;

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

void tables(void) {
    /* root = TOML_READ("m = [1, 2, 3]"); */
    /* root = TOML_READ("k1 = 1\nk2 = true\nk3='Hello'"); */

    root = TOML_READ("k1 = 7\nk2 = 'Hello, \"World\"'");
    s1 = s7_apply_function(s7, s7_name_to_value(s7, "object->string"),
                           s7_list(s7, 1, root));
    TRACE_S7_DUMP("obj->s", s1);
    s2 = s7_make_string(s7, "<#toml-table k1 = 7, k2 = 'Hello, \"World\"'>");
    TRACE_S7_DUMP("s2", s2);
    TEST_ASSERT_EQUAL_STRING(s7_string(s1), s7_string(s2));

    s2 = s7_apply_function(s7, s7_name_to_value(s7, "format"),
                           s7_list(s7, 3,
                                   s7_f(s7), // return string, no stdout
                                   s7_make_string(s7, "~S"),
                                   root));
    TRACE_S7_DUMP("s2", s2);
    TEST_ASSERT_EQUAL_STRING(s7_string(s1), s7_string(s2));

    /* s2 = s7_apply_function(s7, s7_name_to_value(s7, "display"), */
    /*                        s7_list(s7, 1, root)); */
    /* TRACE_S7_DUMP("s2", s2); */
    /* TEST_ASSERT_EQUAL_STRING(s7_string(s1), s7_string(s2)); */

    /* /\* root tables have empty key  *\/ */
    /* actual = APPLY_1("toml:table-key", root); */
    /* TEST_ASSERT_EQUAL_STRING("", s7_string(actual)); */

    /* k = s7_make_string(s7, "m"); */
    /* s7_pointer m = APPLY_2("toml:table-ref", root, k); */
    /* actual = APPLY_1("toml:table?", m); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */

    /* actual = APPLY_1("toml:table-key", m); */
    /* TEST_ASSERT_EQUAL_STRING("m", s7_string(actual)); */
    /* actual = APPLY_1("toml:table-keys", m); */
    /* s7_pointer v = APPLY_1("list?", actual); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), v); */

    /* //FIXME: test toml:table-key-for-index ('key_in') */
}

void arrays(void) {
    /* root = TOML_READ("m = [1, 2, 3]"); */
    /* root = TOML_READ("k1 = 1\nk2 = true\nk3='Hello'"); */

    root = TOML_READ("k1 = 7\nk2 = 'Hello, \"World\"'");
    s1 = s7_apply_function(s7, s7_name_to_value(s7, "object->string"),
                           s7_list(s7, 1, root));
    TRACE_S7_DUMP("obj->s", s1);
    s2 = s7_make_string(s7, "<#toml-table k1 = 7, k2 = 'Hello, \"World\"'>");
    TRACE_S7_DUMP("s2", s2);
    TEST_ASSERT_EQUAL_STRING(s7_string(s1), s7_string(s2));

    s2 = s7_apply_function(s7, s7_name_to_value(s7, "format"),
                           s7_list(s7, 3,
                                   s7_f(s7), // return string, no stdout
                                   s7_make_string(s7, "~S"),
                                   root));
    TRACE_S7_DUMP("s2", s2);
    TEST_ASSERT_EQUAL_STRING(s7_string(s1), s7_string(s2));

    /* s2 = s7_apply_function(s7, s7_name_to_value(s7, "display"), */
    /*                        s7_list(s7, 1, root)); */
    /* TRACE_S7_DUMP("s2", s2); */
    /* TEST_ASSERT_EQUAL_STRING(s7_string(s1), s7_string(s2)); */

    /* /\* root tables have empty key  *\/ */
    /* actual = APPLY_1("toml:table-key", root); */
    /* TEST_ASSERT_EQUAL_STRING("", s7_string(actual)); */

    /* k = s7_make_string(s7, "m"); */
    /* s7_pointer m = APPLY_2("toml:table-ref", root, k); */
    /* actual = APPLY_1("toml:table?", m); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */

    /* actual = APPLY_1("toml:table-key", m); */
    /* TEST_ASSERT_EQUAL_STRING("m", s7_string(actual)); */
    /* actual = APPLY_1("toml:table-keys", m); */
    /* s7_pointer v = APPLY_1("list?", actual); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), v); */

    /* //FIXME: test toml:table-key-for-index ('key_in') */
}

void timestamps(void) {
    root = TOML_READ("ts = 1979-05-27T00:32:00.999999");
    ts = APPLY_OBJ(root, s7_make_string(s7, "ts"));
    /* TRACE_S7_DUMP("ts", ts); */
    s1 = s7_object_to_string(s7, ts, // s7_nil(s7));
                             s7_make_symbol(s7, "foo"));
    /* s1 = s7_apply_function(s7, s7_name_to_value(s7, "object->string"), */
    /*                        s7_list(s7, 2, */
    /*                                ts, */
    /*                                /\* s7_f(s7) *\/ */
    /*                                s7_make_keyword(s7, "readable") */
    /*                                )); */
    TRACE_S7_DUMP("obj->s", s1);
    /* tomlc99 truncates secfrac to millis */
    /* expected_str = "\"1979-05-27T00:32:00.999\""; */
    /* TEST_ASSERT_EQUAL_STRING(expected_str, s7_string(s1)); */

    /* s2 = s7_apply_function(s7, s7_name_to_value(s7, "format"), */
    /*                        s7_list(s7, 3, */
    /*                                s7_f(s7), // return string, no stdout */
    /*                                s7_make_string(s7, "~A"), */
    /*                                ts)); */
    /* TRACE_S7_DUMP("s2", s2); */
    /* TEST_ASSERT_EQUAL_STRING(expected_str, s7_string(s2)); */
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "toml");

    UNITY_BEGIN();

    /* RUN_TEST(tables); */
    /* RUN_TEST(arrays); */
    RUN_TEST(timestamps);

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
