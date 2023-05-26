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

s7_pointer b, t, k, a, idx, res, actual, expected;
s7_pointer len, m;

bool verbose;
bool debug;

char *cmd;

#define TOML_READ(s) \
    s7_apply_function(s7, s7_name_to_value(s7, "toml:read"),    \
                      s7_list(s7, 1, s7_eval_c_string(s7, s)));

#define APPLY_1(f, o) \
 s7_apply_function(s7, s7_name_to_value(s7, f),    \
                       s7_list(s7, 1, o))

#define APPLY_2(f, o, k)                             \
 s7_apply_function(s7, s7_name_to_value(s7, f),    \
                   s7_list(s7, 2, o, k))

    /* s7_apply_function_star(s7, s7_name_to_value(s7, f), \ */
    /*                            s7_list(s7, 1, v)) */

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

/*
  (toml:read)
  (toml:read "foo")
  (toml:read "foo")
 */
void read_api(void) {
    s7_pointer t = TOML_READ("\"m = { a = 0 }\"");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    cmd = ""
        "(with-input-from-string "
        "    \"t = { i = 1, s = \"Hello\" }\""
        "    toml:read)";
    actual = s7_eval_c_string(s7, cmd);
    res = APPLY_1("toml:map?", actual);
    TEST_ASSERT_EQUAL(res, s7_t(s7));
}

/* (define tlt (toml:read "v = [0, 1, 2]")) */
/* tlt is (a nameless) table, NOT a kv pair!  */
void root_tables(void) {
    t = TOML_READ("\"m = true\")");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "m");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("boolean?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    t = TOML_READ("\"m = 123\")");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "m");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("integer?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    t = TOML_READ("\"m = 1.23\")");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "m");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("real?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    t = TOML_READ("\"m = \"Hello\"\")");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "m");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("string?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    t = TOML_READ("\"m = [1, 2, 3]\")");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "m");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    t = TOML_READ("\"m = { a = 1, b = 2}\")");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "m");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("toml:map?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
}

void table_refs(void) {
    t = TOML_READ("\"m = [1, 2, 3]\")");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "m");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    /* apply table to key: (t k) == (toml:map-ref t k) */
    a = s7_apply_function(s7, t, s7_list(s7, 1, k));
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    // try hash-table-ref - nope, segfault
    /* a = APPLY_2("hash-table-ref", t, k); */
    /* actual = APPLY_1("toml:array?", a); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */
}

/*
 * WARNING: tomlc99 table count ops are typed:
 * ntab for table-valued entries, narr for array-valued entries,
 * and nkv for "key-values", i.e. entries with 'atomic' values
 * so toml:map-length must sum the three: ntab + narr + nkv
 */
void table_length_ops(void) {
    char *toml = ""
        "\"m = { b = true, s = \"Hello!\", "
        "        i = 0, f = 1.2, "
        "        t = { t1 = 1 }, a = [0, 1, 2] }\"";
    t = TOML_READ(toml);
    actual = APPLY_1("toml:map?", t);
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */

    k = s7_make_string(s7, "m");
    s7_pointer m = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("toml:map?", m);
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */

    actual = APPLY_1("toml:map-length", m);
    TEST_ASSERT_EQUAL(6, s7_integer(actual));

    actual = APPLY_1("toml:map-nkval", m);
    TEST_ASSERT_EQUAL(4, s7_integer(actual));
    /* aliases nkval */
    actual = APPLY_1("toml:map-atomic-count", m);
    TEST_ASSERT_EQUAL(4, s7_integer(actual));

    actual = APPLY_1("toml:map-ntab", m);
    TEST_ASSERT_EQUAL(1, s7_integer(actual));
    /* aliases ntab */
    actual = APPLY_1("toml:map-subtable-count", m);
    TEST_ASSERT_EQUAL(1, s7_integer(actual));

    actual = APPLY_1("toml:map-narr", m);
    TEST_ASSERT_EQUAL(1, s7_integer(actual));
    /* aliases narr */
    actual = APPLY_1("toml:map-array-count", m);
    TEST_ASSERT_EQUAL(1, s7_integer(actual));
}

void table_ops(void) {
    char *toml = ""
        "\"m = { b = true, s = \"Hello!\", "
        "        i = 0, f = 1.2, "
        "        t = { t1 = 1 }, a = [0, 1, 2] }\"";
    t = TOML_READ(toml);
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    /* root tables have empty key  */
    actual = APPLY_1("toml:map-key", t);
    TEST_ASSERT_EQUAL_STRING("", s7_string(actual));

    k = s7_make_string(s7, "m");
    s7_pointer m = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("toml:map?", m);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    actual = APPLY_1("toml:map-key", m);
    TEST_ASSERT_EQUAL_STRING("m", s7_string(actual));
    actual = APPLY_1("toml:map-keys", m);
    s7_pointer v = APPLY_1("list?", actual);
    TEST_ASSERT_EQUAL(s7_t(s7), v);

    //FIXME: test toml:map-key-for-index ('key_in')

}

void table_serialization(void) {
    /* t = TOML_READ("\"m = [1, 2, 3]\")"); */
    /* t = TOML_READ("\"k1 = 1\nk2 = true\nk3='Hello'\")"); */

    t = TOML_READ("\"k1 = 7\nk2 = 8\")");

    /* actual = APPLY_FORMAT("\"~A\"", t); */
    res = s7_apply_function(s7, s7_name_to_value(s7, "object->string"),
                            s7_list(s7, 1, t));
    TRACE_S7_DUMP("obj->s", res);

    /* res = s7_apply_function(s7, s7_name_to_value(s7, "format"), */
    /*                         s7_list(s7, 3, */
    /*                                 s7_t(s7), */
    /*                                 s7_make_string(s7, "~A"), */
    /*                                 t)); */
    /* log_debug("xxxxxxxxxxxxxxxx"); */
    /* TRACE_S7_DUMP("fmt", res); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */

    /* /\* root tables have empty key  *\/ */
    /* actual = APPLY_1("toml:map-key", t); */
    /* TEST_ASSERT_EQUAL_STRING("", s7_string(actual)); */

    /* k = s7_make_string(s7, "m"); */
    /* s7_pointer m = APPLY_2("toml:map-ref", t, k); */
    /* actual = APPLY_1("toml:map?", m); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */

    /* actual = APPLY_1("toml:map-key", m); */
    /* TEST_ASSERT_EQUAL_STRING("m", s7_string(actual)); */
    /* actual = APPLY_1("toml:map-keys", m); */
    /* s7_pointer v = APPLY_1("list?", actual); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), v); */

    /* //FIXME: test toml:map-key-for-index ('key_in') */
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "toml");

    UNITY_BEGIN();

    /* RUN_TEST(read_api); */
    /* RUN_TEST(root_tables); */
    /* RUN_TEST(table_refs); */
    /* RUN_TEST(table_length_ops); */
    /* RUN_TEST(table_ops); */
    RUN_TEST(table_serialization);

    /* RUN_TEST(dotted_keys); */
    /* RUN_TEST(array_kind_int); */
    /* RUN_TEST(array_kind_string); */
    /* RUN_TEST(array_kind_string2); */
    /* RUN_TEST(array_nested_ints); */
    /* RUN_TEST(array_nested_mixed); */
    /* RUN_TEST(array_mixed_numbers); */
    /* RUN_TEST(array_kind_table); */

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
