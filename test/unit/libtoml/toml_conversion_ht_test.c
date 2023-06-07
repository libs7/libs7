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

s7_pointer flag, t, tt, k, a, suba, ht;
s7_pointer idx, i0, i1, i3;
s7_pointer subt, subt1, subt2;
s7_pointer sexp, expected, actual;
s7_pointer expected_s7str, actual_s7str;
s7_pointer res, len, m;
s7_pointer tmp, tmp1, tmp2;

char *expected_str, *actual_str, *tmp_str;
char *cmd, *sexp_str;

bool verbose;
bool debug;


/* #define TOML_READ(s) \ */
/*     s7_apply_function(s7, s7_name_to_value(s7, "toml:read"),    \ */
/*                       s7_list(s7, 1, s7_eval_c_string(s7, s))); */

/* #define APPLY_1(f, o) \ */
/*  s7_apply_function(s7, s7_name_to_value(s7, f),    \ */
/*                        s7_list(s7, 1, o)) */

/* #define APPLY_2(f, a, b)                             \ */
/*  s7_apply_function(s7, s7_name_to_value(s7, f),    \ */
/*                    s7_list(s7, 2, a, b)) */

    /* s7_apply_function_star(s7, s7_name_to_value(s7, f), \ */
    /*                            s7_list(s7, 1, v)) */

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

void root_to_hash_table(void)
{
    tt = TOML_READ("fld1 = 1\nfld2 = 2");
    ht = APPLY_1("toml:map->hash-table", tt);
    flag = APPLY_1("hash-table?", ht);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    flag = APPLY_1("c-pointer?", ht);
    TEST_ASSERT_FALSE(s7_boolean(s7, flag));

    sexp_str = "(hash-table \"fld1\" 1 \"fld2\" 2)";
    expected = EVAL(sexp_str);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, ht));
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    res = APPLY_1("object->string", ht);
    TEST_ASSERT_EQUAL_STRING(sexp_str, s7_string(res));
}

void nested_table_to_hash_table(void)
{
    tt = TOML_READ("a = { b = 0 }");
    ht = APPLY_1("toml:map->hash-table", tt);
    TRACE_S7_DUMP("ht", ht);
    flag = APPLY_1("hash-table?", ht);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    flag = APPLY_1("c-pointer?", ht);
    TEST_ASSERT_FALSE(s7_boolean(s7, flag));

    sexp_str = "(hash-table :a (hash-table :b 0))";
    expected = EVAL(sexp_str);
    TRACE_S7_DUMP("expected", expected);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, ht));
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    /* // verify value at "a" of toml-table is a toml-table */
    /* k = s7_make_string(s7, "a"); */
    /* subt = APPLY_2("toml:map-ref", tt, k); */
    /* flag = APPLY_1("toml:map?", subt); */
    /* TEST_ASSERT_TRUE(s7_boolean(s7, flag)); */

    /* // verify value at "a" of hash-table is a hash-table */
    /* k = s7_make_string(s7, "a"); */
    /* subt = APPLY_2("hash-table-ref", ht, k); */
    /* flag = APPLY_1("hash-table?", subt); */
    /* TEST_ASSERT_TRUE(s7_boolean(s7, flag)); */
}

void subarray_to_vector(void)
{
    t = TOML_READ("a = [1, 2, 3]");
    actual = APPLY_1("toml:map->hash-table", t);
    flag = APPLY_1("hash-table?", actual);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("c-pointer?", actual);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    /* TRACE_S7_DUMP("actual", actual); */

    sexp_str = "(hash-table \"a\" #(1 2 3))";
    expected = EVAL(sexp_str);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, actual));
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    actual = APPLY_1("object->string", actual);
    TEST_ASSERT_EQUAL_STRING(sexp_str, s7_string(actual));

    // verify value at "a" of toml-table is a toml-array
    k = s7_make_string(s7, "a");
    subt = APPLY_2("toml:map-ref", t, k);
    flag = APPLY_1("toml:array?", subt);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of hash-table is a vector
    k = s7_make_string(s7, "a");
    subt = APPLY_2("hash-table-ref", actual, k);
    actual = APPLY_1("vector?", subt);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
}

void subarray_of_tables(void)
{
    t = TOML_READ("a = [{t1 = 1}, {t2 = 2}]");
    flag = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    actual = APPLY_1("toml:map->hash-table", t);
    flag = APPLY_1("hash-table?", actual);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("c-pointer?", actual);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    // WARNING: serialization prints '#(...)' but the reader needs
    // '(vector...)'.
    /* TRACE_S7_DUMP("actual  ", actual); */
    /* actual_s7str = s7_object_to_string(s7, actual, true); */
    /* actual_s7str = s7_apply_function(s7, s7_name_to_value(s7, */
    /*                                                       "object->string"), */
    /*                                  s7_list(s7, 2, actual, s7_t(s7))); */
    /* actual_s7str = APPLY_1("object->string", actual); */
    /* TRACE_S7_DUMP("object->string actual  ", actual_s7str); */
    /* log_debug("XXXX %s", s7_string(actual_s7str)); */

    // trying (format ...
    /* actual_str = s7_format(s7, s7_list(s7, 3, */
    /*                                    s7_f(s7), */
    /*                                    s7_make_string(s7, "~S"), */
    /*                                    actual)); */
    /* log_debug("FORMATTED: %s", actual_str); */

    // WARNING: use (vector ...), not #(...)
    sexp_str = "(hash-table \"a\" (vector (hash-table \"t1\" 1) (hash-table \"t2\" 2)))";
    expected = EVAL(sexp_str);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("c-pointer?", expected);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    /* TRACE_S7_DUMP("expected", expected); */

    /* TEST_ASSERT_EQUAL_STRING(sexp, s7_string(actual_str)); */

    expected_s7str = APPLY_1("object->string", expected);
    /* TRACE_S7_DUMP("expected s7str", expected_s7str); */
    /* log_debug("expected str: %s", expected_str); */
    /* TEST_ASSERT_EQUAL_STRING(sexp, s7_string(expected_str)); */

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, actual, expected));
    /* TRACE_S7_DUMP("flag", flag); */
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of toml-table is a toml-array
    k = s7_make_string(s7, "a");
    subt = APPLY_2("toml:map-ref", t, k);
    flag = APPLY_1("toml:array?", subt);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of hash-table is a vector
    k = s7_make_string(s7, "a");
    suba = APPLY_2("hash-table-ref", actual, k);
    flag = APPLY_1("vector?", suba);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    /* TRACE_S7_DUMP("suba", suba); */
    // compare it to expected
    tmp = APPLY_2("hash-table-ref", expected, k);
    flag = APPLY_1("vector?", tmp);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    /* TRACE_S7_DUMP("tmp", tmp); */

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, suba, tmp));
    /* TRACE_S7_DUMP("flag", flag); */
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify that mbrs of vector are hash-tables
    i0 = s7_make_integer(s7, 0);
    subt1 = APPLY_2("vector-ref", suba, i0);
    actual = APPLY_1("hash-table?", subt1);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    i1 = s7_make_integer(s7, 1);
    subt2 = APPLY_2("vector-ref", suba, i1);
    actual = APPLY_1("hash-table?", subt2);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    // compare to expected
    tmp1 = APPLY_2("vector-ref", tmp, i0);
    /* TRACE_S7_DUMP("tmp1", tmp1); */
    flag = APPLY_1("hash-table?", tmp1);
    /* TRACE_S7_DUMP("flag", flag); */
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, subt1, tmp1));
    /* TRACE_S7_DUMP("flag", flag); */
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "toml");

    UNITY_BEGIN();

    /* RUN_TEST(root_to_hash_table); */
    RUN_TEST(nested_table_to_hash_table);
    /* RUN_TEST(subarray_to_vector); */
    /* RUN_TEST(subarray_of_tables); */

    /* RUN_TEST(dotted_keys); */

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
