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

s7_pointer flag, jm, ja, k, a, suba, subvec, ht;
s7_pointer idx, i0, i1, i3;
s7_pointer subt, subt1, subt2;
s7_pointer sexp, expected, actual;
s7_pointer expected_s7str, actual_s7str;
s7_pointer val, res, len, m;
s7_pointer tmp, tmp1, tmp2;

char *expected_str, *actual_str, *tmp_str;
char *cmd, *sexp_str;

bool verbose;
bool debug;

void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

void int_array_to_vec(void)
{
    jm = JSON_READ("{\"a\": [1, 2, 3]}");
    actual = APPLY_1("json:map->hash-table", jm);
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

    // verify value at "a" of json map is a json-array
    k = s7_make_string(s7, "a");
    suba = APPLY_2("json:map-ref", jm, k);
    flag = APPLY_1("json:array?", suba);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of hash-table is a vector
    k = s7_make_string(s7, "a");
    subvec = APPLY_2("hash-table-ref", actual, k);
    actual = APPLY_1("vector?", subvec);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    // verify length
    val = APPLY_1("vector-length", subvec);
    TEST_ASSERT_EQUAL_INT(3, s7_integer(val));

    // verify values are ints
    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 0));
    actual = APPLY_1("integer?", val);
    TEST_ASSERT_EQUAL_INT(1, s7_integer(val));

    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 1));
    actual = APPLY_1("integer?", val);
    TEST_ASSERT_EQUAL_INT(2, s7_integer(val));

    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 2));
    actual = APPLY_1("integer?", val);
    TEST_ASSERT_EQUAL_INT(3, s7_integer(val));
}

void real_array_to_vec(void)
{
    jm = JSON_READ("{\"a\": [1.2, 3.4, 5.6]}");
    actual = APPLY_1("json:map->hash-table", jm);
    flag = APPLY_1("hash-table?", actual);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("c-pointer?", actual);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    /* TRACE_S7_DUMP("actual", actual); */

    sexp_str = "(hash-table \"a\" #(1.2 3.4 5.6))";
    expected = EVAL(sexp_str);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, actual));
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of json map is a json-array
    k = s7_make_string(s7, "a");
    suba = APPLY_2("json:map-ref", jm, k);
    flag = APPLY_1("json:array?", suba);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of hash-table is a vector
    k = s7_make_string(s7, "a");
    subvec = APPLY_2("hash-table-ref", actual, k);
    actual = APPLY_1("vector?", subvec);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    /* TRACE_S7_DUMP("subvec", subvec); */

    // verify length
    val = APPLY_1("vector-length", subvec);
    TEST_ASSERT_EQUAL_INT(3, s7_integer(val));

    // verify values are reals
    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 0));
    /* TRACE_S7_DUMP("val", val); */
    actual = APPLY_1("real?", val);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    TEST_ASSERT_EQUAL_FLOAT(1.2, s7_real(val));

    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 1));
    actual = APPLY_1("real?", val);
    TEST_ASSERT_EQUAL_FLOAT(3.4, s7_real(val));

    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 2));
    actual = APPLY_1("real?", val);
    TEST_ASSERT_EQUAL_FLOAT(5.6, s7_real(val));
}

void bool_array_to_vec(void)
{
    jm = JSON_READ("{\"a\": [true, false, true]}");
    actual = APPLY_1("json:map->hash-table", jm);
    flag = APPLY_1("hash-table?", actual);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("c-pointer?", actual);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    /* TRACE_S7_DUMP("actual", actual); */

    sexp_str = "(hash-table \"a\" #(#t #f #t))";
    expected = EVAL(sexp_str);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, actual));
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of json map is a json-array
    k = s7_make_string(s7, "a");
    suba = APPLY_2("json:map-ref", jm, k);
    flag = APPLY_1("json:array?", suba);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of hash-table is a vector
    k = s7_make_string(s7, "a");
    subvec = APPLY_2("hash-table-ref", actual, k);
    actual = APPLY_1("vector?", subvec);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    /* TRACE_S7_DUMP("subvec", subvec); */

    // verify length
    val = APPLY_1("vector-length", subvec);
    TEST_ASSERT_EQUAL_INT(3, s7_integer(val));

    // verify values are bools
    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 0));
    /* TRACE_S7_DUMP("val", val); */
    actual = APPLY_1("boolean?", val);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    TEST_ASSERT_EQUAL(s7_t(s7), val);

    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 1));
    actual = APPLY_1("boolean?", val);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    TEST_ASSERT_EQUAL(s7_f(s7), val);

    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 2));
    actual = APPLY_1("boolean?", val);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    TEST_ASSERT_EQUAL(s7_t(s7), val);

    // apply vector directly
    val = APPLY_OBJ(subvec, s7_make_integer(s7, 2));
    actual = APPLY_1("boolean?", val);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    TEST_ASSERT_EQUAL(s7_t(s7), val);
}

void null_array_to_vec(void)
{
    jm = JSON_READ("{\"a\": [null, null, null]}");
    actual = APPLY_1("json:map->hash-table", jm);
    flag = APPLY_1("hash-table?", actual);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("c-pointer?", actual);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    /* TRACE_S7_DUMP("actual", actual); */

    sexp_str = "(hash-table \"a\" #(() () ()))";
    expected = EVAL(sexp_str);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, actual));
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of json map is a json-array
    k = s7_make_string(s7, "a");
    suba = APPLY_2("json:map-ref", jm, k);
    flag = APPLY_1("json:array?", suba);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of hash-table is a vector
    k = s7_make_string(s7, "a");
    subvec = APPLY_2("hash-table-ref", actual, k);
    actual = APPLY_1("vector?", subvec);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    TRACE_S7_DUMP("subvec", subvec);

    // verify length
    val = APPLY_1("vector-length", subvec);
    TEST_ASSERT_EQUAL_INT(3, s7_integer(val));

    // verify values are nulls
    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 0));
    actual = APPLY_1("null?", val);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    TEST_ASSERT_EQUAL(s7_nil(s7), val);

    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 1));
    actual = APPLY_1("null?", val);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    TEST_ASSERT_EQUAL(s7_nil(s7), val);

    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 2));
    actual = APPLY_1("null?", val);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    TEST_ASSERT_EQUAL(s7_nil(s7), val);
}

void string_array_to_vec(void)
{
    jm = JSON_READ("{\"a\": [\"msga\", \"msgb\", \"msgc\"]}");
    actual = APPLY_1("json:map->hash-table", jm);
    flag = APPLY_1("hash-table?", actual);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("c-pointer?", actual);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    /* TRACE_S7_DUMP("actual", actual); */

    sexp_str = "(hash-table \"a\" #(\"msga\", \"msgb\", \"msgc\"))";
    expected = EVAL(sexp_str);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, actual));
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of json map is a json-array
    k = s7_make_string(s7, "a");
    suba = APPLY_2("json:map-ref", jm, k);
    flag = APPLY_1("json:array?", suba);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of hash-table is a vector
    k = s7_make_string(s7, "a");
    subvec = APPLY_2("hash-table-ref", actual, k);
    actual = APPLY_1("vector?", subvec);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    TRACE_S7_DUMP("subvec", subvec);

    // verify length
    val = APPLY_1("vector-length", subvec);
    TEST_ASSERT_EQUAL_INT(3, s7_integer(val));

    // verify values are strings
    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 0));
    actual = APPLY_1("string?", val);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    TEST_ASSERT_EQUAL_STRING("msga", s7_string(val));

    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 1));
    actual = APPLY_1("string?", val);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    TEST_ASSERT_EQUAL_STRING("msgb", s7_string(val));

    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 2));
    actual = APPLY_1("string?", val);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    TEST_ASSERT_EQUAL_STRING("msgc", s7_string(val));
}

void mixed_array_to_vec(void)
{
    jm = JSON_READ("{\"a\": [1, 2.3, true, null, \"msga\"]}");
    actual = APPLY_1("json:map->hash-table", jm);
    flag = APPLY_1("hash-table?", actual);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("c-pointer?", actual);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    /* TRACE_S7_DUMP("actual", actual); */

    sexp_str = "(hash-table \"a\" #(1 2.3 #t () \"msga\"))";
    expected = EVAL(sexp_str);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, actual));
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of json map is a json-array
    k = s7_make_string(s7, "a");
    suba = APPLY_2("json:map-ref", jm, k);
    flag = APPLY_1("json:array?", suba);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of hash-table is a vector
    k = s7_make_string(s7, "a");
    subvec = APPLY_2("hash-table-ref", actual, k);
    actual = APPLY_1("vector?", subvec);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    TRACE_S7_DUMP("subvec", subvec);

    // verify length
    val = APPLY_1("vector-length", subvec);
    TEST_ASSERT_EQUAL_INT(5, s7_integer(val));

    // verify values
    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 0));
    actual = APPLY_1("integer?", val);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    TEST_ASSERT_EQUAL_INT(1, s7_integer(val));

    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 1));
    actual = APPLY_1("real?", val);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    TEST_ASSERT_EQUAL_FLOAT(2.3, s7_real(val));

    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 2));
    actual = APPLY_1("boolean?", val);
    TEST_ASSERT_TRUE(s7_boolean(s7, actual));
    TEST_ASSERT_TRUE(s7_boolean(s7, val));

    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 3));
    actual = APPLY_1("null?", val);
    TEST_ASSERT_TRUE(s7_boolean(s7, actual));
    TEST_ASSERT_EQUAL(s7_nil(s7), val);

    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 4));
    actual = APPLY_1("string?", val);
    TEST_ASSERT_TRUE(s7_boolean(s7, actual));
    TEST_ASSERT_EQUAL_STRING("msga", s7_string(val));
}

void nested_int_arrays(void)
{
    jm = JSON_READ("{\"a\": [[1, 2], [3, 4]]}");
    ht = APPLY_1("json:map->hash-table", jm);
    TRACE_S7_DUMP("ht", ht);
    flag = APPLY_1("hash-table?", ht);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    flag = APPLY_1("c-pointer?", ht);
    TEST_ASSERT_FALSE(s7_boolean(s7, flag));

    sexp_str = "(hash-table \"a\" #( #(1 2) #(3 4) ))";
    expected = EVAL(sexp_str);
    TRACE_S7_DUMP("expected", expected);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, ht));
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of json map is a json-array
    k = s7_make_string(s7, "a");
    suba = APPLY_2("json:map-ref", jm, k);
    flag = APPLY_1("json:array?", suba);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of hash-table is a vector
    k = s7_make_string(s7, "a");
    subvec = APPLY_2("hash-table-ref", ht, k);
    actual = APPLY_1("vector?", subvec);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    TRACE_S7_DUMP("subvec", subvec);

    // verify length
    val = APPLY_1("vector-length", subvec);
    TEST_ASSERT_EQUAL_INT(2, s7_integer(val));

    // verify values
    val = APPLY_2("vector-ref", subvec, s7_make_integer(s7, 0));
    TRACE_S7_DUMP("elt 0", val);
    actual = APPLY_1("vector?", val);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    /* TEST_ASSERT_EQUAL_INT(1, s7_integer(val)); */
}

void nested_table_arrays(void)
{
    jm = JSON_READ("{\"a\": [{\"t1\": 1}, {\"t2\": 2}]}");
    flag = APPLY_1("json:map?", jm);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    ht = APPLY_1("json:map->hash-table", jm);
    /* TRACE_S7_DUMP("ht", ht); */
    flag = APPLY_1("hash-table?", ht);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("c-pointer?", ht);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    sexp_str = "(hash-table \"a\" (vector (hash-table \"t1\" 1) (hash-table \"t2\" 2)))";
    expected = EVAL(sexp_str);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("c-pointer?", expected);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    /* TRACE_S7_DUMP("expected", expected); */

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, ht));
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, ht, expected));
    /* TRACE_S7_DUMP("flag", flag); */
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    /* expected_s7str = APPLY_1("object->string", expected); */
    /* /\* TRACE_S7_DUMP("expected s7str", expected_s7str); *\/ */
    /* /\* log_debug("expected str: %s", expected_str); *\/ */
    /* /\* TEST_ASSERT_EQUAL_STRING(sexp, s7_string(expected_str)); *\/ */

    /* flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"), */
    /*                          s7_list(s7, 2, actual, expected)); */
    /* /\* TRACE_S7_DUMP("flag", flag); *\/ */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */

    /* // verify value at "a" of toml-table is a toml-array */
    /* k = s7_make_string(s7, "a"); */
    /* subt = APPLY_2("json:map-ref", jm, k); */
    /* flag = APPLY_1("json:array?", subt); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */

    /* // verify value at "a" of hash-table is a vector */
    /* k = s7_make_string(s7, "a"); */
    /* suba = APPLY_2("hash-table-ref", actual, k); */
    /* flag = APPLY_1("vector?", suba); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */
    /* /\* TRACE_S7_DUMP("suba", suba); *\/ */
    /* // compare it to expected */
    /* tmp = APPLY_2("hash-table-ref", expected, k); */
    /* flag = APPLY_1("vector?", tmp); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */
    /* /\* TRACE_S7_DUMP("tmp", tmp); *\/ */

    /* flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"), */
    /*                          s7_list(s7, 2, suba, tmp)); */
    /* /\* TRACE_S7_DUMP("flag", flag); *\/ */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */

    /* // verify that mbrs of vector are hash-tables */
    /* i0 = s7_make_integer(s7, 0); */
    /* subt1 = APPLY_2("vector-ref", suba, i0); */
    /* actual = APPLY_1("hash-table?", subt1); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */
    /* i1 = s7_make_integer(s7, 1); */
    /* subt2 = APPLY_2("vector-ref", suba, i1); */
    /* actual = APPLY_1("hash-table?", subt2); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */

    /* // compare to expected */
    /* tmp1 = APPLY_2("vector-ref", tmp, i0); */
    /* /\* TRACE_S7_DUMP("tmp1", tmp1); *\/ */
    /* flag = APPLY_1("hash-table?", tmp1); */
    /* /\* TRACE_S7_DUMP("flag", flag); *\/ */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */

    /* flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"), */
    /*                          s7_list(s7, 2, subt1, tmp1)); */
    /* /\* TRACE_S7_DUMP("flag", flag); *\/ */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "json");

    UNITY_BEGIN();

    RUN_TEST(int_array_to_vec);
    RUN_TEST(real_array_to_vec);
    RUN_TEST(bool_array_to_vec);
    RUN_TEST(null_array_to_vec);
    RUN_TEST(string_array_to_vec);
    RUN_TEST(mixed_array_to_vec);

    RUN_TEST(nested_int_arrays);
    RUN_TEST(nested_table_arrays);

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
