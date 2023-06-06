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

void setUp(void) {}
void tearDown(void) {}

void map_to_ht_ints(void)
{
    tt = JSON_READ("{\"fld1\": 1, \"fld2\": 2}");
    ht = APPLY_1("json:map->hash-table", tt);
    flag = APPLY_1("hash-table?", ht);
    TRACE_S7_DUMP("ht", ht);
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

void map_to_ht_reals(void)
{
    tt = JSON_READ("{\"fld1\": 1.1, \"fld2\": 2.2}");
    ht = APPLY_1("json:map->hash-table", tt);
    flag = APPLY_1("hash-table?", ht);
    TRACE_S7_DUMP("ht", ht);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    flag = APPLY_1("c-pointer?", ht);
    TEST_ASSERT_FALSE(s7_boolean(s7, flag));

    sexp_str = "(hash-table \"fld1\" 1.1 \"fld2\" 2.2)";
    expected = EVAL(sexp_str);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, ht));
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    res = APPLY_1("object->string", ht);
    TEST_ASSERT_EQUAL_STRING(sexp_str, s7_string(res));
}

void map_to_ht_bools(void)
{
    tt = JSON_READ("{\"fld1\": true, \"fld2\": false}");
    ht = APPLY_1("json:map->hash-table", tt);
    flag = APPLY_1("hash-table?", ht);
    TRACE_S7_DUMP("ht", ht);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    flag = APPLY_1("c-pointer?", ht);
    TEST_ASSERT_FALSE(s7_boolean(s7, flag));

    // NB: #f value removes key from ht
    // lookup of missing key returns #f
    // but (keys ht) will not list the key

    sexp_str = "(hash-table \"fld1\" #t)";
    expected = EVAL(sexp_str);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, ht));
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    res = APPLY_1("object->string", ht);
    TEST_ASSERT_EQUAL_STRING(sexp_str, s7_string(res));
}

void map_to_ht_nulls(void)
{
    tt = JSON_READ("{\"fld1\": true, \"fld2\": null}");
    ht = APPLY_1("json:map->hash-table", tt);
    flag = APPLY_1("hash-table?", ht);
    TRACE_S7_DUMP("ht", ht);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    flag = APPLY_1("c-pointer?", ht);
    TEST_ASSERT_FALSE(s7_boolean(s7, flag));

    // NB: #f value removes key from ht
    // lookup of missing key returns #f
    // but (keys ht) will not list the key

    sexp_str = "(hash-table \"fld1\" #t \"fld2\" ())";
    expected = EVAL(sexp_str);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, ht));
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    res = APPLY_1("object->string", ht);
    TEST_ASSERT_EQUAL_STRING(sexp_str, s7_string(res));
}

void map_to_ht_strings(void)
{
    tt = JSON_READ("{\"fld1\": \"hello\", \"fld2\": \"world\"}");
    ht = APPLY_1("json:map->hash-table", tt);
    flag = APPLY_1("hash-table?", ht);
    TRACE_S7_DUMP("ht", ht);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    flag = APPLY_1("c-pointer?", ht);
    TEST_ASSERT_FALSE(s7_boolean(s7, flag));

    sexp_str = "(hash-table \"fld1\" \"hello\" \"fld2\" \"world\")";
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
    tt = JSON_READ("{\"a\": {\"b\": 0}}");
    ht = APPLY_1("json:map->hash-table", tt);
    TRACE_S7_DUMP("ht", ht);
    flag = APPLY_1("hash-table?", ht);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    flag = APPLY_1("c-pointer?", ht);
    TEST_ASSERT_FALSE(s7_boolean(s7, flag));

    sexp_str = "(hash-table \"a\" (hash-table \"b\" 0))";
    expected = EVAL(sexp_str);
    TRACE_S7_DUMP("expected", expected);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, ht));
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    res = APPLY_1("object->string", ht);
    TRACE_S7_DUMP("res", res);
    TEST_ASSERT_EQUAL_STRING(sexp_str, s7_string(res));

    /* // verify value at "a" of toml-table (not ht) is a toml-table */
    k = s7_make_string(s7, "a");
    subt = APPLY_2("json:map-ref", tt, k);
    TRACE_S7_DUMP("subt", subt);
    flag = APPLY_1("json:map?", subt);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    // verify value at "a" of hash-table is a hash-table
    k = s7_make_string(s7, "a");
    subt = APPLY_2("hash-table-ref", ht, k);
    flag = APPLY_1("hash-table?", subt);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    // verify value at "b" of "a" hash-table is 0
    k = s7_make_string(s7, "b");
    res = APPLY_2("hash-table-ref", subt, k);
    flag = APPLY_1("number?", res);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    TEST_ASSERT_EQUAL_INT(0, s7_integer(res));
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "json");

    UNITY_BEGIN();

    RUN_TEST(map_to_ht_ints);
    RUN_TEST(map_to_ht_reals);
    RUN_TEST(map_to_ht_bools);
    RUN_TEST(map_to_ht_nulls);
    RUN_TEST(map_to_ht_strings);
    RUN_TEST(nested_table_to_hash_table);

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
