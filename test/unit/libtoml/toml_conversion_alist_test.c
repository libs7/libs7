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

s7_pointer flag, t, subt, k, a, suba, idx, ht;
s7_pointer sexp, expected, actual;
s7_pointer res, len, m, tmp;

char *expected_str, *actual_str, *tmp_str;
char *cmd;

bool verbose;
bool debug;


#define EVAL(s) s7_eval_c_string(s7, s)

#define TOML_READ(s) \
    s7_apply_function(s7, s7_name_to_value(s7, "toml:read"),    \
                      s7_list(s7, 1, s7_eval_c_string(s7, s)));

#define APPLY_1(f, o) \
 s7_apply_function(s7, s7_name_to_value(s7, f),    \
                       s7_list(s7, 1, o))

#define APPLY_2(f, a, b)                             \
 s7_apply_function(s7, s7_name_to_value(s7, f),    \
                   s7_list(s7, 2, a, b))

#define APPLY_3(f, a, b, c)                          \
 s7_apply_function(s7, s7_name_to_value(s7, f),    \
                   s7_list(s7, 3, a, b, c))

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
    t = TOML_READ("\"fld1 = 1\nfld2 = 2\")");
    actual = APPLY_1("toml:map->hash-table", t);
    flag = APPLY_1("hash-table?", actual);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("c-pointer?", actual);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    sexp = "(hash-table \"fld1\" 1 \"fld2\" 2)";
    expected = EVAL(sexp);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, actual));
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    actual_str = APPLY_1("object->string", actual);
    TEST_ASSERT_EQUAL_STRING(sexp, s7_string(actual_str));
}

void nested_table_to_hash_table(void)
{
    t = TOML_READ("\"a = { b = 0 }\")");
    actual = APPLY_1("toml:map->hash-table", t);
    flag = APPLY_1("hash-table?", actual);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("c-pointer?", actual);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    sexp = "(hash-table \"a\" (hash-table \"b\" 0))";
    expected = EVAL(sexp);
    flag = APPLY_1("hash-table?", expected);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"),
                             s7_list(s7, 2, expected, actual));
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    actual_str = APPLY_1("object->string", actual);
    TEST_ASSERT_EQUAL_STRING(sexp, s7_string(actual_str));

    // verify value at "a" of toml-table is a toml-table
    k = s7_make_string(s7, "a");
    subt = APPLY_2("toml:map-ref", t, k);
    flag = APPLY_1("toml:map?", subt);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // verify value at "a" of hash-table is a hash-table
    k = s7_make_string(s7, "a");
    subt = APPLY_2("hash-table-ref", actual, k);
    actual = APPLY_1("hash-table?", subt);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
}

void subarray_to_vector(void)
{
    t = TOML_READ("\"a = [1, 2, 3]\")");
    actual = APPLY_1("toml:map->hash-table", t);
    flag = APPLY_1("hash-table?", actual);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("c-pointer?", actual);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    TRACE_S7_DUMP("actual", actual);

    sexp = "(hash-table \"a\" #(1 2 3))";
    /* expected = EVAL(sexp); */
    /* flag = APPLY_1("hash-table?", expected); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */

    /* flag = s7_apply_function(s7, s7_name_to_value(s7, "equal?"), */
    /*                          s7_list(s7, 2, expected, actual)); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */

    /* actual_str = APPLY_1("object->string", actual); */
    /* TEST_ASSERT_EQUAL_STRING(sexp, s7_string(actual_str)); */

    /* // verify value at "a" of toml-table is a toml-table */
    /* k = s7_make_string(s7, "a"); */
    /* subt = APPLY_2("toml:map-ref", t, k); */
    /* flag = APPLY_1("toml:map?", subt); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */

    /* // verify value at "a" of hash-table is a hash-table */
    /* k = s7_make_string(s7, "a"); */
    /* subt = APPLY_2("hash-table-ref", actual, k); */
    /* actual = APPLY_1("hash-table?", subt); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "toml");

    UNITY_BEGIN();

    /* RUN_TEST(root_to_hash_table); */
    /* RUN_TEST(nested_table_to_hash_table); */
    RUN_TEST(subarray_to_vector);

    /* RUN_TEST(dotted_keys); */

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
