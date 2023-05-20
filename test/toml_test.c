#include "gopt.h"
#include "log.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"
#include "c_stacktrace.h"
#include "trace.h"

#include "common.h"

#include "libs7.h"

s7_scheme *s7;

extern struct option options[];

s7_pointer t, k, a, idx, res, actual, expected;

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

/* (define t (toml:read "m = { a = 0, b = \"B\", v = [1, 2, 3] }" )) */
/* (c-pointer? t) */
/* (define subt (toml:table-in t "m")) */
/* (toml:table-ref subt "b") */
/* (define vec (toml:table-ref subt "v")) */
/* (toml:array-kind vec) */
/* (toml:key-exists? t "a") */

/*
  (toml:read)
  (toml:read "foo")
  (toml:read "foo")
 */
void read_api(void) {
    s7_pointer t = TOML_READ("\"m = { a = 0 }\"");
    actual = APPLY_1("toml:table?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    cmd = ""
        "(with-input-from-string "
        "    \"t = { i = 1, s = \\\"Hello\\\" }\""
        "    toml:read)";
    actual = s7_eval_c_string(s7, cmd);
    res = APPLY_1("toml:table?", actual);
    TEST_ASSERT_EQUAL(res, s7_t(s7));
}

/* (define tlt (toml:read "v = [0, 1, 2]")) */
/* tlt is (a nameless) table, NOT a kv pair!  */
void top_level_tables(void) {
    /* t = TOML_READ("\"m = { a = 0, b = \\\"B\\\", v = [1, 2, 3] }\")"); */
    /* actual = APPLY_1("toml:table?", t); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */

    t = TOML_READ("\"m = [1, 2, 3]\")");
    actual = APPLY_1("toml:table?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "m");
    a = APPLY_2("toml:table-ref", t, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
}

void table_refs(void) {
    t = TOML_READ("\"m = [1, 2, 3]\")");
    actual = APPLY_1("toml:table?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "m");
    a = APPLY_2("toml:table-ref", t, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    /* apply table to key: (t k) == (toml:table-ref t k) */
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
 * so toml:table-length must sum the three: ntab + narr + nkv
 */
void table_length_ops(void) {
    char *toml = ""
        "\"m = { b = true, s = \\\"B\\\", "
        "        i = 0, f = 1.2, "
        "        t = { t1 = 1 }, a = [0, 1, 2] }\"";
    t = TOML_READ(toml);
    actual = APPLY_1("toml:table?", t);
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */

    k = s7_make_string(s7, "m");
    s7_pointer m = APPLY_2("toml:table-ref", t, k);
    actual = APPLY_1("toml:table?", m);
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */

    actual = APPLY_1("toml:table-length", m);
    TEST_ASSERT_EQUAL(6, s7_integer(actual));

    actual = APPLY_1("toml:table-nkval", m);
    TEST_ASSERT_EQUAL(4, s7_integer(actual));
    /* aliases nkval */
    actual = APPLY_1("toml:table-atomic-count", m);
    TEST_ASSERT_EQUAL(4, s7_integer(actual));

    actual = APPLY_1("toml:table-ntab", m);
    TEST_ASSERT_EQUAL(1, s7_integer(actual));
    /* aliases ntab */
    actual = APPLY_1("toml:table-subtable-count", m);
    TEST_ASSERT_EQUAL(1, s7_integer(actual));

    actual = APPLY_1("toml:table-narr", m);
    TEST_ASSERT_EQUAL(1, s7_integer(actual));
    /* aliases narr */
    actual = APPLY_1("toml:table-array-count", m);
    TEST_ASSERT_EQUAL(1, s7_integer(actual));

    /* a = s7_apply_function(s7, t, s7_list(s7, 1, k)); */
    /* actual = APPLY_1("toml:array?", a); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */

    // try hash-table-ref - nope, segfault
    /* a = APPLY_2("hash-table-ref", t, k); */
    /* actual = APPLY_1("toml:array?", a); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */
}

void arrays(void) {
    char *cmd = "(toml:read \"a = [1, 2, 3]\")";
    actual = s7_eval_c_string(s7, cmd);
    TEST_ASSERT_TRUE(s7_is_c_pointer(actual));
    TEST_ASSERT_TRUE(s7_c_pointer_type(actual) == s7_make_symbol(s7, "toml_array_t"));
    /*     sexp_expected = "\"path.txt\""; */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */
}

int main(int argc, char **argv)
{
    init_exceptions(argv[0]);

    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "toml");

    UNITY_BEGIN();

    /* RUN_TEST(read_api); */
    /* RUN_TEST(top_level_tables); */
    /* RUN_TEST(table_refs); */
    RUN_TEST(table_length_ops);
    /* RUN_TEST(table_ops); */
    /* RUN_TEST(arrays); */

    return UNITY_END();
}
