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

s7_pointer root, m, map, k, keys, vec, vec2, idx;
s7_pointer ts, year, month, day, hour, minute, second, secfrac, offset;
s7_pointer flag, val, res, actual, expected;
s7_pointer len;

s7_pointer len, m;

char *expected_str;

bool verbose;
bool debug;

char *cmd;

    /* s7_apply_function_star(s7, s7_name_to_value(s7, f), \ */
    /*                            s7_list(s7, 1, v)) */

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

void case_a(void) {
    root = TOML_READ("best-day-ever = 1987-07-05T17:45:00Z");
    TRACE_S7_DUMP("root", root);
    flag = APPLY_1("toml:table?", root);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    k = s7_make_string(s7, "best-day-ever");
    ts = APPLY_2("toml:table-ref", root, k);
    TRACE_S7_DUMP("ts", ts);
    flag = APPLY_1("toml:datetime?", ts);
    TRACE_S7_DUMP("flag", flag);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    year = APPLY_1("toml:date-fullyear", ts);
    /* flag = APPLY_1("integer?", year); */
    /* TEST_ASSERT_TRUE(flag); */

    /* toml = "\"a = \\\"Hello\tthere\\\"\""; */
    /* t = TOML_READ(toml); */
    /* actual = APPLY_1("toml:table?", t); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */
    /* k = s7_make_string(s7, "a"); */
    /* a = APPLY_2("toml:table-ref", t, k); */
    /* actual = APPLY_1("string?", a); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */
    /* TEST_ASSERT_EQUAL_STRING("Hello\tthere", s7_string(a)); */
}


int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "toml");

    UNITY_BEGIN();

    RUN_TEST(case_a);
    /* RUN_TEST(double_quotes); */
    /* RUN_TEST(multiline_double_quotes); */
    /* RUN_TEST(multiline_single_quotes); */

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
