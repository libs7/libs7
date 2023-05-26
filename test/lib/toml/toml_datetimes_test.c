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
s7_pointer ts, year, month, day, hour, minute, second, millis, offset;
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

void fields(void) {
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

    year = APPLY_1("toml:date-year", ts);
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

void apply_datetime(void) {
    root = TOML_READ("dt = 1987-07-05T17:45:00Z");
    TRACE_S7_DUMP("root", root);
    flag = APPLY_1("toml:table?", root);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    k = s7_make_string(s7, "dt");
    ts = APPLY_2("toml:table-ref", root, k);
    TRACE_S7_DUMP("ts", ts);
    flag = APPLY_1("toml:datetime?", ts);
    TRACE_S7_DUMP("flag", flag);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    k = s7_make_string(s7, "year");
    year = APPLY_2("toml:datetime-ref", ts, k);
    flag = APPLY_1("integer?", year);
    TEST_ASSERT_TRUE(flag);

    year = APPLY_OBJ(ts, k);
    TRACE_S7_DUMP("year", year);
    flag = APPLY_1("integer?", year);
    TRACE_S7_DUMP("flag", flag);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    TEST_ASSERT_EQUAL_INT(1987, s7_integer(year));

    k = s7_make_string(s7, "month");
    month = APPLY_OBJ(ts, k);
    flag = APPLY_1("integer?", month);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    TEST_ASSERT_EQUAL_INT(7, s7_integer(month));

    k = s7_make_string(s7, "day");
    day = APPLY_OBJ(ts, k);
    flag = APPLY_1("integer?", day);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    TEST_ASSERT_EQUAL_INT(05, s7_integer(day));

    k = s7_make_string(s7, "hour");
    hour = APPLY_OBJ(ts, k);
    flag = APPLY_1("integer?", hour);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    TEST_ASSERT_EQUAL_INT(17, s7_integer(hour));

    k = s7_make_string(s7, "minute");
    minute = APPLY_OBJ(ts, k);
    flag = APPLY_1("integer?", minute);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    TEST_ASSERT_EQUAL_INT(45, s7_integer(minute));

    k = s7_make_string(s7, "second");
    second = APPLY_OBJ(ts, k);
    flag = APPLY_1("integer?", second);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    TEST_ASSERT_EQUAL_INT(00, s7_integer(second));

    k = s7_make_string(s7, "offset");
    res = APPLY_OBJ(ts, k);
    flag = APPLY_1("string?", res);
    TEST_ASSERT_EQUAL_STRING("Z", s7_string(res));
    /* 1987-07-05T17:45:00Z */
}

void offset_date_time(void) {
    root = TOML_READ("odt1 = 1979-05-27T07:32:00Z\n"
                     "odt2 = 1979-05-27T00:32:00-07:00\n"
                     "odt3 = 1979-05-27T00:32:00.999999-17:00\n"
                     "odt4 = 1979-05-27 07:32:00Z");
    TRACE_S7_DUMP("root", root);
    /* flag = APPLY_1("toml:table?", root); */
    /* TEST_ASSERT_TRUE(s7_boolean(s7, flag)); */

    /* k = s7_make_string(s7, "odt1"); */
    /* ts = APPLY_OBJ(root, k); */
    /* TRACE_S7_DUMP("ts", ts); */
    /* flag = APPLY_1("toml:datetime?", ts); */
    /* TRACE_S7_DUMP("flag", flag); */
    /* TEST_ASSERT_TRUE(s7_boolean(s7, flag)); */

    /* // "odt3 = 1979-05-27T00:32:00.999999-07:00\n" */
    /* k = s7_make_string(s7, "odt3"); */
    /* ts = APPLY_OBJ(root, k); */
    /* TRACE_S7_DUMP("ODT3", ts); */
    /* flag = APPLY_1("toml:datetime?", ts); */
    /* TRACE_S7_DUMP("flag", flag); */
    /* TEST_ASSERT_TRUE(s7_boolean(s7, flag)); */
    /* k = s7_make_string(s7, "millisecond"); */
    /* millis = APPLY_OBJ(ts, k); */
    /* TRACE_S7_DUMP("millis", millis); */
    /* /\* TEST_ASSERT_EQUAL_INT(00, s7_integer(second)); *\/ */

    /* k = s7_make_string(s7, "offset"); */
    /* offset = APPLY_OBJ(ts, k); */
    /* TRACE_S7_DUMP("offset", offset); */
    /* flag = APPLY_1("string?", offset); */
    /* /\* TRACE_S7_DUMP("flag", flag); *\/ */
    /* TEST_ASSERT_TRUE(s7_boolean(s7, flag)); */
}

/* local date-time: offset omitted */
void local_date_time(void) {
    root = TOML_READ("ldt1 = 1979-05-27T07:32:00\n"
                     "ldt2 = 1979-05-27T00:32:00.999999");
    TRACE_S7_DUMP("root", root);
    /* flag = APPLY_1("toml:table?", root); */
    /* TEST_ASSERT_TRUE(s7_boolean(s7, flag)); */
    /* k = s7_make_string(s7, "ldt1"); */
    /* ts = APPLY_2("toml:table-ref", root, k); */
    /* TRACE_S7_DUMP("ts", ts); */
    /* flag = APPLY_1("toml:datetime?", ts); */
    /* TRACE_S7_DUMP("flag", flag); */
    /* TEST_ASSERT_TRUE(s7_boolean(s7, flag)); */
}

void local_date(void) {
    root = TOML_READ("ld1 = 1979-05-27");
    k = s7_make_string(s7, "ld1");
    ts = APPLY_OBJ(root, k);
    TRACE_S7_DUMP("ts", ts);
    flag = APPLY_1("toml:datetime?", ts);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
}

void local_time(void) {
    root = TOML_READ("lt1 = 07:32:00\n"
                     "lt2 = 00:32:00.999999");
    TRACE_S7_DUMP("root", root);
    flag = APPLY_1("toml:table?", root);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    k = s7_make_string(s7, "ldt1");
    ts = APPLY_2("toml:table-ref", root, k);
    TRACE_S7_DUMP("ts", ts);
    flag = APPLY_1("toml:datetime?", ts);
    TRACE_S7_DUMP("flag", flag);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "toml");

    UNITY_BEGIN();

    RUN_TEST(fields);
    /* RUN_TEST(apply_datetime); */
    /* RUN_TEST(offset_date_time); */
    /* RUN_TEST(local_date_time); */
    /* RUN_TEST(local_date); */
    /* RUN_TEST(local_time); */

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
