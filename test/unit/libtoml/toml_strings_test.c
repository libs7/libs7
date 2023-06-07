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

char *expected_str;

bool verbose;
bool debug;

char *cmd;

/* #define DQ3 "\"\"\"" */
/* #define SQ3 "'''" */

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

void line_ending_backslash_file(void) {
    s7_pointer inport = s7_open_input_file(s7, "test/libtoml/data/strings.toml", "r");
    bool is_input_port = s7_is_input_port(s7, inport);
    TEST_ASSERT_EQUAL(true, is_input_port);
    /* t = TOML_READ(inport); */
    t = s7_apply_function(s7, s7_name_to_value(s7, "toml:read"),
                          s7_list(s7, 1, inport));
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "str1");
    s7_pointer str1 = APPLY_2("toml:map-ref", t, k);
    b = APPLY_1("string?", str1);
    TEST_ASSERT_EQUAL(s7_t(s7), b);
    TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.",
                             s7_string(str1));

    k = s7_make_string(s7, "str2");
    s7_pointer str2 = APPLY_2("toml:map-ref", t, k);
    b = APPLY_1("string?", str1);
    TEST_ASSERT_EQUAL(s7_t(s7), b);
    TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.",
                             s7_string(str2));
    TEST_ASSERT_EQUAL_STRING(s7_string(str1), s7_string(str2));

    k = s7_make_string(s7, "str3");
    s7_pointer str3 = APPLY_2("toml:map-ref", t, k);
    b = APPLY_1("string?", str3);
    TEST_ASSERT_EQUAL(s7_t(s7), b);
    TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.",
                             s7_string(str3));
    TEST_ASSERT_EQUAL_STRING(s7_string(str2), s7_string(str3));
}

void line_ending_backslash_inline(void) {
    t = TOML_READ("str1 = \"The quick brown fox jumps over the lazy dog.\"");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "str1");
    s7_pointer str1 = APPLY_2("toml:map-ref", t, k);
    b = APPLY_1("string?", str1);
    TEST_ASSERT_EQUAL(s7_t(s7), b);
    TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.",
                             s7_string(str1));

    /* k = s7_make_string(s7, "str2"); */
    /* s7_pointer str2 = APPLY_2("toml:map-ref", t, k); */
    /* b = APPLY_1("string?", str1); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), b); */
    /* TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.", */
    /*                          s7_string(str2)); */
    /* TEST_ASSERT_EQUAL_STRING(s7_string(str1), s7_string(str2)); */

    /* k = s7_make_string(s7, "str3"); */
    /* s7_pointer str3 = APPLY_2("toml:map-ref", t, k); */
    /* b = APPLY_1("string?", str3); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), b); */
    /* TEST_ASSERT_EQUAL_STRING("The quick brown fox jumps over the lazy dog.", */
    /*                          s7_string(str3)); */
    /* TEST_ASSERT_EQUAL_STRING(s7_string(str2), s7_string(str3)); */
}

void double_quotes(void) {
    t = TOML_READ("a = \"Hello\"");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("string?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    t = TOML_READ("a = \"Hello\tthere\"");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("string?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    TEST_ASSERT_EQUAL_STRING("Hello\tthere", s7_string(a));
}

void multiline_double_quotes(void) {
    t = TOML_READ("a = \"\"\"Hello\"\"\"");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("string?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    t = TOML_READ("a = " DQ3 "Hello\tthere" DQ3 "");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("string?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    TEST_ASSERT_EQUAL_STRING("Hello\tthere", s7_string(a));

    // swallow \n after opening delim
    char *expected_str = "The quick brown fox jumps over the lazy dog.";
    t = TOML_READ(""
        "a = "
        DQ3 "\nThe quick brown fox jumps over the lazy dog." DQ3
                  "");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("string?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    TEST_ASSERT_EQUAL_STRING(expected_str, s7_string(a));

    //WARNING: tomlc99 does not seem to handle "line ending backslash",
    // which should collapse whitespace.
    t = TOML_READ(""
        "a =" DQ3 "\nThe quick brown \\n" "\n" "\n" "fox jumps over \\n"
        "the lazy dog."
        DQ3
                  "");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("string?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    /* TEST_ASSERT_EQUAL_STRING(expected_str, s7_string(a)); */
}

void multiline_single_quotes(void) {
    t = TOML_READ("a = " SQ3 "Hello" SQ3 "");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("string?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    t = TOML_READ("a = " SQ3 "Hello\tthere" SQ3 "");
    actual = APPLY_1("toml:map?", t);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", t, k);
    actual = APPLY_1("string?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    TEST_ASSERT_EQUAL_STRING("Hello\tthere", s7_string(a));
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "toml");

    UNITY_BEGIN();

    /* RUN_TEST(line_ending_backslash_file); */
    RUN_TEST(line_ending_backslash_inline);
    /* RUN_TEST(double_quotes); */
    /* RUN_TEST(multiline_double_quotes); */
    /* RUN_TEST(multiline_single_quotes); */

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
