#include "log.h"
#include "unity.h"
/* #include "utarray.h" */
#include "utstring.h"

#include "utils.h"

#include "libmustachios7_s7.h"
#include "libs7.h"

#include "trace.h"
#ifdef DEBUG_TRACE
#include "debug.h"
#endif

#include "common.h"
/* #include "s7_common.h" */


s7_scheme *s7;

UT_string *setter;
UT_string *sexp;

s7_pointer t_hello_var;
s7_pointer t_hello_list;

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    if (fflush(stdout) == EOF) {
        /* Handle error */
    }
}

void tearDown(void) {
    if (fflush(stdout) == EOF) {
        /* Handle error */
    }
}

s7_pointer t, data, actual;
char *d, *expected, *actual_s;

void no_interpolation(void) {
    /* Mustache-free templates should render as-is. */
    t = s7_make_string(s7, "Hello from {Mustache}!");
    data = read_json(s7, "{}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Hello from {Mustache}!", actual_s);
}

void basic_interpolation(void) {
    /* Unadorned tags should interpolate content into the template. */
    t = s7_make_string(s7, "Hello, {{subject}}!");
    data = read_json(s7, "{\"subject\": \"world\"}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Hello, world!", actual_s);

    /* multimap reader macro */
    data = read_json(s7, "{\"subject\": \"world\"}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Hello, world!", actual_s);

    /* multimap reader macro 2 */
    data = read_json(s7, "{\"subject\": \"world\"}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Hello, world!", actual_s);

    // HTML escaping
    /* Basic interpolation should be HTML escaped. */
    t = s7_make_string(s7, "These characters should be HTML escaped: {{forbidden}}");
    data = read_json(s7, "{\"forbidden\": \"& \\\" < >\"}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("These characters should be HTML escaped: &amp; &quot; &lt; &gt;", actual_s);

    // Triple mustaches
    /* Triple mustaches should interpolate without HTML escaping. */
    t = s7_make_string(s7, "These characters should not be HTML escaped: {{{forbidden}}}");
    data = read_json(s7, "{\"forbidden\": \"& \\\" < >\"}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("These characters should not be HTML escaped: & \" < >", actual_s);

    // Ampersand
    /* Ampersand should interpolate without HTML escaping. */
    t = s7_make_string(s7, "These characters should not be HTML escaped: {{&forbidden}}");
    data = read_json(s7, "{\"forbidden\": \"& \\\" < >\"}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("These characters should not be HTML escaped: & \" < >", actual_s);
}

void integer_interpolation(void) {
    /* Integers should interpolate seamlessly. */
    data = read_json(s7, "{\"mph\": 85}");
    // basic
    t = s7_make_string(s7, "{{mph}} miles an hour!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("85 miles an hour!", actual_s);
    // triple mustache
    t = s7_make_string(s7, "{{{mph}}} miles an hour!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("85 miles an hour!", actual_s);
    // ampersand
    t = s7_make_string(s7, "{{&mph}} miles an hour!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("85 miles an hour!", actual_s);
}

void decimal_interpolation(void) {
    /* Decimals should interpolate seamlessly with proper significance. */
    data = read_json(s7, "{\"power\": \"1.210\"}");
    // basic
    t = s7_make_string(s7, "{{power}} jiggawatts!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("1.210 jiggawatts!", actual_s);

    // triple mustache
    t = s7_make_string(s7, "{{{power}}} jiggawatts!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("1.210 jiggawatts!", actual_s);

    // ampersand
    t = s7_make_string(s7, "{{&power}} jiggawatts!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("1.210 jiggawatts!", actual_s);
}

void null_interpolation(void) {
    /* Nulls should interpolate as the empty string. */
    data = read_json(s7, "{\"cannot\": null}");

    // basic
    t = s7_make_string(s7, "I ({{cannot}}) be seen!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("I () be seen!", actual_s);

    // triple mustache
    t = s7_make_string(s7, "I ({{{cannot}}}) be seen!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("I () be seen!", actual_s);

    // ampersand
    t = s7_make_string(s7, "I ({{&cannot}}) be seen!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("I () be seen!", actual_s);

    // false != null
    data = read_json(s7, "{\"cannot\": false}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("I (false) be seen!", actual_s);
}

void context_miss_interpolation(void) {
    /* Failed context lookups should default to empty strings. */
    data = read_json(s7, "{}");
    char *expected_s = "I () be seen!";

    // basic
    t = s7_make_string(s7, "I ({{cannot}}) be seen!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);

    // triple mustache
    TEST_ASSERT_EQUAL_STRING(expected_s, actual_s);
    t = s7_make_string(s7, "I ({{{cannot}}}) be seen!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING(expected_s, actual_s);

    // ampersand
    TEST_ASSERT_EQUAL_STRING(expected_s, actual_s);
    t = s7_make_string(s7, "I ({{&cannot}}) be seen!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING(expected_s, actual_s);
}

void dotted_name_interpolation(void) {
    /* The equals sign (used on both sides) should permit delimiter changes. */
    data = read_json(s7, "{\"person\": {\"name\": \"Joe\"}}");
    // basic
    t = s7_make_string(s7, "\"{{person.name}}\" == \"{{#person}}{{name}}{{/person}}\"");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("\"Joe\" == \"Joe\"", actual_s);

    // triple mustach
    t = s7_make_string(s7, "\"{{{person.name}}}\" == \"{{#person}}{{name}}{{/person}}\"");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("\"Joe\" == \"Joe\"", actual_s);
    // ampersand
    t = s7_make_string(s7, "\"{{&person.name}}\" == \"{{#person}}{{name}}{{/person}}\"");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("\"Joe\" == \"Joe\"", actual_s);
}

void long_dotted_name_interpolation(void) {
    /* Dotted names should be functional to any level of nesting. */
    data = read_json(s7,
      "{\"a\": {\"b\": {\"c\": {\"d\": {\"e\": {\"name\": \"Phil\"}}}}}}");
    t = s7_make_string(s7, "{{a.b.c.d.e.name}} == Phil");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Phil == Phil", actual_s);

    // with quoting
    t = s7_make_string(s7, "\"{{a.b.c.d.e.name}}\" == \"Phil\"");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("\"Phil\" == \"Phil\"", actual_s);

    // broken chains
    /* Any falsey value prior to the last part of the name should yield ''. */
    data = read_json(s7, "{\"a\": {}}");
    t = s7_make_string(s7, "\"{{a.b.c}}\" == \"\"");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("\"\" == \"\"", actual_s);
    // w/o quotes
    t = s7_make_string(s7, "{{a.b.c}} == ");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING(" == ", actual_s);

    // Broken Chain Resolution
    /* Each part of a dotted name should resolve only against its parent. */
    data = read_json(s7,
                     "{\"a\": {\"b\": {}}, "
                     " \"c\": {\"name\": \"Jim\"}}");
    if (!s7_is_c_pointer(data)) {
        fflush(NULL);
        const char *errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
        log_error("errport: %s", errmsg);
        fflush(NULL);
        TEST_FAIL_MESSAGE(errmsg);
    }
    t = s7_make_string(s7, "\"{{a.b.c.name}}\" == \"\"");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("\"\" == \"\"", actual_s);

    // Initial resolution
    /* The first part of a dotted name should resolve as any other name. */
    data = read_json(s7,
      "{\"a\": {\"b\": {\"c\": {\"d\": {\"e\": {\"name\": \"Phil\"}}}}},"
      " \"b\": {\"c\": {\"d\": {\"e\": {\"name\": \"Wrong\"}}}}}");
    t = s7_make_string(s7, "{{#a}}{{b.c.d.e.name}}{{/a}} == Phil");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Phil == Phil", actual_s);

    // Context precedence
    /* Dotted names should be resolved against former resolutions. */
    data = read_json(s7,
                     "{\"a\": {\"b\": {}},"
                     " \"b\": {\"c\": \"ERROR\"}}");
    t = s7_make_string(s7, "{{#a}}{{b.c}}{{/a}}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("", actual_s);
}

void implicit_iterators(void) {
    // Basic Interpolation
    /* Unadorned tags should interpolate content into the template. */
    data = read_json(s7, "\"world\"");
    t = s7_make_string(s7, "Hello, {{.}}!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Hello, world!", actual_s);

    // HTML escaping
    /* Basic interpolation should be HTML escaped. */
    data = read_json(s7, "\"& \\\" < >\"");
    t = s7_make_string(s7, "These characters should be HTML escaped: {{.}}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("These characters should be HTML escaped: &amp; &quot; &lt; &gt;", actual_s);

    // Triple mustaches
    /* Triple mustaches should interpolate without HTML escaping. */
    t = s7_make_string(s7, "These characters should not be HTML escaped: {{{.}}}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("These characters should not be HTML escaped: & \" < >", actual_s);

    // Ampersand
    /* Ampersand should interpolate without HTML escaping. */
    t = s7_make_string(s7, "These characters should not be HTML escaped: {{&.}}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("These characters should not be HTML escaped: & \" < >", actual_s);

    // Basic Integer Interpolation
    /* Integers should interpolate seamlessly. */
    data = read_json(s7, "85");
    t = s7_make_string(s7, "{{.}} miles an hour!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("85 miles an hour!", actual_s);
}

void whitespace_sensitivity(void) {
    // Surrounding whitespace
    /* Interpolation should not alter surrounding whitespace. */
    data = read_json(s7, "{\"string\": \"---\"}");
    t = s7_make_string(s7, "| {{string}} |");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("| --- |", actual_s);

    // Triple mustache
    t = s7_make_string(s7, "| {{{string}}} |");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("| --- |", actual_s);
    // Ampersand
    t = s7_make_string(s7, "| {{&string}} |");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("| --- |", actual_s);

    // Standalone
    /* Standalone interpolation should not alter surrounding whitespace. */
    data = read_json(s7, "{\"string\": \"---\"}");
    t = s7_make_string(s7, "  {{string}}\n");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("  ---\n", actual_s);

    // Triple mustache
    t = s7_make_string(s7, "  {{{string}}}\n");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("  ---\n", actual_s);

    // Ampersand
    t = s7_make_string(s7, "  {{&string}}\n");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("  ---\n", actual_s);
}

void whitespace_insensitivity(void) {
    // Interpolation With Padding
    /* Superfluous in-tag whitespace should be ignored. */
    data = read_json(s7, "{\"string\": \"---\"}");
    t = s7_make_string(s7, "|{{ string }}|");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("|---|", actual_s);

    // Triple mustache
    t = s7_make_string(s7, "|{{{ string }}}|");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("|---|", actual_s);

    // Ampersand
    t = s7_make_string(s7, "|{{& string }}|");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("|---|", actual_s);

    // Standalone
    /* Standalone interpolation should not alter surrounding whitespace. */
    t = s7_make_string(s7, "  {{string}}\n");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("  ---\n", actual_s);

    // Triple mustache
    t = s7_make_string(s7, "  {{{string}}}\n");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("  ---\n", actual_s);

    // Ampersand
    t = s7_make_string(s7, "  {{&string}}\n");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("  ---\n", actual_s);
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "mustachios7");

    t_hello_var = s7_make_string(s7, "Hi, {{name}}!");
    t_hello_list = s7_make_string(s7,
                                  "Hi, {{#names}}{{name}}, {{/names}}!");

    /* disable garbage collection for tests, so we don't have to bother with gc_protect */
    /* s7_gc_on(s7, s7_f(s7)); */

    UNITY_BEGIN();

    RUN_TEST(no_interpolation);
    RUN_TEST(basic_interpolation);
    RUN_TEST(integer_interpolation);
    RUN_TEST(decimal_interpolation);
    RUN_TEST(null_interpolation);
    RUN_TEST(context_miss_interpolation);
    RUN_TEST(dotted_name_interpolation);
    RUN_TEST(long_dotted_name_interpolation);
    RUN_TEST(implicit_iterators);
    RUN_TEST(whitespace_sensitivity);
    RUN_TEST(whitespace_insensitivity);

    int rc = UNITY_END();
    if (fflush(stdout) == EOF) {
        /*_Handle_error_*/
    }
    return rc;
}
