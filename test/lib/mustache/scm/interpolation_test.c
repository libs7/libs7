#include "log.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"

#include "libmustachios7_s7.h"
#include "libs7.h"

#include "common.h"
#include "trace.h"
#include "utils.h"

/* #include "libmustachios7_s7.h" */
/* #include "libs7.h" */

/* #include "trace.h" */
/* #include "common.h" */
/* #include "s7_common.h" */

#include "debug.h"

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
    d = "'()";
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Hello from {Mustache}!", actual_s);
}

void basic_interpolation(void) {
    /* Unadorned tags should interpolate content into the template. */
    t = s7_make_string(s7, "Hello, {{subject}}!");
    d = "'((:subject \"world\"))";
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Hello, world!", actual_s);

    /* /\* multimap reader macro *\/ */
    /* d = "#M((:subject \"world\"))"; */
    /* data   = s7_eval_c_string(s7, d); */
    /* actual = apply_render(s7, t, data); */
    /* actual_s = (char*)s7_string(actual); */
    /* TEST_ASSERT_EQUAL_STRING("Hello, world!", actual_s); */
    /* /\* multimap reader macro 2 *\/ */
    /* data = read_data_string(s7, "#M((:subject \"world\"))"); */
    /* actual = apply_render(s7, t, data); */
    /* actual_s = (char*)s7_string(actual); */
    /* TEST_ASSERT_EQUAL_STRING("Hello, world!", actual_s); */

    /* // HTML escaping */
    /* /\* Basic interpolation should be HTML escaped. *\/ */
    /* t = s7_make_string(s7, "These characters should be HTML escaped: {{forbidden}}"); */
    /* d = "'((:forbidden \"& \\\" < >\"))"; */
    /* data   = s7_eval_c_string(s7, d); */
    /* actual = apply_render(s7, t, data); */
    /* actual_s = (char*)s7_string(actual); */
    /* TEST_ASSERT_EQUAL_STRING("These characters should be HTML escaped: &amp; &quot; &lt; &gt;", actual_s); */

    /* // Triple mustaches */
    /* /\* Triple mustaches should interpolate without HTML escaping. *\/ */
    /* t = s7_make_string(s7, "These characters should not be HTML escaped: {{{forbidden}}}"); */
    /* d = "'((:forbidden \"& \\\" < >\"))"; */
    /* data   = s7_eval_c_string(s7, d); */
    /* actual = apply_render(s7, t, data); */
    /* actual_s = (char*)s7_string(actual); */
    /* TEST_ASSERT_EQUAL_STRING("These characters should not be HTML escaped: & \" < >", actual_s); */

    /* // Ampersand */
    /* /\* Ampersand should interpolate without HTML escaping. *\/ */
    /* t = s7_make_string(s7, "These characters should not be HTML escaped: {{&forbidden}}"); */
    /* d = "'((:forbidden \"& \\\" < >\"))"; */
    /* data   = s7_eval_c_string(s7, d); */
    /* actual = apply_render(s7, t, data); */
    /* actual_s = (char*)s7_string(actual); */
    /* TEST_ASSERT_EQUAL_STRING("These characters should not be HTML escaped: & \" < >", actual_s); */
}

void integer_interpolation(void) {
    /* Integers should interpolate seamlessly. */
    d = "'((:mph 85))";
    // basic
    t = s7_make_string(s7, "{{mph}} miles an hour!");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("85 miles an hour!", actual_s);
    // triple mustache
    t = s7_make_string(s7, "{{{mph}}} miles an hour!");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("85 miles an hour!", actual_s);
    // ampersand
    t = s7_make_string(s7, "{{&mph}} miles an hour!");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("85 miles an hour!", actual_s);
}

void decimal_interpolation(void) {
    /* Decimals should interpolate seamlessly with proper significance. */
    d = "'((:power \"1.210\"))";
    // basic
    t = s7_make_string(s7, "{{power}} jiggawatts!");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("1.210 jiggawatts!", actual_s);
    /* NB: unquoted decimals are evaluated by s7 so trailing 0s are lost */
    d = "'((:power 1.210))";
    // basic
    t = s7_make_string(s7, "{{power}} jiggawatts!");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("1.21 jiggawatts!", actual_s);
    // triple mustache
    t = s7_make_string(s7, "{{{power}}} jiggawatts!");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("1.21 jiggawatts!", actual_s);
    // ampersand
    t = s7_make_string(s7, "{{&power}} jiggawatts!");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("1.21 jiggawatts!", actual_s);
}

void null_interpolation(void) {
    /* Nulls should interpolate as the empty string. */
    d = "'((:cannot ()))";
    // basic
    t = s7_make_string(s7, "I ({{cannot}}) be seen!");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("I () be seen!", actual_s);

    // triple mustache
    t = s7_make_string(s7, "I ({{{cannot}}}) be seen!");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("I () be seen!", actual_s);
    // ampersand
    t = s7_make_string(s7, "I ({{&cannot}}) be seen!");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("I () be seen!", actual_s);
    /* caution: be careful about quoting */
    d = "'((:cannot '()))"; // not null (quoted inside a quoted expr)
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("I ('()) be seen!", actual_s);
    d = "'((:cannot #f))"; // false != null
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("I (#f) be seen!", actual_s);
}

void context_miss_interpolation(void) {
    /* Failed context lookups should default to empty strings. */
    d = "'()";
    char *expected_s = "I () be seen!";
    // basic
    t = s7_make_string(s7, "I ({{cannot}}) be seen!");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    // triple mustache
    TEST_ASSERT_EQUAL_STRING(expected_s, actual_s);
    t = s7_make_string(s7, "I ({{{cannot}}}) be seen!");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING(expected_s, actual_s);
    // ampersand
    TEST_ASSERT_EQUAL_STRING(expected_s, actual_s);
    t = s7_make_string(s7, "I ({{&cannot}}) be seen!");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING(expected_s, actual_s);
}

/* void dotted_names_basic_interpolation(void) { */
void dotted_name_interpolation(void) {
    /* The equals sign (used on both sides) should permit delimiter changes. */
    d = "'((:person ((:name Joe))))";
    // basic
    t = s7_make_string(s7, "\"{{person.name}}\" == \"{{#person}}{{name}}{{/person}}\"");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("\"Joe\" == \"Joe\"", actual_s);

    // triple mustach
    t = s7_make_string(s7, "\"{{{person.name}}}\" == \"{{#person}}{{name}}{{/person}}\"");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("\"Joe\" == \"Joe\"", actual_s);
    // ampersand
    t = s7_make_string(s7, "\"{{&person.name}}\" == \"{{#person}}{{name}}{{/person}}\"");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("\"Joe\" == \"Joe\"", actual_s);
}

void long_dotted_name_interpolation(void) {
    /* Dotted names should be functional to any level of nesting. */
    d = "'((:a ((:b ((:c ((:d ((:e ((:name Phil))))))))))))";
    t = s7_make_string(s7, "{{a.b.c.d.e.name}} == Phil");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Phil == Phil", actual_s);
    // with quoting
    t = s7_make_string(s7, "\"{{a.b.c.d.e.name}}\" == \"Phil\"");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("\"Phil\" == \"Phil\"", actual_s);

    // broken chains
    /* Any falsey value prior to the last part of the name should yield ''. */
    data   = s7_eval_c_string(s7, "'((:a ()))");
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
    data   = s7_eval_c_string(s7,
                              "'((:a ((:b ()))) "
                              "  (:c ((:name Jim))))");
    t = s7_make_string(s7, "\"{{a.b.c.name}}\" == \"\"");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("\"\" == \"\"", actual_s);

    // Initial resolution
    /* The first part of a dotted name should resolve as any other name. */
    data   = s7_eval_c_string(s7,
                              "'((:a ((:b ((:c ((:d ((:e ((:name Phil))))))))))) "
                              "  (:b ((:c ((:d ((:e ((:name Wrong))))))))))");
    t = s7_make_string(s7, "{{#a}}{{b.c.d.e.name}}{{/a}} == Phil");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Phil == Phil", actual_s);

    // Context precedence
    /* Dotted names should be resolved against former resolutions. */
    data   = s7_eval_c_string(s7,
                              "'((:a ((:b ())))"
                              "  (:b ((:c ERROR))))");
    t = s7_make_string(s7, "{{#a}}{{b.c}}{{/a}}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("", actual_s);
}

void implicit_iterators(void) {
    // Basic Interpolation
    /* Unadorned tags should interpolate content into the template. */
    data = s7_make_string(s7, "world");
    t = s7_make_string(s7, "Hello, {{.}}!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Hello, world!", actual_s);
    // HTML escaping
    /* Basic interpolation should be HTML escaped. */
    data = s7_make_string(s7, "& \" < >");
    t = s7_make_string(s7, "These characters should be HTML escaped: {{.}}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("These characters should be HTML escaped: &amp; &quot; &lt; &gt;", actual_s);
    // Triple mustaches
    /* Triple mustaches should interpolate without HTML escaping. */
    data = s7_make_string(s7, "& \" < >");
    t = s7_make_string(s7, "These characters should not be HTML escaped: {{{.}}}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("These characters should not be HTML escaped: & \" < >", actual_s);
    // Ampersand
    /* Ampersand should interpolate without HTML escaping. */
    data = s7_make_string(s7, "& \" < >");
    t = s7_make_string(s7, "These characters should not be HTML escaped: {{&.}}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("These characters should not be HTML escaped: & \" < >", actual_s);

    // Basic Integer Interpolation
    /* Integers should interpolate seamlessly. */
    data = s7_make_integer(s7, 85);
    t = s7_make_string(s7, "{{.}} miles an hour!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("85 miles an hour!", actual_s);
}

void whitespace_sensitivity(void) {
    // Surrounding whitespace
    /* Interpolation should not alter surrounding whitespace. */
    d = "'((:string \"---\"))";
    t = s7_make_string(s7, "| {{string}} |");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("| --- |", actual_s);
    // Triple mustache
    t = s7_make_string(s7, "| {{{string}}} |");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("| --- |", actual_s);
    // Ampersand
    t = s7_make_string(s7, "| {{&string}} |");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("| --- |", actual_s);

    // Standalone
    /* Standalone interpolation should not alter surrounding whitespace. */
    d = "'((:string \"---\"))";
    t = s7_make_string(s7, "  {{string}}\n");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("  ---\n", actual_s);
    // Triple mustache
    t = s7_make_string(s7, "  {{{string}}}\n");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("  ---\n", actual_s);
    // Ampersand
    t = s7_make_string(s7, "  {{&string}}\n");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("  ---\n", actual_s);
}

void whitespace_insensitivity(void) {
    // Interpolation With Padding
    /* Superfluous in-tag whitespace should be ignored. */
    d = "'((:string \"---\"))";
    t = s7_make_string(s7, "|{{ string }}|");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("|---|", actual_s);

    // Triple mustache
    t = s7_make_string(s7, "|{{{ string }}}|");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("|---|", actual_s);
    // Ampersand
    t = s7_make_string(s7, "|{{& string }}|");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("|---|", actual_s);

    // Standalone
    /* Standalone interpolation should not alter surrounding whitespace. */
    d = "'((:string \"---\"))";
    t = s7_make_string(s7, "  {{string}}\n");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("  ---\n", actual_s);
    // Triple mustache
    t = s7_make_string(s7, "  {{{string}}}\n");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("  ---\n", actual_s);
    // Ampersand
    t = s7_make_string(s7, "  {{&string}}\n");
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("  ---\n", actual_s);
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    t_hello_var = s7_make_string(s7, "Hi, {{name}}!");
    t_hello_list = s7_make_string(s7,
                                  "Hi, {{#names}}{{name}}, {{/names}}!");

    /* disable garbage collection for tests, so we don't have to bother with gc_protect */
    /* s7_gc_on(s7, s7_f(s7)); */

    UNITY_BEGIN();

    /* RUN_TEST(no_interpolation); */
    RUN_TEST(basic_interpolation);
    /* RUN_TEST(integer_interpolation); */
    /* RUN_TEST(decimal_interpolation); */
    /* RUN_TEST(null_interpolation); */
    /* RUN_TEST(context_miss_interpolation); */
    /* RUN_TEST(dotted_name_interpolation); */
    /* RUN_TEST(long_dotted_name_interpolation); */
    /* RUN_TEST(implicit_iterators); */
    /* RUN_TEST(whitespace_sensitivity); */
    /* RUN_TEST(whitespace_insensitivity); */

    int rc = UNITY_END();
    if (fflush(stdout) == EOF) {
        /*_Handle_error_*/
    }
    return rc;
}
