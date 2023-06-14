#include "unity.h"
#include "config.h"
#include "macros.h"
#include "common.h"
#include "libs7.h"
/* #include "mustache_s7.h" */

s7_scheme *s7;

s7_pointer mustache_render;

char *errmsg = "";
char *datamap;
bool ht = false;
char *empty_map;

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

void no_interpolation(void) {
    /* Mustache-free templates should render as-is. */
    S7_RENDER_TEST("Hello from {Mustache}!",
                   empty_map, "Hello from {Mustache}!");
}

void basic_interpolation(void) {
    /* Unadorned tags should interpolate content into the template. */
    datamap = ht
        ? "#m(:subject \"world\")"
        : "'((:subject . \"world\"))";
    S7_RENDER_TEST("Hello, {{subject}}!",
                   datamap, "Hello, world!");

    /* symbol (unquoted!) */
    datamap = ht
        ? "#m(:subject world)"
        : "'((:subject . world))";
    S7_RENDER_TEST("Hello, {{subject}}!",
                   datamap, "Hello, world!");

    // symbol keys
    /* datamap = ht */
    /*     ? "#m(subject world)" */
    /*     : "'((subject world))"; */
    /* S7_RENDER_TEST("Hello, {{subject}}!", */
    /*                datamap, "Hello, world!"); */

    /* multimap reader macro */
    datamap = ht
        ? "#m(:subject \"world\")"
        : "'((:subject . \"world\"))";
    S7_RENDER_TEST("Hello, {{subject}}!",
                   datamap, "Hello, world!");

    // HTML escaping
    /* Basic interpolation should be HTML escaped. */
    datamap = ht
        ? "#m(:forbidden \"& \\\" < >\")"
        : "'((:forbidden . \"& \\\" < >\"))";
    S7_RENDER_TEST("These characters should be HTML escaped: {{forbidden}}",
                   datamap,
                   "These characters should be HTML escaped: &amp; &quot; &lt; &gt;");

    // Triple mustaches
    /* Triple mustaches should interpolate without HTML escaping. */
    datamap = ht
        ? "#m(:forbidden \"& \\\" < >\")"
        : "'((:forbidden . \"& \\\" < >\"))";
    S7_RENDER_TEST("These characters should not be HTML escaped: {{{forbidden}}}",
                   datamap,
                   "These characters should not be HTML escaped: & \" < >");

    // Ampersand
    /* Ampersand should interpolate without HTML escaping. */
    S7_RENDER_TEST("These characters should not be HTML escaped: {{&forbidden}}",
                   datamap,
                   "These characters should not be HTML escaped: & \" < >");
}

void integer_interpolation(void) {
    /* Integers should interpolate seamlessly. */
    datamap = "'((:mph 85))";
    S7_RENDER_TEST("{{mph}} miles an hour!", datamap, "(85) miles an hour!");

    datamap = ht
        ? "#m(:mph 85)"
        : "'((:mph . 85))";
    S7_RENDER_TEST("{{mph}} miles an hour!", datamap, "85 miles an hour!");

    // triple mustache
    S7_RENDER_TEST("{{{mph}}} miles an hour!", datamap, "85 miles an hour!");

    // ampersand
    S7_RENDER_TEST("{{&mph}} miles an hour!", datamap, "85 miles an hour!");
}

void decimal_interpolation(void) {
    /* Decimals should interpolate seamlessly with proper significance. */
    datamap = ht
        ? "#m(:power \"1.210\")"
        : "'((:power . \"1.210\"))";
    S7_RENDER_TEST("{{power}} jiggawatts!", datamap, "1.210 jiggawatts!");

    // basic
    S7_RENDER_TEST("{{power}} jiggawatts!", datamap, "1.210 jiggawatts!");

    // triple mustache
    S7_RENDER_TEST("{{{power}}} jiggawatts!", datamap, "1.210 jiggawatts!");

    // ampersand
    S7_RENDER_TEST("{{&power}} jiggawatts!", datamap, "1.210 jiggawatts!");
}

void null_interpolation(void) {
    /* Nulls should interpolate as the empty string. */
    // basic
    datamap = ht
        ? "#m(:cannot ())"
        : "'((:cannot ()))";

    S7_RENDER_TEST("I ({{cannot}}) be seen!",
                   datamap,
                   "I () be seen!");

    // triple mustache
    S7_RENDER_TEST("I ({{{cannot}}}) be seen!",
                   datamap, "I () be seen!");

    // ampersand
    S7_RENDER_TEST("I ({{&cannot}}) be seen!",
                   datamap, "I () be seen!");

    /* caution: be careful about quoting */
    // not null (quoted inside a quoted expr)
    S7_RENDER_TEST("I ({{&cannot}}) be seen!",
                   "#m(:cannot '())",
                   "I ('()) be seen!");
    S7_RENDER_TEST("I ({{&cannot}}) be seen!",
                   "'((:cannot . '()))",
                   "I ('()) be seen!");
    S7_RENDER_TEST("I ({{&cannot}}) be seen!",
                   "'((:cannot '()))",
                   "I (('())) be seen!");

     // false falsey? depends on type, alist v. ht
    S7_RENDER_TEST("I ({{&cannot}}) be seen!",
                   "#m(:cannot #f)", "I () be seen!");
    S7_RENDER_TEST("I ({{&cannot}}) be seen!",
                   "'((:cannot . #f))", "I (#f) be seen!");
    S7_RENDER_TEST("I ({{&cannot}}) be seen!",
                   "'((:cannot #f))", "I ((#f)) be seen!");
}

void context_miss_interpolation(void) {
    /* Failed context lookups should default to empty strings. */
    // basic
    S7_RENDER_TEST("I ({{cannot}}) be seen!", empty_map, "I () be seen!");

    // triple mustache
    S7_RENDER_TEST("I ({{{cannot}}}) be seen!", empty_map, "I () be seen!");

    // ampersand
    S7_RENDER_TEST("I ({{&cannot}}) be seen!", empty_map, "I () be seen!");
}

void dotted_name_interpolation(void) {
    /* The equals sign (used on both sides) should permit delimiter changes. */
    // basic
    datamap = ht
        ? "#m(:person #m(:name Joe))"
        : "'((:person (:name . Joe)))";
    S7_RENDER_TEST("\"{{person.name}}\" == \"{{#person}}{{name}}{{/person}}\"",
                   datamap,
                   "\"Joe\" == \"Joe\"");

    /* // triple mustach */
    S7_RENDER_TEST("\"{{{person.name}}}\" == \"{{#person}}{{name}}{{/person}}\"",
                   datamap,
                   "\"Joe\" == \"Joe\"");

    // ampersand
    S7_RENDER_TEST("\"{{&person.name}}\" == \"{{#person}}{{name}}{{/person}}\"",
                   datamap,
                   "\"Joe\" == \"Joe\"");
}

void long_dotted_name_interpolation(void) {
    /* Dotted names should be functional to any level of nesting. */
    // w/o quoting
    datamap = ht
        ? "#m(:a #m(:b #m(:c #m(:d #m(:e #m(:name Phil))))))"
        : "'((:a (:b (:c (:d (:e (:name . Phil))))))))))))";
    S7_RENDER_TEST("{{a.b.c.d.e.name}} == Phil",
                   datamap,
                   "Phil == Phil");

    // with quoting
    datamap = ht
        ? "#m(:a #m(:b #m(:c #m(:d #m(:e #m(:name \"Phil\"))))))"
        : "'((:a (:b (:c (:d (:e (:name . \"Phil\"))))))))))))";
    S7_RENDER_TEST("\"{{a.b.c.d.e.name}}\" == \"Phil\"",
                   datamap,
                   "\"Phil\" == \"Phil\"");

    // broken chains
    /* Any falsey value prior to the last part of the name should yield ''. */
    datamap = ht
        ? "#m(:a ())"
        : "'((:a ()))";
    S7_RENDER_TEST("\"{{a.b.c}}\" == \"\"", datamap, "\"\" == \"\"");

    // w/o quotes
    S7_RENDER_TEST("{{a.b.c}} == ", datamap, " == ");

    // Broken Chain Resolution
    /* Each part of a dotted name should resolve only against its parent. */
    datamap = ht
        ? "#m(:a #m(:b ()) "
        "     :c #m(:name Jim))"
        : "'((:a ((:b ()))) "
        "  (:c ((:name Jim))))";
    S7_RENDER_TEST("\"{{a.b.c.name}}\" == \"\"",
                   datamap,
                   "\"\" == \"\"");

    // Initial resolution
    /* The first part of a dotted name should resolve as any other name. */
    datamap = ht
        ? "'#m(:a #m(:b #m(:c #m(:d #m(:e #m(:name Phil))))) "
        "      :b #m(:c #m(:d #m(:e #m(:name Wrong)))))"
        : "'((:a (:b (:c (:d (:e (:name . Phil))))))))))) "
        "    (:b (:c (:d (:e (:name . Wrong))))))))))";
    S7_RENDER_TEST("{{#a}}{{b.c.d.e.name}}{{/a}} == Phil",
                   datamap,
                   "Phil == Phil");

    // Context precedence
    /* Dotted names should be resolved against former resolutions. */
    datamap = ht
        ? "#m(:a #m(:b ())"
        "     :b #m(:c ERROR))"
        : "'((:a ((:b ())))"
        "  (:b ((:c ERROR))))";

    S7_RENDER_TEST("{{#a}}{{b.c}}{{/a}}",
                   datamap,
                   "");

}

void implicit_iterators(void) {
    // Basic Interpolation
    /* Unadorned tags should interpolate content into the template. */
    S7_RENDER_TEST("Hello, {{.}}!", "\"world\"", "Hello, world!");

    // HTML escaping
    /* Basic interpolation should be HTML escaped. */
    S7_RENDER_TEST("These characters should be HTML escaped: {{.}}",
                   "\"& \\\" < >\"",
                   "These characters should be HTML escaped: &amp; &quot; &lt; &gt;");

    // Triple mustaches
    /* Triple mustaches should interpolate without HTML escaping. */
    S7_RENDER_TEST("These characters should not be HTML escaped: {{{.}}}",
                   "\"& \\\" < >\"",
                   "These characters should not be HTML escaped: & \" < >");

    // Ampersand
    /* Ampersand should interpolate without HTML escaping. */
    S7_RENDER_TEST("These characters should not be HTML escaped: {{&.}}",
                   "\"& \\\" < >\"",
                   "These characters should not be HTML escaped: & \" < >");
    // Basic Integer Interpolation
    /* Integers should interpolate seamlessly. */
    S7_RENDER_TEST("{{.}} miles an hour!", "85", "85 miles an hour!");
}

void whitespace_sensitivity(void) {
    datamap = ht
        ? "#m(:string \"---\")"
        : "'((:string . \"---\"))";

    // Surrounding whitespace
    /* Interpolation should not alter surrounding whitespace. */
    S7_RENDER_TEST("| {{string}} |", datamap, "| --- |");

    // Triple mustache
    S7_RENDER_TEST("| {{{string}}} |", datamap, "| --- |");

    // Ampersand
    S7_RENDER_TEST("| {{&string}} |", datamap, "| --- |");

    // Standalone
    /* Standalone interpolation should not alter surrounding whitespace. */
    S7_RENDER_TEST("  {{string}}\n", datamap, "  ---\n");

    // Triple mustache
    S7_RENDER_TEST("  {{{string}}}\n", datamap, "  ---\n");

    // Ampersand
    S7_RENDER_TEST("  {{&string}}\n", datamap, "  ---\n");
}

void whitespace_insensitivity(void) {
    // Interpolation With Padding
    /* Superfluous in-tag whitespace should be ignored. */
    S7_RENDER_TEST("|{{ string }}|", datamap, "|---|");

    // Triple mustache
    S7_RENDER_TEST("|{{{ string }}}|", datamap, "|---|");

    // Ampersand
    S7_RENDER_TEST("|{{& string }}|", datamap, "|---|");

    // Standalone
    /* Standalone interpolation should not alter surrounding whitespace. */
    S7_RENDER_TEST("  {{string}}\n", datamap, "  ---\n");

    // Triple mustache
    S7_RENDER_TEST("  {{{string}}}\n", datamap, "  ---\n");

    // Ampersand
    S7_RENDER_TEST("  {{&string}}\n", datamap, "  ---\n");
}

int main(int argc, char **argv)
{
    s7 = initialize("mst_scm_interpolation_test", argc, argv);

    libs7_load_clib(s7, "mustachios");
    libs7_load_clib(s7, "json");
    libs7_load_clib(s7, "toml");

    mustache_render = s7_name_to_value(s7, "mustache:render");

    UNITY_BEGIN();

    ht = true;
    empty_map = "#m()";
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

    ht = false;
    empty_map = "'()";
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
