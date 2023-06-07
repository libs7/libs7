#include "unity.h"
#include "config.h"
#include "common.h"
#include "macros.h"

#include "libs7.h"

s7_scheme *s7;
s7_pointer toml_read;
s7_pointer mustache_render;

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
    TOML_RENDER_TEST("Hello from {Mustache}!", "", "Hello from {Mustache}!");
}

void basic_interpolation(void) {
    /* Unadorned tags should interpolate content into the template. */
    TOML_RENDER_TEST("Hello, {{subject}}!",
                     "subject = \"world\"",
                     "Hello, world!");

    // HTML escaping
    /* Basic interpolation should be HTML escaped. */
    TOML_RENDER_TEST(
          "These characters should be HTML escaped: {{forbidden}}",
          "forbidden = \"& \\\" < >\"",
          "These characters should be HTML escaped: &amp; &quot; &lt; &gt;");

    // Triple mustaches
    /* Triple mustaches should interpolate without HTML escaping. */
    TOML_RENDER_TEST(
        "These characters should not be HTML escaped: {{{forbidden}}}",
        "forbidden =  \"& \\\" < >\"",
        "These characters should not be HTML escaped: & \" < >");

    // Ampersand
    /* Ampersand should interpolate without HTML escaping. */
    TOML_RENDER_TEST(
        "These characters should not be HTML escaped: {{&forbidden}}",
        "forbidden = \"& \\\" < >\"",
        "These characters should not be HTML escaped: & \" < >");
}

void integer_interpolation(void) {
    /* Integers should interpolate seamlessly. */
    // basic
    TOML_RENDER_TEST(
                     "{{mph}} miles an hour!",
                     "mph = 85",
                     "85 miles an hour!");
    // triple mustache
    TOML_RENDER_TEST("{{{mph}}} miles an hour!",
                     "mph = 85", "85 miles an hour!");

    // ampersand
    TOML_RENDER_TEST("{{&mph}} miles an hour!",
                     "mph = 85", "85 miles an hour!");
}

void decimal_interpolation(void) {
    /* NB: tomlc99 truncates trailing decimal 0s */

    /* Decimals should interpolate seamlessly with proper significance. */
    // basic
    TOML_RENDER_TEST("{{power}} jiggawatts!",
                     "power = 1.210",
                     "1.21 jiggawatts!");

    // triple mustache
    TOML_RENDER_TEST("{{{power}}} jiggawatts!",
                     "power = 1.210",
                     "1.21 jiggawatts!");

    // ampersand
    TOML_RENDER_TEST("{{&power}} jiggawatts!",
                     "power = 1.210",
                     "1.21 jiggawatts!");
}

void null_interpolation(void) {
    /* Nulls should interpolate as the empty string. */
    /* TOML_RENDER_TEST("I ({{cannot}}) be seen!", */
    /*                  "cannot = {}", */
    /*                  "I () be seen!"); */

    /* // triple mustache */
    /* TOML_RENDER_TEST("I ({{{cannot}}}) be seen!", */
    /*                  "cannot = {}", */
    /*                  "I () be seen!"); */

    /* // ampersand */
    /* TOML_RENDER_TEST("I ({{&cannot}}) be seen!", */
    /*                  "cannot = {}", */
    /*                  "I () be seen!"); */

    // tomlc99 bug? does not error on unquoted kv values
    // instead treats them as null?
    TOML_RENDER_TEST("I ({{cannot}}) be seen!",
                     "cannot = asdf",
                     "I () be seen!");
}

void context_miss_interpolation(void) {
    /* Failed context lookups should default to empty strings. */
    TOML_RENDER_TEST("I ({{cannot}}) be seen!",
                     "",
                     "I () be seen!");

    // triple mustache
    TOML_RENDER_TEST("I ({{{cannot}}}) be seen!",
                     "",
                     "I () be seen!");

    // ampersand
    TOML_RENDER_TEST("I ({{&cannot}}) be seen!",
                     "",
                     "I () be seen!");
}

void dotted_name_basic_interpolation(void) {
    /* The equals sign (used on both sides) should permit delimiter changes. */
    // basic
    TOML_RENDER_TEST("{{#person}}{{name}}{{/person}}",
                     "person = { name = 'Joe' }",
                     "Joe");

    TOML_RENDER_TEST("{{person.name}}",
                     "person = { name = \"Joe\" }",
                     "Joe");

    TOML_RENDER_TEST("{{person.name}} == {{#person}}{{name}}{{/person}}",
                     "person = { name = 'Joe' }",
                     "Joe == Joe");

    // triple mustach
    TOML_RENDER_TEST("{{{person.name}}} == {{#person}}{{name}}{{/person}}",
                     "person = { name = 'Joe' }",
                     "Joe == Joe");

    // ampersand
    TOML_RENDER_TEST("{{&person.name}} == {{#person}}{{name}}{{/person}}",
                     "person = { name = 'Joe' }",
                     "Joe == Joe");
}

void long_dotted_name_interpolation(void) {
    /* Dotted names should be functional to any level of nesting. */
    TOML_RENDER_TEST("{{a.b.c.d.e.name}} == Phil",
                     "a = {b = {c = {d = {e = {name = 'Phil'}}}}}",
                     "Phil == Phil");

    // broken chains
    /* Any falsey value prior to the last part of the name should yield ''. */
    TOML_RENDER_TEST("{{a.b.c}}\" == \"",
                     "a = {}",
                     "\" == \"");

    // w/o quotes
    TOML_RENDER_TEST("{{a.b.c}} == ",
                     "a = {}",
                     " == ");

    // Broken Chain Resolution
    /* Each part of a dotted name should resolve only against its parent. */
    TOML_RENDER_TEST("{{a.b.c.name}}\" == \"",
                     "a = { b = {}}\nc = { name = 'Jim' }",
                     "\" == \"");

    // Initial resolution
    /* The first part of a dotted name should resolve as any other name. */
    TOML_RENDER_TEST("{{#a}}{{b.c.d.e.name}}{{/a}} == Phil",
                     "a = {b = {c = {d = {e = {name = 'Phil'}}}}}\n"
                     "b = {c = {d = {e = {name = 'Wrong'}}}}",
                     "Phil == Phil");

    // Context precedence
    /* Dotted names should be resolved against former resolutions. */
    TOML_RENDER_TEST("{{#a}}{{b.c}}{{/a}}",
                     "a = {b = {}}\n"
                     "b = {c = 'ERROR'}",
                     "");
}

void implicit_iterators(void) {
    // Basic Interpolation
    /* Unadorned tags should interpolate content into the template. */
    /* WARNING: does not apply to toml */
    /* TOML_RENDER_TEST("Hello, {{.}}!", */
    /*                  "world",  // invalid toml */
    /*                  "Hello, world!"); */

    /* Ditto for triple html escaping, mustaches, ampersand, and
       integers */
}

void whitespace_sensitivity(void) {
    // Surrounding whitespace
    /* Interpolation should not alter surrounding whitespace. */
    TOML_RENDER_TEST("| {{string}} |",
                     "string = \"---\"",
                     "| --- |");

    // Triple mustache
    TOML_RENDER_TEST("| {{{string}}} |",
                     "string = \"---\"",
                     "| --- |");

    // Ampersand
    TOML_RENDER_TEST("| {{&string}} |",
                     "string = \"---\"",
                     "| --- |");

    // Standalone
    /* Standalone interpolation should not alter surrounding whitespace. */
    TOML_RENDER_TEST("  {{string}}\n",
                     "string = \"---\"",
                     "  ---\n");
    // Triple mustache
    TOML_RENDER_TEST("  {{{string}}}\n",
                     "string = \"---\"",
                     "  ---\n");
    // Ampersand
    TOML_RENDER_TEST("  {{&string}}\n",
                     "string = \"---\"",
                     "  ---\n");
}

void whitespace_insensitivity(void) {
    // Interpolation With Padding
    /* Superfluous in-tag whitespace should be ignored. */
    TOML_RENDER_TEST("|{{ string }}|",
                     "string = \"---\"",
                     "|---|");

    // Triple mustache
    TOML_RENDER_TEST("|{{{ string }}}|",
                     "string = \"---\"",
                     "|---|");
    // Ampersand
    TOML_RENDER_TEST("|{{& string }}|",
                     "string = \"---\"",
                     "|---|");

    // Standalone
    /* Standalone interpolation should not alter surrounding whitespace. */
    TOML_RENDER_TEST("  {{string}}\n",
                     "string = \"---\"",
                     "  ---\n");
    // Triple mustache
    TOML_RENDER_TEST("  {{{string}}}\n",
                     "string = \"---\"",
                     "  ---\n");
/*     // Ampersand */
    TOML_RENDER_TEST("  {{&string}}\n",
                     "string = \"---\"",
                     "  ---\n");
}

int main(int argc, char **argv)
{
    s7 = initialize("mst_toml_comments_test", argc, argv);

    libs7_load_clib(s7, "mustachios");
    libs7_load_clib(s7, "toml");
    libs7_load_clib(s7, "json");

    toml_read = s7_name_to_value(s7, "toml:read");
    TRACE_S7_DUMP("toml_read", toml_read);

    mustache_render = s7_name_to_value(s7, "mustache:render");

    UNITY_BEGIN();

    RUN_TEST(no_interpolation);
    RUN_TEST(basic_interpolation);
    RUN_TEST(integer_interpolation);
    RUN_TEST(decimal_interpolation);
    RUN_TEST(null_interpolation);
    RUN_TEST(context_miss_interpolation);
    RUN_TEST(dotted_name_basic_interpolation);
    RUN_TEST(long_dotted_name_interpolation);
    RUN_TEST(whitespace_sensitivity);
    RUN_TEST(whitespace_insensitivity);

    // not applicable for toml: RUN_TEST(implicit_iterators);

    int rc = UNITY_END();
    if (fflush(stdout) == EOF) {
        /*_Handle_error_*/
    }
    return rc;
}
