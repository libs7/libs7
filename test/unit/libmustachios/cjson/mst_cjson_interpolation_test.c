#include "unity.h"
#include "config.h"
#include "macros.h"
#include "common.h"

#include "libs7.h"

s7_scheme *s7;
s7_pointer json_read;
s7_pointer mustache_render;

char *d, *actual_str;
char *s, *str, *expected_str;
const char *template, *result;
size_t rsz;
int rc;

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


void no_interpolation(void) {
    /* Mustache-free templates should render as-is. */
    S7_RENDER_TEST(s7_f(s7),
	"Hello from {Mustache}!", "{}", "Hello from {Mustache}!");
}

void basic_interpolation(void) {
    /* Unadorned tags should interpolate content into the template. */
    S7_RENDER_TEST(s7_f(s7),
                   "Hello, {{subject}}!",
                   "{\"subject\": \"world\"}",
                   "Hello, world!");

    // HTML escaping
    /* Basic interpolation should be HTML escaped. */
    S7_RENDER_TEST(s7_f(s7),
          "These characters should be HTML escaped: {{forbidden}}",
          "{\"forbidden\": \"& \\\" < >\"}",
          "These characters should be HTML escaped: &amp; &quot; &lt; &gt;");

    // Triple mustaches
    /* Triple mustaches should interpolate without HTML escaping. */
    S7_RENDER_TEST(s7_f(s7),
        "These characters should not be HTML escaped: {{{forbidden}}}",
        "{\"forbidden\":  \"& \\\" < >\"}",
        "These characters should not be HTML escaped: & \" < >");

    // Ampersand
    /* Ampersand should interpolate without HTML escaping. */
    S7_RENDER_TEST(s7_f(s7),
        "These characters should not be HTML escaped: {{&forbidden}}",
        "{\"forbidden\": \"& \\\" < >\"}",
        "These characters should not be HTML escaped: & \" < >");
}

void integer_interpolation(void) {
    /* Integers should interpolate seamlessly. */
    // basic
    S7_RENDER_TEST(s7_f(s7),
                   "{{mph}} miles an hour!",
                   "{\"mph\": 85}",
                   "85 miles an hour!");
    // triple mustache
    S7_RENDER_TEST(s7_f(s7),
                   "{{{mph}}} miles an hour!",
                   "{\"mph\": 85}",
                   "85 miles an hour!");

    // ampersand
    S7_RENDER_TEST(s7_f(s7),
	"{{&mph}} miles an hour!",
                     "{\"mph\": 85}", "85 miles an hour!");
}

void decimal_interpolation(void) {
    /* NB: tomlc99 truncates trailing decimal 0s */

    /* Decimals should interpolate seamlessly with proper significance. */
    // basic
    S7_RENDER_TEST(s7_f(s7),
	"{{power}} jiggawatts!",
                     "{\"power\": 1.210}",
                     "1.21 jiggawatts!");

    // triple mustache
    S7_RENDER_TEST(s7_f(s7),
	"{{{power}}} jiggawatts!",
                      "{\"power\": 1.210}",
                     "1.21 jiggawatts!");

    // ampersand
    S7_RENDER_TEST(s7_f(s7),
	"{{&power}} jiggawatts!",
                      "{\"power\": 1.210}",
                     "1.21 jiggawatts!");
}

void null_interpolation(void) {
    /* Nulls should interpolate as the empty string. */
    S7_RENDER_TEST(s7_f(s7),
	"I ({{cannot}}) be seen!",
                     "{\"cannot\": null}",
                     "I () be seen!");

    // triple mustache
    S7_RENDER_TEST(s7_f(s7),
	"I ({{{cannot}}}) be seen!",
                      "{\"cannot\": null}",
                      "I () be seen!");

    // ampersand
    S7_RENDER_TEST(s7_f(s7),
	"I ({{&cannot}}) be seen!",
                      "{\"cannot\": null}",
                      "I () be seen!");
}

void context_miss_interpolation(void) {
    /* Failed context lookups should default to empty strings. */
    S7_RENDER_TEST(s7_f(s7),
	"I ({{cannot}}) be seen!",
                     "{}",
                     "I () be seen!");

    // triple mustache
    S7_RENDER_TEST(s7_f(s7),
	"I ({{{cannot}}}) be seen!",
                      "{}",
                      "I () be seen!");

    // ampersand
    S7_RENDER_TEST(s7_f(s7),
	"I ({{&cannot}}) be seen!",
                      "{}",
                      "I () be seen!");
}

void dotted_name_basic_interpolation(void) {
    /* The equals sign (used on both sides) should permit delimiter changes. */
    // basic
    S7_RENDER_TEST(s7_f(s7),
	"{{#person}}{{name}}{{/person}}",
                      "{\"person\": {\"name\": \"Joe\"}}",
                      "Joe");

    S7_RENDER_TEST(s7_f(s7),
	"{{person.name}}",
                      "{\"person\": {\"name\": \"Joe\"}}",
                     "Joe");

    S7_RENDER_TEST(s7_f(s7),
	"{{person.name}} == {{#person}}{{name}}{{/person}}",
                      "{\"person\": {\"name\": \"Joe\"}}",
                     "Joe == Joe");

    // triple mustach
    S7_RENDER_TEST(s7_f(s7),
	"{{{person.name}}} == {{#person}}{{name}}{{/person}}",
                      "{\"person\": {\"name\": \"Joe\"}}",
                     "Joe == Joe");

    // ampersand
    S7_RENDER_TEST(s7_f(s7),
	"{{&person.name}} == {{#person}}{{name}}{{/person}}",
                      "{\"person\": {\"name\": \"Joe\"}}",
                     "Joe == Joe");
}

void long_dotted_name_interpolation(void) {
    /* Dotted names should be functional to any level of nesting. */
    S7_RENDER_TEST(s7_f(s7),
	"{{a.b.c.d.e.name}} == Phil",
                      "{\"a\": {\"b\": {\"c\": {\"d\": {\"e\": {\"name\": \"Phil\"}}}}}}",
                      "Phil == Phil");

    // broken chains
    /* Any falsey value prior to the last part of the name should yield ''. */
    S7_RENDER_TEST(s7_f(s7),
	"{{a.b.c}}\" == \"",
                      "{\"a\": {}}",
                      "\" == \"");

    // w/o quotes
    S7_RENDER_TEST(s7_f(s7),
	"{{a.b.c}} == ",
                      "{\"a\": {}}",
                      " == ");

    // Broken Chain Resolution
    /* Each part of a dotted name should resolve only against its parent. */
    S7_RENDER_TEST(s7_f(s7),
	"{{a.b.c.name}}\" == \"",
                      "{\"a\": {\"b\": {}},\n\"c\": {\"name\": \"Jim\"}}",
                      "\" == \"");

    // Initial resolution
    /* The first part of a dotted name should resolve as any other name. */
    S7_RENDER_TEST(s7_f(s7),
	"{{#a}}{{b.c.d.e.name}}{{/a}} == Phil",
                      "{\"a\": {\"b\": {\"c\": {\"d\": {\"e\": {\"name\": \"Phil\"}}}}},"
                      " \"b\": {\"c\": {\"d\": {\"e\": {\"name\": \"Wrong\"}}}}}",
                     "Phil == Phil");

    // Context precedence
    /* Dotted names should be resolved against former resolutions. */
    S7_RENDER_TEST(s7_f(s7),
	"{{#a}}{{b.c}}{{/a}}",
                     "{\"a\": {\"b\": {}},"
                     " \"b\": {\"c\": \"ERROR\"}}",
                     "");
}

void implicit_iterators(void) {
    // Basic Interpolation
    /* Unadorned tags should interpolate content into the template. */
    S7_RENDER_TEST(s7_f(s7),
	"Hello, {{.}}!",
                   "\"world\"",
                   "Hello, world!");

    /* // HTML escaping */
    /* S7_RENDER_TEST(s7_f(s7),
	"These characters should be HTML escaped: {{.}}\n", */
    /*                   "\"& \\\" < >\"", */
    /*                   "These characters should be HTML escaped: &amp; &quot; &lt; &gt;\n"); */

    /* // Triple mustache */
    /* S7_RENDER_TEST(s7_f(s7),
	"These characters should not be HTML escaped: {{{.}}}\n", */
    /*                   "\"& \\\" < >\"", */
    /*                   "These characters should not be HTML escaped: & \" < >\n"); */

    /* // Ampersand */
    /* S7_RENDER_TEST(s7_f(s7),
	"These characters should not be HTML escaped: {{&.}}\n", */
    /*                   "\"& \\\" < >\"", */
    /*                   "These characters should not be HTML escaped: & \" < >\n"); */

    /* // Basic Integer Interpolation */
    /* // Integers should interpolate seamlessly.", */
    /* S7_RENDER_TEST(s7_f(s7),
	"\"{{.}} miles an hour!\"", */
    /*                   "85", */
    /*                   "\"85 miles an hour!\""); */
}

void whitespace_sensitivity(void) {
    // Surrounding whitespace
    /* Interpolation should not alter surrounding whitespace. */
    S7_RENDER_TEST(s7_f(s7),
	"| {{string}} |",
                      "{\"string\": \"---\"}",
                      "| --- |");

    // Triple mustache
    S7_RENDER_TEST(s7_f(s7),
	"| {{{string}}} |",
                      "{\"string\": \"---\"}",
                      "| --- |");

    // Ampersand
    S7_RENDER_TEST(s7_f(s7),
	"| {{&string}} |",
                      "{\"string\": \"---\"}",
                      "| --- |");

    // Standalone
    /* Standalone interpolation should not alter surrounding whitespace. */
    S7_RENDER_TEST(s7_f(s7),
	"  {{string}}\n",
                      "{\"string\": \"---\"}",
                      "  ---\n");
    // Triple mustache
    S7_RENDER_TEST(s7_f(s7),
	"  {{{string}}}\n",
                      "{\"string\": \"---\"}",
                      "  ---\n");
    // Ampersand
    S7_RENDER_TEST(s7_f(s7),
	"  {{&string}}\n",
                      "{\"string\": \"---\"}",
                      "  ---\n");
}

void whitespace_insensitivity(void) {
    // Interpolation With Padding
    /* Superfluous in-tag whitespace should be ignored. */
    S7_RENDER_TEST(s7_f(s7),
	"|{{ string }}|",
                      "{\"string\": \"---\"}",
                      "|---|");

    // Triple mustache
    S7_RENDER_TEST(s7_f(s7),
	"|{{{ string }}}|",
                      "{\"string\": \"---\"}",
                     "|---|");
    // Ampersand
    S7_RENDER_TEST(s7_f(s7),
	"|{{& string }}|",
                      "{\"string\": \"---\"}",
                      "|---|");

    // Standalone
    /* Standalone interpolation should not alter surrounding whitespace. */
    S7_RENDER_TEST(s7_f(s7),
	"  {{string}}\n",
                      "{\"string\": \"---\"}",
                      "  ---\n");
    // Triple mustache
    S7_RENDER_TEST(s7_f(s7),
	"  {{{string}}}\n",
                      "{\"string\": \"---\"}",
                      "  ---\n");
    // Ampersand
    S7_RENDER_TEST(s7_f(s7),
	"  {{&string}}\n",
                      "{\"string\": \"---\"}",
                      "  ---\n");
}

int main(int argc, char **argv)
{
    s7 = initialize("mst_cjson_interpolation_test", argc, argv);

    libs7_load_clib(s7, "mustachios");
    /* libs7_load_clib(s7, "toml"); */
    libs7_load_clib(s7, "json");

    json_read = s7_name_to_value(s7, "json:read");
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
    RUN_TEST(implicit_iterators);
    RUN_TEST(whitespace_sensitivity);
    RUN_TEST(whitespace_insensitivity);

    int rc = UNITY_END();
    if (fflush(stdout) == EOF) {
        /*_Handle_error_*/
    }
    return rc;
}
