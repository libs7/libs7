#include "unity.h"
#include "config.h"
#include "common.h"
#include "macros.h"
#include "libs7.h"

s7_scheme *s7;

s7_pointer mustache_render;

char *errmsg = "";
bool ht = false;
char *datamap;

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

/* test cases from https://github.com/mustache/spec/blob/master/specs/inverted.yml */
/* and https://github.com/mustache/spec/blob/master/specs/inverted.yml *\/ */

void truthy(void) {
    /* Truthy sections should have their contents rendered. */
    datamap = ht
        ? "#m(:flag #t)"
        : "'((:flag . #t))";
    S7_RENDER_TEST("{{#flag}}This should be rendered.{{/flag}}",
                   datamap,
                   "This should be rendered.");
}

void falsey(void) {
    /* Falsey sections should have their contents omitted. */
    datamap = ht
        ? "#m(:flag #f)"
        : "'((:flag . #f))";
    S7_RENDER_TEST("{{#flag}}This should not be rendered.{{/flag}}",
                   datamap,
                   "");
    /* Null is falsey */
    datamap = ht
        ? "#m(:null ())"
        : "'((:null . ()))";
    S7_RENDER_TEST("{{#null}}This should not be rendered.{{/null}}",
                   datamap,
                   "");
}

void contexts(void) {
    /* Objects and hashes should be pushed onto the context stack. */
    datamap = ht
        ? "#m(:context #m(:name Joe))"
        : "'((:context (:name . Joe)))";
    S7_RENDER_TEST("{{#context}}Hi {{name}}.{{/context}}",
                   datamap,
                   "Hi Joe.");

    // Parent contexts
    /* Names missing in the current context are looked up in the stack. */
    datamap = ht
        ? "#m(:a \"foo\" :b \"wrong\" :sec #m(:b \"bar\") :c #m(:d \"baz\"))"
        : "'((:a . \"foo\") (:b . \"wrong\") (:sec (:b . \"bar\")) (:c (:d . \"baz\")))";
    S7_RENDER_TEST("{{#sec}}{{a}}, {{b}}, {{c.d}}{{/sec}}",
                   datamap,
                   "foo, bar, baz");

    // Variable test
    /* Non-false sections have their value at the top of context,
     * accessible as {{.}} or through the parent context. This gives
     * a simple way to display content conditionally if a variable exists. */
    datamap = ht
        ? "#m(:foo \"bar\")"
        : "'((:foo . \"bar\"))";
    S7_RENDER_TEST("{{#foo}}{{.}} is {{foo}}{{/foo}}",
                   datamap,
                   "bar is bar");

    // List Contexts
    /* All elements on the context stack should be accessible within lists. */
    datamap = ht
        ? "'#m(:tops "
        "       #(#m(:tname ((:upper A) (:lower a)) "
        "            :middles #(#m(:mname 1"
        "                          :bottoms "
        "                              #(#m(:bname x) "
        "                                #m(:bname y)))))))"

        : "'((:tops . "
        "    #(((:tname (:upper . A) (:lower . a)) "
        "       (:middles . "
        "         #(((:mname . 1) "
        "            (:bottoms . "
        "              #(((:bname . x)) "
        "                ((:bname . y)))))))))))";
    errmsg = datamap;
    S7_RENDER_TEST("{{#tops}}{{#middles}}{{tname.lower}}{{mname}}.{{#bottoms}}{{tname.upper}}{{mname}}{{bname}}.{{/bottoms}}{{/middles}}{{/tops}}",
                   datamap,
                   "a1.A1x.A1y.");

    // Deeply Nested Contexts
    /* All elements on the context stack should be accessible. */
    datamap = ht
        ? "#m(:a #m(:one 1) "
        "     :b #m(:two 2) "
        "     :c #m(:three 3 "
        "           :d #m(:four 4 "
        "                 :five 5)))"
        : "'((:a ((:one . 1))) "
        "    (:b ((:two . 2))) "
        "    (:c ((:three . 3) "
        "       (:d ((:four . 4) "
        "            (:five . 5)))))) ";
    S7_RENDER_TEST("{{#a}}\n{{one}}\n{{#b}}\n{{one}}{{two}}{{one}}\n{{#c}}\n{{one}}{{two}}{{three}}{{two}}{{one}}\n{{#d}}\n{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}\n{{#five}}\n{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}\n{{one}}{{two}}{{three}}{{four}}{{.}}6{{.}}{{four}}{{three}}{{two}}{{one}}\n{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}\n{{/five}}\n{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}\n{{/d}}\n{{one}}{{two}}{{three}}{{two}}{{one}}\n{{/c}}\n{{one}}{{two}}{{one}}\n{{/b}}\n{{one}}\n{{/a}}\n",
                   datamap,
                   "1\n121\n12321\n1234321\n123454321\n12345654321\n123454321\n1234321\n12321\n121\n1\n");


    // Context Misses
    /* Failed context lookups should be considered falsey. */
    datamap = ht
        ? "#m()"
        : "'()";
    S7_RENDER_TEST("[{{#missing}}Found key 'missing'!{{/missing}}]",
                   datamap,
                   "[]");
}

void lists(void) {
    /* Lists should be iterated; list items should visit the context stack. */
    datamap = ht
        ?"#m(:list "
        "    #(#m(:item 1) "
        "      #m(:item 2) "
        "      #m(:item 3)))"
        : "'((:list . "
        "    #(((:item . 1)) "
        "      ((:item . 2)) "
        "      ((:item . 3)))))";
    S7_RENDER_TEST("\"{{#list}}{{item}}{{/list}}\"",
                   datamap,
                   "\"123\"");

    // Empty List
    /* Empty lists should behave like falsey values. */
    datamap = ht
        ? "#m(:list #())"
        : "'((:list . #()))";
    S7_RENDER_TEST("\"{{#list}}Yay lists!{{/list}}\"",
                   datamap,
                   "\"\"");
}

void doubled(void) {
    /* Multiple sections per template should be permitted. */
    datamap = ht
        ? "#m(:flag #t :two \"second\")"
        : "'((:flag . #t) (:two . \"second\"))";
    S7_RENDER_TEST("{{#flag}}\n* first\n{{/flag}}\n* {{two}}\n{{#flag}}\n* third\n{{/flag}}\n",
                   datamap,
                   "* first\n* second\n* third\n");
}

void nested(void) {
    // Nested (Truthy)
    /* Nested truthy sections should have their contents rendered. */
    datamap = ht
        ? "#m(:flag #t)"
        : "'((:flag #t))";
    S7_RENDER_TEST("| A {{#flag}}B {{#flag}}C{{/flag}} D{{/flag}} E |",
                   datamap,
                   "| A B C D E |");

    // Nested (Falsey)
    /* Nested falsey sections should be omitted. */
    datamap = ht
        ? "#m(:flag #f)"
        : "'((:flag . #f))";
    S7_RENDER_TEST("| A {{#flag}}B {{#flag}}C{{/flag}} D{{/flag}} E |",
                   datamap,
                   "| A  E |");
}

void implicit_iterators(void) {
    // Strings
    /* Implicit iterators should directly interpolate strings. */
    datamap = ht
        ? "#m(:list #(\"a\", \"b\", \"c\", \"d\", \"e\"))"
        : "'((:list . #(\"a\", \"b\", \"c\", \"d\", \"e\")))";
    S7_RENDER_TEST("\"{{#list}}({{.}}){{/list}}\"",
                   datamap,
                   "\"(a)(b)(c)(d)(e)\"");

    // Integers
    /* Implicit iterators should cast integers to strings and interpolate. */
    datamap = ht
        ? "#m(:list #(1 2 3 4 5))"
        : "'((:list . #(1 2 3 4 5)))";
    S7_RENDER_TEST("\"{{#list}}({{.}}){{/list}}\"",
                   datamap,
                   "\"(1)(2)(3)(4)(5)\"");

    // Decimals
    /* Implicit iterators should cast decimals to strings and interpolate. */
    datamap = ht
        ? "#m(:list #(1.1 2.2 3.3 4.4 5.5))"
        : "'((:list . #(1.1 2.2 3.3 4.4 5.5)))";
    S7_RENDER_TEST("\"{{#list}}({{.}}){{/list}}\"",
                   datamap,
                   "\"(1.1)(2.2)(3.3)(4.4)(5.5)\"");

    // with format string
    datamap = ht
        ? "#m(:list #(1.1 2.2 3.3 4.4 5.5))"
        : "'((:list . #(1.1 2.2 3.3 4.4 5.5)))";
    S7_RENDER_TEST("\"{{#list}}({{.%%05.2f}}){{/list}}\"",
                   datamap,
                   "\"(01.10)(02.20)(03.30)(04.40)(05.50)\"");

    // Arrays (vectors)
    /* Implicit iterators should allow iterating over nested arrays. */
    datamap = ht
        ? "#m(:list "
        "    #( "
        "     #(1 2 3) "
        "     #(\"a\" \"b\" \"c\")))"
        : "'((:list . "
        "    #( "
        "     #(1 2 3) "
        "     #(\"a\" \"b\" \"c\"))))";
    S7_RENDER_TEST("\"{{#list}}({{#.}}{{.}}{{/.}}){{/list}}\"",
                   datamap,
                   "\"(123)(abc)\"");
}

void dotted_names(void) {
    // Truthy
    /* Dotted names should be valid for Section tags. */
    datamap = ht
        ? "#m(:a #m(:b #m(:c #t)))"
        : "'((:a (:b (:c . #t))))";
    S7_RENDER_TEST("\"{{#a.b.c}}Here{{/a.b.c}}\" == \"Here\"",
                   datamap,
                   "\"Here\" == \"Here\"");

    // Falsey
    datamap = ht
        ? "#m(:a #m(:b #m(:c #f)))"
        : "'((:a ((:b ((:c #f))))))";
    S7_RENDER_TEST("\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\"",
                   datamap,
                   "\"\" == \"\"");

    // Broken Chains
    /* Dotted names that cannot be resolved should be considered falsey. */
    datamap = ht
        ? "#m(:a ())"
        : "'((:a ()))";
    S7_RENDER_TEST("\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\"",
                   datamap,
                   "\"\" == \"\"");
}

void whitespace(void) {
    // Surrounding Whitespace
    /* Sections should not alter surrounding whitespace. */
    datamap = ht
        ? "#m(:flag #t)"
        : "'((:flag #t))";
    S7_RENDER_TEST(" | {{#flag}}\t|\t{{/flag}} | \n",
                   datamap,
                   " | \t|\t | \n");

    // Internal Whitespace
    /* Sections should not alter internal whitespace. */
    datamap = ht
        ? "#m(:flag #t)"
        : "'((:flag #t))";
    S7_RENDER_TEST(" | {{#flag}} {{! Important Whitespace }}\n {{/flag}} | \n",
                   datamap,
                   " |  \n  | \n");

    // Indented Inline Sections
    /* Single-line sections should not alter surrounding whitespace. */
    datamap = ht
        ? "#m(:flag #t)"
        : "'((:flag #t))";
    S7_RENDER_TEST(" {{#flag}}YES{{/flag}}\n {{#flag}}GOOD{{/flag}}\n",
                   datamap,
                   " YES\n GOOD\n");
}

void standalone(void) {
    // Lines
    /* Standalone lines should be removed from the template. */
    datamap = ht
        ? "#m(:flag #t)"
        : "'((:flag #t))";
    S7_RENDER_TEST("| This Is\n{{#flag}}\n|\n{{/flag}}\n| A Line\n",
                   datamap,
                   "| This Is\n|\n| A Line\n");

    // Indented Standalone Lines
    /* Indented standalone lines should be removed from the template. */
    datamap = ht
        ? "#m(:flag #t)"
        : "'((:flag #t))";
    S7_RENDER_TEST("| This Is\n  {{#flag}}\n|\n  {{/flag}}\n| A Line\n",
                   datamap,
                   "| This Is\n|\n| A Line\n");

    // Standalone Line Endings
    /* \"\\r\\n\" should be considered a newline for standalone tags. */
    datamap = ht
        ? "#m(:flag #t)"
        : "'((:flag #t))";
    S7_RENDER_TEST("|\r\n{{#flag}}\r\n{{/flag}}\r\n|",
                   datamap,
                   "|\r\n|");

    // Standalone Without Previous Line
    /* Standalone tags should not require a newline to precede them. */
    datamap = ht
        ? "#m(:flag #t)"
        : "'((:flag #t))";
    S7_RENDER_TEST("  {{#flag}}\n#{{/flag}}\n/",
                   datamap,
                   "#\n/");

    // Standalone Without Newline
    /* Standalone tags should not require a newline to follow them. */
    datamap = ht
        ? "#m(:flag #t)"
        : "'((:flag #t))";
    S7_RENDER_TEST("#{{#flag}}\n/\n  {{/flag}}",
                   datamap,
                   "#\n/\n");

    // Padding
    /* Superfluous in-tag whitespace should be ignored. */
    datamap = ht
        ? "#m(:flag #t)"
        : "'((:flag #t))";
    S7_RENDER_TEST("|{{# flag }}={{/ flag }}|",
                   datamap,
                   "|=|");
}

int main(int argc, char **argv)
{
    s7 = initialize("mst_scm_comments_test", argc, argv);

    libs7_load_clib(s7, "mustachios");
    libs7_load_clib(s7, "json");
    libs7_load_clib(s7, "toml");

    mustache_render = s7_name_to_value(s7, "mustache:render");

    UNITY_BEGIN();

    ht = true;
    RUN_TEST(truthy);
    RUN_TEST(falsey);
    RUN_TEST(contexts);
    RUN_TEST(lists);
    RUN_TEST(doubled);
    RUN_TEST(nested);
    RUN_TEST(implicit_iterators);
    RUN_TEST(dotted_names);
    RUN_TEST(whitespace);
    RUN_TEST(standalone);

    ht = false;
    RUN_TEST(truthy);
    RUN_TEST(falsey);
    RUN_TEST(contexts);
    RUN_TEST(lists);
    RUN_TEST(doubled);
    RUN_TEST(nested);
    RUN_TEST(implicit_iterators);
    RUN_TEST(dotted_names);
    RUN_TEST(whitespace);
    RUN_TEST(standalone);

    int rc = UNITY_END();
    if (fflush(stdout) == EOF) {
        /* Handle error */
    }
    return rc;
}
