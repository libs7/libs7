#include "log.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"

#include "mustach_s7.h"
#include "libs7.h"

#include "common.h"
#include "trace.h"
#include "utils.h"


#ifdef DEVBUILD
#include "debug.h"
#endif

s7_scheme *s7;

UT_string *setter;
UT_string *sexp;

s7_pointer t_hello_var;
s7_pointer t_hello_list;

s7_pointer t, data, actual;
char *d = "'()", *actual_s;

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

/* test cases from https://github.com/mustache/spec/blob/master/specs/comments.yml */
/* and https://github.com/mustache/spec/blob/master/specs/comments.json */
void comments_inline(void) {
    /* Comment blocks should be removed from the template. */
    t = s7_make_string(s7, "12345{{! Comment Block! }}67890");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("1234567890", actual_s);
}

void comments_multiline(void) {
    /* Multiline comments should be permitted. */
    t = s7_make_string(s7,
                       "12345{{! \n"
                       "This is a \n"
                       "multi-line comment...\n"
                       "}}67890");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("1234567890", actual_s);
}

void comments_standalone(void) {
    /* All standalone comment lines should be removed. */
    t = s7_make_string(s7,
                       "Begin.\n"
                       "{{! Comment Block! }}\n"
                       "End.\n"),
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Begin.\nEnd.\n", actual_s);
}

void comments_indented_standalone(void) {
    /* All standalone comment lines should be removed. */
    t = s7_make_string(s7, "Begin.\n  {{! Indented Comment Block! }}\nEnd.\n");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Begin.\nEnd.\n", actual_s);
}

void comments_standalone_line_endings(void) {
    /* "\\r\\n\" should be considered a newline for standalone tags. */
    t = s7_make_string(s7, "Begin.\n  {{! Indented Comment Block! }}\nEnd.\n");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Begin.\nEnd.\n", actual_s);
}

void comments_standalone_without_previous_line(void) {
    /* Standalone tags should not require a newline to precede them. */
    t = s7_make_string(s7, "  {{! I'm Still Standalone }}\n!");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("!", actual_s);
}

void comments_standalone_without_newline(void) {
    /* Standalone tags should not require a newline to follow them. */
    t = s7_make_string(s7,  "!\n  {{! I'm Still Standalone }}");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("!\n", actual_s);
}

void comments_multiline_standalone(void) {
    /* All standalone comment lines should be removed. */
    t = s7_make_string(s7, "Begin.\n{{!\nSomething's going on here...\n}}\nEnd.\n");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Begin.\nEnd.\n", actual_s);
}

void comments_indented_multiline_standalone(void) {
    /* All standalone comment lines should be removed. */
    t = s7_make_string(s7, "Begin.\n  {{!\n    Something's going on here...\n  }}\nEnd.\n");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("Begin.\nEnd.\n", actual_s);
}

void comments_indented_inline(void) {
    /* Inline comments should not strip whitespace. */
    t = s7_make_string(s7, "  12 {{! 34 }}\n");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("  12 \n", actual_s);
}

void comments_surrounding_whitespace(void) {
    /* Comment removal should preserve surrounding whitespace. */
    t = s7_make_string(s7, "12345 {{! Comment Block! }} 67890");
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("12345  67890", actual_s);
}

void comments_variable_name_collision(void) {
    /* Comments must never render, even if variable with same name exists. */
    t = s7_make_string(s7, "comments never show: >{{! comment }}<");
    d = "'((\"! comment\" 1) "
        "  (\"! comment \" 2) "
        "  (\"!comment\" 3) "
        "  (:!comment 4) "
        "  (:comment 5)) ";
    data   = s7_eval_c_string(s7, d);
    actual = apply_render(s7, t, data);
    actual_s = (char*)s7_string(actual);
    TEST_ASSERT_EQUAL_STRING("comments never show: ><", actual_s);
}

int main(int argc, char **argv)
{
    s7 = initialize("comments", argc, argv);

    libs7_load_clib(s7, "mustachios7");

    t_hello_var = s7_make_string(s7, "Hi, {{name}}!");
    t_hello_list = s7_make_string(s7,
                                  "Hi, {{#names}}{{name}}, {{/names}}!");
    data   = s7_eval_c_string(s7, d);

    /* disable garbage collection for tests, so we don't have to bother with gc_protect */
    /* s7_gc_on(s7, s7_f(s7)); */

    UNITY_BEGIN();

    RUN_TEST(comments_inline);
    RUN_TEST(comments_multiline);
    RUN_TEST(comments_standalone);
    RUN_TEST(comments_indented_standalone);
    RUN_TEST(comments_standalone_line_endings);
    RUN_TEST(comments_standalone_without_previous_line);
    RUN_TEST(comments_standalone_without_newline);
    RUN_TEST(comments_multiline_standalone);
    RUN_TEST(comments_indented_multiline_standalone);
    RUN_TEST(comments_indented_inline);
    RUN_TEST(comments_surrounding_whitespace);
    RUN_TEST(comments_variable_name_collision);

    int rc = UNITY_END();
    if (fflush(stdout) == EOF) {
        /* Handle error */
    }
    return rc;
}
