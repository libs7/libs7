#include "unity.h"
#include "config.h"
#include "utils.h"
#include "macros.h"

#include "libs7.h"

#include "common.h"

s7_scheme *s7;
s7_pointer json_read;
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

/* test cases from https://github.com/mustache/spec/blob/master/specs/comments.yml */
/* and https://github.com/mustache/spec/blob/master/specs/comments.json */

void comments_inline(void) {
    /* Comment blocks should be removed from the template. */
    /* cJSON *data; */
    /* data = cJSON_Parse("{}"); */
    /* if (!data) TEST_FAIL_MESSAGE("cJSON_Parse error"); */
    S7_RENDER_TEST(s7_f(s7),
                   "12345{{! Comment Block! }}67890",
                   "{}",
                   "1234567890");
}

void comments_multiline(void) {
    /* Multiline comments should be permitted. */
    S7_RENDER_TEST(s7_f(s7),
                   "12345{{! \n"
                   "This is a \n"
                   "multi-line comment...\n"
                   "}}67890",
                   "{}",
                   "1234567890");
}

void comments_standalone(void) {
    /* All standalone comment lines should be removed. */
    S7_RENDER_TEST(s7_f(s7),
                   "Begin.\n"
                   "{{! Comment Block! }}\n"
                   "End.\n",
                   "{}",
                   "Begin.\nEnd.\n");
}

void comments_indented_standalone(void) {
    /* All standalone comment lines should be removed. */
    S7_RENDER_TEST(s7_f(s7),
                   "Begin.\n"
                   "  {{! Indented Comment Block! }}\n"
                   "End.\n",
                   "{}",
                   "Begin.\nEnd.\n");
}

void comments_standalone_line_endings(void) {
    /* "\\r\\n\" should be considered a newline for standalone tags. */
    S7_RENDER_TEST(s7_f(s7),
                   "Begin.\n"
                   "  {{! Indented Comment Block! }}\n"
                   "End.\n",
                   "{}",
                   "Begin.\nEnd.\n");
}

void comments_standalone_without_previous_line(void) {
    /* Standalone tags should not require a newline to precede them. */
    S7_RENDER_TEST(s7_f(s7),
                   "  {{! I'm Still Standalone }}\n!",
                   "{}",
                   "!");
}

void comments_standalone_without_newline(void) {
    /* Standalone tags should not require a newline to follow them. */
    S7_RENDER_TEST(s7_f(s7),
                   "!\n  {{! I'm Still Standalone }}",
                   "{}",
                   "!\n");
}

void comments_multiline_standalone(void) {
    /* All standalone comment lines should be removed. */
    S7_RENDER_TEST(s7_f(s7),
                   "Begin.\n"
                   "{{!\nSomething's going on here...\n}}\n"
                   "End.\n",
                   "{}",
                   "Begin.\nEnd.\n");
}

void comments_indented_multiline_standalone(void) {
    /* All standalone comment lines should be removed. */
    S7_RENDER_TEST(s7_f(s7),
                   "Begin.\n"
                   "  {{!\n    Something's going on here...\n"
                   "  }}\n"
                   "End.\n",
                   "{}",
                   "Begin.\nEnd.\n");
}

void comments_indented_inline(void) {
    /* Inline comments should not strip whitespace. */
    S7_RENDER_TEST(s7_f(s7),
                   "  12 {{! 34 }}\n",
                   "{}",
                   "  12 \n");
}

void comments_surrounding_whitespace(void) {
    /* Comment removal should preserve surrounding whitespace. */
    S7_RENDER_TEST(s7_f(s7),
                   "12345 {{! Comment Block! }} 67890",
                   "{}",
                   "12345  67890");
}

void comments_variable_name_collision(void) {
    /* Comments must never render, even if variable with same name exists. */
    S7_RENDER_TEST(s7_f(s7),
                   "comments never show: >{{! comment }}<",
                   "{\n"
                   "\"! comment\": 1,\n"
                   "\"! comment \": 2,\n"
                   "\"!comment\": 3,\n"
                   "\"comment\": 4 }",
                   "comments never show: ><");
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
