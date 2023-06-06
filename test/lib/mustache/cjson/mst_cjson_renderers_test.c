#include <unistd.h>

#include "config.h"
#include "gopt.h"
#include "log.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"

#include "utils.h"
#include "macros.h"

#include "libs7.h"

#include "common.h"

s7_scheme *s7;

s7_pointer json_read;
s7_pointer mustache_render;

extern struct option options[];

UT_string *setter;
UT_string *sexp;
bool verbose;
bool debug;

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

/* s7_flush_output_port(s7, s7_current_output_port(s7)); */
/* char *s = s7_object_to_c_string(s7, actual); */
/* log_debug("result: %s", s); */
/* free(s); */

/* mustachios tests call the C api directly: */
/* CJSON_RENDER_TEST("Hello, {{subject}}!", */
/*                  "{\"subject\": \"world\"}", */
/*                  "Hello, world!"); */
/* #define CJSON_RENDER_TEST(t, d, expected)                             \ */
/*     do { cJSON *data = cJSON_Parse(d);                                \ */
/*         if (data == 0) {                                              \ */
/*             TEST_FAIL_MESSAGE("cJSON_Parse error");                   \ */
/*             cJSON_free(data);                                      \ */
/*         } else {                                                      \ */
/*             const char *result = mustache_json_render(t, 0, data, 0);   \ */
/*             TEST_ASSERT_EQUAL_STRING(expected, result);         \ */
/*             cJSON_free(data);                                      \ */
/*             free((void*)result);                                \ */
/*         }                                                       \ */
/*     } while(0) */


/* libmustachios_s7 calls the s7 api */
/* S7_RENDER_TEST("Hello, {{subject}}!", */
/*                "{\"subject\": \"world\"}", */
/*                "Hello, world!"); */

void to_string(void) {
    S7_RENDER_TEST("Hello, {{subject}}!",
                   "{\"subject\": \"world\"}",
                   "Hello, world!");
}

/* void test_object(void) { */
/*     char *t = "Hi, {{name}}!"; */
/*     char *json_str = "{\"name\": \"Bob\"}"; */
/*     void *json_c = mustach_encode_json(json_str, strlen(json_str)); */

/*     char *actual; */
/*     size_t ct = mustach_asprintf(&actual, t, strlen(t), json_c, 0, 0); */
/*     (void)ct; */
/*     char *expected = "Hi, Bob!"; */
/*     TEST_ASSERT_EQUAL_STRING(expected, actual); */
/* } */

/* void test_single_dot(void) { */
/*     static int flags = 0; */
/*     (void)flags; */
/*     flags = Mustach_With_SingleDot; // | Mustach_With_IncPartial; */

/*     char *expected = "3.14"; */
/*     char *t = "{{#key}}{{.}}{{/key}}"; */
/*     char *json_str = "{\"key\": 3.14}"; */
/*     void *json_c = mustach_deserialize(json_str, strlen(json_str)); */
/*     char *actual; */
/*     size_t ct = mustach_asprintf(&actual, t, strlen(t), json_c, 0, 0); */
/*     (void)ct; */
/*     TEST_ASSERT_EQUAL_STRING(expected, actual); */

/*     expected = " 1 2"; */
/*     t = "{{#array}} {{.}}{{/array}}"; */
/*     json_str = "{\"array\":[1,2]}"; */
/*     json_c = mustach_deserialize(json_str, strlen(json_str)); */
/*     actual = NULL; */
/*     ct = 0, ct = mustach_asprintf(&actual, t, strlen(t), json_c, 0, 0); */
/*     TEST_ASSERT_EQUAL_STRING(expected, actual); */
/* } */

/* void test_inverted(void) { */
/*     static int flags = 0; */
/*     (void)flags; */
/*     /\* flags = Mustach_With_SingleDot; // | Mustach_With_IncPartial; *\/ */

/*     char *expected = "1, 2, "; */
/*     char *t = "{{#xs}}{{x}}, {{/xs}}"; */
/*     char *json_str = "{\"xs\": [{\"x\":1}, {\"x\":2}]}"; */
/*     void *json_c = mustach_deserialize(json_str, strlen(json_str)); */
/*     char* actual; */
/*     int ct = mustach_asprintf(&actual, t, strlen(t), json_c, 0, 0); */
/*     TEST_ASSERT_EQUAL_STRING(expected, actual); */

/*     // NB: we use ~ instead of ^ for negation */
/*     expected = "1, 2"; */
/*     t = "{{#xs}}{{x}}{{~last}}, {{/last}}{{/xs}}"; */
/*     json_str = "{\"xs\": [{\"x\":1}, {\"x\":2, \"last\":true}]}"; */
/*     json_c = mustach_deserialize(json_str, strlen(json_str)); */
/*     actual = NULL; */
/*     ct = 0, ct = mustach_asprintf(&actual, t, strlen(t), json_c, 0, 0); */
/*     TEST_ASSERT_EQUAL_STRING(expected, actual); */
/* } */

int main(int argc, char **argv)
{
    s7 = initialize("cjson_renderers_test", argc, argv);

    libs7_load_clib(s7, "mustachios");
    libs7_load_clib(s7, "toml");
    libs7_load_clib(s7, "json");

    json_read = s7_name_to_value(s7, "json:read");
    mustache_render = s7_name_to_value(s7, "mustache:render");

    /* char *script_dir = "./test"; */
    /* s7_pointer newpath; */
    /* newpath =  s7_add_to_load_path(s7, script_dir); */
    /* (void)newpath; */

    UNITY_BEGIN();

    RUN_TEST(to_string);

    return UNITY_END();
}
