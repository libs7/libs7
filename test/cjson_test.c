#include <unistd.h>

#include "gopt.h"
#include "log.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"

#include "libs7.h"
#include "libmustachios7_s7.h"

#include "common.h"

s7_scheme *s7;

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

/*
  encoding json from string
*/
void test_encoding(void) {
    char *json_str = "{\"name\": \"Bob\"}";
    s7_pointer json
        = s7_call(s7, s7_name_to_value(s7, "json:read"),
                  s7_list(s7, 1, s7_make_string(s7, json_str)));
    log_debug("pointer? %d", s7_is_c_pointer(json));
    s7_pointer t = s7_make_string(s7, "Hi, {{name}}!");
    s7_pointer result = s7_call(s7, s7_name_to_value(s7, "mustache:render"),
                                s7_list(s7, 2, t, json));
    log_debug("xxxx");
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
    if ( !getenv("BAZEL_TEST") ) {
        log_error("This test must be run in a Bazel environment: bazel test //path/to/test (or bazel run)" );
        exit(EXIT_FAILURE);
    }

    /* log_trace("WS: %s", getenv("TEST_WORKSPACE")); */
    /* log_debug("ARGV[0]: %s", argv[0]); */
    /* log_debug("CWD: %s", getcwd(NULL, 0)); */

    argc = gopt (argv, options);
    (void)argc;
    gopt_errors (argv[0], options);

    set_options("mustachios7", options);

    if (debug) print_debug_env();

    s7 = libs7_init();

    libs7_load_clib(s7, "mustachios7");

    char *script_dir = "./test";
    s7_pointer newpath;
    newpath =  s7_add_to_load_path(s7, script_dir);
    (void)newpath;

    UNITY_BEGIN();

    RUN_TEST(test_encoding);
    /* RUN_TEST(test_object); */
    /* RUN_TEST(test_single_dot); */
    /* RUN_TEST(test_inverted); */

    return UNITY_END();
}
