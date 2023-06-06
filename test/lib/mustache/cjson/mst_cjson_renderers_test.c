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
void setUp(void)
{
    /* log_info("setup"); */
}

void tearDown(void)
{
    /* log_info("teardown"); */
}

/* render API
 *
 * (mustache:render #f t d) -> string
 * (mustache:render #t t d) -> current-output-port and string
 * (mustache:render '() t d) -> current-output-port only
 * (mustache:render port t d) -> file or string port
 */
void render_to_string(void)
{
    S7_RENDER_TEST(s7_f(s7),
                   "{{msg}}, world!",
                   "{\"msg\": \"Hello\"}",
                   "Hello, world!");
}

/*
 * sink #t sends to current-output-port (cop) and returns string.
 * to test this we set cop to a string port
 */
void render_to_cop_ret_string(void)
{
    s7_pointer osp = s7_open_output_string(s7);
    s7_pointer old_cop = s7_set_current_output_port(s7, osp);
    S7_RENDER_TEST(s7_t(s7),
                   "{{msg}}, world!",
                   "{\"msg\": \"Hello\"}",
                   "Hello, world!");
    const char *s = s7_get_output_string(s7, s7_current_output_port(s7));
    TEST_ASSERT_EQUAL_STRING("Hello, world!", s);
    s7_set_current_output_port(s7, old_cop);
}

/*
 * sink '() sends to current-output-port (cop), returns NULL
 * to test this we set cop to a string port
 */
void render_to_cop_ret_NULL(void)
{
    /* const char *fmt = s7_format(s7, */
    /*                            s7_list(s7, 3, */
    /*                                    s7_nil(s7), // return NULL */
    /*                                    s7_make_string(s7, "~A~%"), */
    /*                                    s7_make_string(s7, "FOO"))); */
    /* TEST_ASSERT_EQUAL(NULL, fmt); */

    s7_pointer osp = s7_open_output_string(s7);
    s7_pointer old_cop = s7_set_current_output_port(s7, osp);
    S7_RENDER_TEST(s7_nil(s7),
                   "{{msg}}, world!",
                   "{\"msg\": \"Hello\"}",
                   s7_string(s7_nil(s7))); // macro compares strings
    const char *s = s7_get_output_string(s7, s7_current_output_port(s7));
    TEST_ASSERT_EQUAL_STRING("Hello, world!", s);
    s7_set_current_output_port(s7, old_cop);
}

void render_to_string_port(void)
{
    s7_pointer osp = s7_open_output_string(s7);
    S7_RENDER_TEST(osp,
                   "{{msg}}, world!",
                   "{\"msg\": \"Hello\"}",
                   s7_string(s7_nil(s7))); // macro compares strings
    const char *s = s7_get_output_string(s7, osp);
    TEST_ASSERT_EQUAL_STRING("Hello, world!", s);
}

void render_to_file_port(void)
{
    s7_pointer ofp = s7_open_output_file(s7, "cjson_test.out", "w");
    S7_RENDER_TEST(ofp,
                   "{{msg}}, world!",
                   "{\"msg\": \"Hello\"}",
                   s7_string(s7_nil(s7))); // macro compares strings

    s7_pointer ifp = s7_open_input_file(s7, "cjson_test.out", "r");
    if (!s7_is_input_port(s7, ifp)) {
        log_error("s7_open_input_file failed for %s?", "cjson_test.out");
    }
    log_debug("0xxxxxxxxxxxxxxxx");
    s7_pointer actual = s7_call(s7, s7_name_to_value(s7, "read-string"),
                                s7_list(s7, 2,
                                        s7_make_integer(s7, 100),
                                        ifp));
    TRACE_S7_DUMP("actual", actual);
    log_debug("2xxxxxxxxxxxxxxxx");
    TEST_ASSERT_EQUAL_STRING("Hello, world!", s7_string(actual));
}

/* **************************************************************** */
s7_pointer _render_with_error(s7_pointer sink, const char *t, const char *d)
{
    s7_pointer t7 = s7_make_string(s7, t);
    s7_pointer d7 = s7_make_string(s7, d);

    const char *sexp =""
        "(catch #t "
        "  (lambda () "
        "    (let ((datum (json:read d))) "
        "      (mustache:render sink t datum))) "
        "  (lambda (type info) "
        "    (apply format #f info))) ";
        /* "    (newline *stdout*) " */
        /* "    type)) "; */

    s7_pointer env = s7_inlet(s7,
                              s7_list(s7, 3,
                                      s7_cons(s7, s7_make_symbol(s7, "sink"), sink),
                                      s7_cons(s7, s7_make_symbol(s7, "d"), d7),
                                      s7_cons(s7, s7_make_symbol(s7, "t"), t7)));
    s7_pointer res = s7_eval_c_string_with_environment(s7, sexp, env);
    return res;
}

void bad_sinks(void) {
    s7_pointer res = _render_with_error(s7_make_integer(s7, 99),
                                        "{{msg}}, world!",
                                        "{\"msg\": \"Hello\"}");
    const char *expected = "mustache:render first argument, 99, is an integer but should be a boolean, output port, or '()";
    TEST_ASSERT_EQUAL_STRING(expected, s7_string(res));

    res = _render_with_error(s7_make_string(s7, "foo"),
                             "{{msg}}, world!",
                             "{\"msg\": \"Hello\"}");
    expected = "mustache:render first argument, \"foo\", is a string but should be a boolean, output port, or '()";
    TEST_ASSERT_EQUAL_STRING(expected, s7_string(res));

    res = _render_with_error(s7_current_input_port(s7),
                             "{{msg}}, world!",
                             "{\"msg\": \"Hello\"}");
    expected = "mustache:render first argument, *stdin*, is an input port but should be a boolean, output port, or '()";
    TEST_ASSERT_EQUAL_STRING(expected, s7_string(res));

    s7_pointer inport = s7_open_input_string(s7, "badarg: input string port");
    res = _render_with_error(inport,
                             "{{msg}}, world!",
                             "{\"msg\": \"Hello\"}");
    expected = "mustache:render first argument, #<input-string-port>, is an input port but should be a boolean, output port, or '()";
    TEST_ASSERT_EQUAL_STRING(expected, s7_string(res));

    s7_pointer lst = s7_list(s7, 1, s7_make_integer(s7, 1));
    res = _render_with_error(lst,
                             "{{msg}}, world!",
                             "{\"msg\": \"Hello\"}");
    expected = "mustache:render first argument, (1), is a pair but should be a boolean, output port, or '()";
    TEST_ASSERT_EQUAL_STRING(expected, s7_string(res));

    s7_pointer vec = s7_make_vector(s7, 1);
    res = _render_with_error(vec,
                             "{{msg}}, world!",
                             "{\"msg\": \"Hello\"}");
    expected = "mustache:render first argument, #(()), is a vector but should be a boolean, output port, or '()";
    TEST_ASSERT_EQUAL_STRING(expected, s7_string(res));

    s7_pointer ht = s7_make_hash_table(s7, 1);
    res = _render_with_error(ht,
                             "{{msg}}, world!",
                             "{\"msg\": \"Hello\"}");
    expected = "mustache:render first argument, (hash-table), is a hash-table but should be a boolean, output port, or '()";
    TEST_ASSERT_EQUAL_STRING(expected, s7_string(res));
}

int main(int argc, char **argv)
{
    s7 = initialize("cjson_renderers_test", argc, argv);

    libs7_load_clib(s7, "mustachios");
    libs7_load_clib(s7, "toml");
    libs7_load_clib(s7, "json");

    json_read = s7_name_to_value(s7, "json:read");
    mustache_render = s7_name_to_value(s7, "mustache:render");

    UNITY_BEGIN();

    /* RUN_TEST(render_to_string);         /\* (mustache:render #f t d) *\/ */
    /* RUN_TEST(render_to_cop_ret_string); /\* (mustache:render #t t d) *\/ */
    /* RUN_TEST(render_to_cop_ret_NULL);   /\* (mustache:render '() t d) *\/ */

    /* RUN_TEST(render_to_string_port); */
    RUN_TEST(render_to_file_port);

    /* RUN_TEST(bad_sinks); */

    return UNITY_END();
}
