// TODO: test bad args, e.g. (mustache:render 12 34)

#include "gopt.h"
#include "log.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"

#include "config.h"
#include "mustache_s7.h"
#include "libs7.h"

#include "common.h"
#include "s7_common.h"

s7_scheme *s7;

bool ht = false;
char *datamap;

extern struct option options[];

bool verbose;
bool debug;

char *t_hello = "Hi, {{name}}!";


/* s7_pointer t_hello; */
s7_pointer t, data, actual;
char *actual_s;

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    /* log_info("setup"); */
    s7_flush_output_port(s7, s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_error_port(s7));
    fflush(stderr);
    if (fflush(stdout) == EOF) {
        /* Handle error */
    }
}

void tearDown(void) {
    /* log_info("teardown"); */
    s7_flush_output_port(s7, s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_error_port(s7));
    fflush(stderr);
    if (fflush(stdout) == EOF) {
        /* Handle error */
    }
}

/* s7_flush_output_port(s7, s7_current_output_port(s7)); */
/* char *s = s7_object_to_c_string(s7, actual); */
/* log_debug("result: %s", s); */
/* free(s); */

void test_api_compile_template(void) {
    /* (let ((t (mustache:compile-template "Hi, {{:name}}"))) */
    /*  (apply t data)) */
    /* s7_pointer tp = mustach_compile_template(t); */
}

void test_api_port_default(void) {
    /* default :port #f - return string, do NOT send to port */
    datamap = ht ? "'((:name \"Bob\"))" : "(hash-table :name \"Bob\")";
    S7_RENDER_TEST(t_hello, datamap, "Hi, Bob!");
}

void test_api_port_f(void) {
    /* explicit :port #f - return string, do NOT send to port */
    data = s7_eval_c_string(s7, "(hash-table :name \"Bob\")");
    actual = apply_render(s7, t_hello, data);
    actual_s = (char*)s7_string(actual); // DO NOT free(actual_s)!
    /* s7_pointer env = make_render_env(s7, t, scm_data); */
    /* char *sexp = "(mustache:render t d :port #f)))"; */
    /* s7_pointer actual = s7_eval_c_string_with_environment(s7, sexp, env); */
    TEST_ASSERT_EQUAL_STRING("Hi, Bob!", actual_s);
    s7_flush_output_port(s7, s7_current_output_port(s7));
}

/* void test_api_port_t(void) { */
/*     /\* :port #t - send to current-output-port and return string *\/ */
/*     data = s7_eval_c_string(s7, "(hash-table :name \"Bob\")"); */
/*     actual = apply_render(s7, t_hello, data); */
/*     actual_s = (char*)s7_string(actual); // DO NOT free(actual_s)! */
/*     TEST_ASSERT_EQUAL_STRING("Hi, Bob!", actual_s); */
/*     s7_newline(s7, s7_current_output_port(s7)); */
/*     s7_flush_output_port(s7, s7_current_output_port(s7)); */
/* } */

/* void test_api_port_nil(void) { */
/*     /\* :port '() - send to current-output-port, do not return string *\/ */
/*     data = s7_eval_c_string(s7, "(hash-table :name \"Bob\")"); */
/*     actual = apply_render_port(s7, t_hello, data, s7_nil(s7)); */
/*     actual_s = (char*)s7_string(actual); // DO NOT free(actual_s)! */
/*     TEST_ASSERT_EQUAL(actual, s7_unspecified(s7)); */
/*     s7_newline(s7, s7_current_output_port(s7)); */
/*     s7_flush_output_port(s7, s7_current_output_port(s7)); */
/*     /\* DUMP("values", actual); *\/ */
/* } */

/* void test_api_port_stdout(void) { */
/*     data = s7_eval_c_string(s7, "(hash-table :name \"Bob\")"); */
/*     actual = apply_render_port(s7, t_hello, data, */
/*                                s7_name_to_value(s7, "*stdout*")); */
/*     actual_s = (char*)s7_string(actual); // DO NOT free(actual_s)! */
/*     TEST_ASSERT_EQUAL(actual, s7_unspecified(s7)); */
/*     s7_newline(s7, s7_current_output_port(s7)); */
/*     s7_flush_output_port(s7, s7_current_output_port(s7)); */
/* } */

/* void test_api_port_string(void) { */
/*     data = s7_eval_c_string(s7, "(hash-table :name \"Bob\")"); */
/*     s7_pointer string_oport = s7_open_output_string(s7); */
/*     actual = apply_render_port(s7, t_hello, data, string_oport); */
/*     /\* actual_s = (char*)s7_string(actual); // DO NOT free(actual_s)! *\/ */
/*     actual_s = (char*)s7_get_output_string(s7, string_oport); */
/*     TEST_ASSERT_EQUAL_STRING("Hi, Bob!", actual_s); */
/* } */

void test_api_with_output_to_string(void) {
    /* with-output-to-string sets current-output-port to a
       string-port, then returns string. So mustache:render must write
       to current-output-port only, so we pass :port '()
    */
    data = s7_eval_c_string(s7, "(hash-table :name \"Bob\")");
    s7_pointer env = make_render_env(s7, t_hello, data);
    char *sexp = ""
        "(with-output-to-string "
        "  (lambda () "
        "    (mustache:render t d '())))"
        ;
    s7_pointer actual = s7_eval_c_string_with_environment(s7, sexp, env);
    TEST_ASSERT_EQUAL_STRING("Hi, Bob!", s7_string(actual));
}

/* void test_api_call_with_output_string(void) { */
/*     /\* (call-with-output-string proc) */
/*        open a string port, apply proc to it, return string */
/*     *\/ */
/*     data = s7_eval_c_string(s7, "(hash-table :name \"Bob\")"); */
/*     s7_pointer env = make_render_env(s7, t_hello, data); */
/*     char *sexp = "" */
/*         "(call-with-output-string " */
/*         "  (lambda (output-string-port) " */
/*         "    (mustache:render t d :port output-string-port)))" */
/*         ; */
/*     s7_pointer actual = s7_eval_c_string_with_environment(s7, sexp, env); */
/*     TEST_ASSERT_EQUAL_STRING("Hi, Bob!", s7_string(actual)); */
/* } */

int main(int argc, char **argv)
{
    s7 = s7_initialize(argc, argv);

    UNITY_BEGIN();

    ht = true;
    RUN_TEST(test_api_port_default);
    ht = false;
    RUN_TEST(test_api_port_default);

    /* RUN_TEST(test_api_port_f); */
    /* RUN_TEST(test_api_port_t); */
    /* RUN_TEST(test_api_port_nil); */
    /* RUN_TEST(test_api_port_stdout); */
    /* RUN_TEST(test_api_port_string); */
    RUN_TEST(test_api_with_output_to_string);
    /* RUN_TEST(test_api_call_with_output_string); */

    int rc = UNITY_END();
    if (fflush(stdout) == EOF) {
        /* Handle error */
    }
    return rc;
}
