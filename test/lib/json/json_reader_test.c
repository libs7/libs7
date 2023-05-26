#include "gopt.h"
#include "log.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"
#include "trace.h"

#include "common.h"

#include "libs7.h"

s7_scheme *s7;

extern struct option options[];

s7_pointer root, jo, k, keys, ja, idx;
s7_pointer flag, res, actual, expected;
s7_pointer len, m, map, v, vec;

char *expected_str;

bool verbose;
bool debug;

char *cmd;

#define DQ "\\\""

#define K(s) "\\\"" #s "\\\""

/* #define JSON_READ(s) \ */
/*     s7_apply_function(s7, s7_name_to_value(s7, "json:read"),    \ */
/*                       s7_list(s7, 1, \ */
/*                               s7_eval_c_string(s7, "\"" s "\""))); */
/*                               /\* s7_eval_c_string(s7, s))); *\/ */

/* #define APPLY_1(f, o) \ */
/*  s7_apply_function(s7, s7_name_to_value(s7, f),    \ */
/*                        s7_list(s7, 1, o)) */

/* #define APPLY_MAP(m, o) \ */
/*     s7_apply_function(s7, m, s7_list(s7, 1, o)) */

/* #define APPLY_2(f, o, k)                             \ */
/*  s7_apply_function(s7, s7_name_to_value(s7, f),    \ */
/*                    s7_list(s7, 2, o, k)) */

    /* s7_apply_function_star(s7, s7_name_to_value(s7, f), \ */
    /*                            s7_list(s7, 1, v)) */

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

/*
  (json:read)
  (json:read "foo")
  (json:read "foo")
 */
void read_api(void) {
    log_debug("read_api");
    jo = JSON_READ("{ \"m\": { \"a\": 0 }}");
    /* jo = JSON_READ("{" K(m) ": { " K(a) ": 0 }}"); */
    /* s7_pointer jstr = s7_eval_c_string(s7, js); */
    /* TRACE_S7_DUMP("jstr", jstr); */
    /* jo = s7_apply_function(s7, s7_name_to_value(s7, "json:read"), */
    /*                   s7_list(s7, 1, jstr)); */

    actual = APPLY_1("json:map?", jo);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);

    /* cmd = "" */
    /*     "(with-input-from-string " */
    /*     "    \"jo = { i = 1, s = \\\"Hello\\\" }\"" */
    /*     "    json:read)"; */
    /* actual = s7_eval_c_string(s7, cmd); */
    /* res = APPLY_1("json:map?", actual); */
    /* TEST_ASSERT_EQUAL(res, s7_t(s7)); */
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "json");

    UNITY_BEGIN();

    RUN_TEST(read_api);

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
