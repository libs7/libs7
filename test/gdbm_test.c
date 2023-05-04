#include <libgen.h>
#include <pwd.h>
#include <stdlib.h>             /* putenv */
#include <unistd.h>             /* getcwd */
#include <sys/errno.h>
#include <sys/types.h>

#include "gopt.h"
#include "log.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"

#include "common.h"

#include "libs7.h"

s7_scheme *s7;

extern struct option options[];

char *sexp_input;
char *sexp_expected;

UT_string *setter;
UT_string *sexp;
s7_pointer actual;
s7_pointer expected;

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

void test_gdbm(void) {
    s7_add_to_load_path(s7, "../libs7/scm");
    s7_load(s7, "string.scm");
    /* sexp_input = "(gdbm:version)"; */
    sexp_input = "(caddr (string-split (gdbm:version) #\\space))";
    // "GDBM version 1.23. 04/02/2022 (built Apr 30 2023 20:17:04)";
    sexp_expected = "1.23.";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_make_string(s7, sexp_expected)));

    // gdbf = gdbm_open (argv[1], 0, GDBM_READER, 0, NULL);
    // (gdbm:open filename size flags mode func)

    sexp_input = ""
        "(let ((gfile (gdbm:open \"test.gdbm\" 1024 gdbm:GDBM_NEWDB #o664 "
        "                        (lambda (str) (format *stderr* \"str: ~S~%\" str))))) "
        " (gdbm:store gfile \"1\" \"1234\" gdbm:GDBM_REPLACE) "
	" (gdbm:fetch gfile \"1\") "
        " (gdbm:close gfile)) "
        ;
    sexp_expected = "#<unspecified>";
    utstring_renew(sexp);
    utstring_printf(sexp, "%s", sexp_input);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    /* s7_flush_output_port(s7, s7_current_output_port(s7)); */
    /* char *s = s7_object_to_c_string(s7, actual); */
    /* log_debug("result: %s", s); */
    /* free(s); */
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, s7_unspecified(s7)));  // make_string(s7, sexp_expected)));
}

int main(int argc, char **argv)
{
    //FIXME: throw error if run outside of bazel
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

    set_options("gdbm", options);

    if (debug)
        print_debug_env();

    s7 = libs7_init();

    libs7_load_clib(s7, "gdbm");

    char *script_dir = "./test";
    s7_pointer newpath;
    newpath =  s7_add_to_load_path(s7, script_dir);
    (void)newpath;

    /* debugging: */
    /* s7_pointer loadpath = s7_load_path(s7); */
    /* char *s = s7_object_to_c_string(s7, loadpath); */
    /* log_debug("load path: %s", s); */
    /* free(s); */

    utstring_new(sexp);

    UNITY_BEGIN();

    RUN_TEST(test_gdbm);

    utstring_free(sexp);
    return UNITY_END();
}
