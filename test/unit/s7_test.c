/* tests extracted from ffitest.c */

#include "gopt.h"
#include "liblogc.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"

#include "libs7.h"

#include "s7plugin_test_config.h"

s7_scheme *s7;

extern struct option options[];

char *sexp_input;
char *sexp_expected;

UT_string *setter;
UT_string *sexp;
s7_pointer actual;
s7_pointer expected;

int  s7plugin_verbosity = 0;
extern int  libs7_verbosity;
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

s7_pointer p;
s7_int i, gc_loc;

void test_booleans(void) {
    TEST_ASSERT_TRUE(  s7_is_null(s7, s7_nil(s7)) );
    TEST_ASSERT_FALSE( s7_is_pair(s7_nil(s7)) );
    TEST_ASSERT_TRUE(  s7_is_boolean(s7_t(s7)));
    TEST_ASSERT_TRUE(  s7_is_boolean(s7_f(s7)));
    TEST_ASSERT_FALSE( s7_boolean(s7, s7_f(s7)) );
    TEST_ASSERT_TRUE(  s7_boolean(s7, s7_t(s7)) );

    p = s7_make_boolean(s7, true);
    TEST_ASSERT_TRUE ( (p == s7_t(s7)) );
    p = s7_make_boolean(s7, false);
    TEST_ASSERT_TRUE ( (p == s7_f(s7)) );
}

void test_equality(void) {

    TEST_ASSERT_TRUE_MESSAGE(s7_is_eq(s7_f(s7), s7_f(s7)),
                             "(eq? #f #f) -> #f?");

}

void test_pairs(void) {
    p = s7_cons(s7, s7_f(s7), s7_t(s7));
    gc_loc = s7_gc_protect(s7, p);
    TEST_ASSERT_TRUE_MESSAGE(p == s7_gc_protected_at(s7, gc_loc),
                     "gc protect error");
        /* fprintf(stderr, "%d: %s is not gc protected at %" ld64 ": %s?\n", __LINE__, s1 = TO_STR(p), gc_loc, s2 = TO_STR(s7_gc_protected_at(s7, gc_loc))); free(s1); free(s2); */

    TEST_ASSERT_TRUE_MESSAGE(s7_car(p) == s7_f(s7),
                             "(car '(#f #t)) is not #f?");
}

void test_misc(void) {
    s7_provide(s7, "ffitest");
    TEST_ASSERT_TRUE_MESSAGE( s7_is_provided(s7, "ffitest"),
                              "*features* doesn't provide 'ffitest?" );
}

int main(int argc, char **argv)
{
    s7 = s7_plugin_initialize("cwalk", argc, argv);

    utstring_new(sexp);

    UNITY_BEGIN();

    RUN_TEST(test_booleans);
    RUN_TEST(test_equality);
    RUN_TEST(test_misc);

    utstring_free(sexp);
    return UNITY_END();
}
