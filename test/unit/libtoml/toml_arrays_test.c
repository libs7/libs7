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

s7_pointer root;
s7_pointer b, t, k, a, idx, res, actual, expected;
s7_pointer lst, alst, vec, ht, s1, s2;
s7_pointer len, m;
s7_pointer tmp;

bool verbose;
bool debug;

char *cmd;

void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

void array_refs(void) {
    root = TOML_READ("a = [1, 2, 3]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    /* apply table to key: (t k) == (toml:map-ref t k) */
    a = s7_apply_function(s7, root, s7_list(s7, 1, k));
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    idx = s7_make_integer(s7, 0);
    res = APPLY_2("toml:array-ref", a, idx);
    TEST_ASSERT_EQUAL(1, s7_integer(res));

    idx = s7_make_integer(s7, 2);
    res = APPLY_2("toml:array-ref", a, idx);
    TEST_ASSERT_EQUAL(3, s7_integer(res));

    // index out-of-range throws error
    /* idx = s7_make_integer(s7, 3); */
    /* res = APPLY_2("toml:array-ref", a, idx); */
    /* TEST_ASSERT_EQUAL(s7_f(s7), res); */
}

void array_length_ops(void) {
    root = TOML_READ("a = [1, 2, 3]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    len = APPLY_1("toml:array-length", a);
    TEST_ASSERT_EQUAL(3, s7_integer(len));
}

void array_ops(void) {
    root = TOML_READ(""
                     "m = { b = true, s = \"Hello!\", "
                     "        i = 0, f = 1.2, "
                     "        t = { t1 = 1 }, a = [0, 1, 2] }"
                     "");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    /* root tables have empty key  */
    actual = APPLY_1("toml:map-key", root);
    TEST_ASSERT_EQUAL_STRING("", s7_string(actual));

    k = s7_make_string(s7, "m");
    s7_pointer m = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:map?", m);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    actual = APPLY_1("toml:map-key", m);
    TEST_ASSERT_EQUAL_STRING("m", s7_string(actual));
    actual = APPLY_1("toml:map-keys", m);
    s7_pointer v = APPLY_1("list?", actual);
    TEST_ASSERT_EQUAL(s7_t(s7), v);

    //FIXME: test toml:map-key-for-index ('key_in')

}

void array_kind_int(void) {
    root = TOML_READ("a = [1, 2, 3]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = APPLY_1("toml:array-key", a);
    TEST_ASSERT_EQUAL_STRING("a", s7_string(k));

    res = APPLY_1("toml:array-kind", a);
    TEST_ASSERT_EQUAL_CHAR('v', s7_character(res));

    res = APPLY_1("toml:array-type", a);
    TEST_ASSERT_EQUAL_CHAR('i', s7_character(res));

    len = APPLY_2("toml:array-length", a, k);
    TEST_ASSERT_EQUAL(3, s7_integer(len));

    len = APPLY_2("length", a, k);
    TEST_ASSERT_EQUAL(3, s7_integer(len));

    /* actual = s7_eval_c_string(s7, cmd); */
    /* actual = APPLY_1("toml:map?", m); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */
    /* TEST_ASSERT_TRUE(s7_is_c_pointer(actual)); */
    /* TEST_ASSERT_TRUE(s7_c_pointer_type(actual) == s7_make_symbol(s7, "toml_array_t")); */
    /*     sexp_expected = "path.txt\""; */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */
}

void array_kind_string(void) {
    root = TOML_READ("colors = [\"red\", \"yellow\", \"green\"]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "colors");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = APPLY_1("toml:array-key", a);
    TEST_ASSERT_EQUAL_STRING("colors", s7_string(k));

    res = APPLY_1("toml:array-kind", a);
    TEST_ASSERT_EQUAL_CHAR('v', s7_character(res));

    res = APPLY_1("toml:array-type", a);
    TEST_ASSERT_EQUAL_CHAR('s', s7_character(res));

    len = APPLY_2("toml:array-length", a, k);
    TEST_ASSERT_EQUAL(3, s7_integer(len));
}

void array_kind_string2(void) {
    root = TOML_READ("strings = [\"all\", 'strings', "
                     "\"are the same\","
                     "'''type''']");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "strings");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = APPLY_1("toml:array-key", a);
    TEST_ASSERT_EQUAL_STRING("strings", s7_string(k));

    res = APPLY_1("toml:array-kind", a);
    TEST_ASSERT_EQUAL_CHAR('v', s7_character(res));

    res = APPLY_1("char?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), res);

    res = APPLY_1("toml:array-type", a);
    TEST_ASSERT_EQUAL_CHAR('s', s7_character(res));

    res = APPLY_1("char?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), res);

    len = APPLY_2("toml:array-length", a, k);
    TEST_ASSERT_EQUAL(4, s7_integer(len));
}

void array_nested_ints(void) {
    root = TOML_READ("nested_ints =  [ [ 1, 2 ], [3, 4, 5] ]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "nested_ints");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = APPLY_1("toml:array-key", a);
    TEST_ASSERT_EQUAL_STRING("nested_ints", s7_string(k));

    res = APPLY_1("toml:array-kind", a);
    TEST_ASSERT_EQUAL_CHAR('a', s7_character(res));

    /* array not 'v', so array-type is undefined */
    res = APPLY_1("toml:array-type", a);
    TEST_ASSERT_EQUAL(s7_undefined(s7), res);

    len = APPLY_2("toml:array-length", a, k);
    TEST_ASSERT_EQUAL(2, s7_integer(len));
}

void array_nested_mixed(void) {
    root = TOML_READ("nested_mixed_array = [ [ 1, 2 ], ['a', 'b', 'c']]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "nested_mixed_array");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = APPLY_1("toml:array-key", a);
    TEST_ASSERT_EQUAL_STRING("nested_mixed_array", s7_string(k));

    res = APPLY_1("toml:array-kind", a);
    TEST_ASSERT_EQUAL_CHAR('a', s7_character(res));

    /* array not 'v', so array-type is undefined */
    res = APPLY_1("toml:array-type", a);
    TEST_ASSERT_EQUAL(s7_undefined(s7), res);

    len = APPLY_2("toml:array-length", a, k);
    TEST_ASSERT_EQUAL(2, s7_integer(len));

    s7_pointer suba = APPLY_2("toml:array-ref", a, s7_make_integer(s7,1));
    res = APPLY_1("toml:array?", suba);
    TEST_ASSERT_EQUAL(s7_t(s7), res);
}

void array_mixed_numbers(void) {
    root = TOML_READ("numbers = [ 0.1, 0.2, 0.5, 1, 2, 5 ]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "numbers");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = APPLY_1("toml:array-key", a);
    TEST_ASSERT_EQUAL_STRING("numbers", s7_string(k));

    res = APPLY_1("toml:array-kind", a);
    TEST_ASSERT_EQUAL_CHAR('v', s7_character(res));

    res = APPLY_1("toml:array-type", a);
    TEST_ASSERT_EQUAL('m', s7_character(res));

    len = APPLY_2("toml:array-length", a, k);
    TEST_ASSERT_EQUAL(6, s7_integer(len));

    s7_pointer suba = APPLY_2("toml:array-ref", a, s7_make_integer(s7,1));
    res = APPLY_1("number?", suba);
    TEST_ASSERT_EQUAL(s7_t(s7), res);
}

void array_kind_table(void) {
    /* kind: value */
    root = TOML_READ("a = [ {a = 0}, {b = 1}]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = APPLY_1("toml:array-key", a);
    TEST_ASSERT_EQUAL_STRING("a", s7_string(k));

    res = APPLY_1("toml:array-kind", a);
    TEST_ASSERT_EQUAL_CHAR('t', s7_character(res));

    res = APPLY_1("toml:array-type", a);
    TEST_ASSERT_EQUAL(s7_undefined(s7), res);

    len = APPLY_2("toml:array-length", a, k);
    TEST_ASSERT_EQUAL(2, s7_integer(len));
}

void array_to_list(void) {
    root = TOML_READ("a = [1, 2, 3]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    lst = APPLY_1("toml:array->list", a);
    actual = APPLY_1("list?", lst);

    tmp = s7_eval_c_string(s7, "'(1 2 3)");
    b = APPLY_2("equal?", lst, tmp);
    TEST_ASSERT_EQUAL(s7_t(s7), b);
}

void array_to_vector(void) {
    root = TOML_READ("a = [1, 2, 3]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    lst = APPLY_1("toml:array->vector", a);
    actual = APPLY_1("vector?", lst);

    tmp = s7_eval_c_string(s7, "#(1 2 3)");
    b = APPLY_2("equal?", lst, tmp);
    TEST_ASSERT_EQUAL(s7_t(s7), b);
}

void array_to_string(void) {
    // bools
    root = TOML_READ("a = [true, false]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    res = APPLY_1("object->string", a);
    TEST_ASSERT_EQUAL_STRING("#<toml-array true, false>", s7_string(res));

    // ints
    root = TOML_READ("a = [1, 2, 3]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    res = APPLY_1("object->string", a);
    TEST_ASSERT_EQUAL_STRING("#<toml-array 1, 2, 3>", s7_string(res));

    // doubles
    root = TOML_READ("a = [1.2, 2.3, 3.4]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    res = APPLY_1("object->string", a);
    TEST_ASSERT_EQUAL_STRING("#<toml-array 1.2, 2.3, 3.4>", s7_string(res));

    // strings - double quote
    root = TOML_READ("a = [\"Hi\", \"there\", \"world\"]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    res = APPLY_1("object->string", a);
    TEST_ASSERT_EQUAL_STRING("#<toml-array \"Hi\", \"there\", \"world\">",
                             s7_string(res));

    // strings - single quote
    root = TOML_READ("a = ['Hi', 'there', 'world']");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    res = APPLY_1("object->string", a);
    TEST_ASSERT_EQUAL_STRING("#<toml-array \"Hi\", \"there\", \"world\">",
                             s7_string(res));
}

void nested_array_to_string(void) {
    root = TOML_READ("a = [[1, 2], [3, 4]]");
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    TRACE_LOG_DEBUG("running object->string", "");
    res = APPLY_1("object->string", a);
    TEST_ASSERT_EQUAL_STRING("#<toml-array #<toml-array 1, 2>, #<toml-array 3, 4>>", s7_string(res));

    char *toml = "a = [[1, 2, [3, 4]], [5, 6]]";
    root = TOML_READ(toml);
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    k = s7_make_string(s7, "a");
    a = APPLY_2("toml:map-ref", root, k);
    actual = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));
    TRACE_LOG_DEBUG("running object->string", "");
    res = APPLY_1("object->string", a);
    TEST_ASSERT_EQUAL_STRING("#<toml-array #<toml-array 1, 2, #<toml-array 3, 4>>, #<toml-array 5, 6>>", s7_string(res));
}

/* burnt sushi: tests/valid/table/array-one.toml */
void root_array_1(void) {
    root = TOML_READ(""
        "[[people]]\n"
        "first_name = \"Bruce\"\n"
        "last_name = \"Springsteen\"\n"
                     "");
    TRACE_S7_DUMP("root", root);
    actual = APPLY_1("toml:map?", root);
    TEST_ASSERT_EQUAL(actual, s7_t(s7));

    ht = APPLY_1("toml:map->hash-table", root);
    /* TRACE_S7_DUMP("ht", ht); */
    /* b = APPLY_1("hash-table?", ht); */
    TEST_ASSERT_EQUAL(true, s7_is_hash_table(ht));

    // project from toml-table
    k = s7_make_string(s7, "people");
    a = APPLY_2("toml:map-ref", root, k);
    b = APPLY_1("toml:array?", a);
    TEST_ASSERT_EQUAL(s7_t(s7), b);
    TRACE_S7_DUMP("people", a);

    // and from hash-table
    k = s7_make_keyword(s7, "people");
    a = APPLY_2("hash-table-ref", ht, k);
    TRACE_S7_DUMP("people", a);
    tmp = s7_type_of(s7, a);
    TRACE_S7_DUMP("typ", tmp);
    TEST_ASSERT_EQUAL(true, s7_is_vector(a));
    /* b = APPLY_1("hash-table?", a); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), b); */
    TRACE_S7_DUMP("people", a);

    /* toml = "a = [[1, 2, [3, 4]], [5, 6]]\""; */
    /* root = TOML_READ(toml); */
    /* actual = APPLY_1("toml:map?", root); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */

    /* k = s7_make_string(s7, "a"); */
    /* a = APPLY_2("toml:map-ref", root, k); */
    /* actual = APPLY_1("toml:array?", a); */
    /* TEST_ASSERT_EQUAL(actual, s7_t(s7)); */
    /* TRACE_LOG_DEBUG("running object->string", ""); */
    /* res = APPLY_1("object->string", a); */
    /* TEST_ASSERT_EQUAL_STRING("[[1, 2, [3, 4]], [5, 6]]", s7_string(res)); */
}

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "toml");

    UNITY_BEGIN();

    RUN_TEST(array_refs);
    RUN_TEST(array_length_ops);
    RUN_TEST(array_ops);

    RUN_TEST(array_kind_int);
    RUN_TEST(array_kind_string);
    RUN_TEST(array_kind_string2);
    RUN_TEST(array_nested_ints);
    RUN_TEST(array_nested_mixed);
    RUN_TEST(array_mixed_numbers);
    RUN_TEST(array_kind_table);

    RUN_TEST(array_to_string);
    RUN_TEST(nested_array_to_string);
    RUN_TEST(array_to_list);
    RUN_TEST(array_to_vector);

    // burntsushi tests

    RUN_TEST(root_array_1);

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
