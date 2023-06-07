#include "unity.h"
#include "config.h"
#include "macros.h"
#include "common.h"

#include "libs7.h"

s7_scheme *s7;
s7_pointer json_read;
s7_pointer mustache_render;

s7_pointer flag, t, tt, k, a, suba, ht;

char *d, *actual_str;
char *s, *str, *expected_str;
const char *template, *result;
size_t rsz;
int rc;

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

void to_hash_table(void) {
    tt = JSON_READ("{\"fld1\": 1, \"fld2\": 2}");
    flag = APPLY_1("json:datum?", tt);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));
    S7_RENDER_OBJ_TEST(s7_f(s7),
                       "fld1: {{fld1}}",
                       tt,
                       "fld1: 1");

    ht = APPLY_1("json:map->hash-table", tt);
    flag = APPLY_1("hash-table?", ht);
    TRACE_S7_DUMP("ht", ht);
    TEST_ASSERT_TRUE(s7_boolean(s7, flag));

    flag = APPLY_1("json:datum?", ht);
    TEST_ASSERT_FALSE(s7_boolean(s7, flag));
    S7_RENDER_OBJ_TEST(s7_f(s7),
                       "fld1: {{fld1}}",
                       ht,
                       "fld1: 1");
}

int main(int argc, char **argv)
{
    s7 = initialize("mst_cjson_interpolation_test", argc, argv);

    libs7_load_clib(s7, "json");
    libs7_load_clib(s7, "toml");
    libs7_load_clib(s7, "mustachios");

    json_read = s7_name_to_value(s7, "json:read");
    mustache_render = s7_name_to_value(s7, "mustache:render");

    UNITY_BEGIN();

    RUN_TEST(to_hash_table);

    int rc = UNITY_END();
    if (fflush(stdout) == EOF) {
        /*_Handle_error_*/
    }
    return rc;
}
