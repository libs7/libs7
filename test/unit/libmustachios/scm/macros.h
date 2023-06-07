#define S7_RENDER_TEST(t, d, expected)                                  \
    do {s7_pointer data = s7_eval_c_string(s7, d);                      \
        if (data == 0) {                                                \
            TEST_FAIL_MESSAGE("s7_eval_c_string error");                \
        } else {                                                        \
            s7_pointer tplt = s7_make_string(s7, t);                    \
            s7_pointer actual = s7_call(s7, mustache_render,            \
                                 s7_list(s7, 3, s7_f(s7), tplt, data)); \
            if (actual == 0) {                                          \
                TEST_FAIL_MESSAGE("mustache:render error");             \
            } else {                                                    \
                TEST_ASSERT_EQUAL_STRING(expected, s7_string(actual));  \
            }                                                           \
        }                                                               \
    } while(0)

#define S7_RENDER_SINK_TEST(sink, t, d, expected)                       \
    do {s7_pointer data = s7_eval_c_string(s7, d);                      \
        if (data == 0) {                                                \
            TEST_FAIL_MESSAGE("s7_eval_c_string error");                \
        } else {                                                        \
            s7_pointer tplt = s7_make_string(s7, t);                    \
            s7_pointer actual = s7_call(s7, mustache_render,            \
                                     s7_list(s7, 3, sink, tplt, data)); \
            if (actual == 0) {                                          \
                TEST_FAIL_MESSAGE("mustache:render error");             \
            } else {                                                    \
                TEST_ASSERT_EQUAL_STRING(expected, s7_string(actual));  \
            }                                                           \
        }                                                               \
    } while(0)

/* #define SCM_RENDER_TEST(t, d, expected)                               \ */
/*     do {                                                                \ */
/*         s7_pointer tt = s7_apply_function(s7, toml_read,                \ */
/*                                           s7_list(s7, 1,                \ */
/*                                                   s7_make_string(s7, d))); \ */
/*         if (tt == 0) {                                                  \ */
/*             TEST_FAIL_MESSAGE("toml_read error");                       \ */
/*         } else {                                                        \ */
/*             s7_pointer tplt = s7_make_string(s7, t);                    \ */
/*             s7_pointer actual = s7_call(s7, mustache_render,            \ */
/*                                         s7_list(s7, 3, s7_f(s7), tplt, tt)); \ */
/*             if (actual == 0) {                                          \ */
/*                 TEST_FAIL_MESSAGE("mustache:render error");             \ */
/*             } else {                                                    \ */
/*                 TEST_ASSERT_EQUAL_STRING(expected, s7_string(actual)); \ */
/*             }                                                           \ */
/*         }                                                               \ */
/*     } while(0) */

/* #define SCM_RENDER_SINK_TEST(sink, t, d, expected)                      \ */
/*     do {                                                                \ */
/*         s7_pointer tt = s7_apply_function(s7, toml_read,                \ */
/*                                           s7_list(s7, 1,                \ */
/*                                                   s7_make_string(s7, d))); \ */
/*         if (tt == 0) {                                                  \ */
/*             TEST_FAIL_MESSAGE("toml_read error");                       \ */
/*         } else {                                                        \ */
/*             s7_pointer tplt = s7_make_string(s7, t);                    \ */
/*             s7_pointer actual = s7_call(s7, mustache_render,            \ */
/*                                         s7_list(s7, 3, sink, tplt, tt)); \ */
/*             if (actual == 0) {                                          \ */
/*                 TEST_FAIL_MESSAGE("mustache:render error");             \ */
/*             } else {                                                    \ */
/*                 TEST_ASSERT_EQUAL_STRING(expected, s7_string(actual)); \ */
/*             }                                                           \ */
/*         }                                                               \ */
/*     } while(0) */
