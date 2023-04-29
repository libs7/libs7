#ifndef S7_UTILS_H
#define S7_UTILS_H

#ifdef __cplusplus
extern "C" {
#endif

    #include <stdlib.h>
    #include "s7.h"

    void local_memset(void *s, uint8_t val, size_t n);

    /* s7_int safe_strlen(const char *str); */
    /* { */
    /*     const char *tmp = str; */
    /*     if ((!tmp) || (!(*tmp))) return(0); */
    /*     for (; *tmp; ++tmp); */
    /*     return(tmp - str); */
    /* } */

    char *copy_string_with_length(const char *str, s7_int len);
    char *copy_string(const char *str);

#define local_strcmp(S1, S2) (strcmp(S1, S2) == 0)

#define c_strings_are_equal(Str1, Str2) (local_strcmp(Str1, Str2))

    bool safe_strcmp(const char *s1, const char *s2);

    bool local_strncmp(const char *s1, const char *s2, size_t n);

#define strings_are_equal_with_length(Str1, Str2, Len) (local_strncmp(Str1, Str2, Len))

#ifdef __cplusplus
}
#endif

#endif
