#ifndef _TOMLX_H_
#define _TOMLX_H_

#include <stdbool.h>

#include "toml.h"

#define PRINT_SYNTAX_SCM_DISPLAY 0
#define PRINT_SYNTAX_SCM_WRITE   1
#define PRINT_SYNTAX_TOML        2
#define PRINT_SYNTAX_TOML_RAW    3

#define TOML_NONDATUM   0
#define TOML_BOOL       1
#define TOML_INT        2
#define TOML_DOUBLE     3
#define TOML_STRING     4
#define TOML_TIMESTAMP  5
#define TOML_ARRAY      6
#define TOML_TABLE      7

struct tomlx_item_s {
    int  type; // TOML_BOOL etc.
    union {
        toml_table_t     *t;
        toml_array_t     *a;
        toml_timestamp_t *ts; /* ts must be freed after use */
        char             *s; /* string value. s must be freed after use */
        int               b; /* bool value */
        int64_t           i; /* int value */
        double            d; /* double value */
    } u;
};

struct tomlx_item_s *tomlx_make_item(void* val, int type);

bool tomlx_table_is_empty(toml_table_t *tt);

size_t tomlx_table_length(toml_table_t *tt);

struct tomlx_item_s *tomlx_table_ref(toml_table_t *tt,
                                     const char *key);

toml_datum_t tomlx_table_datum_for_key(toml_table_t *tt, char *key, int *typ);

void *tomlx_table_seq_for_key(toml_table_t *tt, char *key, int *typ);

char *tomlx_table_to_string(toml_table_t *tt, int print_syntax);

struct tomlx_item_s *tomlx_array_ref(toml_array_t *array,
                                     int idx);

toml_datum_t tomlx_array_datum_for_idx(toml_array_t *ta, int idx, int *typ);

void *tomlx_array_seq_for_idx(toml_array_t *ta, int idx, int *typ);

char *tomlx_array_to_string(toml_array_t *ta, int print_syntax);

char *tomlx_datetime_to_string(toml_timestamp_t *ts, int print_syntax);

char *tomlx_format_datetime(toml_timestamp_t* ts, const char *fmt);

toml_table_t *tomlx_read_string(const char *s);

toml_table_t *tomlx_read_fp(FILE *f);

toml_table_t *tomlx_read_file(const char *fname);

#endif
