#ifndef TOMLX_H
#define TOMLX_H

#include <stdbool.h>

#include "toml.h"

#define TOML_NONDATUM   0
#define TOML_BOOL       1
#define TOML_INT        2
#define TOML_DOUBLE     3
#define TOML_STRING     4
#define TOML_TIMESTAMP  5
#define TOML_ARRAY      6
#define TOML_TABLE      7

toml_datum_t tomlx_array_datum_for_idx(toml_array_t *ta, int idx, int *typ);

void *tomlx_array_seq_for_idx(toml_array_t *ta, int idx, int *typ);

char *tomlx_array_to_string(toml_array_t *ta, bool use_write);

char *tomlx_datetime_to_string(toml_timestamp_t *ts, bool use_write);

toml_datum_t tomlx_table_datum_for_key(toml_table_t *tt, char *key, int *typ);

void *tomlx_table_seq_for_key(toml_table_t *tt, char *key, int *typ);

char *tomlx_table_to_string(toml_table_t *tt, bool use_write);

#endif
