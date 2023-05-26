#ifndef LIBTOML_S7_H
#define LIBTOML_S7_H

#include "log.h"
#include "s7.h"
#include "trace.h"
#include "toml.h"

#define TOML_NONDATUM   0
#define TOML_BOOL       1
#define TOML_INT        2
#define TOML_DOUBLE     3
#define TOML_STRING     4
#define TOML_TIMESTAMP  5
#define TOML_ARRAY      6
#define TOML_TABLE      7

extern s7_pointer integer_string;

/* signatures for use in s7_definition */
extern s7_pointer pl_tx, pl_xx, pl_xxs,pl_sx, pl_sxi, pl_ix, pl_iis, pl_isix, pl_bxs;

/* for error msgs */
extern s7_pointer c_pointer_string, string_string, character_string, boolean_string, real_string, complex_string;
s7_pointer integer_string;

s7_pointer toml_read_file(s7_scheme *sc, char *fname);

/* in toml_table_s7.c */
extern int toml_table_type_tag;
s7_pointer is_toml_table(s7_scheme *s7, s7_pointer args);
s7_pointer toml_table_length(s7_scheme *s7, s7_pointer args);
s7_pointer toml_table_keys(s7_scheme *s7, s7_pointer args);
s7_pointer toml_table_values(s7_scheme *s7, s7_pointer args);
s7_pointer toml_table_ref(s7_scheme *s7, s7_pointer args);
char *toml_table_to_string(toml_table_t *tt, bool use_write);
s7_pointer toml_table_to_hash_table(s7_scheme *s7, toml_table_t *ta, bool clone);
void toml_table_init(s7_scheme *s7, s7_pointer cur_env);

/* in toml_array_s7.c */
extern int toml_array_type_tag;
s7_pointer is_toml_array(s7_scheme *s7, s7_pointer args);
s7_pointer toml_array_length(s7_scheme *sc, s7_pointer args);
s7_pointer toml_array_ref(s7_scheme *sc, s7_pointer args);
char      *toml_array_to_string(toml_array_t *tt, bool use_write);
s7_pointer toml_array_to_list(s7_scheme *s7, toml_array_t *ta, bool clone);
s7_pointer toml_array_to_vector(s7_scheme *s7, toml_array_t *ta, bool clone);
void toml_array_init(s7_scheme *s7, s7_pointer cur_env);

/* in toml_datetime_s7.c */
extern int toml_datetime_type_tag;
s7_pointer toml_datetime_to_hash_table(s7_scheme *s7, toml_timestamp_t *ts);
char *toml_datetime_to_string(toml_timestamp_t *ts, bool use_write);
void toml_datetime_init(s7_scheme *s7, s7_pointer cur_env);

#endif
