#ifndef LIBTOML_S7_H
#define LIBTOML_S7_H

#include "log.h"
#include "s7.h"
#include "trace.h"

/* in toml_table_s7.c */
extern int toml_table_type_tag;
s7_pointer is_toml_table(s7_scheme *s7, s7_pointer args);
s7_pointer toml_table_length(s7_scheme *s7, s7_pointer args);
void toml_table_init(s7_scheme *s7);

/* in toml_array_s7.c */
extern int toml_array_type_tag;
s7_pointer is_toml_array(s7_scheme *s7, s7_pointer args);
void toml_array_init(s7_scheme *s7);

#endif
