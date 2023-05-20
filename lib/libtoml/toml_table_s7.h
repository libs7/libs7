#ifndef TOML_TABLE_S7_H
#define TOML_TABLE_S7_H

#include <stdlib.h>

#include "log.h"
#include "toml.h"
#include "trace.h"
#include "s7.h"

extern int toml_table_type_tag;

s7_pointer is_toml_table(s7_scheme *s7, s7_pointer args);

void toml_table_init(s7_scheme *s7);

#endif
