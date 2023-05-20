#ifndef TOML_ARRAY_S7_H
#define TOML_ARRAY_S7_H

#include <stdlib.h>

#include "log.h"
#include "toml.h"
#include "trace.h"
#include "s7.h"

extern int toml_array_type_tag;

void toml_array_init(s7_scheme *s7);

#endif
