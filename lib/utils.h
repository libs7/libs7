#ifndef UTILS_H
#define UTILS_H

#include "s7.h"

/* WARNING: result must be freed */
char *libs7_input_port_to_c_string(s7_scheme *s7, s7_pointer port);

#endif

