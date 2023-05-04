#ifndef LIBS7_H
#define LIBS7_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "log.h"
#include "s7.h"

    s7_scheme *libs7_init(void);

    s7_pointer libs7_load_clib(s7_scheme *s7, char *lib);

#ifdef __cplusplus
}
#endif

#endif
