#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "log.h"
#include "s7.h"

s7_scheme *libs7_init(void);

void clib_dload(s7_scheme *s7,
                char *libname,   /* e.g. libc_s7 */
                char *libns,     /* e.g. libc */
                char *dso_ext);   /* .dylib or .so */

void clib_sinit(s7_scheme *s7,
                void (fnptr)(s7_scheme *sc),
                char *libns,     /* e.g. libc */
                char *dso_ext);  /* .dylib or .so */
