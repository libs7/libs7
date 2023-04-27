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

    void clib_dload_global(s7_scheme *s7,
                           char *libname,   /* e.g. libc_s7 */
                           char *libns,     /* e.g. libc */
                           char *dso_ext);   /* .dylib or .so */

    void clib_dload_ns(s7_scheme *s7,
                       char *libname,   /* e.g. libc_s7 */
                       char *libns,     /* e.g. libc */
                       char *dso_ext);   /* .dylib or .so */

    void clib_sinit(s7_scheme *s7,
                    s7_pointer (fnptr)(s7_scheme *sc),
                    char *libns);     /* e.g. libc */


#ifdef __cplusplus
}
#endif

#endif
