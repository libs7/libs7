#ifndef _LIBDUNE_S7_H
#define _LIBDUNE_S7_H

#include "config.h"
#include "libs7.h"

extern s7_pointer integer_string;

/* signatures for use in s7_definition */
extern s7_pointer pl_tx, pl_xx, pl_xxs,pl_sx, pl_sxi, pl_ix, pl_iis, pl_isix, pl_bxs;

/* for error msgs */
extern s7_pointer c_pointer_string, string_string, character_string, boolean_string, real_string, complex_string;
s7_pointer integer_string;


#endif
