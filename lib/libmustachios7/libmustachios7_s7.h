/*
 Author: José Bollo <jobol@nonadev.net>

 https://gitlab.com/jobol/mustach

 SPDX-License-Identifier: ISC
*/

#ifndef _libmustachios7_s7_h_included_
#define _libmustachios7_s7_h_included_

/*
 * mustach-s7 is intended to make integration of s7
 * library by providing integrated functions.
 */

#include "libs7.h"
#include "mustachios7_s7.h"
#include "mustachios7_cjson.h"

/* ****************************************************************
 * libs7-specific API
*/

/* by default we render scm data */
s7_pointer libs7_mustach_render(s7_scheme *s7, s7_pointer args);

s7_pointer libmustachios7_mustach_render_json(s7_scheme *s7, s7_pointer args);

s7_pointer libmustachios7_s7_init(s7_scheme *sc);


#endif

