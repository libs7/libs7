/*
 Author: José Bollo <jobol@nonadev.net>

 https://gitlab.com/jobol/mustach

 SPDX-License-Identifier: ISC
*/

#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifdef _WIN32
#include <malloc.h>
#endif

#include "log.h"
#include "mustach.h"
#include "mustachios7_wrap.h"

#ifdef DEVBUILD
#include "ansi_colors.h"
/* #include "s7.h" */
//#include "logging.h"
/* extern s7_scheme *s7; */
/* s7_pointer xx; */
#include "config.h"
#endif

/* #if !defined(INCLUDE_PARTIAL_EXTENSION) */
/* # define INCLUDE_PARTIAL_EXTENSION ".mustache" */
/* #endif */

/* global hook for partials */
int (*mustach_wrap_get_partial)(const char *name, struct mustach_sbuf *sbuf) = NULL;

void datasource_init(struct datasource_s *ds,
                     const struct mustach_ds_methods_s *methods, // itf,
                     void *stack, // struct tstack_s*
                     int flags,
                     mustach_emit_cb_t *emitcb, mustach_write_cb_t *writecb)
{
    if (flags & Mustach_With_Compare)
        flags |= Mustach_With_Equal;
    ds->stack = stack;
    ds->methods = methods;
    ds->flags = flags;
    ds->emitcb = emitcb;
    ds->writecb = writecb;
}

/* int mustach_wrap_file(const char *template, size_t length, const struct mustach_ds_methods_s *itf, void *closure, int flags, FILE *file) */
/* { */
/* 	struct datasource_s w; */
/* 	datasource_init(&w, itf, closure, flags, NULL, NULL); */
/* 	return mustach_file(template, length, &mustach_ds_methods_s, &w, flags, file); */
/* } */

/* int mustach_wrap_fd(const char *template, size_t length, const struct mustach_ds_methods_s *itf, void *closure, int flags, int fd) */
/* { */
/* 	struct datasource_s w; */
/* 	datasource_init(&w, itf, closure, flags, NULL, NULL); */
/* 	return mustach_fd(template, length, &mustach_ds_methods_s, &w, flags, fd); */
/* } */

/* int mustach_wrap_mem(const char *template, size_t length, const struct mustach_ds_methods_s *itf, void *closure, int flags, char **result, size_t *size) */
/* { */
/* #ifdef DEVBUILD */
/*     log_debug("mustach_wrap_mem"); */
/* #endif */
/* 	struct datasource_s w; */
/* 	datasource_init(&w, itf, closure, flags, NULL, NULL); */
/* 	return mustach_mem(template, length, &mustach_ds_methods_s, &w, flags, result, size); */
/* } */

/* int mustach_wrap_write(const char *template, size_t length, const struct mustach_ds_methods_s *itf, void *closure, int flags, mustach_write_cb_t *writecb, void *writeclosure) */
/* { */
/* 	struct datasource_s w; */
/* 	datasource_init(&w, itf, closure, flags, NULL, writecb); */
/* 	return mustach_file(template, length, &mustach_ds_methods_s, &w, flags, writeclosure); */
/* } */

/* int mustach_wrap_emit(const char *template, size_t length, const struct mustach_ds_methods_s *itf, void *closure, int flags, mustach_emit_cb_t *emitcb, void *emitclosure) */
/* { */
/* 	struct datasource_s w; */
/* 	datasource_init(&w, itf, closure, flags, emitcb, NULL); */
/* 	return mustach_file(template, length, &mustach_ds_methods_s, &w, flags, emitclosure); */
/* } */

