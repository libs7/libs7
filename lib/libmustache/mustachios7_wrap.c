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

#ifdef DEBUGGING
#include "ansi_colors.h"
/* #include "s7.h" */
//#include "debug.h"
/* extern s7_scheme *s7; */
/* s7_pointer xx; */
#endif
#include "trace.h"

/* #if !defined(INCLUDE_PARTIAL_EXTENSION) */
/* # define INCLUDE_PARTIAL_EXTENSION ".mustache" */
/* #endif */

/* global hook for partials */
int (*mustach_wrap_get_partial)(const char *name, struct mustach_sbuf *sbuf) = NULL;

/* internal structure for wrapping */
/* struct wrap { */
/*     int predicate; /\* so mustach.c can signal a predicate metatag *\/ */

/* 	/\* original interface *\/ */
/* 	const struct mustach_wrap_itf *itf; */

/* 	/\* original closure *\/ */
/* 	void *closure; */

/* 	/\* flags *\/ */
/* 	int flags; */

/* 	/\* emiter callback *\/ */
/* 	mustach_emit_cb_t *emitcb; */

/* 	/\* write callback *\/ */
/* 	mustach_write_cb_t *writecb; */
/* }; */

/* length given by masking with 3 */
/* enum comp {                     /\* 'comp' means relop *\/ */
/* 	C_no = 0, */
/* 	C_eq = 1, */
/* 	C_lt = 5, */
/* 	C_le = 6, */
/* 	C_gt = 9, */
/* 	C_ge = 10 */
/* }; */

/* enum sel { */
/* 	S_none = 0, */
/* 	S_ok = 1, */
/* 	S_objiter = 2, */
/* 	S_ok_or_objiter = S_ok | S_objiter */
/* }; */

/* const struct mustach_itf mustach_wrap_itf = { */
/* 	.start = start, */
/* 	.put = NULL, */
/* 	.enter = enter, */
/* 	.next = next, */
/* 	.leave = leave, */
/* 	.partial = partial, */
/* 	.get = get, */
/* 	.emit = emit, */
/* 	.stop = stop, */
/*         .dump_closure = dump_closure */
/* }; */

void wrap_init(struct wrap *wrap, const struct mustach_wrap_itf *itf, void *closure, int flags, mustach_emit_cb_t *emitcb, mustach_write_cb_t *writecb)
{
	if (flags & Mustach_With_Compare)
		flags |= Mustach_With_Equal;
	wrap->closure = closure;
	wrap->itf = itf;
	wrap->flags = flags;
	wrap->emitcb = emitcb;
	wrap->writecb = writecb;
}

/* int mustach_wrap_file(const char *template, size_t length, const struct mustach_wrap_itf *itf, void *closure, int flags, FILE *file) */
/* { */
/* 	struct wrap w; */
/* 	wrap_init(&w, itf, closure, flags, NULL, NULL); */
/* 	return mustach_file(template, length, &mustach_wrap_itf, &w, flags, file); */
/* } */

/* int mustach_wrap_fd(const char *template, size_t length, const struct mustach_wrap_itf *itf, void *closure, int flags, int fd) */
/* { */
/* 	struct wrap w; */
/* 	wrap_init(&w, itf, closure, flags, NULL, NULL); */
/* 	return mustach_fd(template, length, &mustach_wrap_itf, &w, flags, fd); */
/* } */

/* int mustach_wrap_mem(const char *template, size_t length, const struct mustach_wrap_itf *itf, void *closure, int flags, char **result, size_t *size) */
/* { */
/* #ifdef DEBUGGING */
/*     log_debug("mustach_wrap_mem"); */
/* #endif */
/* 	struct wrap w; */
/* 	wrap_init(&w, itf, closure, flags, NULL, NULL); */
/* 	return mustach_mem(template, length, &mustach_wrap_itf, &w, flags, result, size); */
/* } */

/* int mustach_wrap_write(const char *template, size_t length, const struct mustach_wrap_itf *itf, void *closure, int flags, mustach_write_cb_t *writecb, void *writeclosure) */
/* { */
/* 	struct wrap w; */
/* 	wrap_init(&w, itf, closure, flags, NULL, writecb); */
/* 	return mustach_file(template, length, &mustach_wrap_itf, &w, flags, writeclosure); */
/* } */

/* int mustach_wrap_emit(const char *template, size_t length, const struct mustach_wrap_itf *itf, void *closure, int flags, mustach_emit_cb_t *emitcb, void *emitclosure) */
/* { */
/* 	struct wrap w; */
/* 	wrap_init(&w, itf, closure, flags, emitcb, NULL); */
/* 	return mustach_file(template, length, &mustach_wrap_itf, &w, flags, emitclosure); */
/* } */

