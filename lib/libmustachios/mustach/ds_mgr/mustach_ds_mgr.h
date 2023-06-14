/*
 Author: José Bollo <jobol@nonadev.net>

 https://gitlab.com/jobol/mustach

 SPDX-License-Identifier: ISC
*/

#ifndef _MUSTACHE_DS_MGR_H_
#define _MUSTACHE_DS_MGR_H_

#if !defined(INCLUDE_PARTIAL_EXTENSION)
# define INCLUDE_PARTIAL_EXTENSION ".mustache"
#endif

/*
 * mustach-wrap is intended to make integration of JSON
 * libraries easier by wrapping mustach extensions in a
 * single place.
 *
 * As before, using mustach and only mustach is possible
 * (by using only mustach.h) but does not implement high
 * level features coming with extensions implemented by
 * this high level wrapper.
 */
#include "mustach.h"

/* length given by masking with 3 */
enum comp {                     /* 'comp' means relop */
	C_no = 0,
	C_eq = 1,
	C_lt = 5,
	C_le = 6,
	C_gt = 9,
	C_ge = 10
};

enum sel {
	S_none = 0,
	S_ok = 1,
	S_objiter = 2,
	S_ok_or_objiter = S_ok | S_objiter
};

/*
 * Definition of the writing callbacks for mustach functions
 * producing output to callbacks.
 *
 * Two callback types are defined:
 *
 * @mustach_write_cb_t:
 *
 *    callback receiving the escaped data to be written as 3 parameters:
 *
 *    1. the 'closure', the same given to the wmustach_... function
 *    2. a pointer to a 'buffer' containing the characters to be written
 *    3. the size in bytes of the data pointed by 'buffer'
 *
 * @mustach_emit_cb_t:
 *
 *    callback receiving the data to be written and a flag indicating
 *    if escaping should be done or not as 4 parameters:
 *
 *    1. the 'closure', the same given to the emustach_... function
 *    2. a pointer to a 'buffer' containing the characters to be written
 *    3. the size in bytes of the data pointed by 'buffer'
 *    4. a boolean indicating if 'escape' should be done
 */
#ifndef _mustach_output_callbacks_defined_
#define _mustach_output_callbacks_defined_
typedef int mustach_write_cb_t(void *closure, const char *buffer, size_t size);
typedef int mustach_emit_cb_t(void *closure, const char *buffer, size_t size, int escape);
#endif

/* internal structure for wrapping */
// was: struct wrap
struct datasource_s { // rename: struct datasource_s
    int predicate; /* so mustach.c can signal a predicate metatag */

    /* original interface */
    const struct mustach_ds_methods_s *methods; // itf;

    /* original closure */
    void *stack; // *closure;

    /* flags */
    int flags;

    /* emiter callback */
    mustach_emit_cb_t *emitcb;

    /* write callback */
    mustach_write_cb_t *writecb;
};

/**
 * Flags specific to mustach wrap
 */
#define Mustach_With_SingleDot            4     /* obsolete, always set */
#define Mustach_With_Equal                8
#define Mustach_With_Compare             16
#define Mustach_With_JsonPointer         32
#define Mustach_With_ObjectIter          64
#define Mustach_With_IncPartial         128     /* obsolete, always set */
#define Mustach_With_EscFirstCmp        256
#define Mustach_With_PartialDataFirst   512
#define Mustach_With_ErrorUndefined    1024

/* don't include ErrorUndefined  in *_ALL_*_Extensions */
/* also do not include JsonPointer 32 */
#undef  Mustach_With_All_JSON_Extensions
#define Mustach_With_All_JSON_Extensions (1023 & ~(32))

#undef  Mustach_With_All_TOML_Extensions
#define Mustach_With_All_TOML_Extensions (1023 & ~(32))

#undef  Mustach_With_All_SCM_Extensions
#define Mustach_With_All_SCM_Extensions  (1023 & ~(32))

/**
 * mustach_ds_methods_s - high level wrap of mustach - interface for callbacks
 *
 * The functions sel, subsel, enter and next should return 0 or 1.
 *
 * All other functions should normally return MUSTACH_OK (zero).
 *
 * If any function returns a negative value, it means an error that
 * stop the processing and that is reported to the caller. Mustach
 * also has its own error codes. Using the macros MUSTACH_ERROR_USER
 * and MUSTACH_IS_ERROR_USER could help to avoid clashes.
 *
 * @start: If defined (can be NULL), starts the mustach processing
 *         of the closure, called at the very beginning before any
 *         mustach processing occurs.
 *
 * @stop: If defined (can be NULL), stops the mustach processing
 *        of the closure, called at the very end after all mustach
 *        processing occurered. The status returned by the processing
 *        is passed to the stop.
 *
 * @compare: If defined (can be NULL), compares the value of the
 *           currently selected item with the given value and returns
 *           a negative value if current value is lesser, a positive
 *           value if the current value is greater or zero when
 *           values are equals.
 *           If 'compare' is NULL, any comparison in mustach
 *           is going to fails.
 *
 * @sel: Selects the item of the given 'name'. If 'name' is NULL
 *       Selects the current item. Returns 1 if the selection is
 *       effective or else 0 if the selection failed.
 *
 * @subsel: Selects from the currently selected object the value of
 *          the field of given name. Returns 1 if the selection is
 *          effective or else 0 if the selection failed.
 *
 * @enter: Enters the section of 'name' if possible.
 *         Musts return 1 if entered or 0 if not entered.
 *         When 1 is returned, the function 'leave' will always be called.
 *         Conversely 'leave' is never called when enter returns 0 or
 *         a negative value.
 *         When 1 is returned, the function must activate the first
 *         item of the section.
 *
 * @next: Activates the next item of the section if it exists.
 *        Musts return 1 when the next item is activated.
 *        Musts return 0 when there is no item to activate.
 *
 * @leave: Leaves the last entered section
 *
 * @get: Returns in 'sbuf' the value of the current selection if 'key'
 *       is zero. Otherwise, when 'key' is not zero, return in 'sbuf'
 *       the name of key of the current selection, or if no such key
 *       exists, the empty string. Must return 1 if possible or
 *       0 when not possible or an error code.
 */

// mustach_ds_methods_s common to all datatypes - json, toml, scheme
// each implementation specializes it in mustach_<impl>.c,
// e.g. mustache_tomlc99.c defines mustach_datasource_toml
// (so it s/b 'mustach_impl_itf', or "mustach_datasource_itf")

// Comparison with 'struct mustach_ds_mgr_methods_s' (in mustach.h):
//   common flds: start, stop, enter, next, leave, get, dump_stack
//   mustach_ds_methods_s only: compare, sel, subsel
//   mustach_ds_mgr_methods_s only     : put, partial, emit

// rename: mustach_datasource_itf
struct mustach_ds_methods_s {
    int (*start)(void *stack);
    void (*stop)(void *closure, int status);
    int (*compare)(void *closure, const char *value);
    int (*sel)(void *closure, const char *name);
    int (*subsel)(void *closure, const char *name);
    int (*enter)(void *closure, int objiter);
    int (*next)(void *closure);
    /* int (*next)(int (*)(struct tstack_s *e)); */
    int (*leave)(void *closure, struct mustach_sbuf *sbuf);
    int (*format)(void *closure, const char *fmt, struct mustach_sbuf *sbuf, int key);
    void (*dump_stack)(void *closure);
};

/**
 * Mustach interface used internally by mustach wrapper functions.
 * Can be used for overriding behaviour.
 */
static const struct mustach_ds_mgr_methods_s mustach_ds_methods_s;

void datasource_init(struct datasource_s *wrap, const struct mustach_ds_methods_s *itf, void *closure, int flags, mustach_emit_cb_t *emitcb, mustach_write_cb_t *writecb);

/**
 * Global hook for providing partials. When set to a not NULL value, the pointed
 * function replaces the default behaviour and is called to provide the partial
 * of the given 'name' in 'sbuf'.
 * The function must return MUSTACH_OK when it filled 'sbuf' with value of partial
 * or must return an error code if it failed.
 */
extern int (*mustach_wrap_get_partial)(const char *name, struct mustach_sbuf *sbuf);

/**
 * mustach_wrap_file - Renders the mustache 'template' in 'file' for an abstract
 * wrapper of interface 'itf' and 'closure'.
 *
 * @template: the template string to instantiate
 * @length:   length of the template or zero if unknown and template null terminated
 * @itf:      the interface of the abstract wrapper
 * @closure:  the closure of the abstract wrapper
 * @file:     the file where to write the result
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
int mustach_wrap_file(const char *template, size_t length, const struct mustach_ds_methods_s *itf, void *closure, int flags, FILE *file);

/**
 * mustach_wrap_fd - Renders the mustache 'template' in 'fd' for an abstract
 * wrapper of interface 'itf' and 'closure'.
 *
 * @template: the template string to instantiate
 * @length:   length of the template or zero if unknown and template null terminated
 * @itf:      the interface of the abstract wrapper
 * @closure:  the closure of the abstract wrapper
 * @fd:       the file descriptor number where to write the result
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
/* __attribute__((unused)) static int mustach_wrap_fd(const char *template, size_t length, const struct mustach_ds_methods_s *itf, void *closure, int flags, int fd); */

/**
 * mustach_wrap_mem - Renders the mustache 'template' in 'result' for an abstract
 * wrapper of interface 'itf' and 'closure'.
 *
 * @template: the template string to instantiate
 * @length:   length of the template or zero if unknown and template null terminated
 * @itf:      the interface of the abstract wrapper
 * @closure:  the closure of the abstract wrapper
 * @result:   the pointer receiving the result when 0 is returned
 * @size:     the size of the returned result
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
int mustach_wrap_mem(const char *template, size_t length, const struct mustach_ds_methods_s *itf, void *closure, int flags, char **result, size_t *size);

/**
 * mustach_wrap_write - Renders the mustache 'template' for an abstract
 * wrapper of interface 'itf' and 'closure' to custom writer
 * 'writecb' with 'writeclosure'.
 *
 * @template: the template string to instantiate
 * @length:   length of the template or zero if unknown and template null terminated
 * @itf:      the interface of the abstract wrapper
 * @closure:  the closure of the abstract wrapper
 * @writecb:  the function that write values
 * @closure:  the closure for the write function
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
__attribute__((unused)) static int mustach_wrap_write(const char *template, size_t length, const struct mustach_ds_methods_s *itf, void *closure, int flags, mustach_write_cb_t *writecb, void *writeclosure);

/**
 * mustach_wrap_emit - Renders the mustache 'template' for an abstract
 * wrapper of interface 'itf' and 'closure' to custom emiter 'emitcb'
 * with 'emitclosure'.
 *
 * @template: the template string to instantiate
 * @length:   length of the template or zero if unknown and template null terminated
 * @itf:      the interface of the abstract wrapper
 * @closure:  the closure of the abstract wrapper
 * @emitcb:   the function that emit values
 * @closure:  the closure for the write function
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
__attribute__((unused)) static int mustach_wrap_emit(const char *template, size_t length, const struct mustach_ds_methods_s *itf, void *closure, int flags, mustach_emit_cb_t *emitcb, void *emitclosure);

#endif

