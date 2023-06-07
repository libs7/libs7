/*
 Author: José Bollo <jobol@nonadev.net>

 https://gitlab.com/jobol/mustach

 SPDX-License-Identifier: ISC
*/

#ifndef _mustach_h_included_
#define _mustach_h_included_

#include <stdbool.h>
#include <stdio.h>

struct mustach_sbuf; /* see below */

/**
 * Current version of mustach and its derivates
 */
#define MUSTACH_VERSION 102
#define MUSTACH_VERSION_MAJOR (MUSTACH_VERSION / 100)
#define MUSTACH_VERSION_MINOR (MUSTACH_VERSION % 100)

/**
 * Maximum nested imbrications supported
 */
#define MUSTACH_MAX_DEPTH  256

/**
 * Maximum length of tags in mustaches {{...}}
 */
#define MUSTACH_MAX_LENGTH 4096

/**
 * Maximum length of delimitors (2 normally but extended here)
 */
#define MUSTACH_MAX_DELIM_LENGTH 8

/**
 * Flags specific to mustach core
 */
#define Mustach_With_NoExtensions   0
#define Mustach_With_Colon          1
#define Mustach_With_EmptyTag       2
#define Mustach_With_AllExtensions  3

/*
 * Definition of error codes returned by mustach
 */
#define MUSTACH_OK                       0
#define MUSTACH_ERROR_SYSTEM            -1
#define MUSTACH_ERROR_UNEXPECTED_END    -2
#define MUSTACH_ERROR_EMPTY_TAG         -3
#define MUSTACH_ERROR_TAG_TOO_LONG      -4
#define MUSTACH_ERROR_BAD_SEPARATORS    -5
#define MUSTACH_ERROR_TOO_DEEP          -6
#define MUSTACH_ERROR_CLOSING           -7
#define MUSTACH_ERROR_BAD_UNESCAPE_TAG  -8
#define MUSTACH_ERROR_INVALID_ITF       -9
#define MUSTACH_ERROR_ITEM_NOT_FOUND    -10
#define MUSTACH_ERROR_PARTIAL_NOT_FOUND -11
#define MUSTACH_ERROR_UNDEFINED_TAG     -12

/*
 * You can use definition below for user specific error
 *
 * The macro MUSTACH_ERROR_USER is involutive so for any value
 *   value = MUSTACH_ERROR_USER(MUSTACH_ERROR_USER(value))
 */
#define MUSTACH_ERROR_USER_BASE         -100
#define MUSTACH_ERROR_USER(x)           (MUSTACH_ERROR_USER_BASE-(x))
#define MUSTACH_IS_ERROR_USER(x)        (MUSTACH_ERROR_USER(x) >= 0)

/*
  Predicate operators
*/
#ifdef ALT_SYNTAX
#define PREDOP_FIRST '^'
#define NEGOP '~'
#else
#define PREDOP_FIRST '~'
#define NEGOP '^'
#endif

#define PREDOP_LAST    '$'
#define PREDOP_BUTLAST '?'
// predicate flags
#define NULL_P    0
#define FIRST_P   1
#define LAST_P    2
#define BUTLAST_P 3

/**
 * mustach_ds_mgr_methods_s - pure abstract mustach - interface for callbacks
 *
 * The functions enter and next should return 0 or 1.
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
 * @put: If defined (can be NULL), writes the value of 'name'
 *       to 'file' with 'escape' or not.
 *       As an extension (see NO_ALLOW_EMPTY_TAG), the 'name' can be
 *       the empty string. In that later case an implementation can
 *       return MUSTACH_ERROR_EMPTY_TAG to refuse empty names.
 *       If NULL and 'format' NULL the error MUSTACH_ERROR_INVALID_ITF
 *       is returned.
 *
 * @enter: Enters the section of 'name' if possible.
 *         Musts return 1 if entered or 0 if not entered.
 *         When 1 is returned, the function 'leave' will always be called.
 *         Conversely 'leave' is never called when enter returns 0 or
 *         a negative value.
 *         When 1 is returned, the function must activate the first
 *         item of the section.
 *
 * @next: = has_next. Activates the next item of the section if it exists.
 *        Must return true when the next item is activated.
 *        Must return false when there is no item to activate.
 *
 * @leave: Leaves the last entered section
 *
 * @partial: If defined (can be NULL), returns in 'sbuf' the content of the
 *           partial of 'name'. @see mustach_sbuf
 *           If NULL but 'format' not NULL, 'format' is used instead of partial.
 *           If NULL and 'format' NULL and 'put' not NULL, 'put' is called with
 *           a true FILE.
 *
 * @emit: If defined (can be NULL), writes the 'buffer' of 'size' with 'escape'.
 *        If NULL the standard function 'fwrite' is used with a true FILE.
 *        If not NULL that function is called instead of 'fwrite' to output
 *        text.
 *        It implies that if you define either 'partial' or 'format' callback,
 *        the meaning of 'FILE *file' is abstract for mustach's process and
 *        then you can use 'FILE*file' pass any kind of pointer (including NULL)
 *        to the function 'fmustach'. An example of a such behaviour is given by
 *        the implementation of 'mustach_json_c_write'.
 *
 * @format: If defined (can be NULL), returns in 'sbuf' the value of 'name'.
 *       As an extension (see NO_ALLOW_EMPTY_TAG), the 'name' can be
 *       the empty string. In that later case an implementation can
 *       return MUSTACH_ERROR_EMPTY_TAG to refuse empty names.
 *       If 'format' is NULL and 'put' NULL the error MUSTACH_ERROR_INVALID_ITF
 *       is returned.
 *
 * @stop: If defined (can be NULL), stops the mustach processing
 *        of the closure, called at the very end after all mustach
 *        processing occurered. The status returned by the processing
 *        is passed to the stop.
 *
 * The array below summarize status of callbacks:
 *
 *    FULLY OPTIONAL:   start partial
 *    MANDATORY:        enter next leave
 *    COMBINATORIAL:    put emit format
 *
 * Not definig a MANDATORY callback returns error MUSTACH_ERROR_INVALID_ITF.
 *
 * For COMBINATORIAL callbacks the array below summarize possible combinations:
 *
 *  combination  : put     : emit    : format     : abstract FILE
 *  -------------+---------+---------+---------+-----------------------
 *  HISTORIC     : defined : NULL    : NULL    : NO: standard FILE
 *  MINIMAL      : NULL    : NULL    : defined : NO: standard FILE
 *  CUSTOM       : NULL    : defined : defined : YES: abstract FILE
 *  DUCK         : defined : NULL    : defined : NO: standard FILE
 *  DANGEROUS    : defined : defined : any     : YES or NO, depends on 'partial'
 *  INVALID      : NULL    : any     : NULL    : -
 *
 * The DUCK case runs on one leg. 'format' is not used if 'partial' is defined
 * but is used for 'partial' if 'partial' is NULL. Thus for clarity, do not use
 * it that way but define 'partial' and let 'format' be NULL.
 *
 * The DANGEROUS case is special: it allows abstract FILE if 'partial' is defined
 * but forbids abstract FILE when 'partial' is NULL.
 *
 * The INVALID case returns error MUSTACH_ERROR_INVALID_ITF.
 */
struct mustach_ds_mgr_methods_s { //TODO: rename struct mustach_ds_mgr_methods
    int (*start)(void *closure);
    void (*stop)(void *closure, int status);
    int (*put)(void *closure, const char *name, const char *fmt, int escape, FILE *file);
    int (*enter)(void *datasource, const char *name);
    /* int (*open_section)(void *datasource, const char *name); */
    int (*next)(void *closure);
    int (*leave)(void *closure, struct mustach_sbuf *sbuf);
    int (*partial)(void *closure, const char *name, const char *fmt, struct mustach_sbuf *sbuf);
    int (*emit)(void *closure, const char *buffer, size_t size, int escape, FILE *file);
    int (*format)(void *closure, const char *name, const char *fmt, struct mustach_sbuf *sbuf);
    void (*dump_stack)(void *closure);
};

/*
 * closure_hdr
 * every closure impl must begin with this
 */
struct closure_hdr {
    int predicate;
};

/**
 * mustach_sbuf - Interface for handling zero terminated strings
 *
 * That structure is used for returning zero terminated strings -in 'value'-
 * to mustach. The callee can provide a function for releasing the returned
 * 'value'. Three methods for releasing the string are possible.
 *
 *  1. no release: set either 'freecb' or 'releasecb' with NULL (done by default)
 *  2. release without closure: set 'freecb' to its expected value
 *  3. release with closure: set 'releasecb' and 'closure' to their expected values
 *
 * @value: The value of the string. That value is not changed by mustach -const-.
 *
 * @freecb: The function to call for freeing the value without closure.
 *          For convenience, signature of that callback is compatible with 'free'.
 *          Can be NULL.
 *
 * @releasecb: The function to release with closure.
 *             Can be NULL.
 *
 * @closure: The closure to use for 'releasecb'.
 *
 * @length: Length of the value or zero if unknown and value null terminated.
 *          To return the empty string, let it to zero and let value to NULL.
 */
struct mustach_sbuf {
    const char *value;
    union {
        void (*freecb)(void*);
        void (*releasecb)(const char *value, void *closure);
    };
    void *closure; // not used?
    size_t length;
    bool   lambda;              /* true if value produced by lambda */
};

/**
 * mustach_file - Renders the mustache 'template' in 'file' for 'itf' and 'closure'.
 *
 * @template: the template string to instantiate
 * @length:   length of the template or zero if unknown and template null terminated
 * @itf:      the interface to the functions that mustach calls
 * @closure:  the closure to pass to functions called
 * @file:     the file where to write the result
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
extern int mustach_file(const char *template, size_t length, const struct mustach_ds_mgr_methods_s *itf, void *closure, int flags, FILE *file);

/**
 * mustach_fd - Renders the mustache 'template' in 'fd' for 'itf' and 'closure'.
 *
 * @template: the template string to instantiate
 * @length:   length of the template or zero if unknown and template null terminated
 * @itf:      the interface to the functions that mustach calls
 * @closure:  the closure to pass to functions called
 * @fd:       the file descriptor number where to write the result
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
extern int mustach_fd(const char *template, size_t length, const struct mustach_ds_mgr_methods_s *itf, void *closure, int flags, int fd);

/**
 * mustach_mem - Renders the mustache 'template' in 'result' for 'itf' and 'closure'.
 *
 * @template: the template string to instantiate
 * @length:   length of the template or zero if unknown and template null terminated
 * @itf:      the interface to the functions that mustach calls
 * @closure:  the closure to pass to functions called
 * @result:   the pointer receiving the result when 0 is returned. Null terminated.
 * @size:     the size of the returned result, not including terminating null (like strlen).
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
extern int mustach_mem(const char *template, size_t length,
                       const struct mustach_ds_mgr_methods_s *itf,
                       void *closure, // struct wrap*
                       int flags,
                       char **result, size_t *size);

/***************************************************************************
* compatibility with version before 1.0
*/
#ifdef __GNUC__
#define DEPRECATED_MUSTACH(func) func __attribute__ ((deprecated))
#elif defined(_MSC_VER)
#define DEPRECATED_MUSTACH(func) __declspec(deprecated) func
#elif !defined(DEPRECATED_MUSTACH)
#pragma message("WARNING: You need to implement DEPRECATED_MUSTACH for this compiler")
#define DEPRECATED_MUSTACH(func) func
#endif
/**
 * OBSOLETE use mustach_file
 *
 * fmustach - Renders the mustache 'template' in 'file' for 'itf' and 'closure'.
 *
 * @template: the template string to instantiate, null terminated
 * @itf:      the interface to the functions that mustach calls
 * @closure:  the closure to pass to functions called
 * @file:     the file where to write the result
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
DEPRECATED_MUSTACH(extern int fmustach(const char *template, const struct mustach_ds_mgr_methods_s *itf, void *closure, FILE *file));

/**
 * OBSOLETE use mustach_fd
 *
 * fdmustach - Renders the mustache 'template' in 'fd' for 'itf' and 'closure'.
 *
 * @template: the template string to instantiate, null terminated
 * @itf:      the interface to the functions that mustach calls
 * @closure:  the closure to pass to functions called
 * @fd:       the file descriptor number where to write the result
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
DEPRECATED_MUSTACH(extern int fdmustach(const char *template, const struct mustach_ds_mgr_methods_s *itf, void *closure, int fd));

/**
 * OBSOLETE use mustach_mem
 *
 * mustach - Renders the mustache 'template' in 'result' for 'itf' and 'closure'.
 *
 * @template: the template string to instantiate, null terminated
 * @itf:      the interface to the functions that mustach calls
 * @closure:  the closure to pass to functions called
 * @result:   the pointer receiving the result when 0 is returned
 * @size:     the size of the returned result
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
DEPRECATED_MUSTACH(extern int mustach(const char *template, const struct mustach_ds_mgr_methods_s *itf, void *closure, char **result, size_t *size));

#endif

