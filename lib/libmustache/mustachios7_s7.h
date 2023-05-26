/*
 Author: José Bollo <jobol@nonadev.net>

 https://gitlab.com/jobol/mustach

 SPDX-License-Identifier: ISC
*/

#ifndef _mustachios7_s7_h_included_
#define _mustachios7_s7_h_included_

/*
 * mustach-s7 is intended to make integration of s7
 * library by providing integrated functions.
 */

#include "libs7.h"
#include "mustachios7_scm.h"

struct tstack_s {
    int predicate;
    bool lambda;
    s7_pointer root;
    s7_pointer selection;       /* ? */
    int depth;                  /* stack height; 1 stackframe per hashtag */
    struct {
        s7_pointer cont;        /* context? containing object? */
        s7_pointer obj;         /* selection? current elt of ctx?  */
        s7_pointer iter;
        int is_objiter;         /* obj type has iterators */
        size_t index, count;    /* current idx, sibling count */
        int predicate;         /*  */
        bool lambda;            /*  */
        int  workbuf_idx;
    } stack[MUSTACH_MAX_DEPTH];
};

/**
 * Wrap interface used internally by mustach s7 functions.
 * Can be used for overriding behaviour.
 */
extern const struct mustach_wrap_itf mustach_wrap_itf_scm;

/* /\* **************************************************************** *\/ */
/* int mustach_fprintf(FILE * restrict file, */
/*                     const char *restrict template, size_t tlength, */
/*                     void *json_root, // cJSON*, json_object, s7_pointer, etc. */
/*                     int data_schema, // JSON or SCHEME */
/*                     int flags); */

/* /\* s7_pointer mustach_deserialize(char *sexp_str, size_t len); *\/ */

/* int mustach_dprintf(int fd, */
/*                      const char * restrict template, size_t tlength, */
/*                      void *json_root, */
/*                      int data_schema, // JSON or SCHEME */
/*                      int flags); */

/* /\* **************************************************************** *\/ */
/* /\** */
/*  * mustach_s7_file - Renders the mustache 'template' in 'file' for 'root'. */
/*  * */
/*  * @template: the template string to instanciate */
/*  * @length:   length of the template or zero if unknown and template null terminated */
/*  * @root:     the root json object to render */
/*  * @file:     the file where to write the result */
/*  * */
/*  * Returns 0 in case of success, -1 with errno set in case of system error */
/*  * a other negative value in case of error. */
/*  *\/ */
/* extern int mustach_s7_file(const char *template, size_t length, s7_pointer root, int flags, FILE *file); */

/* /\** */
/*  * mustach_s7_fd - Renders the mustache 'template' in 'fd' for 'root'. */
/*  * */
/*  * @template: the template string to instanciate */
/*  * @length:   length of the template or zero if unknown and template null terminated */
/*  * @root:     the root json object to render */
/*  * @fd:       the file descriptor number where to write the result */
/*  * */
/*  * Returns 0 in case of success, -1 with errno set in case of system error */
/*  * a other negative value in case of error. */
/*  *\/ */
/* extern int mustach_s7_fd(const char *template, size_t length, s7_pointer root, int flags, int fd); */


/* /\** */
/*  * mustach_s7_mem - Renders the mustache 'template' in 'result' for 'root'. */
/*  * */
/*  * @template: the template string to instanciate */
/*  * @length:   length of the template or zero if unknown and template null terminated */
/*  * @root:     the root json object to render */
/*  * @result:   the pointer receiving the result when 0 is returned */
/*  * @size:     the size of the returned result */
/*  * */
/*  * Returns 0 in case of success, -1 with errno set in case of system error */
/*  * a other negative value in case of error. */
/*  *\/ */
/* extern int mustach_s7_mem(const char *template, size_t length, s7_pointer root, int flags, char **result, size_t *size); */

/* /\** */
/*  * mustach_s7_write - Renders the mustache 'template' for 'root' to custom writer 'writecb' with 'closure'. */
/*  * */
/*  * @template: the template string to instanciate */
/*  * @length:   length of the template or zero if unknown and template null terminated */
/*  * @root:     the root json object to render */
/*  * @writecb:  the function that write values */
/*  * @closure:  the closure for the write function */
/*  * */
/*  * Returns 0 in case of success, -1 with errno set in case of system error */
/*  * a other negative value in case of error. */
/*  *\/ */
/* extern int mustach_s7_write(const char *template, size_t length, s7_pointer root, int flags, mustach_write_cb_t *writecb, void *closure); */

/* /\** */
/*  * mustach_s7_emit - Renders the mustache 'template' for 'root' to custom emiter 'emitcb' with 'closure'. */
/*  * */
/*  * @template: the template string to instanciate */
/*  * @length:   length of the template or zero if unknown and template null terminated */
/*  * @root:     the root json object to render */
/*  * @emitcb:   the function that emit values */
/*  * @closure:  the closure for the write function */
/*  * */
/*  * Returns 0 in case of success, -1 with errno set in case of system error */
/*  * a other negative value in case of error. */
/*  *\/ */
/* int mustach_s7_emit(const char *template, size_t length, s7_pointer root, int flags, mustach_emit_cb_t *emitcb, void *closure); */

/**
 * libs7_mustach_render
 *
 * @template
 * @data
 * @port (defaults to #f which returns a string)
 * @flags (optional)
 */

/* /\* **************************************************************** */
/*  * libs7-specific API */
/* *\/ */

/* /\* by default we render scm data *\/ */
/* s7_pointer libs7_mustach_render(s7_scheme *s7, s7_pointer args); */

/* s7_pointer libmustachios7_mustach_render_json(s7_scheme *s7, s7_pointer args); */

/* s7_pointer libmustachios7_s7_init(s7_scheme *sc); */

#endif

