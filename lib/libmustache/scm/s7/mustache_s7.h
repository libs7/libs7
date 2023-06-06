/*
 Original author: José Bollo <jobol@nonadev.net>
 https://gitlab.com/jobol/mustach

 Modifications: G. Reynolds

 SPDX-License-Identifier: ISC
*/

#ifndef _MUSTACHE_S7_H_
#define _MUSTACHE_S7_H_

/*
 * mustach-s7 is intended to make integration of s7
 * library by providing integrated functions.
 */

#include "libs7.h"
#include "mustache_scm_ds_mgr.h"

/* FIXME: put this in mustache_s7.c */
/* WARNING: always initialize, e.g.
       memset(&stack, '\0', sizeof(struct tstack_s));
*/
struct tstack_s {
    int predicate;
    bool lambda;
    s7_pointer root;
    s7_pointer selection;       /* ? */
    int depth;                  /* stack height; 1 stackframe per hashtag */
    struct {
        s7_pointer ctx;        /* context? containing object? */
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

const char *mustache_scm_render(const char *template,
                                size_t template_sz,
                                s7_pointer data,
                                int _flags,
                                errno_t *status);

int mustache_scm_frender(FILE *f,
                         const char *template,
                         size_t template_sz,
                         s7_pointer data,
                         int _flags);

int mustache_scm_fdrender(int fd,
                          const char *template,
                          size_t template_sz,
                          s7_pointer data,
                          int _flags);

#endif

