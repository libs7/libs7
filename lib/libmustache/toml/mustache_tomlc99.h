/*
 Original author: José Bollo <jobol@nonadev.net>
 https://gitlab.com/jobol/mustach
 SPDX-License-Identifier: ISC

 Modified by Gregg Reynolds
*/

#ifndef _MUSTACHE_TOMLC99_H_
#define _MUSTACHE_TOMLC99_H_

#include "toml.h"
#include "tomlx.h"
#include "mustache_toml_ds_mgr.h"

/**
 * Wrap interface used internally by mustach toml functions.
 * Can be used for overriding behaviour.
 */

// was: struct mustach_wrap_itf
extern const struct mustach_ds_methods_s toml_methods;

/* extern int mustach_tomlc99_render_to_file(const char *template, size_t length, cJSON *root, int flags, FILE *file); */

/* void mustach_free(void *json_c); */

/**
 * mustach_tomlc99_render_to_fd - Renders the mustache 'template' in 'fd' for 'root'.
 *
 * @fd:       the file descriptor number where to write the result
 * @template: the template string to instanciate
 * @tlength:   length of the template or zero if unknown and template null terminated
 * @json_root:     the root json object to render
 * @data_schema:  JSON or SCHEME
 * @flags
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
/* extern int mustach_tomlc99_render_to_fd(const char *template, size_t length, cJSON *root, int flags, int fd); */

/**
 * mustache_toml_render - Render to string.
 *
 * @template: the template string to instanciate
 * @length:   length of the template or zero if unknown and template null terminated
 * @root:     the root toml_table_t* object to render
 * @flags:    flags
 *
 * Returns pointer to rendered string, null-terminated, or NULL in
 * case of error (errno is set). Returned string must be freed by caller.
 */
const char *mustache_toml_render(const char *template,
                                 size_t template_sz,
                                 toml_table_t *root,
                                 int flags);

int mustache_toml_frender(FILE * restrict file,
                          const char *template,
                          size_t template_sz,
                          toml_table_t *root,
                          int flags);

int mustache_toml_fdrender(int fd,
                          const char *template,
                          size_t template_sz,
                          toml_table_t *root,
                          int flags);



/* extern int mustache_toml_render(const char *template, */
/*                                             size_t template_sz, */
/*                                             toml_table_t *root, */
/*                                             int flags, */
/*                                             char **result, size_t *result_sz); */

/* mustach_toml_write - Renders the mustache 'template' for 'root' to custom writer 'writecb' with 'closure'. */

/* @template: the template string to instanciate */
/* @length:   length of the template or zero if unknown and template null terminated */
/* @root:     the root json object to render */
/* @writecb:  the function that write values */
/* @closure:  the closure for the write function */

/* Returns 0 in case of success, -1 with errno set in case of system error */
/* a other negative value in case of error. */
/* extern int mustach_toml_write(const char *template, size_t length, cJSON *root, int flags, mustach_write_cb_t *writecb, void *closure); */

/* mustach_toml_emit - Renders the mustache 'template' for 'root' to custom emiter 'emitcb' with 'closure'. */

/* @template: the template string to instanciate */
/* @length:   length of the template or zero if unknown and template null terminated */
/* @root:     the root json object to render */
/* @emitcb:   the function that emit values */
/* @closure:  the closure for the write function */

/* Returns 0 in case of success, -1 with errno set in case of system error */
/* a other negative value in case of error. */
/* extern int mustach_toml_emit(const char *template, size_t length, cJSON *root, int flags, mustach_emit_cb_t *emitcb, void *closure); */

/* toml_table_t *mustach_toml_read_string(char *s); */
/* toml_table_t *mustach_toml_read_file(char *fname); */

#endif

