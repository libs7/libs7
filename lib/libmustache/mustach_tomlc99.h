/*
 Original author: José Bollo <jobol@nonadev.net>
 https://gitlab.com/jobol/mustach
 SPDX-License-Identifier: ISC

 Modified by Gregg Reynolds
*/

#ifndef _mustach_tomlc99_h_included_
#define _mustach_tomlc99_h_included_

#include "toml.h"
#include "mustach_toml.h"

/**
 * Wrap interface used internally by mustach cJSON functions.
 * Can be used for overriding behaviour.
 */

extern const struct mustach_wrap_itf mustach_wrap_itf_toml;

/**
 * mustach_fprintf - Renders the mustache 'template' in 'file' for 'root'.
 *
 * @file:     the file where to write the result
 * @template: the template string to instanciate
 * @tlength:   length of the template or zero if unknown and template null terminated
 * @json_root:    the root data object to render
 * @data_schema:  JSON or SCHEME
 * @flags
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
/* int mustach_fprintf(FILE * restrict file, */
/*                     const char *restrict template, size_t tlength, */
/*                     void *json_root, // cJSON*, json_object, s7_pointer, etc. */
/*                     int data_schema, // JSON or SCHEME */
/*                     int flags); */

/* extern int mustach_toml_file(const char *template, size_t length, cJSON *root, int flags, FILE *file); */

/* void *mustach_encode_cjson(char *json_str, size_t len); */

/* void mustach_free(void *json_c); */

/**
 * mustach_toml_fd - Renders the mustache 'template' in 'fd' for 'root'.
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
/* int mustach_dprintf(int fd, */
/*                      const char * restrict template, size_t tlength, */
/*                      void *json_root, */
/*                      int data_schema, // JSON or SCHEME */
/*                      int flags); */

/* extern int mustach_toml_fd(const char *template, size_t length, cJSON *root, int flags, int fd); */

/**
 * mustach_toml_mem - Renders the mustache 'template' in 'result' for 'root'.
 *
 * @template: the template string to instanciate
 * @length:   length of the template or zero if unknown and template null terminated
 * @root:     the root json object to render
 * @result:   the pointer receiving the result when 0 is returned
 * @size:     the size of the returned result
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
/* extern int mustach_toml_mem(const char *template, size_t length, cJSON *root, int flags, char **result, size_t *size); */


/**
 * mustach_sprintf - Applies the mustache 'template' to 'json_root' and writes output to str.
 *
 * @ret:      ptr to dynamically allocated buffer containing result
 * @template: the template string to instantiate
 * @length:   length of the template or zero if unknown and template null terminated
 * @json_root:     the root json object to render
 * @data_schema:  JSON or SCHEME
 * @flags
 *
 * Returns number of characters printed or a negative value if an error occurs.
 */
/* size_t mustach_asprintf(char **ret, */
/*                         const char *template, size_t tlength, */
/*                         void *json_root, */
/*                         int data_schema, // JSON or SCHEME */
/*                         int flags); */

/**
 * mustach_toml_write - Renders the mustache 'template' for 'root' to custom writer 'writecb' with 'closure'.
 *
 * @template: the template string to instanciate
 * @length:   length of the template or zero if unknown and template null terminated
 * @root:     the root json object to render
 * @writecb:  the function that write values
 * @closure:  the closure for the write function
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
/* extern int mustach_toml_write(const char *template, size_t length, cJSON *root, int flags, mustach_write_cb_t *writecb, void *closure); */

/**
 * mustach_toml_emit - Renders the mustache 'template' for 'root' to custom emiter 'emitcb' with 'closure'.
 *
 * @template: the template string to instanciate
 * @length:   length of the template or zero if unknown and template null terminated
 * @root:     the root json object to render
 * @emitcb:   the function that emit values
 * @closure:  the closure for the write function
 *
 * Returns 0 in case of success, -1 with errno set in case of system error
 * a other negative value in case of error.
 */
/* extern int mustach_toml_emit(const char *template, size_t length, cJSON *root, int flags, mustach_emit_cb_t *emitcb, void *closure); */

#endif

