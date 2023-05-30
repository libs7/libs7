/*
 Author: José Bollo <jobol@nonadev.net>

 https://gitlab.com/jobol/mustach

 SPDX-License-Identifier: ISC
*/

#ifndef _mustach_toml_ds_mgr_h_included_
#define _mustach_toml_ds_mgr_h_included_

#include "mustach.h"
#include "mustachios7_wrap.h"

#ifndef _mustach_output_callbacks_defined_
#define _mustach_output_callbacks_defined_
typedef int mustach_write_cb_t(void *closure, const char *buffer, size_t size);
typedef int mustach_emit_cb_t(void *closure, const char *buffer, size_t size, int escape);
#endif

/**
 * Mustach interface used internally by mustach wrapper functions.
 * Can be used for overriding behaviour.
 */
static const struct mustach_ds_mgr_methods_s toml_ds_mgr_methods;

int mustach_toml_file(const char *template, size_t length,
                      const struct mustach_ds_methods_s *methods, // itf
                      void *closure,
                      int flags, FILE *file);

int mustach_toml_fd(const char *template, size_t length,
                    const struct mustach_ds_methods_s *methods, // itf
                    void *closure,
                    int flags, int fd);

int mustach_toml_render_to_string(const char *template, size_t length,
                                  const struct mustach_ds_methods_s *methods,
                                  void *closure,
                                  int flags, char **result, size_t *size);

int mustach_toml_write(const char *template, size_t length,
                       const struct mustach_ds_methods_s *methods,
                       void *closure,
                       int flags,
                       mustach_write_cb_t *writecb, void *writeclosure);

int mustach_toml_emit(const char *template, size_t length,
                      const struct mustach_ds_methods_s *methods,
                      void *closure,
                      int flags,
                      mustach_emit_cb_t *emitcb, void *emitclosure);

#endif

