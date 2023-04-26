/* #include <wordexp.h> */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "log.h"
#include "libs7.h"

#include "libc_s7.h"
#include "libm_s7.h"
#include "libcwalk_s7.h"
#include "libdl_s7.h"

// WITH_MAIN && ! WITH_NOTCURSES && ! WITH_C_LOADER
// ! USE_SND

// WITH_C_LOADER means: s7 'load' can handle .so files using dlopen.
// no C_LOADER means: s7.c does not include 'load_shared_object()'.
// Loads will only load scm source files, using fopen. So (load
// "foo.so") will not work?

/* void libc_s7_init(s7_scheme *sc); */

int main(int argc, char **argv)
{
  s7_scheme *s7 = libs7_init();
  fprintf(stderr, "s7: %s\n", S7_DATE);

  /* s7_pointer wrde = s7_make_integer(s7, (s7_int)WRDE_SYNTAX); */

  /* now init static lib*_s7 libs */

  clib_sinit(s7, libc_s7_init, "libc");

  /* s7_pointer e = s7_inlet(s7, s7_nil(s7)); */
  /* s7_int gc_loc = s7_gc_protect(s7, e); */
  /* s7_pointer old_e = s7_set_curlet(s7, e); */
  /* libc_s7_init(s7); */
  /* s7_pointer libs = s7_slot(s7, s7_make_symbol(s7, "*libraries*")); */
  /* s7_define(s7, s7_nil(s7), s7_make_symbol(s7, "*libc*"), e); */
  /* s7_slot_set_value(s7, libs, s7_cons(s7, s7_cons(s7, s7_make_semipermanent_string(s7, "libc.scm"), e), s7_slot_value(libs))); */

  /* s7_set_curlet(s7, old_e);       /\* restore incoming (curlet) *\/ */
  /* s7_gc_unprotect_at(s7, gc_loc); */

  /* TODO: add scm libs from runfiles to *load-path* */

  /* char *script = "repl/repl.scm"; */
  char *script = "external/libs7/repl/repl.scm";

  if (argc == 2)
    {
      fprintf(stderr, "load %s\n", argv[1]);
      if (!s7_load(s7, argv[1]))
	{
	  fprintf(stderr, "can't load %s\n", argv[1]);
	  return(2);
	}}
  else
    {
    /* s7_pointer lp = s7_load_path(s7); */
    /* char *s = s7_object_to_c_string(s7, lp); */
    /* log_debug("load-path: %s", s); */
    /* free(s); */

    /* log_debug("pwd: %s", getcwd(NULL,0)); */

        if (!s7_load(s7, script)) {
            log_error("failed: load %s", script);
        }
      s7_eval_c_string(s7, "((*repl* 'run))");
    }
  return(0);
}

