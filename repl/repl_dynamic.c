#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "log.h"
#include "libs7.h"

#if ! defined(CLIBS_LINK_RUNTIME)
#include "libc_s7.h"
#include "libm_s7.h"
#include "libcwalk_s7.h"
#include "libdl_s7.h"
#endif

void s7_repl(s7_scheme *s7)
{
    /* log_debug("s7_repl starting"); */

    /* s7_pointer lp = s7_load_path(s7); */
    /* char *s = s7_object_to_c_string(s7, lp); */
    /* log_debug("load-path: %s", s); */
    /* free(s); */


  bool repl_loaded = false;

#if defined(CLIBS_LINK_RUNTIME)
    clib_dload_ns(s7, "libc_s7", "libc", DSO_EXT);
    clib_dload_ns(s7, "libdl_s7", "libdl", DSO_EXT);
    clib_dload_global(s7, "libm_s7", "libm.scm", DSO_EXT);
    clib_dload_global(s7, "libcwalk_s7", "libcwalk.scm", DSO_EXT);
#else  /* link:static? or link:shared? */
    clib_sinit(s7, libc_s7_init, "libc");
    clib_sinit(s7, libdl_s7_init, "libdl");
    clib_sinit(s7, libm_s7_init, "libm");
    clib_sinit(s7, libcwalk_s7_init, "libcwalk");
#endif

  /* /\* s7_pointer e = s7_inlet(s7, set_clist_2(s7, make_symbol(s7, "init_func", 9), make_symbol(s7, "libc_s7_init", 12))); *\/ */
  /* s7_pointer init_sym    = s7_make_symbol(s7, "init_func"); */
  /* s7_pointer init_fn_sym = s7_make_symbol(s7, "libc_s7_init"); */
  /* s7_pointer init_list = s7_list(s7, 2, init_sym, init_fn_sym); */
  /* s7_pointer e = s7_inlet(s7, init_list); */

  /* /\* s7_int gc_loc = s7_gc_protect(s7, e); *\/ */
  /* s7_int gc_loc = s7_gc_protect(s7, e); */

  /* /\* s7_pointer old_e = s7_set_curlet(s7, e);   /\\* e is now (curlet) so loaded names from libc will be placed there, not in (rootlet) *\\/ *\/ */
  /* s7_pointer old_e = s7_set_curlet(s7, e); */

  /* char *script = "scm/s7/repl.scm"; */
  /* char *libc_s7_path = "lib/libc_s7.so"; */

  /* /\* char *script = "external/libs7/scm/s7/repl.scm"; *\/ */
  /* /\* char *libc_s7_path = "external/libs7/lib/libc_s7.so"; *\/ */

  /* /\* log_debug("LOADING libc_s7.so"); *\/ */
  /* s7_pointer val = s7_load_with_environment(s7, libc_s7_path, e); */

  /* if (val) */
  /*   { */
  /*       /\* log_debug("loaded libc_s7.so"); *\/ */
  /*     /\* s7_pointer libs = global_slot(s7->libraries_symbol); *\/ */
  /*     s7_pointer libs = s7_slot(s7, s7_make_symbol(s7, "*libraries*")); */

  /*     /\* uint64_t hash = raw_string_hash((const uint8_t *)"*libc*", 6);  /\\* hack around an idiotic gcc 10.2.1 warning *\\/ *\/ */
  /*     /\* s7_define(s7, sc->nil, new_symbol(s7, "*libc*", 6, hash, hash % SYMBOL_TABLE_SIZE), e); *\/ */
  /*     s7_define(s7, s7_nil(s7), s7_make_symbol(s7, "*libc*"), e); */

  /*     /\* slot_set_value(libs, cons(s7, cons(s7, s7_make_semipermanent_string(s7, "libc.scm"), e), slot_value(libs))); *\/ */
  /*     s7_slot_set_value(s7, libs, s7_cons(s7, s7_cons(s7, s7_make_semipermanent_string(s7, "libc.scm"), e), s7_slot_value(libs))); */
  /*   } else { */
  /*       log_error("FAILED loading libc_s7.so"); */
  /* } */

  /* s7_set_curlet(s7, old_e);       /\* restore incoming (curlet) *\/ */
  /* s7_gc_unprotect_at(s7, gc_loc); */

  /* if (!val) /\* s7_load was unable to find/load libc_s7.so *\/ */
  /*   dumb_repl(s7); */
  /* else */
    /* { */
#if S7_DEBUGGING
      s7_autoload(s7, make_symbol(s7, "compare-calls", 13), s7_make_string(s7, "compare-calls.scm"));
      s7_autoload(s7, make_symbol(s7, "get-overheads", 13), s7_make_string(s7, "compare-calls.scm"));
#endif

      /* char *script = "repl/repl.scm"; */
      char *script = "external/libs7/repl/repl.scm";

      s7_provide(s7, "libc.scm");
      if (!repl_loaded) {
          /* log_debug("loading repl.scm"); */
          if (!s7_load(s7, script)) {
              log_error("failed: load repl.scm");
          }
      }
      s7_eval_c_string(s7, "((*repl* 'run))");
    /* } */
}

int main(int argc, char **argv) // , char **envp)
{
  s7_scheme *s7 = libs7_init();
  /* fprintf(stderr, "s7: %s\n", S7_DATE); */

  /* for (char **env = envp; *env != 0; env++) */
  /*  { */
  /*    char *thisEnv = *env; */
  /*    printf("%s\n", thisEnv); */
  /*  } */
  /* char *pwd = getcwd(NULL, 0); */
  /* fprintf(stderr, "PWD: %s\n", pwd); */
  /* free(pwd); */

  if (argc == 2) {
      fprintf(stderr, "load %s\n", argv[1]);
      if (!s7_load(s7, argv[1]))
          {
              fprintf(stderr, "can't load %s\n", argv[1]);
              return(2);
          }
  } else {
#ifdef S7_LOAD_PATH
      s7_add_to_load_path(s7, S7_LOAD_PATH);
#else
      /* char *dir = realdir(argv[0]); */
      /*   log_debug("Axxxxxxxxxxxxxxxx"); */
      /* if (dir) { */
      /*     s7_add_to_load_path(s7, dir); */
      /*     free(dir); */
      /* } */
#endif
      s7_repl(s7);
  }
  return(0);
}
