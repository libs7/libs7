/* #include <wordexp.h> */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "log.h"
#include "s7.h"

// WITH_MAIN && ! WITH_NOTCURSES && ! WITH_C_LOADER
// ! USE_SND

// WITH_C_LOADER means: s7 'load' can handle .so files using dlopen.
// no C_LOADER means: s7.c does not include 'load_shared_object()'.
// Loads will only load scm source files, using fopen. So (load
// "foo.so") will not work?

static void dumb_repl(s7_scheme *sc)
{
  while (true)
    {
      char buffer[512];
      fprintf(stdout, "\n> ");
      if (!fgets(buffer, 512, stdin)) break;  /* error or ctrl-D */
      if (((buffer[0] != '\n') || (strlen(buffer) > 1)))
	{
	  char response[1024];
	  snprintf(response, 1024, "(write %s)", buffer);
	  s7_eval_c_string(sc, response);
	}}
  fprintf(stdout, "\n");
  if (ferror(stdin))
    fprintf(stderr, "read error on stdin\n");
}

void libc_s7_init(s7_scheme *sc);

int main(int argc, char **argv)
{
  s7_scheme *sc = s7_init();
  fprintf(stderr, "s7: %s\n", S7_DATE);

  /* s7_pointer wrde = s7_make_integer(sc, (s7_int)WRDE_SYNTAX); */

  /* now init static lib*_s7 libs */
  s7_pointer e = s7_inlet(sc, s7_nil(sc));
  s7_int gc_loc = s7_gc_protect(sc, e);
  s7_pointer old_e = s7_set_curlet(sc, e);
  libc_s7_init(sc);
  s7_pointer libs = s7_slot(sc, s7_make_symbol(sc, "*libraries*"));
  s7_define(sc, s7_nil(sc), s7_make_symbol(sc, "*libc*"), e);
  s7_slot_set_value(sc, libs, s7_cons(sc, s7_cons(sc, s7_make_semipermanent_string(sc, "libc.scm"), e), s7_slot_value(libs)));

  s7_set_curlet(sc, old_e);       /* restore incoming (curlet) */
  s7_gc_unprotect_at(sc, gc_loc);

  /* TODO: add scm libs from runfiles to *load-path* */

  if (argc == 2)
    {
      fprintf(stderr, "load %s\n", argv[1]);
      if (!s7_load(sc, argv[1]))
	{
	  fprintf(stderr, "can't load %s\n", argv[1]);
	  return(2);
	}}
  else
    {
    /* s7_pointer lp = s7_load_path(sc); */
    /* char *s = s7_object_to_c_string(sc, lp); */
    /* log_debug("load-path: %s", s); */
    /* free(s); */

    /* log_debug("pwd: %s", getcwd(NULL,0)); */

      /* dumb_repl(sc); */
        /* log_debug("loading repl.scm"); */
        if (!s7_load(sc, "../libs7/scm/s7/repl.scm")) {
        /* if (!s7_load(sc, "scm/s7/repl.scm")) { */
            log_error("failed: load repl.scm");
        }
      s7_eval_c_string(sc, "((*repl* 'run))");
    }
  return(0);
}

