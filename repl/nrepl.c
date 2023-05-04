#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _MSC_VER
  #include <errno.h>
  #include <unistd.h>
#endif

#include "log.h"
#include "s7.h"

s7_scheme *sc;

/* obazl
   bazel will not #include c sources so we compile notcurses_s7.c separately.
   and must include here stuff from notcurses_s7.c needed here
*/
/* #include "notcurses_s7.c" */

/* #include <locale.h> */
#include <notcurses/notcurses.h>
#include <notcurses/direct.h>
#include <notcurses/version.h>
/* prototypes make the compiler happy */
void libc_s7_init(s7_scheme *sc);
void notcurses_s7_init(s7_scheme *sc);
/* void libdl_s7_init(s7_scheme *sc); */
/* void libm_s7_init(s7_scheme *sc); */

/* uint64_t raw_string_hash(const uint8_t *key, s7_int len); */

/* end notcurses_s7.c inclusions */

/* libc stuff used in nrepl.scm (this is extracted from libc_s7.c created by cload.scm from libc.scm)
   moved to nrepl_nlibc.c (obazl)
 */

/* catch sigint */
#include <signal.h>

static struct sigaction new_action, old_action;
static s7_scheme *s7;
static struct notcurses *nc;

void eval_sigint_handler(int signum)
{
  s7_error(s7, s7_t(s7), s7_list(s7, 1, s7_make_string(s7, "interrupted")));
}

static s7_pointer set_sigint_handler(s7_scheme *sc, s7_pointer args)
{
  s7 = sc;
  new_action.sa_handler = eval_sigint_handler;
  sigemptyset(&new_action.sa_mask);
  new_action.sa_flags = SA_RESTART;
  sigaction(SIGINT, &new_action, NULL);
  return(s7_f(sc));
}

void exit_sigint_handler(int signum)
{
  s7_quit(s7);
  notcurses_stop(nc); /* using the actual old_action ("fatal_handler") does not clean up completely -- mouse chatter */
  exit(0);
}

static s7_pointer unset_sigint_handler(s7_scheme *sc, s7_pointer args)
{
  s7 = sc;
  nc = (struct notcurses *)s7_c_pointer(s7_car(args));
  old_action.sa_handler = exit_sigint_handler;
  sigemptyset(&old_action.sa_mask);
  old_action.sa_flags = SA_RESTART;
  sigaction(SIGINT, &old_action, NULL);
  return(s7_f(sc));
}


/* moved to nrepl_nlibc.c:
static void init_nlibc(s7_scheme *sc)
*/

int main(int argc, char **argv)
{
    log_debug("nrepl main");

    /* initialize, in order:
       s7, either libc_s7 or nrepl_nlibc, notcurses_s7
    */

    s7_scheme *sc;
    sc = s7_init();

    log_debug("s7 initialized");

    s7_pointer e = s7_inlet(sc, s7_nil(sc));
    s7_int gc_loc = s7_gc_protect(sc, e);
    s7_pointer old_e = s7_set_curlet(sc, e);
    libc_s7_init(sc);
    s7_pointer libs = s7_slot(sc, s7_make_symbol(sc, "*libraries*"));
    s7_define(sc, s7_nil(sc), s7_make_symbol(sc, "*libc*"), e);
    s7_slot_set_value(sc, libs, s7_cons(sc, s7_cons(sc, s7_make_semipermanent_string(sc, "libc.scm"), e), s7_slot_value(libs)));

    s7_set_curlet(sc, old_e);       /* restore incoming (curlet) */
    s7_gc_unprotect_at(sc, gc_loc);

    log_debug("libc_s7 initialized");

//FIXME: do we need this or should we just use libc_s7?
//  init_nlibc(sc);

    e = s7_inlet(sc, s7_nil(sc));
    gc_loc = s7_gc_protect(sc, e);
    old_e = s7_set_curlet(sc, e);
    notcurses_s7_init(sc);
    libs = s7_slot(sc, s7_make_symbol(sc, "*libraries*"));
    s7_define(sc, s7_nil(sc), s7_make_symbol(sc, "*notcurses*"), e);
    s7_slot_set_value(sc, libs, s7_cons(sc, s7_cons(sc, s7_make_semipermanent_string(sc, "notcurses"), e), s7_slot_value(libs)));

    s7_set_curlet(sc, old_e);       /* restore incoming (curlet) */
    s7_gc_unprotect_at(sc, gc_loc);

    log_debug("notcurses_s7 initialized");

  s7_define_function(sc, "set-sigint-handler", set_sigint_handler, 0, 0, false, "");
  s7_define_function(sc, "unset-sigint-handler", unset_sigint_handler, 1, 0, false, "");

  /*FIXME notcurses_s7_init(sc); */

  if (argc >= 2)
    {
      if (strcmp(argv[1], "-e") == 0)
	{
	  s7_pointer x;
	  x = s7_eval_c_string(sc, argv[2]);
	  fprintf(stdout, "%s\n", s7_object_to_c_string(sc, x));
	  return(0);
	}
      fprintf(stderr, "load %s\n", argv[1]);
      errno = 0;
      if (!s7_load(sc, argv[1]))
	{
	  fprintf(stderr, "%s: %s\n", strerror(errno), argv[1]);
	  return(2);
	}
    }
  else
    {
#ifdef _MSC_VER
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
	}
    }
  fprintf(stdout, "\n");
  if (ferror(stdin))
    fprintf(stderr, "read error on stdin\n");
#else // not _MSC_VER
    s7_add_to_load_path(sc, "s7"); /* FIXME: hardcoded path */

#ifdef S7_LOAD_PATH
      s7_add_to_load_path(sc, S7_LOAD_PATH);
#endif
#if (!NREPL_DEBUGGING)
      //FIXME: hard-coded sys path
      s7_add_to_load_path(sc, "/usr/local/share/s7");
      #include "nrepl-bits.h"
      s7_load_c_string(sc, (const char *)nrepl_scm, nrepl_scm_len);
#else  /* NREPL_DEBUGGING == USE_SND == defined, i.e. no_main */
    if (!s7_load(sc, "scm/nrepl.scm")) {
        log_error("load failed: nrepl.scm");
	return(1);
    }
#endif
    log_debug("launching nrepl");
    // nrepl.scm now loaded
    //TODO: verify by checking for *nrepl*
    // extracted from nrepl.scm:
    s7_eval_c_string(sc, ""
                     "(with-let *nrepl* "
                     "  (start) "
                     "  (run) "
                     "  (stop)) ");
    log_debug("exiting nrepl");
#endif  // #ifdef _MSC_VER
    }
  return(0);
}
