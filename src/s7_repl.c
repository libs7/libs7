#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "utstring.h"
#include "s7.h"

//FIXME: what about cross-compilation? building on mac targeting linux?
#if defined(__APPLE__)
#define DSO_EXT ".dylib"
#define DSO_EXT_LEN 6
#else
#define DSO_EXT ".so"
#define DSO_EXT_LEN 3
#endif

#ifndef USE_SND
  #define USE_SND 0
#endif
#ifndef WITH_MAIN
  #define WITH_MAIN 0
#endif

#if WITH_MAIN && WITH_NOTCURSES
  #define S7_MAIN 1
  #include "nrepl.c"
  /* gcc -o nrepl s7.c -O2 -I. -Wl,-export-dynamic -lm -ldl -DWITH_MAIN -DWITH_NOTCURSES -lnotcurses-core */
#else

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

void s7_repl(s7_scheme *sc)
{
    printf("s7.c: s7_repl\n");
#if (!WITH_C_LOADER)
  dumb_repl(sc);
#else
#if WITH_NOTCURSES
  s7_load(sc, "nrepl.scm");
#else
  /* s7_pointer old_e, e, val; */
  s7_pointer e, val;            /* obazl */
  s7_int gc_loc;
  bool repl_loaded = false;
  (void)repl_loaded; // obazl prevent -Wunused-but-set-variable
  /* try to get lib_s7.so from the repl's directory, and set *libc*.
   *   otherwise repl.scm will try to load libc.scm which will try to build libc_s7.so locally, but that requires s7.h
   */
  e = s7_inlet(sc, list_2(sc, s7_make_symbol(sc, "init_func"), s7_make_symbol(sc, "libc_s7_init")));
  gc_loc = s7_gc_protect(sc, e);
  // i couldn't figure out how to get at the libc_s7 stuff in curlet, so:
  //  old_e = s7_set_curlet(sc, e);   /* e is now (curlet) so loaded names from libc will be placed there, not in (rootlet) */

  /* printf("loading %s/%s\n", TOSTRING(OBAZL_RUNFILES_DIR), "/libc_s7.o"); */
  printf("loading libc_s7.o\n");
  printf("cwd: %s\n", getcwd(NULL, 0));

  val = s7_load_with_environment(sc, "libc_s7.so",
                                 /* "libs7/src/libc_s7.so", */
                                 /* TOSTRING(OBAZL_RUNFILES_DIR) */
                                 /* "/libc_s7.so", //OBAZL */
                                 e);
  if (val)
    {
        /* obazl printf("repl: load libc_s7.so succeeded\n"); */
      s7_pointer libs;
      uint64_t hash;
      hash = raw_string_hash((const uint8_t *)"*libc*", 6);  /* hack around an idiotic gcc 10.2.1 warning */
      s7_define(sc, sc->nil, new_symbol(sc, "*libc*", 6, hash, hash % SYMBOL_TABLE_SIZE), e);
      libs = global_slot(sc->libraries_symbol);
      slot_set_value(libs, cons(sc, cons(sc, make_permanent_string("libc.scm"), e), slot_value(libs)));
    }
  else
    {
        printf("repl: load libc_s7.so failed\n");
      val = s7_load(sc, "repl.scm");
      if (val) repl_loaded = true;
    }
  /* s7_set_curlet(sc, old_e);       /\* restore incoming (curlet) *\/ */
  s7_gc_unprotect_at(sc, gc_loc);

  if (!val) /* s7_load was unable to find/load libc_s7.so or repl.scm */
    dumb_repl(sc);
  else
    {
#if S7_DEBUGGING
      s7_autoload(sc, s7_make_symbol(sc, "compare-calls"), s7_make_string(sc, "compare-calls.scm"));
      s7_autoload(sc, s7_make_symbol(sc, "get-overheads"), s7_make_string(sc, "compare-calls.scm"));
#endif
      /* s7_provide(sc, "libc.scm"); */

      /* OBAZL: loading repl.scm doesn't work in emacs */
      printf("repl_loaded? %d\n", repl_loaded); /* OBAZL */
      if (!repl_loaded) {
          printf("Loading repl.scm\n"); /* OBAZL */
          s7_load(sc, "s7/repl.scm");
                  /* TOSTRING(OBAZL_RUNFILES_DIR) */
                  /* "/repl.scm"); /\* OBAZL *\/ */
      }
      /* s7_eval_c_string(sc, "((*repl* 'run))"); */
    }
#endif
#endif
}

#if WITH_MAIN && (!USE_SND)

#if (!MS_WINDOWS) && WITH_C_LOADER
static char *realdir(const char *filename) /* this code courtesy Lassi Kortela 4-Nov-19 */
{
  char *path;
  char *p;
  /* s7_repl wants to load libc_s7.o (for tcsetattr et al), but if it is started in a directory other than the libc_s7.so
   *   directory, it fails (it tries to build the library but that requires s7.h and libc.scm).  So here we are trying to
   *   guess the libc_s7 directory from the command line program name.  This can't work in general, but it works often
   *   enough to be worth the effort.  If S7_LOAD_PATH is set, it is used instead.
   */
  if (!strchr(filename, '/'))
    return(NULL);

  if (!(path = realpath(filename, NULL))) /* in Windows maybe GetModuleFileName(NULL, buffer, buffer_size) */
    {
      fprintf(stderr, "%s: %s\n", strerror(errno), filename);
      exit(2);
    }
  if (!(p = strrchr(path, '/')))
    {
      free(path);
      fprintf(stderr, "please provide the full pathname for %s\n", filename);
      exit(2);
    }
  if (p > path) *p = '\0'; else p[1] = 0;
  return(path);
}
#endif

/* task: determine run mode: bazel run, bazel test, or standalone. */
void _init_libc_s7(s7_scheme *s7)
{
    UT_string *libc_s7;
    utstring_new(libc_s7);
    char *dso_dir;

    if (getenv("BAZEL_TEST")) {
        /* running under 'bazel test' */
        printf("ERROR: "
               "This tool ain't ready for 'bazel test'.\n");
        exit(EXIT_FAILURE);
    }
    else if (getenv("BUILD_WORKING_DIRECTORY")) {
        /* running under 'bazel run' */
        /* runfiles are under cwd */
        dso_dir = getcwd(NULL, 0);
        printf("CWD: %s\n", dso_dir);
        utstring_printf(libc_s7, "%s/%s",
                        dso_dir,
                        "/src/libc_s7" DSO_EXT);
    } else {
        /* running outside of bazel */
        /* build_wd = launch_dir; */
        /* bzl_mode = false; */
        /* config_xdg_dirs(); */
    }

    s7_config_libc_s7(s7, utstring_body(libc_s7));
}

/* ################################################################ */
int main(int argc, char **argv)
{
  s7_scheme *s7;
  s7 = s7_init();
  fprintf(stderr, "s7: %s\n", S7_DATE);

  _init_libc_s7(s7);

  /* TODO: put 'libs7' on *load-path* */

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
#if (MS_WINDOWS) || (!WITH_C_LOADER) || ((defined(__linux__)) && (!defined(__GLIBC__))) /* musl? */
      dumb_repl(s7);
#else
#ifdef S7_LOAD_PATH
      s7_add_to_load_path(s7, S7_LOAD_PATH);
#else
      char *dir;
      dir = realdir(argv[0]);
      if (dir)
	{
	  s7_add_to_load_path(s7, dir);
	  free(dir);
	}
#endif
      s7_repl(s7);
#endif
    }
  return(0);
}
#endif
#endif
