/* moved here from the tail end of s7.c */

#ifndef USE_SND
  #define USE_SND 0
#endif
#ifndef WITH_MAIN
  #define WITH_MAIN 0
#endif

#if WITH_MAIN && WITH_NOTCURSES
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
#if (!WITH_C_LOADER)
  dumb_repl(sc);
#else
#if WITH_NOTCURSES
  s7_load(sc, "nrepl.scm");
#else
  /* try to get lib_s7.so from the repl's directory, and set *libc*.
   *   otherwise repl.scm will try to load libc.scm which will try to build libc_s7.so locally, but that requires s7.h
   */
  bool repl_loaded = false;
  s7_pointer e = s7_inlet(sc, set_clist_2(sc, make_symbol(sc, "init_func", 9), make_symbol(sc, "libc_s7_init", 12)));
  s7_int gc_loc = s7_gc_protect(sc, e);
  s7_pointer old_e = s7_set_curlet(sc, e);   /* e is now (curlet) so loaded names from libc will be placed there, not in (rootlet) */
  s7_pointer val = s7_load_with_environment(sc, "libc_s7.so", e);
  if (val)
    {
      s7_pointer libs = global_slot(sc->libraries_symbol);
      uint64_t hash = raw_string_hash((const uint8_t *)"*libc*", 6);  /* hack around an idiotic gcc 10.2.1 warning */
      s7_define(sc, sc->nil, new_symbol(sc, "*libc*", 6, hash, hash % SYMBOL_TABLE_SIZE), e);
      slot_set_value(libs, cons(sc, cons(sc, s7_make_semipermanent_string(sc, "libc.scm"), e), slot_value(libs)));
    }

  s7_set_curlet(sc, old_e);       /* restore incoming (curlet) */
  s7_gc_unprotect_at(sc, gc_loc);

  if (!val) /* s7_load was unable to find/load libc_s7.so */
    dumb_repl(sc);
  else
    {
#if S7_DEVBUILD
      s7_autoload(sc, make_symbol(sc, "compare-calls", 13), s7_make_string(sc, "compare-calls.scm"));
      s7_autoload(sc, make_symbol(sc, "get-overheads", 13), s7_make_string(sc, "compare-calls.scm"));
#endif // S7_DEVBUILD
      s7_provide(sc, "libc.scm");
      if (!repl_loaded) s7_load(sc, "repl.scm");
      s7_eval_c_string(sc, "((*repl* 'run))");
    }
#endif // #else not WITH_NOTCURSES
#endif // # else not WITH_CLOADER
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
#endif // #if (!MS_WINDOWS) && WITH_C_LOADER

int main(int argc, char **argv)
{
  s7_scheme *sc = s7_init();
  fprintf(stderr, "s7: %s\n", S7_DATE);

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
#if (MS_WINDOWS) || (!WITH_C_LOADER) || ((defined(__linux__)) && (!defined(__GLIBC__))) /* musl? */
      dumb_repl(sc);
#else
#ifdef S7_LOAD_PATH
      s7_add_to_load_path(sc, S7_LOAD_PATH);
#else
      char *dir = realdir(argv[0]);
      if (dir)
	{
	  s7_add_to_load_path(sc, dir);
	  free(dir);
	}
#endif // ! S7_LOAD_PATH, WITH_MAIN, WITH_CLOADER
      // ! WINDOWS || WITH_CLOADER || linux w/GLIBC
      s7_repl(sc);
#endif // false: #if (MS_WINDOWS) || (!WITH_C_LOADER) || ((defined(__linux__)) && (!defined(__GLIBC__))) /* musl? */
    }
  return(0);
}

