#include <unistd.h>

#include "log.h"
#include "s7.h"

/* static void s7_set_b_7p_function(s7_scheme *sc, s7_pointer f, s7_b_7p_t df) {add_opt_func(sc, f, o_b_7p, (void *)df);} */
/* #if WITH_SYSTEM_EXTRAS */
/*   s7_set_b_7p_function(sc, global_value(sc->is_directory_symbol), is_directory_b_7p); */
/*   s7_set_b_7p_function(sc, global_value(sc->file_exists_symbol), file_exists_b_7p); */
/* #endif */

/* #if WITH_SYSTEM_EXTRAS */
/*   sc->is_directory_symbol =          defun("directory?",	is_directory,		1, 0, false); */
/*   sc->file_exists_symbol =           defun("file-exists?",	file_exists,		1, 0, false); */
/*   sc->delete_file_symbol =           defun("delete-file",	delete_file,		1, 0, false); */
/*   sc->getenv_symbol =                defun("getenv",		getenv,			1, 0, false); */
/*   sc->system_symbol =                defun("system",		system,			1, 1, false); */
/* #if (!MS_WINDOWS) */
/*   sc->directory_to_list_symbol =     defun("directory->list",   directory_to_list,	1, 0, false); */
/*   sc->file_mtime_symbol =            defun("file-mtime",	file_mtime,		1, 0, false); */
/* #endif */
/* #endif */

static bool is_directory(const char *filename)
{
#if (!MS_WINDOWS)
  #ifdef S_ISDIR
    struct stat statbuf;
    return((stat(filename, &statbuf) >= 0) &&
	   (S_ISDIR(statbuf.st_mode)));
  #endif
#endif
  return(false);
}


// in struct s7_scheme {
/* #if WITH_SYSTEM_EXTRAS */
/*   s7_pointer is_directory_symbol, file_exists_symbol, delete_file_symbol, getenv_symbol, system_symbol, directory_to_list_symbol, file_mtime_symbol; */
/* #endif */
// }

/* #if WITH_SYSTEM_EXTRAS */
#include <fcntl.h>

/* #define is_string(p)                   (type(p) == T_STRING) */

/* -------------------------------- directory? -------------------------------- */
static bool is_directory_b_7p(s7_scheme *sc, s7_pointer p)
{
  if (!s7_is_string(p))
      log_error("is_directory bad arg");
    /* sole_arg_wrong_type_error_nr(sc, sc->is_directory_symbol, p, sc->type_names[T_STRING]); */
  if (string_length(p) >= 2)
    {
      block_t *b = expand_filename(sc, string_value(p));
      if (b)
	{
	  bool result = is_directory((char *)block_data(b));
	  liberate(sc, b);
	  return(result);
	}}
  return(is_directory(string_value(p)));
}

static s7_pointer g_is_directory(s7_scheme *sc, s7_pointer args)
{
  #define H_is_directory "(directory? str) returns #t if str is the name of a directory"
  #define Q_is_directory s7_make_signature(sc, 2, sc->is_boolean_symbol, sc->is_string_symbol)
  return(s7_make_boolean(sc, is_directory_b_7p(sc, car(args))));
}

/* -------------------------------- file-exists? -------------------------------- */
static bool file_probe(const char *arg)
{
#if (!MS_WINDOWS)
  return(access(arg, F_OK) == 0);
#else
  int32_t fd = open(arg, O_RDONLY, 0);
  if (fd == -1) return(false);
  close(fd);
  return(true);
#endif
}

static bool file_exists_b_7p(s7_scheme *sc, s7_pointer p)
{
  if (!is_string(p))
    sole_arg_wrong_type_error_nr(sc, sc->file_exists_symbol, p, sc->type_names[T_STRING]);
  if (string_length(p) >= 2)
    {
      block_t *b = expand_filename(sc, string_value(p));
      if (b)
	{
	  bool result = file_probe((char *)block_data(b));
	  liberate(sc, b);
	  return(result);
	}}
  return(file_probe(string_value(p)));
}

static s7_pointer g_file_exists(s7_scheme *sc, s7_pointer args)
{
  #define H_file_exists "(file-exists? filename) returns #t if the file exists"
  #define Q_file_exists s7_make_signature(sc, 2, sc->is_boolean_symbol, sc->is_string_symbol)
  return(s7_make_boolean(sc, file_exists_b_7p(sc, car(args))));
}

/* -------------------------------- delete-file -------------------------------- */
static s7_pointer g_delete_file(s7_scheme *sc, s7_pointer args)
{
  #define H_delete_file "(delete-file filename) deletes the file filename."
  #define Q_delete_file s7_make_signature(sc, 2, sc->is_integer_symbol, sc->is_string_symbol)

  s7_pointer name = car(args);
  if (!is_string(name))
    return(sole_arg_method_or_bust(sc, name, sc->delete_file_symbol, args, sc->type_names[T_STRING]));
  if (string_length(name) > 2)
    {
      block_t *b = expand_filename(sc, string_value(name));
      if (b)
	{
	  s7_int result = unlink((char *)block_data(b));
	  liberate(sc, b);
	  return(make_integer(sc, result));
	}}
  return(make_integer(sc, unlink(string_value(name))));
}

/* -------------------------------- getenv -------------------------------- */
static s7_pointer g_getenv(s7_scheme *sc, s7_pointer args) /* r7rs says #f if no such variable. this used to return "" in that case, 6-May-22 */
{
  #define H_getenv "(getenv var) returns the value of an environment variable, or #f if none is found"
  #define Q_getenv s7_make_signature(sc, 2, s7_make_signature(sc, 2, sc->is_string_symbol, sc->not_symbol), sc->is_string_symbol)

  char *result;
  s7_pointer name = car(args);
  if (!is_string(name))
    return(sole_arg_method_or_bust(sc, name, sc->getenv_symbol, args, sc->type_names[T_STRING]));
  result = getenv(string_value(name));
  return((result) ? s7_make_string(sc, result) : sc->F);
}

/* -------------------------------- system -------------------------------- */
static s7_pointer g_system(s7_scheme *sc, s7_pointer args)
{
  #define H_system "(system command) executes the command.  If the optional second argument is #t, \
system captures the output as a string and returns it."
  #define Q_system s7_make_signature(sc, 3, s7_make_signature(sc, 2, sc->is_integer_symbol, sc->is_string_symbol), sc->is_string_symbol, sc->is_boolean_symbol)

#ifdef __EMSCRIPTEN__
  return s7_nil(sc);
#else
  s7_pointer name = car(args);

  if (!is_string(name))
    return(sole_arg_method_or_bust(sc, name, sc->system_symbol, args, sc->type_names[T_STRING]));

  if ((is_pair(cdr(args))) &&
      (cadr(args) == sc->T))
    {
      #define BUF_SIZE 256
      char buf[BUF_SIZE];
      char *str = NULL;
      int32_t cur_len = 0, full_len = 0;
      FILE *fd = popen(string_value(name), "r");
      while (fgets(buf, BUF_SIZE, fd))
	{
	  s7_int buf_len = safe_strlen(buf);
	  if (cur_len + buf_len >= full_len)
	    {
	      full_len += BUF_SIZE * 2;
	      str = (str) ? (char *)Realloc(str, full_len) : (char *)Malloc(full_len);
	    }
	  memcpy((void *)(str + cur_len), (void *)buf, buf_len);
	  cur_len += buf_len;
 	}
      pclose(fd);
      if (str)
	{
	  block_t *b = mallocate_block(sc);
	  block_data(b) = (void *)str;
	  block_set_index(b, TOP_BLOCK_LIST);
	  return(block_to_string(sc, b, cur_len));
	}
      return(nil_string);
    }
  return(make_integer(sc, system(string_value(name))));
#endif
}


/* #if (!MS_WINDOWS) */
#include <dirent.h>

/* -------------------------------- directory->list -------------------------------- */
static s7_pointer directory_to_list_1(s7_scheme *sc, const char *dir_name)
{
  s7_pointer result;
  DIR *dpos;
  sc->w = sc->nil;
  if ((dpos = opendir(dir_name)))
    {
      struct dirent *dirp;
      while ((dirp = readdir(dpos)))
	sc->w = cons_unchecked(sc, s7_make_string(sc, dirp->d_name), sc->w);
      closedir(dpos);
    }
  result = sc->w;
  sc->w = sc->unused;
  return(result);
}

static s7_pointer g_directory_to_list(s7_scheme *sc, s7_pointer args)
{
  #define H_directory_to_list "(directory->list directory) returns the contents of the directory as a list of strings (filenames)."
  #define Q_directory_to_list s7_make_signature(sc, 2, sc->is_list_symbol, sc->is_string_symbol)   /* can return nil */

  s7_pointer name = car(args);
  if (!is_string(name))
    return(method_or_bust_p(sc, name, sc->directory_to_list_symbol, sc->type_names[T_STRING]));
  if (string_length(name) >= 2)
    {
      block_t *b = expand_filename(sc, string_value(name));
      if (b)
	{
	  s7_pointer result = directory_to_list_1(sc, (char *)block_data(b));
	  liberate(sc, b);
	  return(result);
	}}
  return(directory_to_list_1(sc, string_value(name)));
}

/* -------------------------------- file-mtime -------------------------------- */
static s7_pointer g_file_mtime(s7_scheme *sc, s7_pointer args)
{
  #define H_file_mtime "(file-mtime file): return the write date of file"
  #define Q_file_mtime s7_make_signature(sc, 2, sc->is_integer_symbol, sc->is_string_symbol)

  struct stat statbuf;
  int32_t err;
  s7_pointer name = car(args);

  if (!is_string(name))
    return(sole_arg_method_or_bust(sc, name, sc->file_mtime_symbol, args, sc->type_names[T_STRING]));
  if (string_length(name) >= 2)
    {
      block_t *b = expand_filename(sc, string_value(name));
      if (b)
	{
	  err = stat((char *)block_data(b), &statbuf);
	  liberate(sc, b);
	  if (err < 0)
	    file_error_nr(sc, "file-mtime", strerror(errno), string_value(name));
	  return(make_integer(sc, (s7_int)(statbuf.st_mtime)));
	}}
  err = stat(string_value(name), &statbuf);
  if (err < 0)
    file_error_nr(sc, "file-mtime", strerror(errno), string_value(name));
  return(make_integer(sc, (s7_int)(statbuf.st_mtime)));
}

/* #endif */

void system_extras_init(s7_scheme *sc)
{
#define defun(Scheme_Name, C_Name, Req, Opt, Rst)                       \
    s7_define_typed_function(sc, Scheme_Name, g_ ## C_Name, Req, Opt, Rst, H_ ## C_Name, Q_ ## C_Name)

    defun("directory?",	        is_directory,		1, 0, false);
    defun("file-exists?",	file_exists,		1, 0, false);
    defun("delete-file",	delete_file,		1, 0, false);
    defun("getenv",		getenv,			1, 0, false);
    defun("system",		system,			1, 1, false);
#if (!MS_WINDOWS)
    defun("directory->list",    directory_to_list,	1, 0, false);
    defun("file-mtime",	        file_mtime,		1, 0, false);
#endif
}


#endif /* with_system_extras */

