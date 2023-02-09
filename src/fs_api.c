//FIXME: add cp, mktemp, mkdtemp, etc.

#include <fnmatch.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "s7.h"

static s7_pointer FILE__symbol;

/*
  Remove leading ./ and any ../ for path that does not exist on fs
  Returns char* that must be freed by client.

  Web search finds manpage for Linux canonicalize_filename, but it's
  not on macos.
 */

/* FIXME: remove leading './' */
/* FIXME: optimize. this does waaay too much copying. */
/* FIXME: throw an error if too many '..' */
/* e.g. a/b/../../../../../c */
char *canonical_path(char *path)
{
    /* printf("canonical_path: %s\n", path); */

    if (path[0] == '.' && path[1] == '/') {
        path+=2;
    }
    char *buf = calloc(strlen(path), sizeof(char*));
    memset(buf, '\0', strlen(path));

    char *src_cursor = path;
    char *dst_cursor = buf;
    char *rewind     = buf;

    /* char *src_anchor = path; */
    /* char *dst_anchor = path; */
    /* char *start  = path; */

    /* char *prev_slash = NULL; */
    /* char *prevprev_slash = NULL; */
    /* bool backup = false; */

    /* char *start = src_anchor; */

    /* int len  = strlen(src_cursor); */
    /* int mvlen  = 0; */
    /* int i; */
    /* int buflen; */

    /* bool editing = false; */
    int dotcount   = 0;    /* consecutive '.' */
    /* int slashcount = 0;    /\* consecutive '/' or "/./" *\/ */

    /* printf("prev_slash: %s (%x)\n", prev_slash, prev_slash); */
    /* printf("slash ct: %d\n", slashcount); */

    // dst_cursor lags by 1
    *dst_cursor = *src_cursor++; /* prime */
    while (*src_cursor != '\0') {
        /* printf("\nbuf: %s\n", buf); */
        /* printf("dst_cursor: %s\n", dst_cursor); */
        /* printf("src_cursor: %s\n", src_cursor); */
        /* printf("rewind: %s\n", rewind); */

        if ( *(src_cursor-1) == '/' ) {
            while (*src_cursor == '.') {
                dotcount++;
                src_cursor++;
            }
        }
        if (dotcount > 2) {
            printf("ERR: too many dots\n");
            exit(EXIT_FAILURE);
        }
        /* printf("src_cursor (advanced): %s\n", src_cursor); */
        /* printf("dot ct: %d\n", dotcount); */

        if (*src_cursor == '/') {
            /* printf("*src_cursor == '/'\n"); */
            // src: aaa/bbb</>../ccc, aaa/bbb</>./ccc, aaa/bbb</>/ccc, aaa/bbb</>ccc
            if ( *dst_cursor == '/') {
                /* printf("*dst_cursor == '/'\n"); */
                if (dotcount == 0) {
                    // src: aaa/</>//bbb/..., dst: (a)aa</>
                    while (*src_cursor == '/')
                        src_cursor++; /* advance past consecutive '/' */
                    // src: aaa////<b>bb/..., dst: (a)aa</>
                } else {
                    if (dotcount == 1) {
                        // src: aaa/bbb/.</>ccc, dst: aaa/(b)bb</>
                        // src: aaa/bbb/.</>././ccc, dst: aaa/(b)bb</>
                        /* advance past consecutive /./ */
                        while(*src_cursor == '/') {
                            // printf("while %s\n", src_cursor);
                            src_cursor++;
                            if (*src_cursor != '.') break;
                        }
                        /* src_cursor++; */
                    } else {
                        if (dotcount == 2) {
                            // printf("Rewinding dst\n");
                            // src: aaa/bbb/..</>ccc
                            // dst: aaa/(b)bb</>

                            src_cursor++;
                            // reset dst_cursor
                            dst_cursor = rewind;
                            if (dst_cursor == buf) {
                                // re-prime
                                *dst_cursor = *src_cursor;
                                src_cursor++;
                            } else
                                dst_cursor--;


                            // printf("resetting rewind ptr: %s\n", rewind);
                            // reset rewind ptr
                            if ( rewind != buf) {
                                // dst rewind ptr:  aaa/bbb/<c>
                                rewind-=2; /* skip over preceding '/' */
                                while ( *rewind != '/' && rewind != buf) rewind--;
                            }
                            // printf("reset rewind ptr: %s\n", rewind);
                            // printf("reset dst_cursor: %s\n", dst_cursor);
                            // printf("reset src_cursor: %s\n", src_cursor);
                            /* *dst_cursor = *src_cursor; */
                            /* src_cursor++; */
                            dotcount = 0;
                            /* exit(EXIT_FAILURE); */
                        }
                    }
                }
            } else {
                // printf("*dst_cursor != '/'\n");
                // src_cursor == '/', dst_cursor != '/'
                // src: aaa</>bbb/...., dst: aa<a>
                dst_cursor++;
                *dst_cursor = *src_cursor++;
                // src: aaa/<b>bb/...., dst: aaa</>
            }
        } else {
            dotcount=0;
            // printf("both cursors non-'/'\n");
            // src_cursor != '/'
            if (*dst_cursor == '/') {
                // printf("resetting rewind ptr\n");
                dst_cursor++;
                rewind = dst_cursor;
            } else {
                dst_cursor++;
            }
            // src: a<a>a/bbb/..., dst: <a>  (initial state, primed)
            // src: aaa/b<b>b/..., dst: aaa/<b>
            *dst_cursor = *src_cursor++;
            // src: aa<a>/bbb/..., dst: a<a>
            // src: aaa/bb<b>/..., dst: aaa/b<b>
        }
    } /* end while (*src_cursor != '\0') { */
    dst_cursor++;
    // printf("DONE dst_cursor: %s\n", dst_cursor);
    *dst_cursor = '\0';
    // printf("DONE buf: %s\n", buf);
    return buf;
}

/*                 buflen = strlen(buf) - 2; */
/*                 bool hit = false; */
/*                 if (*(buf + buflen) == '/') { */
/*                     printf("searching buf backwards starting at: %s\n", */
/*                            buf + buflen-1); */
/*                     for (i=buflen-1; i>0; i--) { */
/*                         printf("check: %s\n", buf + i); */
/*                         if (*(buf + i) == '/') { */
/*                             printf("bingo: %s\n", (char*)buf + i); */
/*                             hit = true; */
/*                             prev_slash = buf + i; */
/*                             break; */
/*                         } */
/*                     } */
/*                     if ( !hit) prev_slash = buf - 1; */
/*                     printf("new prev_slash: %s (%x)\n", prev_slash, prev_slash); */
/*                     printf("new buf: %s\n", buf); */
/*                     if (prev_slash == NULL) */
/*                         prev_slash = buf; */
/*                     /\* prev_slash = strrchr(buf, '/'); *\/ */
/*                     /\* prev_slash--; *\/ */
/*                     /\* prev_slash = strrchr(prev_slash, '/'); *\/ */
/*                 } else { */
/*                     prev_slash = strrchr(buf, '/'); */
/*                 } */
/*             } else { */
/*                 if (dotcount > 0) { */
/*                     /\* we're in ./ prefix  *\/ */
/*                     /\* slashcount++; *\/ */
/*                     src_cursor++; */
/*                     continue; */
/*                 } else { */
/*                     /\* we're in a filename somewhere *\/ */
/*                     /\* src_anchor = src_cursor; *\/ */
/*                     /\* dst_anchor = dst_cursor; *\/ */
/*                     *dst_cursor++ = *src_cursor++; */
/*                 } */
/*             } */
/*             /\* } *\/ */
/*             if (slashcount == 1) { */
/*                 src_cursor++; // continue */
/*             } */
/*             else if (dotcount == 2) { */
/*                 printf("slashct: %d, dotcount 2\n", slashcount); */
/*                 if (slashcount == 2) { */
/*                     /\* at end of "/../" *\/ */
/*                     // back up one level */
/*                     /\* printf("BACKING UP\n"); *\/ */
/*                     /\* printf("buf: %s\n", buf); *\/ */
/*                     /\* printf("prev_slash: %s\n", prev_slash); *\/ */
/*                     dst_cursor = prev_slash; // + 1; */
/*                     dst_cursor++; */
/*                     *dst_cursor = '\0'; */
/*                     /\* dst_cursor--; *\/ */
/*                     src_cursor++; */
/*                     slashcount = 1; */
/*                     dotcount = 0; */

/*                     /\* printf("Searching buf backwards starting at: %s\n", *\/ */
/*                     /\*        buf + buflen-1); *\/ */
/*                     /\* for (i=buflen-1; i>0; i--) { *\/ */
/*                     /\*     /\\* printf("check: %s\n", buf + i); *\\/ *\/ */
/*                     /\*     if (*(buf + i) == '/') { *\/ */
/*                     /\*         /\\* printf("bingo: %s\n", (char*)buf + i); *\\/ *\/ */
/*                     /\*         prev_slash = buf + i; *\/ */
/*                     /\*         break; *\/ */
/*                     /\*     } *\/ */
/*                     /\* } *\/ */
/*                     /\* if (prev_slash == NULL) *\/ */
/*                     /\*     prev_slash = buf; *\/ */

/*                 } else { */
/*                     /\* end of "/././" *\/ */
/*                     src_cursor++; */
/*                 } */
/*             } */
/*             else {              /\* consecutive '/' and '.' *\/ */
/*                 /\* printf("at %s, dotcount: %d, slashcount: %d\n", *\/ */
/*                 /\*        src_cursor, dotcount, slashcount); *\/ */
/*                 src_cursor++; */
/*             } */
/*         } */
/*         else if (*src_cursor == '.') { */
/*             printf("DOT at %s, slashcount: %d\n", */
/*                    src_cursor, slashcount); */
/*             if (slashcount == 0) { /\* "foo.ml" *\/ */
/*                 /\* careful: "./a/b", "../a/b" *\/ */
/*                 if (src_cursor == start) { */
/*                     src_cursor++; */
/*                     if (*src_cursor == '.') */
/*                         src_cursor++; */
/*                     slashcount++; /\* fake *\/ */
/*                     dotcount++; */
/*                 } else { */
/*                     dotcount=0;       /\* e.g. a/.hidden *\/ */
/*                     *dst_cursor++ = *src_cursor++; */
/*                     *dst_cursor = '\0'; */
/*                 } */
/*             } */
/*             else if (slashcount > 0) { /\* we've seen at least one '/' *\/ */
/*                 dotcount++;       /\* e.g. a/.hidden *\/ */
/*                 src_cursor++; */
/*             } else {            /\* e.g. "a.c" *\/ */
/*                 /\* careful:  "../a/b" *\/ */
/*                 src_cursor++; */
/*             } */
/*         } */
/*         else {                  /\* not '.' nor '/' *\/ */
/*             if (slashcount > 1) { /\* we've seen consecutive '/', '.' *\/ */
/*                 if (dotcount == 1) { */
/*                     /\* printf("Char at %s, sc: %d, dc: 1\n", *\/ */
/*                     /\*        src_cursor, slashcount); *\/ */
/*                     if (*(src_cursor - 1) == '/') { */
/*                         /\* printf("the b in a/./b, a/././b, etc.\n"); *\/ */
/*                         *dst_cursor++ = *src_cursor++; */
/*                         *dst_cursor = '\0'; */
/*                         slashcount = dotcount = 0; */
/*                     } else { */
/*                         /\* printf("the h in  a/././.hidden\n"); *\/ */
/*                         --src_cursor; */
/*                         *dst_cursor++ = *src_cursor++; */
/*                         src_cursor++; */
/*                         *dst_cursor = '\0'; */
/*                         slashcount = dotcount = 0; */
/*                     } */
/*                 } else { /\* slashct > 1 *\/ */
/*                     /\* dotcount may be any *\/ */
/*                     /\* e.g. the c in "a/./c", "a///c", etc. *\/ */
/*                     /\* cp from src_cursor-1 to src_anchor, reset counters *\/ */
/*                     src_anchor = src_cursor; */
/*                     dst_anchor = dst_cursor; */
/*                     /\* printf("shifting at src_cursor %s, src_anchor %s, buf %s\n", *\/ */
/*                     /\*        src_cursor, src_anchor, buf); *\/ */
/*                     /\* printf("dotcount: %d, slashcount: %d\n", *\/ */
/*                     /\*        dotcount, slashcount); *\/ */
/*                     /\* mvlen = strlen(src_cursor); *\/ */
/*                     /\* printf("mvlen: %d\n", mvlen); *\/ */
/*                     /\* printf("src_anchor: %s\n", src_anchor); *\/ */
/*                     /\* printf("dst_anchor: %s\n", dst_anchor); *\/ */
/*                     while ( *src_cursor != '\0') { */
/*                         /\* printf("buf: %s, src_cursor: %s\n", *\/ */
/*                         /\*        buf, src_cursor); *\/ */
/*                         *dst_cursor++ = *src_cursor++; */

/*                         /\* printf("src_anchor: %s\n", src_anchor); *\/ */
/*                     } */
/*                     *dst_cursor = '\0'; */
/*                     /\* printf("buf: %s\n", buf); *\/ */
/*                     src_cursor = src_anchor; */
/*                     dst_cursor = dst_anchor; */
/*                     slashcount = dotcount = 0; */
/*                 } */
/*             } */
/*             else if (slashcount == 1) { */
/*                 /\* printf("CHAR at %s, sc: %d, dc: %d\n", *\/ */
/*                 /\*        src_cursor, slashcount, dotcount); *\/ */
/*                 if (dotcount > 0) { */
/*                     /\* b of a/.b *\/ */
/*                     --src_cursor; */
/*                     *dst_cursor++ = *src_cursor++; */
/*                     *dst_cursor++ = *src_cursor++; */
/*                     *dst_cursor = '\0'; */
/*                      slashcount = dotcount = 0; */
/*                 } else { */
/*                     /\* b of a/b *\/ */
/*                     *dst_cursor++ = *src_cursor++; */
/*                     *dst_cursor = '\0'; */
/*                     slashcount = dotcount = 0; */
/*                 } */
/*             } else { */
/*                 *dst_cursor++ = *src_cursor++; */
/*             } */
/*         } */
/*     } */
/*     return buf; */
/* } */

/* one arg: a path string */
static s7_pointer g_canonical_path(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* _path;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    _path = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_string(sc, (char*)canonical_path(_path)));
}

/* -------- chdir -------- */
static s7_pointer s7__chdir(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__chdir_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__chdir_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)chdir(s7__chdir_0)));
}

/* -------- mkdir -------- */
static s7_pointer s7__mkdir(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__mkdir_0;
  int s7__mkdir_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__mkdir_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__mkdir_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)mkdir(s7__mkdir_0, s7__mkdir_1)));
}

/* -------- getcwd -------- */
static s7_pointer s7__getcwd(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__getcwd_0;
  size_t s7__getcwd_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__getcwd_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__getcwd_1 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_string(sc, (char*)getcwd(s7__getcwd_0, s7__getcwd_1)));
}

/* -------- fnmatch -------- */
static s7_pointer s7__fnmatch(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__fnmatch_0;
  char* s7__fnmatch_1;
  int s7__fnmatch_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__fnmatch_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__fnmatch_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fnmatch_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)fnmatch(s7__fnmatch_0, s7__fnmatch_1, s7__fnmatch_2)));
}

/* -------- link -------- */
static s7_pointer s7__link(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__link_0;
  char* s7__link_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__link_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__link_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_integer(sc, (s7_int)link(s7__link_0, s7__link_1)));
}


/* -------- unlink -------- */
static s7_pointer s7__unlink(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__unlink_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__unlink_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)unlink(s7__unlink_0)));
}


/* -------- rmdir -------- */
static s7_pointer s7__rmdir(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__rmdir_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__rmdir_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)rmdir(s7__rmdir_0)));
}


/* -------- rename -------- */
static s7_pointer s7__rename(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__rename_0;
  char* s7__rename_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__rename_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__rename_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_integer(sc, (s7_int)rename(s7__rename_0, s7__rename_1)));
}

/* -------- tmpfile -------- */
static s7_pointer s7__tmpfile(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void*)tmpfile(), FILE__symbol, s7_f(sc)));
}

static s7_pointer g_realpath(s7_scheme *sc, s7_pointer args)
{
    char *s7_dl_realpath_0, *res;
    if (s7_is_string(s7_car(args)))
        s7_dl_realpath_0 = (char*)s7_string(s7_car(args));
    else return(s7_wrong_type_arg_error(sc, "realpath", 1, s7_car(args), "string"));
    res = realpath(s7_dl_realpath_0, NULL);
    if (res) {s7_pointer str; str = s7_make_string(sc, res); free(res); return(str);}
    return(s7_f(sc));
}

/* **************************************************************** */
/* getcwd, chdir, fnmatch */
void init_fs_api(s7_scheme *sc)
{
    s7_pointer cur_env, pl_issi, pl_ss, pl_isi, pl_iss, pl_ssi, pl_sss, pl_is, pl_xt; //  pl_ssix,
    /* s7_int gc_loc; */

    s7_pointer s,i, t, x;
    t = s7_t(sc);
    s = s7_make_symbol(sc, "string?");
    i = s7_make_symbol(sc, "integer?");
    x = s7_make_symbol(sc, "c-pointer?");

    pl_is = s7_make_signature(sc, 2, i, s);
    pl_ss = s7_make_signature(sc, 2, s, s);
    pl_iss = s7_make_signature(sc, 3, i, s, s);
    pl_isi = s7_make_signature(sc, 3, i, s, i);
    pl_issi = s7_make_signature(sc, 4, i, s, s, i);
    pl_ssi = s7_make_signature(sc, 3, s, s, i);
    pl_sss = s7_make_signature(sc, 3, s, s, s);
    pl_xt = s7_make_signature(sc, 2, x, t);

    /* pl_ssix = s7_make_signature(sc, 4, s, s, i, x); */

    cur_env = s7_curlet(sc);

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "->canonical-path"),
              s7_make_typed_function(sc, "canonical-path", g_canonical_path,
                                     1, 0, false,
                                     "char* canonical_path(char* path)",
                                     pl_ss));

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "getcwd"),
              s7_make_typed_function(sc, "getcwd", s7__getcwd, 2, 0, false, "char* getcwd(char* size_t)", pl_ssi));

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "chdir"),
              s7_make_typed_function(sc, "chdir", s7__chdir,
                                     1, 0, false,
                                     "int chdir(char*)",
                                     pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "mkdir"),
            s7_make_typed_function(sc, "mkdir", s7__mkdir, 2, 0, false, "int mkdir(char* int)", pl_isi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "realpath"),
            s7_make_typed_function(sc, "realpath", g_realpath,
                                   2, 0, false,
                                   "(realpath file-name resolved-name) - second arg is ignored, returns resolved name.",
                                   pl_sss));

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "fnmatch"),
              s7_make_typed_function(sc, "fnmatch", s7__fnmatch,
                                     3, 0, false,
                                     "(fnmatch pattern file flags); returns zero on success. flag as in fnmatch(3)",
                                     pl_issi));

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "link"),
              s7_make_typed_function(sc, "link", s7__link,
                                     2, 0, false,
                                     "int link(const char*, const char*)",
                                     pl_iss));

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "unlink"),
              s7_make_typed_function(sc, "unlink", s7__unlink,
                                     1, 0, false,
                                     "int unlink(const char*)",
                                     pl_is));

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "rename"),
              s7_make_typed_function(sc, "rename", s7__rename,
                                     2, 0, false,
                                     "int rename(const char*, const char*)",
                                     pl_iss));

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "rmdir"),
              s7_make_typed_function(sc, "rmdir", s7__rmdir,
                                     1, 0, false,
                                     "int rmdir(const char*)",
                                     pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "tmpfile"),
            s7_make_typed_function(sc, "tmpfile", s7__tmpfile,
                                   0, 0, false,
                                   "FILE* tmpfile(void)",
                                   pl_xt));


#ifdef FNM_NOMATCH
    s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_NOMATCH"), s7_make_integer(sc, (s7_int)FNM_NOMATCH));
#endif
#ifdef FNM_EXTMATCH
    s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_EXTMATCH"), s7_make_integer(sc, (s7_int)FNM_EXTMATCH));
#endif
#ifdef FNM_CASEFOLD
    s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_CASEFOLD"), s7_make_integer(sc, (s7_int)FNM_CASEFOLD));
#endif
#ifdef FNM_LEADING_DIR
    s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_LEADING_DIR"), s7_make_integer(sc, (s7_int)FNM_LEADING_DIR));
#endif
#ifdef FNM_FILE_NAME
    s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_FILE_NAME"), s7_make_integer(sc, (s7_int)FNM_FILE_NAME));
#endif
#ifdef FNM_PERIOD
    s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_PERIOD"), s7_make_integer(sc, (s7_int)FNM_PERIOD));
#endif
#ifdef FNM_NOESCAPE
    s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_NOESCAPE"), s7_make_integer(sc, (s7_int)FNM_NOESCAPE));
#endif
#ifdef FNM_PATHNAME
    s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_PATHNAME"), s7_make_integer(sc, (s7_int)FNM_PATHNAME));
#endif

#ifdef S_IRWXO
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IRWXO"), s7_make_integer(sc, (s7_int)S_IRWXO));
#endif
#ifdef S_IXOTH
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IXOTH"), s7_make_integer(sc, (s7_int)S_IXOTH));
#endif
#ifdef S_IWOTH
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IWOTH"), s7_make_integer(sc, (s7_int)S_IWOTH));
#endif
#ifdef S_IROTH
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IROTH"), s7_make_integer(sc, (s7_int)S_IROTH));
#endif
#ifdef S_IRWXG
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IRWXG"), s7_make_integer(sc, (s7_int)S_IRWXG));
#endif
#ifdef S_IXGRP
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IXGRP"), s7_make_integer(sc, (s7_int)S_IXGRP));
#endif
#ifdef S_IWGRP
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IWGRP"), s7_make_integer(sc, (s7_int)S_IWGRP));
#endif
#ifdef S_IRGRP
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IRGRP"), s7_make_integer(sc, (s7_int)S_IRGRP));
#endif
#ifdef S_IRWXU
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IRWXU"), s7_make_integer(sc, (s7_int)S_IRWXU));
#endif
#ifdef S_IXUSR
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IXUSR"), s7_make_integer(sc, (s7_int)S_IXUSR));
#endif
#ifdef S_IWUSR
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IWUSR"), s7_make_integer(sc, (s7_int)S_IWUSR));
#endif
#ifdef S_IRUSR
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IRUSR"), s7_make_integer(sc, (s7_int)S_IRUSR));
#endif
}


