//FIXME: add cp, mktemp, mkdtemp, etc.

#include <fnmatch.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

/* #include "fs_api.h" */

/* #if INTERFACE */
#include "s7.h"
/* #endif */

/*
  Remove leading ./ and any ../ for path that does not exist on fs
  Returns char* that must be freed by client.

  Web search finds manpage for Linux canonicalize_filename, but it's
  not on macos.
 */

//FIXME: switch to cwalk

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
            printf("ERR: too many dots?\n");
            /* exit(EXIT_FAILURE); */
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

/* **************************************************************** */
void fs_api_init(s7_scheme *sc)
{
    s7_pointer cur_env, pl_ss;
    s7_pointer s;

    s = s7_make_symbol(sc, "string?");
    pl_ss = s7_make_signature(sc, 2, s, s);
    cur_env = s7_curlet(sc);

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "->canonical-path"),
              s7_make_typed_function(sc, "canonical-path", g_canonical_path,
                                     1, 0, false,
                                     "char* canonical_path(char* path)",
                                     pl_ss));

}

