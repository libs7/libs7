#include <assert.h>
#include <ctype.h>
#include <fcntl.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <unistd.h>

#include "config.h"

#include "s7.h"
/* #include "log.h" */
#include "utarray.h"
#include "utstring.h"

/* #include "error_handler_dune.h" */

#if defined(DEVBUILD)
extern bool debug;
#endif
#if defined(TRACING)
extern bool trace;
#endif

/* s7_pointer old_err_port; */
/* const char *_errmsg = NULL; */
/* s7_int gc_loc = -1; */

/* s7_pointer s7_read_thunk_catcher(s7_scheme *s7, s7_pointer args); */
/* s7_pointer s7_read_thunk_catcher; */

#define ERRSEXP "(with-let (owlet) " \
    "(format #t \"file: ~A, line ~A\n\" error-file error-line))"

/* FIXME: rename, this routine fixes baddot errors
 */
const char *dunefile_to_string(const char *dunefile_name)
{
    TRACE_ENTRY(dunefile_to_string);
#if defined(TRACING)
    log_trace("dunefile: %s", dunefile_name);
                  //utstring_body(dunefile_name));
#endif
    /* core/dune file size: 45572 */
    // 2K

    //FIXME: malloc
/* #define DUNE_BUFSZ 131072 */
/*     /\* static char inbuf[DUNE_BUFSZ]; *\/ */
/*     /\* memset(inbuf, '\0', DUNE_BUFSZ); *\/ */
/*     static char outbuf[DUNE_BUFSZ + 20]; */
/*     memset(outbuf, '\0', DUNE_BUFSZ); */

    size_t file_size;
    char *inbuf;
    struct stat stbuf;
    int fd;
    FILE *instream = NULL;

    fd = open(dunefile_name, O_RDONLY);
    if (fd == -1) {
        /* Handle error */
        log_error("fd open error");
        goto cleanup;
    }

    if ((fstat(fd, &stbuf) != 0) || (!S_ISREG(stbuf.st_mode))) {
        /* Handle error */
        log_error("fstat error");
        goto cleanup;
    }

    file_size = stbuf.st_size;
#if defined(DEVBUILD)
    log_debug("filesize: %d", file_size);
#endif

    inbuf = (char*)calloc(file_size, sizeof(char));
    if (inbuf == NULL) {
        /* Handle error */
        log_error("malloc file_size fail");
        goto cleanup;
    }

    /* FIXME: what about e.g. unicode in string literals? */
    errno = 0;
    instream = fdopen(fd, "r");
    if (instream == NULL) {
        /* Handle error */
        log_error("fdopen failure: %s", dunefile_name);
        /* printf(RED "ERROR" CRESET "fdopen failure: %s\n", */
        /*        dunefile_name); */
               /* utstring_body(dunefile_name)); */
        perror(NULL);
        close(fd);
        goto cleanup;
    } else {
#if defined(DEVBUILD)
        log_debug("fdopened %s",
                  dunefile_name);
        /* utstring_body(dunefile_name)); */
#endif
    }

    // now read the entire file
    size_t read_ct = fread(inbuf, 1, file_size, instream);
#if defined(DEVBUILD)
    log_debug("read_ct: %d", read_ct);
#endif
    if (read_ct != file_size) {
        if (ferror(instream) != 0) {
            /* printf(RED "ERROR" CRESET "fread error 2 for %s\n", */
            /*        dunefile_name); */
            /* utstring_body(dunefile_name)); */
            log_error("fread error 2 for %s\n",
                      dunefile_name);
            /* utstring_body(dunefile_name)); */
            exit(EXIT_FAILURE); //FIXME: exit gracefully
        } else {
            if (feof(instream) == 0) {
                /* printf(RED "ERROR" CRESET "fread error 3 for %s\n", */
                /*        dunefile_name); */
                /* utstring_body(dunefile_name)); */
                log_error("fread error 3 for %s\n",
                          dunefile_name);
                /* utstring_body(dunefile_name)); */
                exit(EXIT_FAILURE); //FIXME: exit gracefully
            } else {
                //FIXME
                log_error("WTF????????????????");
                goto cleanup;
            }
        }
    } else {
        close(fd);
        fclose(instream);
    }
    inbuf[read_ct + 1] = '\0';
    uint64_t outFileSizeCounter = file_size * 2;
    char *outbuf = calloc(outFileSizeCounter, sizeof(char));
    /* memset(outbuf, '\0', fileSize); */

    // FIXME: loop over the entire inbuf char by char, testing for
    // . or "\|
    char *inptr = (char*)inbuf;
    /* log_debug("INPTR str: %s", inptr); */
    char *outptr = (char*)outbuf;
    /* char *cursor = inptr; */

    bool eol_string = false;
    while (*inptr) {
        if (*inptr == '.') {
            if (*(inptr+1) == ')'){
                /* log_debug("FOUND DOT: %s", inptr); */
                *outptr++ = '/';
                *outptr++ = *inptr++;
                continue;
            }
        }
        if (*inptr == '"') {
            if (*(inptr+1) == '\\') {
                if (*(inptr+2) == '|') {
                    /* log_debug("FOUND EOL Q"); */
                    *outptr++ = *inptr++; // copy '"'
                    inptr += 2; // point to char after '"\|'
                    eol_string = true;
                    while (eol_string) {
                        if (*inptr == '\0') {
                            *outptr = *inptr;
                            eol_string = false;
                        }
                        if (*inptr == '\n') {
                            /* log_debug("hit eolstring newline"); */
                            // check to see if next line starts with "\|
                            char *tmp = inptr + 1;
                            while (isspace(*tmp)) {tmp++;}
                            /* log_debug("skipped to: %s", tmp); */
                            if (*(tmp) == '"') {
                                if (*(tmp+1) == '\\') {
                                    if (*(tmp+2) == '|') {
                                        // preserve \n
                                        *outptr++ = *inptr;
                                        /* *outptr++ = '\\'; */
                                        /* *outptr++ = 'n'; */
                                        inptr = tmp + 3;
                                        /* log_debug("resuming at %s", inptr); */
                                        continue;
                                    }
                                }
                            }
                            *outptr++ = '"';
                            inptr++; // omit \n
                            eol_string = false;
                        } else {
                            *outptr++ = *inptr++;
                        }
                    }
                }
            }
        }
        *outptr++ = *inptr++;
    }

/*     inptr = (char*)inbuf; */
/*     while (true) { */
/*         cursor = strstr(inptr, ".)"); */

/* /\* https://stackoverflow.com/questions/54592366/replacing-one-character-in-a-string-with-multiple-characters-in-c *\/ */

/*         if (cursor == NULL) { */
/* /\* #if defined(DEVBUILD) *\/ */
/* /\*             if (mibl_debug) log_debug("remainder: '%s'", inptr); *\/ */
/* /\* #endif *\/ */
/*             size_t ct = strlcpy(outptr, (const char*)inptr, file_size); // strlen(outptr)); */
/*             (void)ct;           /\* prevent -Wunused-variable *\/ */
/* /\* #if defined(DEVBUILD) *\/ */
/* /\*             if (mibl_debug) log_debug("concatenated: '%s'", outptr); *\/ */
/* /\* #endif *\/ */
/*             break; */
/*         } else { */
/* #if defined(DEVBUILD) */
/*             log_error("FOUND and fixing \".)\" at pos: %d", cursor - inbuf); */
/* #endif */
/*             size_t ct = strlcpy(outptr, (const char*)inptr, cursor - inptr); */
/* #if defined(DEVBUILD) */
/*             log_debug("copied %d chars", ct); */
/*             /\* log_debug("to buf: '%s'", outptr); *\/ */
/* #endif */
/*             /\* if (ct >= DUNE_BUFSZ) { *\/ */
/*             if (ct >= outFileSizeCounter) { */
/*                 printf("output string has been truncated!\n"); */
/*             } */
/*             outptr = outptr + (cursor - inptr) - 1; */
/*             outptr[cursor - inptr] = '\0'; */
/*             //FIXME: use memcpy */
/*             ct = strlcat(outptr, " ./", outFileSizeCounter); // DUNE_BUFSZ); */
/*             outptr += 3; */

/*             inptr = inptr + (cursor - inptr) + 1; */
/*             /\* printf(GRN "inptr:\n" CRESET " %s\n", inptr); *\/ */

/*             if (ct >= outFileSizeCounter) { // DUNE_BUFSZ) { */
/*                 log_error("write count exceeded output bufsz\n"); */
/*                 /\* printf(RED "ERROR" CRESET "write count exceeded output bufsz\n"); *\/ */
/*                 free(inbuf); */
/*                 exit(EXIT_FAILURE); */
/*                 // output string has been truncated */
/*             } */
/*         } */
/*     } */
    free(inbuf);
    return outbuf;

cleanup:
    //FIXME
    if (instream != NULL)
    {
        fclose(instream);
        close(fd);
    }
    return NULL;
}

/* s7_pointer _s7_error_handler(s7_scheme *s7, s7_pointer args) */
/* { */
/*    /\* log_error("_s7_error_handler\n"); *\/ */
/*     /\* log_info("err: %s", TO_STR(args)); *\/ */

/*     if (strstr(s7_string(s7_car(args)), "unexpected close paren:") != NULL) { */
/* #if defined(TRACING) */
/*         if (mibl_debug) */
/*             printf(RED "Error: BAD DOT" CRESET "\n"); */
/* #endif */

/*         s7_write(s7, s7_make_string(s7, "BADDOT"), */
/*                  s7_current_error_port(s7)); */

/* #ifdef TRACING */
/*         fprintf(stdout, RED "[begin error context]\n"); */
/* #endif */
/*         s7_eval_c_string(s7, ERRSEXP); */
/*         char *sexp = "(do ((e (outlet (owlet)) (outlet e))) " */
/*             "((eq? e (rootlet))) " */
/*             "(format () \"~{~A ~}~%\" e)) "; */
/*         s7_eval_c_string(s7, sexp); */
/*         s7_write(s7, */
/*                  /\* s7_make_string(s7, *\/ */
/*                                 s7_car(args), */
/*                                 /\* ), *\/ */
/*                  // s7_string(s7_car(args)), */
/*                  s7_current_error_port(s7)); */
/* #ifdef TRACING */
/*         fprintf(stdout, "[end error context]" CRESET "\n"); */
/* #endif */

/*         s7_pointer st = s7_eval_c_string(s7, "(stacktrace)"); */
/*         (void)st; */
/*         fprintf(stdout, "STACKTRACEx:\n%s\n", "TO_STR(st)"); */
/*         fflush(NULL); */
/*         return s7_t(s7); */
/*     } else { */
/*         //TODO: write to error port */
/* #ifdef TRACING */
/*         fprintf(stdout, RED "Error:" CRESET " %s\n", */
/*                 s7_string(s7_car(args))); */
/* #endif */
/*         s7_pointer st = s7_eval_c_string(s7, "(debug-print-stacktrace)"); */
/*         (void)st; */
/*         /\* fprintf(stdout, "STACKTRACE:\n%s\n", "TO_STR(st)"); *\/ */

/* #ifdef TRACING */
/*         fprintf(stdout, RED "[begin error context]\n"); */
/* #endif */
/*         s7_eval_c_string(s7, ERRSEXP); */
/*         char *sexp = "(do ((e (outlet (owlet)) (outlet e))) " */
/*             "((eq? e (rootlet))) " */
/*             "(format () \"~{~A ~}~%\" e)) "; */
/*         s7_eval_c_string(s7, sexp); */
/*         s7_write(s7, */
/*                  /\* s7_make_string(s7, s7_car(args)), *\/ */
/*                  // s7_string(s7_car(args)), */
/*                  /\* TO_STR(s7_car(args)), *\/ */
/*                  s7_car(args), */
/*                  s7_current_error_port(s7)); */
/* #ifdef TRACING */
/*         fprintf(stdout, "[end error context]" CRESET "\n"); */
/* #endif */

/*         /\* s7_pointer st = s7_eval_c_string(s7, "(stacktrace)"); *\/ */
/*         /\* fprintf(stdout, "STACKTRACE:\n%s\n", TO_STR(st)); *\/ */
/*         /\* fflush(NULL); *\/ */

/*         /\* printf("EXIT ON ERROR? %s\n" TO_STR(s7_name_to_value(s7, *exit-on-error*))); *\/ */

/*         if (s7_name_to_value(s7, "*exit-on-error*") == s7_t(s7)) { */
/* #ifdef TRACING */
/*             fprintf(stdout, RED "exiting..." CRESET "\n"); */
/* #endif */
/*             exit(EXIT_FAILURE); */
/*         } */

/*         s7_flush_output_port(s7, s7_current_output_port(s7)); */
/*         s7_flush_output_port(s7, s7_current_error_port(s7)); */
/*         fflush(NULL); */

/*         /\* s7_pointer eline = s7_eval_c_string(s7, "(with-let (owlet) error-line"); *\/ */
/*         /\* fprintf(stderr, "file: %s, line: %s\n", TO_STR(efile), TO_STR(eline)); *\/ */

/*         /\* fprintf(stderr, "%s\n", TO_STR(owlet)); *\/ */
/*         /\* fprintf(stderr, "\n"); *\/ */
/*         /\* fprintf(stderr, "%s\n", TO_STR(owlet)); *\/ */
/*         return(s7_f(s7)); */
/*     } */
/* } */

/* s7_pointer _s7_read_error_handler(s7_scheme *s7, s7_pointer args) */
/* { */
/* #ifdef TRACING */
/*     fprintf(stderr, RED "READ ERROR:" CRESET " %s, %s\n", */
/*             s7_string(s7_car(args)), */
/*             s7_string(s7_cdr(args))); */
/* #endif */
/*     (void)args; */
/*     s7_eval_c_string(s7, ERRSEXP); */
/*         s7_flush_output_port(s7, s7_current_output_port(s7)); */
/*         s7_flush_output_port(s7, s7_current_error_port(s7)); */
/*         fflush(NULL); */

/*         return(s7_make_character(s7, 'Q')); */
/* } */

/* void error_config(void) */
/* { */
/*     /\* if (mibl_trace) log_trace(BLU "error_config" CRESET); *\/ */

/*     old_err_port = s7_set_current_error_port(s7, s7_open_output_string(s7)); */
/*     if (old_err_port != s7_nil(s7)) { */
/*         /\* if (s7_is_output_port(s7, old_err_port)) { *\/ */
/*         /\*     s7_flush_output_port(s7, old_err_port); *\/ */
/*         /\* } *\/ */
/*         gc_loc = s7_gc_protect(s7, old_err_port); */
/*     } */
/*     s7_flush_output_port(s7, s7_current_error_port(s7)); */
/* } */

/* void close_error_config(void) // s7_pointer err_port) */
/* { */
/*     s7_close_output_port(s7, s7_current_error_port(s7)); */

/*     s7_set_current_error_port(s7, old_err_port); */
/*     if (gc_loc != -1) */
/*         s7_gc_unprotect_at(s7, gc_loc); */
/* } */

/* char *dunefile_to_string(UT_string *dunefile_name) */
/* { */
/*     /\* FIXME: use malloc, this will rarely be called *\/ */
/*     /\* 16K should be enough for any dunefile? *\/ */
/* #define DUNE_BUFSZ 16384 */
/*     static char buffer[DUNE_BUFSZ]; */
/*     memset(buffer, '\0', DUNE_BUFSZ); */
/*     /\* FIXME: what about e.g. unicode in string literals? *\/ */

/*     FILE *inFp = fopen(utstring_body(dunefile_name), "r"); */
/*     fseek(inFp, 0, SEEK_END); */
/*     uint64_t fileSize = ftell(inFp); */
/*     /\* log_debug("filesize: %d", fileSize); *\/ */
/*     if (fileSize > DUNE_BUFSZ) { */
/*         log_error("dune file size (%d) > DUNE_BUFSZ (%d)\n", fileSize, DUNE_BUFSZ); */
/*         exit(EXIT_FAILURE);     /\* FIXME: exit gracefully *\/ */
/*     } */
/*     rewind(inFp); */

/*     char *fixbuf = malloc(fileSize + 1); */
/*     memset(fixbuf, '\0', fileSize); */

/*     uint64_t outFileSizeCounter = fileSize; */

/*     /\* we fread() bytes from inFp in COPY_BUFFER_MAXSIZE increments, */
/*        until there is nothing left to fread() *\/ */
/*     int read_ct = 0; */
/*     do { */
/*         if (outFileSizeCounter > DUNE_BUFSZ) { */
/*             /\* probably won't see a 16K dune file *\/ */
/*             read_ct = fread(buffer, 1, (size_t) DUNE_BUFSZ, inFp); */
/*             if (read_ct != DUNE_BUFSZ) { */
/*                 if (ferror(inFp) != 0) { */
/*                     log_error("fread error 1 for %s\n", */
/*                               utstring_body(dunefile_name)); */
/*                     exit(EXIT_FAILURE); //FIXME: exit gracefully */
/*                 } */
/*             } */
/*             /\* log_debug("writing"); *\/ */
/*             outFileSizeCounter -= DUNE_BUFSZ; */
/*         } */
/*         else { */
/*             read_ct = fread(buffer, 1, (size_t) outFileSizeCounter, inFp); */
/*             if (read_ct != outFileSizeCounter) { */
/*                 if (ferror(inFp) != 0) { */
/*                     log_error("fread error 2 for %s\n", */
/*                               utstring_body(dunefile_name)); */
/*                     exit(EXIT_FAILURE); //FIXME: exit gracefully */
/*                 } else { */
/*                     if (feof(inFp) == 0) { */
/*                         log_error("fread error 3 for %s\n", */
/*                                   utstring_body(dunefile_name)); */
/*                         exit(EXIT_FAILURE); //FIXME: exit gracefully */
/*                     } */
/*                 } */
/*             } */
/*             outFileSizeCounter = 0ULL; */
/*         } */
/*     } while (outFileSizeCounter > 0); */
/*     /\* log_debug("readed %d bytes", read_ct); *\/ */
/*     fclose(inFp); */

/*     /\* printf(RED "READED:\n" CRESET " %s\n", buffer); *\/ */

/*     // FIXME: loop over the entire buffer */
/*     char *bptr = (char*)buffer; */
/*     char *fptr = (char*)fixbuf; */

/*     regex_t re; */
/*     int rc = regcomp(&re, "\\. *)", REG_EXTENDED); */
/*     assert(rc == 0); */

/*     /\* regmatch_t matches[1]; *\/ */

/*     while (true) { */
/*         /\* printf(RED "bptr:\n" CRESET " %s\n", bptr); *\/ */
/*         /\* printf(RED "fixbuf:\n" CRESET " %s\n", fixbuf); *\/ */

/*         //FIXME: use regex.  When the need arises. */
/*         /\* rc = regexec(&re, bptr, *\/ */
/*         /\*              sizeof(matches)/sizeof(matches[0]), *\/ */
/*         /\*              (regmatch_t*)&matches,0); *\/ */
/*         /\* if (rc == 0) { *\/ */
/*         /\*     printf(MAG "regex match:" CRESET " %s\n", *\/ */
/*         /\*            bptr + matches[0].rm_so); *\/ */
/*         /\*     /\\* char *val = strndup(data+matches[1].rm_so, *\\/ *\/ */
/*         /\*     /\\*                     matches[1].rm_eo - matches[1].rm_so); *\\/ *\/ */
/*         /\* } else { *\/ */
/*         /\*     printf("regex NO match\n"); *\/ */
/*         /\* } *\/ */

/*         char *cursor = strstr((const char*) bptr, ".)"); */


/*         if (cursor == NULL) { */
/*             size_t ct = strlcpy(fptr, (const char*)bptr, strlen(fptr)); */
/*             (void)ct; */
/*             break; */
/*         } else { */
/*             /\* log_debug("FOUND \".)\" at pos: %d", cursor - buffer); *\/ */
/*             size_t ct = strlcpy(fptr, (const char*)bptr, cursor - bptr); */
/*             (void)ct; */
/*             if (ct >= DUNE_BUFSZ) { */
/*                 // output string has been truncated */
/*             } */
/*             fptr = fptr + (cursor - bptr) - 1; */
/*             fptr[cursor - bptr] = '\0'; */
/*             ct = strlcat(fptr, " ./", DUNE_BUFSZ); */
/*             fptr += 3; */

/*             bptr = bptr + (cursor - bptr) + 1; */

/*             /\* printf(GRN "bptr:\n" CRESET " %s\n", bptr); *\/ */

/*             if (ct >= DUNE_BUFSZ) { */
/*                 // output string has been truncated */
/*             } */
/*             /\* log_debug("first seg: %s", fixbuf); *\/ */
/*             /\* log_debug("first seg len: %d", strlen((char*)fixbuf)); *\/ */
/*             /\* log_debug("cursor - buffer = %d", cursor - buffer); *\/ */
/*             /\* log_debug("second seg %s", buffer + 225); *\/ */
/*             /\* ct = strlcat((char*)fixbuf, buffer + (cursor - buffer) + 1, DUNE_BUFSZ); *\/ */
/*             /\* if (ct >= DUNE_BUFSZb) { *\/ */
/*             /\*     // output string has been truncated *\/ */
/*             /\* } *\/ */
/*             /\* log_debug("fixed: %s", (char*)fixbuf); *\/ */
/*         } */

/*     } */
/*     /\* log_debug("final:\n %s", (char*)fixbuf); *\/ */
/*     return fixbuf; */
/* } */

