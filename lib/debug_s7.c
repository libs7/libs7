#include <errno.h>
#include <execinfo.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "libs7.h"

#include "liblogc.h"
/* #if EXPORT */
#include "utarray.h"
/* #endif */

#include "debug_s7.h"

/* bool mibl_debug_s7_config = false; */

char *tostr1 = NULL;
char *tostr2 = NULL;

/* #if EXPORT_INTERFACE */
/* #define TO_STR(x) s7_object_to_c_string(s7, x) */
/* #define NM_TO_STR(x) s7_object_to_c_string(s7, s7_name_to_value(s7, x)) */
/* #define TO_BOOL(x) s7_boolean(s7, s7_name_to_value(s7, x)) */
/* #define LOG_S7_DEBUG(msg, obj) ((tostr1 = TO_STR(obj)), (fprintf(stderr, GRN " S7: " CRESET "%s:%d " #msg ": %s\n", __FILE__, __LINE__, tostr1)), (free(tostr1))) */
/* #endif */

/* extern bool mibl_debug; */
/* extern bool mibl_trace; */

/* #if defined(DEBUG) */
/* https://stackoverflow.com/questions/6934659/how-to-make-backtrace-backtrace-symbols-print-the-function-names */
static void full_write(int fd, const char *buf, size_t len)
{
        while (len > 0) {
                ssize_t ret = write(fd, buf, len);

                if ((ret == -1) && (errno != EINTR))
                        break;

                buf += (size_t) ret;
                len -= (size_t) ret;
        }
}

void s7_show_stack(s7_scheme *sc);

void print_backtrace(s7_scheme *s7)
{
    s7_show_stack(s7);
    /* s7_stacktrace(s7); */

    static const char start[] = "C BACKTRACE ------------\n";
    static const char end[] = "----------------------\n";

    void *bt[1024];
    int bt_size;
    char **bt_syms;
    int i;

    bt_size = backtrace(bt, 1024);
    bt_syms = backtrace_symbols(bt, bt_size);
    full_write(STDERR_FILENO, start, strlen(start));
    for (i = 1; i < bt_size; i++) {
        size_t len = strlen(bt_syms[i]);
        full_write(STDERR_FILENO, bt_syms[i], len);
        full_write(STDERR_FILENO, "\n", 1);
    }
    full_write(STDERR_FILENO, end, strlen(end));
    free(bt_syms);
}
/* #endif */
