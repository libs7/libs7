#include <unistd.h>

#include "config.h"
#include "gopt.h"
/* #include "common.h" */

#include "libs7.h"

s7_scheme *s7;
s7_pointer json_read;
s7_pointer toml_read;
s7_pointer mustache_render;

void cleanup(void)
{
    /* https://wiki.sei.cmu.edu/confluence/display/c/FIO23-C.+Do+not+exit+with+unflushed+data+in+stdout+or+stderr */
    /* Do cleanup */
    /* printf("All cleaned up!\n"); */
    if (fflush(stdout) == EOF) {
        /* Handle error */
    }
}

enum OPTS {
    OPT_SCRIPT,
    OPT_OUTFILE,
    FLAG_HELP,

    LAST
};

struct option options[] = {
    /* 0 */
    [OPT_SCRIPT] = {.long_name="script", .short_name='s',
                    .flags=GOPT_ARGUMENT_REQUIRED},

    [OPT_OUTFILE] = {.long_name="out",.short_name='o',
                     .flags=GOPT_ARGUMENT_REQUIRED},

    [FLAG_HELP] = {.long_name="help",.short_name='h',
                   .flags=GOPT_ARGUMENT_FORBIDDEN},

    [LAST] = {.flags = GOPT_LAST}
};

int main(int argc, char **argv)
{
    atexit(cleanup);

    argc = gopt (argv, options);
    (void)argc;
    gopt_errors (argv[0], options);

    if (options[OPT_SCRIPT].count != 1) {
        log_error("Script arg required");
        exit(EXIT_FAILURE);
    }
    if (options[OPT_OUTFILE].count != 1) {
        log_error("Out arg required");
        exit(EXIT_FAILURE);
    }

    s7_scheme *s7 = libs7_init();

    libs7_load_clib(s7, "mustachios");
    libs7_load_clib(s7, "toml");
    libs7_load_clib(s7, "json");

    json_read = s7_name_to_value(s7, "json:read");
    toml_read = s7_name_to_value(s7, "toml:read");
    mustache_render = s7_name_to_value(s7, "mustache:render");

    s7_pointer res = s7_load(s7, options[OPT_SCRIPT].argument);
    s7_pointer op = s7_open_output_file(s7, options[OPT_OUTFILE].argument, "w");
    s7_display(s7, res, op);
    s7_close_output_port(s7, op);
}
