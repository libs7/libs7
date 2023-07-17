#include <libgen.h>
#include <unistd.h>

#include "config.h"
#include "gopt.h"
#include "cwalk.h"

#include "libs7.h"

s7_scheme *s7;
s7_pointer reader;
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
    OPT_TEMPLATE,
    OPT_DATA,
    FLAG_HELP,

    LAST
};

struct option options[] = {
    /* 0 */
    [OPT_SCRIPT] = {.long_name="script", .short_name='s',
                    .flags=GOPT_ARGUMENT_REQUIRED},

    [OPT_OUTFILE] = {.long_name="out",.short_name='o',
                     .flags=GOPT_ARGUMENT_REQUIRED},

    [OPT_TEMPLATE] = {.long_name="template",.short_name='t',
                     .flags=GOPT_ARGUMENT_REQUIRED},

    [OPT_DATA] = {.long_name="data",.short_name='d',
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
    if (options[OPT_DATA].count != 1) {
        log_error("Data arg required");
        exit(EXIT_FAILURE);
    }
    if (options[OPT_TEMPLATE].count != 1) {
        log_error("Template arg required");
        exit(EXIT_FAILURE);
    }

    s7_scheme *s7 = libs7_init();

    libs7_load_clib(s7, "mustachios");
    libs7_load_clib(s7, "json");
    libs7_load_clib(s7, "toml");
    libs7_load_clib(s7, "sexp");

    /* json_read = s7_name_to_value(s7, "json:read"); */
    /* toml_read = s7_name_to_value(s7, "toml:read"); */
    mustache_render = s7_name_to_value(s7, "mustache:render");

    const char *ext;
    size_t extlen;
    cwk_path_get_extension(options[OPT_DATA].argument, &ext, &extlen);
    log_debug("arg: %s", options[OPT_DATA].argument);

    log_debug("CWD: %s", getcwd(NULL, 0));

    char *tmp = strdup(options[OPT_DATA].argument);
    char *bname = basename(tmp);
    free(tmp);

    if (extlen == 5 && (strncmp(ext, ".json", 5) ==  0)) {
        reader = s7_name_to_value(s7, "json:read");
    }
    else if (extlen == 5 && (strncmp(ext, ".toml", 5) == 0)) {
        reader = s7_name_to_value(s7, "toml:read");
    }
    else if (extlen == 4 && (strncmp(ext, ".scm", 4) == 0)) {
        reader = s7_name_to_value(s7, "read");
    }
    else if (extlen == 4 && (strncmp(ext, ".sexp", 5) == 0)) {
        reader = s7_name_to_value(s7, "sexp:read");
    }
    else if (extlen == 4 && (strncmp(ext, ".dune", 5) == 0)) {
        reader = s7_name_to_value(s7, "sexp:read");
    }
    else if (strncmp(bname, "dune", 4) == 0) {
        reader = s7_name_to_value(s7, "sexp:read");
    } else {
        log_error("Data file extension must be .json, .toml, .dune, .sexp, or .scm");
    }

    s7_define_variable(s7, "reader", reader);

    s7_define_variable(s7, "datafile", s7_make_string(s7, options[OPT_DATA].argument));

    s7_define_variable(s7, "template", s7_make_string(s7, options[OPT_TEMPLATE].argument));

    s7_define_variable(s7, "outfile",
                       s7_make_string(s7, options[OPT_OUTFILE].argument));


    s7_pointer res = s7_load(s7, options[OPT_SCRIPT].argument);
    (void)res;
    /* s7_pointer op = s7_open_output_file(s7, options[OPT_OUTFILE].argument, "w"); */
    /* s7_display(s7, res, op); */
    /* s7_close_output_port(s7, op); */
}
