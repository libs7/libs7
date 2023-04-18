#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "utstring.h"
#include "log.h"
#include "s7.h"

bool verbose;
int main(int argc, char **argv)
{
    /* log_debug("CWD: %s", getcwd(NULL,0)); */
    /* log_debug("BUILD WS root: %s", getenv("BUILD_WORKSPACE_DIRECTORY")); */
    s7_scheme *s7 = s7_init();
    if (verbose)
        log_info("s7: %s", S7_DATE);

    if (argc == 2) {
        /* log_debug("argv[0]: %s", argv[0]); */
        /* log_debug("argv[1]: %s", argv[1]); */
        size_t len = strlen(argv[1]);
        char *script = strndup(argv[1], len);
        s7_add_to_load_path(s7, dirname(argv[1]));
        /* log_debug("script: %s", script); */

        /* s7_pointer lp = s7_load_path(s7); */
        /* char *s = s7_object_to_c_string(s7, lp); */
        /* log_debug("load-path: %s", s); */
        /* free(s); */

        if (verbose)
            log_info("loading %s", script);
        if (!s7_load(s7, script)) {
            fprintf(stderr, "can't load %s\n", argv[1]);
            return(2);
        } else {
            free(script);
        }
    } else {
        log_error("arg required");
    }
    return(0);
}

