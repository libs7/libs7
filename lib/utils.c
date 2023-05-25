#include <stdlib.h>

#include "utils.h"
#include "log.h"
#include "trace.h"

/* WARNING: result must be freed */
char *libs7_input_port_to_c_string(s7_scheme *s7, s7_pointer port)
{
    char *buf;
    //FIXME: size?
    const size_t chunk_sz = 4096;
    buf = calloc(chunk_sz, sizeof(char));
    size_t buf_sz = chunk_sz;

    void *rptr;

    size_t i = 0;
    /* read from current-input-port, one char at a time */
    // s7_pointer cip = s7_current_input_port(s7);
    s7_pointer c;
    while (true) {
        c = s7_read_char(s7, port);
        if (c == s7_eof_object(s7)) {
            buf[i] = '\0';
            break;
        }
        buf[i++] = s7_character(c);
        if (i >= buf_sz) {
            log_info("Reallocating read buf to %d", buf_sz + chunk_sz);
            rptr = realloc(buf, chunk_sz);
            if (rptr == NULL) {
                //FIXME: handle error
                log_error("realloc failure");
            }
        }
    }
    TRACE_LOG_DEBUG("readed string: %s", buf);
    s7_close_input_port(s7, port);
    return buf;
}
