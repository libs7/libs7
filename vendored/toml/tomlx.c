#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>

#include "config.h"
#include "toml.h"
#include "tomlx.h"

toml_datum_t tomlx_array_datum_for_idx(toml_array_t *ta, int idx, int *typ)
{
    TOMLX_ENTRY(tomlx_array_datum_for_idx);
    toml_datum_t datum;

    datum = toml_string_at(ta, idx);
    if (datum.ok) {
        TOMLX_LOG_DEBUG("datum: s", "");
        *typ = TOML_STRING;
        return datum;
    }

    datum = toml_bool_at(ta, idx);
    if (datum.ok) {
        TOMLX_LOG_DEBUG("datum: b", "");
        *typ = TOML_BOOL;
        return datum;
    }

    datum = toml_int_at(ta, idx);
    if (datum.ok) {
        TOMLX_LOG_DEBUG("datum: i", "");
        *typ = TOML_INT;
        return datum;
    }

    datum = toml_double_at(ta, idx);
    if (datum.ok) {
        TOMLX_LOG_DEBUG("datum: d", "");
        *typ = TOML_DOUBLE;
        return datum;
    }

    datum = toml_timestamp_at(ta, idx);
    if (datum.ok) {
        TOMLX_LOG_DEBUG("datum: ts", "");
        *typ = TOML_TIMESTAMP;
        /* not yet supported */
        return datum;
    }
    TOMLX_LOG_DEBUG("datum: NULL", "");
    *typ = TOML_NONDATUM;
    return datum;
}

void *tomlx_array_seq_for_idx(toml_array_t *ta, int idx, int *typ)
{
    TOMLX_ENTRY(tomlx_array_seq_for_idx);

    toml_array_t *a = toml_array_at(ta, idx);
    if (a) {
        TOMLX_LOG_DEBUG("array", "");
        *typ = TOML_ARRAY;
        return a;
    } else {
        TOMLX_LOG_DEBUG("not array", "");
    }

    toml_table_t *subt = toml_table_at(ta, idx);
    if (subt) {
        TOMLX_LOG_DEBUG("table: %p", subt);
        *typ = TOML_TABLE;
        return subt;
    } else {
        TOMLX_LOG_DEBUG("not table", "");
    }
    return NULL;
}

/*
  prints content, not metasyntax (delims and separator)
  * [1, 2, 3] => 123
 */
char *tomlx_array_to_string(toml_array_t *ta, int print_syntax)
{
    TOMLX_ENTRY(tomlx_array_to_string);
    toml_datum_t datum;
    int typ;

    const int BUFSZ = 4096;
    char *buf;          /* WARNING: malloc */
    buf = calloc(BUFSZ, sizeof(char));
    if (!buf) {
        log_error("OOM");
        return NULL;
    } else {
        TOMLX_LOG_DEBUG("callocated %d chars for buffer", BUFSZ);
    }
    size_t bufsz = BUFSZ;
    size_t char_ct = 0;
    int ct;
    (void)ct;

    // is array empty?
    int idx_ct = toml_array_nelem(ta);
    if (idx_ct == 0) {
        buf[0] = '\0';
        return buf;
    } else {
        // are all elements empties?
    }

    // print header
    TOMLX_LOG_DEBUG("snprintfing header", "");
    if (print_syntax == PRINT_SYNTAX_TOML_RAW) {
        // no header metasyntax
    }
    else if (print_syntax == PRINT_SYNTAX_TOML) {
        errno = 0;
        ct = snprintf(buf, 2, "%s", "[");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        }
        char_ct += 1; // do not include terminating '\0'
    } else { // SCM print syntax
        errno = 0;
        ct = snprintf(buf, 14, "%s", "#<toml-array ");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        }
        char_ct += 13; // do not include terminating '\0'
    }
    // print elements
    /* char *k, *v; */
    int len;
    bool empty = false;
    for (int i = 0; i < idx_ct; i++) {
        // print comma
        if (print_syntax == PRINT_SYNTAX_TOML_RAW) {
            ; // do not print separator
        }
        else if (i > 0) {
            if ((char_ct + 3) > bufsz) {
                log_error("realloc for comma");
            }
            else if (!empty) {
                errno = 0;
                TOMLX_LOG_DEBUG("snprintfing comma", "");
                ct = snprintf(buf+char_ct, 3, "%s", ", ");
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TOMLX_LOG_DEBUG("snprintf comma ct: %d", ct);
                }
                char_ct += 2; // do not include terminating '\0'
                TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                TOMLX_LOG_DEBUG("buf: %s", buf);
            } else {
                empty = false; // reset
            }
        }

        // print value
        datum = tomlx_array_datum_for_idx(ta, i, &typ);
        char *seq_str;
        TOMLX_LOG_DEBUG("datum typ: %d", typ);
        if (typ == TOML_NONDATUM) {
            void *seq = tomlx_array_seq_for_idx(ta, i, &typ);
            switch(typ) {
            case TOML_ARRAY:
                TOMLX_LOG_DEBUG("array seq: %p", seq);
                seq_str = tomlx_array_to_string((toml_array_t*)seq, print_syntax);
                TOMLX_LOG_DEBUG("ARRAY str: %s", seq_str);
                len = strlen(seq_str);
                if (len == 0) {
                    empty = true; // print nothing
                } else {
                    len++;  // + 1 for '\0'
                    if ((char_ct + len) > bufsz) {
                        log_error("exceeded bufsz: %d", char_ct + len);
                        // expand buf
                    }
                    errno = 0;
                    TOMLX_LOG_DEBUG("snprintfing array len %d", len);
                    ct = snprintf(buf+char_ct, len, "%s = ", seq_str);
                    if (errno) {
                        log_error("snprintf: %s", strerror(errno));
                        break;
                    } else {
                        TOMLX_LOG_DEBUG("snprintf ct: %d", ct);
                    }
                    char_ct += len - 1; // do not include terminating '\0'
                    TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                    TOMLX_LOG_DEBUG("buf: %s", buf);
                }
                break;
            case TOML_TABLE:
                TOMLX_LOG_DEBUG("table seq: %p", seq);
                seq_str = tomlx_table_to_string((toml_table_t*)seq, print_syntax);
                TOMLX_LOG_DEBUG("TABLE: %s", seq_str);
                len = strlen(seq_str);
                if (len == 0) {
                    empty = true;
                } else {
                    len++;  // + 1 for '\0'
                    if ((char_ct + len) > bufsz) {
                        log_error("exceeded bufsz: %d", char_ct + len);
                        // expand buf
                    }
                    errno = 0;
                    TOMLX_LOG_DEBUG("snprintfing table len %d", len);
                    ct = snprintf(buf+char_ct, len, "%s = ", seq_str);
                    if (errno) {
                        log_error("snprintf: %s", strerror(errno));
                        break;
                    } else {
                        TOMLX_LOG_DEBUG("snprintf ct: %d", ct);
                    }
                    char_ct += len - 1; // do not include terminating '\0'
                    TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                    TOMLX_LOG_DEBUG("buf: %s", buf);
                }
                break;
            default:
                log_error("Bad toml seq type: %d", typ);
            }
        } else {
            switch(typ) {
            case TOML_INT:
                TOMLX_LOG_DEBUG("toml datum val: %d", datum.u.i);
                len = snprintf(NULL, 0, "%lld", datum.u.i);
                len++; // for terminating '\0';
                TOMLX_LOG_DEBUG("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TOMLX_LOG_DEBUG("snprintfing len %d", len);
                ct = snprintf(buf+char_ct, len, "%lld", datum.u.i);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TOMLX_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                TOMLX_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_STRING:
                TOMLX_LOG_DEBUG("toml datum val: %s", datum.u.s);
                // add 2 for quotes
                len = snprintf(NULL, 0, "%s", datum.u.s);
                len++; // for terminating '\0';
                TOMLX_LOG_DEBUG("int str sz: %d", len);
                errno = 0;
                TOMLX_LOG_DEBUG("snprintfing string, len %d", len);
                if (print_syntax) { // with quotes
                    len += 2;
                    if ((char_ct + len) > bufsz) { // + 1 for '\0'
                        log_error("exceeded bufsz: %d", char_ct + len);
                        // expand buf
                    }
                    ct = snprintf(buf+char_ct, len, "\"%s\"", datum.u.s);
                } else {        // without quotes
                    if ((char_ct + len) > bufsz) { // + 1 for '\0'
                        log_error("exceeded bufsz: %d", char_ct + len);
                        // expand buf
                    }
                    ct = snprintf(buf+char_ct, len, "%s", datum.u.s);
                }
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TOMLX_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                TOMLX_LOG_DEBUG("buf: %s", buf);
                free(datum.u.s);
                break;
            case TOML_BOOL:
                // tomlc99 bool val is int
                TOMLX_LOG_DEBUG("toml datum val: %d", datum.u.b);
                if (datum.u.b) {
                    len = 5; // "true" + \0
                } else {
                    len = 6; // "false" + \0
                }
                TOMLX_LOG_DEBUG("bool str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TOMLX_LOG_DEBUG("snprintfing len %d", len);
                if (datum.u.b) {
                    ct = snprintf(buf+char_ct, len, "%s", "true");
                } else {
                    ct = snprintf(buf+char_ct, len, "%s", "false");
                }
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TOMLX_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                TOMLX_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_DOUBLE:
                TOMLX_LOG_DEBUG("toml datum val: %g", datum.u.d);
                len = snprintf(NULL, 0, "%g", datum.u.d);
                len++; // for terminating '\0';
                TOMLX_LOG_DEBUG("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TOMLX_LOG_DEBUG("snprintfing len %d", len);
                ct = snprintf(buf+char_ct, len, "%g", datum.u.d);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TOMLX_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                TOMLX_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_TIMESTAMP:
                log_error("toml timestamp (not yet)");
                if ((char_ct + 18) > bufsz) {
                    // realloc
                } else {
                    errno = 0;
                    snprintf(buf, 18, "%s", "#<toml-timestamp>");
                    if (errno) {
                        log_error("snprintf: %s", strerror(errno));
                        break;
                    }
                    char_ct += 18;
                }
                break;
            case TOML_NONDATUM:
                // should not happen
                log_error("Unexpected TOML_NON_DATUM");
                //FIXME: throw error
                return NULL;
                break;
            default:
                log_error("Bad toml_datum constant: %d", typ);
                //FIXME: throw error
                return NULL;
            }
        }
    }

    // print footer
    TOMLX_LOG_DEBUG("snprintfing footer", "");
    errno = 0;
    ct = snprintf(buf+char_ct,
                  (print_syntax == PRINT_SYNTAX_TOML_RAW)? 1 : 2,
                  "%s",
                  (print_syntax == PRINT_SYNTAX_TOML_RAW)
                  ? ""
                  : (print_syntax == PRINT_SYNTAX_TOML)
                  ? "]"
                  : ">");
    if (errno) {
        log_error("snprintf: %s", strerror(errno));
        return NULL;
    }
    char_ct += 1; // do not include terminating '\0'
    if (strncmp(buf, "[]", 2) == 0)
        buf[0] = '\0';
    TOMLX_LOG_DEBUG("tomlx_array_to_string returning: %s", buf);
    return buf;
}

/* **************************************************************** */
size_t tomlx_table_length(toml_table_t *tt)
{
    int ntab = toml_table_ntab(tt);
    int narr = toml_table_narr(tt);
    int nkv = toml_table_nkval(tt);
    return (size_t) ntab + narr + nkv;
}

toml_datum_t tomlx_table_datum_for_key(toml_table_t *tt, char *key, int *typ)
{
    TOMLX_ENTRY_STR(tomlx_table_datum_for_key, key);
    toml_datum_t datum;

    datum = toml_string_in(tt, key);
    if (datum.ok) {
        TOMLX_LOG_DEBUG("datum: string", "");
        *typ = TOML_STRING;
        return datum;
    }

    datum = toml_bool_in(tt, key);
    if (datum.ok) {
        TOMLX_LOG_DEBUG("datum: bool", "");
        *typ = TOML_BOOL;
        return datum;
    }

    datum = toml_int_in(tt, key);
    if (datum.ok) {
        TOMLX_LOG_DEBUG("datum: int", "");
        *typ = TOML_INT;
        return datum;
    }

    datum = toml_double_in(tt, key);
    if (datum.ok) {
        TOMLX_LOG_DEBUG("datum: double", "");
        *typ = TOML_DOUBLE;
        return datum;
    }

    datum = toml_timestamp_in(tt, key);
    if (datum.ok) {
        TOMLX_LOG_DEBUG("datum: ts", "");
        *typ = TOML_TIMESTAMP;
        return datum;
    }
    TOMLX_LOG_DEBUG("datum: NULL", "");
    *typ = TOML_NONDATUM;
    return datum;
}

void *tomlx_table_seq_for_key(toml_table_t *tt, char *key, int *typ)
{
    TOMLX_ENTRY(tomlx_table_seq_for_key);

    toml_array_t *a = toml_array_in(tt, key);
    if (a) {
        TOMLX_LOG_DEBUG("array", "");
        *typ = TOML_ARRAY;
        return a;
    /* } else { */
    /*     TOMLX_LOG_DEBUG("not array", ""); */
    }

    toml_table_t *subt = toml_table_in(tt, key);
    if (subt) {
        TOMLX_LOG_DEBUG("table: %p", subt);
        *typ = TOML_TABLE;
        return subt;
    /* } else { */
    /*     TOMLX_LOG_DEBUG("not table", ""); */
    }
    return NULL;
}

char *tomlx_table_to_string(toml_table_t *tt, int print_syntax)
{
    TOMLX_ENTRY(tomlx_table_to_string);
    toml_datum_t datum;
    int typ;

    //FIXME: use input string length to estimate bufsize, then malloc
    const int BUFSZ = 4096;
    char *buf;          /* WARNING: malloc */
    buf = calloc(BUFSZ, sizeof(char));
    if (!buf) {
        log_error("OOM");
        return NULL;
    } else {
        TOMLX_LOG_DEBUG("callocated %d chars for buffer", BUFSZ);
    }
    size_t bufsz = BUFSZ;
    size_t char_ct = 0;
    int ct;
    (void)ct;                   /* elim unused-but-set-variable warning */

    // First check for empty

    // print fields
    int ntab = toml_table_ntab(tt);
    int narr = toml_table_narr(tt);
    int nkv = toml_table_nkval(tt);
    int key_ct = ntab + narr + nkv;

    TOMLX_LOG_DEBUG("key ct: %d", key_ct);

    if (key_ct == 0) {
        //FIXME: printing '{}' good for debugging templates
        // empty table
        /* buf[0] = '{'; */
        /* buf[1] = '}'; */
        /* buf[2] = '\0'; */
        buf[0] = '\0';
        return buf;
    }

    // print header
    TOMLX_LOG_DEBUG("snprintfing header", "");
    if (print_syntax == PRINT_SYNTAX_TOML_RAW) {
        ; // print nothing
    }
    else if (print_syntax == PRINT_SYNTAX_TOML) {
        errno = 0;
        ct = snprintf(buf, 2, "%s", "{");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        }
        char_ct += 1; // do not include terminating '\0'
    } else {
        errno = 0;
        ct = snprintf(buf, 14, "%s", "#<toml-table ");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        }
        char_ct += 13; // do not include terminating '\0'
    }

    const char *k; //, *v;
    int len;
    bool empty = false;
    for (int i = 0; i < key_ct; i++) {
        k = toml_key_in(tt, i);
        if (!k) {
            log_error("toml_key_in failure for idx: %d", i);
            return NULL;
        }
        TOMLX_LOG_DEBUG("table key: %s", k);

        // maybe print comma
        if (print_syntax == PRINT_SYNTAX_TOML_RAW) {
            ; // print nothing
        }
        else if (i > 0) {
            if ((char_ct + 3) > bufsz) {
                log_error("realloc for comma");
            }
            else if (!empty) {
                errno = 0;
                TOMLX_LOG_DEBUG("snprintfing comma", "");
                ct = snprintf(buf+char_ct, 3, "%s", ", ");
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TOMLX_LOG_DEBUG("snprintf comma ct: %d", ct);
                }
                char_ct += 2; // do not include terminating '\0'
                TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                TOMLX_LOG_DEBUG("buf: %s", buf);
            } else {
                empty = false; // reset
            }
        }

        char *relop = "";
        int relopsz = 0;
        // print key to buf
        if (print_syntax != PRINT_SYNTAX_TOML_RAW) {
            // print only key, not '='
            relop = " = ";
            relopsz = 3;
        }

        {
            len = strlen(k) + relopsz; // for " = "
            len++; // terminating '\0'
            if ((char_ct + len) > bufsz) { // + 1 for '\0'
                log_error("exceeded bufsz: %d", char_ct + len);
                // expand buf
            }
            errno = 0;
            TOMLX_LOG_DEBUG("snprintfing key len %d", len);
            ct = snprintf(buf+char_ct, len, "%s%s", k, relop);
            if (errno) {
                log_error("snprintf: %s", strerror(errno));
                break;
            } else {
                TOMLX_LOG_DEBUG("snprintf ct: %d", ct);
            }
            char_ct += len - 1; // do not include terminating '\0'
            TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
            TOMLX_LOG_DEBUG("buf: %s", buf);
        }

        // print value
        datum = tomlx_table_datum_for_key(tt, (char*)k, &typ);
        char *seq_str;
        /* TOMLX_LOG_DEBUG("datum typ: %d", typ); */
        if (typ == TOML_NONDATUM) {
            /* log_debug("datum typ: NONDATUM"); */
            void *seq = tomlx_table_seq_for_key(tt, (char*)k, &typ);
            switch(typ) {
            case TOML_ARRAY:
                TOMLX_LOG_DEBUG("array seq: %p", seq);
                seq_str = tomlx_array_to_string((toml_array_t*)seq, print_syntax);
                TOMLX_LOG_DEBUG("ARRAY: %s", seq_str);
                len = strlen(seq_str);
                if (len == 0) {
                    empty =true;
                } else {
                    len++;  // + 1 for '\0'
                    if ((char_ct + len) > bufsz) {
                        log_error("exceeded bufsz: %d", char_ct + len);
                        // expand buf
                    }
                    errno = 0;
                    TOMLX_LOG_DEBUG("snprintfing array len %d", len);
                    ct = snprintf(buf+char_ct, len, "%s = ", seq_str);
                    if (errno) {
                        log_error("snprintf: %s", strerror(errno));
                        break;
                    } else {
                        TOMLX_LOG_DEBUG("snprintf ct: %d", ct);
                    }
                    char_ct += len - 1; // do not include terminating '\0'
                    TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                    TOMLX_LOG_DEBUG("buf: %s", buf);
                }
                break;
            case TOML_TABLE:
                TOMLX_LOG_DEBUG("table seq: %p", seq);
                seq_str = tomlx_table_to_string((toml_table_t*)seq, print_syntax);
                TOMLX_LOG_DEBUG("TABLE: %s", seq_str);
                len = strlen(seq_str);
                if (len == 0) {
                    empty = true; // print nothing
                } else {
                    len++;  // + 1 for '\0'
                    if ((char_ct + len) > bufsz) {
                        log_error("exceeded bufsz: %d", char_ct + len);
                        // expand buf
                    }
                    errno = 0;
                    TOMLX_LOG_DEBUG("snprintfing table len %d", len);
                    ct = snprintf(buf+char_ct, len, "%s = ", seq_str);
                    if (errno) {
                        log_error("snprintf: %s", strerror(errno));
                        break;
                    } else {
                        TOMLX_LOG_DEBUG("snprintf ct: %d", ct);
                    }
                    char_ct += len - 1; // do not include terminating '\0'
                    TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                    TOMLX_LOG_DEBUG("buf: %s", buf);
                }
                break;
            default:
                log_error("Bad toml seq type: %d", typ);
            }
        } else {
            /* log_debug("datum typ: DATUM"); */
            switch(typ) {
            case TOML_INT:
                TOMLX_LOG_DEBUG("toml datum val: %d", datum.u.i);
                len = snprintf(NULL, 0, "%lld", datum.u.i);
                len++; // for terminating '\0';
                TOMLX_LOG_DEBUG("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TOMLX_LOG_DEBUG("snprintfing len %d", len);
                ct = snprintf(buf+char_ct, len, "%lld", datum.u.i);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TOMLX_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                TOMLX_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_STRING:
                /* log_debug("toml datum string: %s", datum.u.s); */
                TOMLX_LOG_DEBUG("toml datum string: %s", datum.u.s);
                // add 2 for quotes
                len = snprintf(NULL, 0, "%s", datum.u.s) + 2;
                len++; // for terminating '\0';
                TOMLX_LOG_DEBUG("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TOMLX_LOG_DEBUG("snprintfing string, len %d", len);
                ct = snprintf(buf+char_ct, len, "'%s'", datum.u.s);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TOMLX_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                TOMLX_LOG_DEBUG("buf: %s", buf);
                free(datum.u.s);
                break;
            case TOML_BOOL:
                // tomlc99 bool val is int
                TOMLX_LOG_DEBUG("toml datum val: %d", datum.u.b);
                if (datum.u.b) {
                    len = 5; // "true" + \0
                } else {
                    len = 6; // "false" + \0
                }
                TOMLX_LOG_DEBUG("bool str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TOMLX_LOG_DEBUG("snprintfing len %d", len);
                if (datum.u.b) {
                    ct = snprintf(buf+char_ct, len, "%s", "true");
                } else {
                    ct = snprintf(buf+char_ct, len, "%s", "false");
                }
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TOMLX_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                TOMLX_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_DOUBLE:
                TOMLX_LOG_DEBUG("toml datum val: %g", datum.u.d);
                len = snprintf(NULL, 0, "%g", datum.u.d);
                len++; // for terminating '\0';
                TOMLX_LOG_DEBUG("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TOMLX_LOG_DEBUG("snprintfing len %d", len);
                ct = snprintf(buf+char_ct, len, "%g", datum.u.d);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TOMLX_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                TOMLX_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_TIMESTAMP:
                seq_str = tomlx_datetime_to_string((toml_timestamp_t*)datum.u.ts, print_syntax);
                TOMLX_LOG_DEBUG("TS: %s", seq_str);
                len = strlen(seq_str) + 1;  // + 1 for '\0'
                if ((char_ct + len) > bufsz) {
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TOMLX_LOG_DEBUG("snprintfing timestamp len %d", len);
                ct = snprintf(buf+char_ct, len, "%s = ", seq_str);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TOMLX_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1; // do not include terminating '\0'
                TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
                TOMLX_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_NONDATUM:
                // should not happen
                log_error("Unexpected TOML_NON_DATUM");
                //FIXME: throw error
                return NULL;
                break;
            default:
                log_error("Bad toml_datum constant: %d", typ);
                //FIXME: throw error
                return NULL;
            }
        }
    }

    // print footer
    errno = 0;
    TOMLX_LOG_DEBUG("snprintfing footer", "");
    ct = snprintf(buf+char_ct,
                  (print_syntax == PRINT_SYNTAX_TOML_RAW) ? 1 : 2,
                  "%s",
                  (print_syntax == PRINT_SYNTAX_TOML_RAW)
                  ? ""
                  : (print_syntax == PRINT_SYNTAX_TOML)
                  ? "}"
                  : ">");
    if (errno) {
        log_error("snprintf: %s", strerror(errno));
        return NULL;
    }
    char_ct += 1; // do not include terminating '\0'

    TOMLX_LOG_DEBUG("tomlx_table_to_string returning: %s", buf);
    return buf;
}

/*
 * format codes: %y, %m, %d, $h, $m, $s, #s
 * leading zeroes always printed.
 * e.g. %m-%y for 1979-05-27T00:32:00-07:00 prints 05-1979
 */
char *tomlx_format_datetime(toml_timestamp_t* ts, const char *fmt)
{
    log_debug("tomlx_format_datetime: %s", fmt);
    /* if (fmt[0] != '$') { */
    /*     log_error("Datetime format string missing initial '$': %s", fmt); */
    /* } */
    static char buf[512];
    char *bufptr = buf;

    int len;

    const char *p =  fmt;
    (void)p;
    while (*p) {
        if (*p == '$') {
            p++;
            // date
            switch(*p) {
            case 'y':
                log_debug("Formatting YEAR");
                len = snprintf(bufptr, 5, "%04d", *ts->year);
                bufptr += len;
                break;
            case 'm':
                log_debug("Formatting MONTH");
                len = snprintf(bufptr, 3, "%02d", *ts->month);
                log_debug("len: %d", len);
                bufptr += len;
                break;
            case 'd':
                log_debug("Formatting DAY");
                break;
            default:
                log_error("Bad date formatting code: %s", p);
                break;
            }
            p++;
            continue;
        }
        *bufptr++ = *p++;
    }
    *bufptr = '\0';
    /* int len = snprintf(buf, 8, "%02d-%4d", *ts->month, *ts->year); */
    log_debug("ts, formatted: %s", buf);
    return strndup(buf, strlen(buf));
}

char *tomlx_datetime_to_string(toml_timestamp_t *ts, int print_syntax)
{
    TOMLX_ENTRY(tomlx_datetime_to_string);
    TOMLX_LOG_DEBUG("print_syntax: %d", print_syntax);
    const int BUFSZ = 4096;
    char *buf;          /* WARNING: malloc */
    buf = calloc(BUFSZ, sizeof(char));
    if (!buf) {
        log_error("OOM");
        return NULL;
    } else {
        TOMLX_LOG_DEBUG("callocated %d chars for buffer", BUFSZ);
    }
    /* size_t bufsz = BUFSZ; */
    size_t char_ct = 0;
    int ct;
    (void)ct;

    if (print_syntax == PRINT_SYNTAX_SCM_WRITE) {
        // print quotation marks
        errno = 0;
        TOMLX_LOG_DEBUG("snprintfing header", "");
        // FIXME: check buf sz
        ct = snprintf(buf, 2, "%s", "\"");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        } else {
            TOMLX_LOG_DEBUG("snprintf hdr ct: %d", ct);
        }
        char_ct += 1; // do not include terminating '\0'
    }

    int zlen;
    if (ts->z == NULL) {
        zlen = 0;
    } else {
        zlen = strlen(ts->z);
        /* log_debug("z: %s", ts->z); */
    }
    /* log_debug("zlen %d", zlen); */

    if (ts->year == NULL) {
        // local time
        if (ts->millisec == NULL) {
            /* log_debug("NO MILLIS"); */
            // e.g. 00:32:00
            snprintf(buf + char_ct, 20 + zlen, "%0.2d:%02.d:%0.2d",
                     *ts->hour, *ts->minute, *ts->second);
            char_ct += 8 + 1;
        }
        else {
            // tomlc99: only 3 decimal places for millis
            // e.g. 00:32:00.999999
            snprintf(buf + char_ct, 24 + zlen, "%0.2d:%02.d:%0.2d.%d",
                     *ts->hour, *ts->minute, *ts->second,
                     *ts->millisec);
            char_ct += 8 + 3;
        }
    }
    else if (ts->hour == NULL) {
        // local date
        snprintf(buf + char_ct, 20 + zlen, "%.4d-%0.2d-%0.2d",
                 *ts->year, *ts->month, *ts->day);
        char_ct += 13;
    }
    else if (ts->millisec == NULL) {
        /* log_debug("NO MILLIS"); */
        // e.g. 1979-05-27T07:32:00
        snprintf(buf + char_ct, 20 + zlen, "%.4d-%0.2d-%0.2dT%0.2d:%02.d:%0.2d%s",
                 *ts->year, *ts->month, *ts->day,
                 *ts->hour, *ts->minute, *ts->second,
                 (zlen>0)? ts->z : "X");
        char_ct += 20 +  zlen - 1;
    }
    else {
        /* log_debug("MILLIS"); */
        // tomlc99: only 3 decimal places for millis
        // e.g. 1979-05-27T00:32:00.999999
        snprintf(buf + char_ct, 24 + zlen, "%.4d-%0.2d-%0.2dT%0.2d:%02.d:%0.2d.%d%s",
                 *ts->year, *ts->month, *ts->day,
                 *ts->hour, *ts->minute, *ts->second,
                 *ts->millisec,
                 (zlen>0)? ts->z : "");
        char_ct += 24 + zlen - 1;
    }
    /* log_debug("buf: %s", buf); */

    // print footer
    if (print_syntax == PRINT_SYNTAX_SCM_WRITE) {
        errno = 0;
        TOMLX_LOG_DEBUG("snprintfing datetime footer", "");
        ct = snprintf(buf+char_ct, 2, "%s", "\"");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        } else {
            TOMLX_LOG_DEBUG("snprintf footer ct: %d", ct);
        }
        char_ct += 1; // do not include terminating '\0'
        TOMLX_LOG_DEBUG("buf len: %d", strlen(buf));
        TOMLX_LOG_DEBUG("buf: %s", buf);
    }
    TOMLX_LOG_DEBUG("tomlx_datetime_to_string returning: %s", buf);
    return buf;
}

/* **************************************************************** */
struct tomlx_item_s *tomlx_make_item(void* val, int type)
{
    TOMLX_ENTRY(tomlx_make_item);
    TOMLX_LOG_DEBUG("type: %d", type);

    struct tomlx_item_s *tmp = malloc(sizeof(struct tomlx_item_s));
    if (!tmp) {
        log_error("OOM");
        //FIXME: handle OOM
    } else {
        /* log_debug("malloc ok"); */
    }
    tmp->type = type;
    switch(type) {
    case TOML_TABLE:
        /* log_debug("Making table item"); */
        tmp->u.t = (toml_table_t*)val;
        break;
    case TOML_ARRAY:
        tmp->u.a = (toml_array_t*)val;
        break;
    case TOML_BOOL:
        tmp->u.b = *(bool*)val;
        break;
    case TOML_INT:
        tmp->u.i = *(int64_t*)val;
        break;
    case TOML_DOUBLE:
        tmp->u.d = *(double*)val;
        break;
    case TOML_STRING:
        tmp->u.s = val;
        break;
    case TOML_TIMESTAMP:
        tmp->u.ts = (toml_timestamp_t*)val;
        break;
    default:
        log_error("Bad arg");
        tmp->u.t = NULL;
        //FIXME
    }
    /* log_debug("RETURNING item"); */
    return tmp;
}

bool tomlx_table_is_empty(toml_table_t *tt)
{
    return ((toml_table_nkval(tt) == 0)
            && (toml_table_narr(tt) == 0)
            && (toml_table_ntab(tt) == 0));
}

/* precon: o->type == TOML_TABLE  */
struct tomlx_item_s *tomlx_table_ref(toml_table_t *tt,
                                     /* struct tomlx_item_s *item, */
                                     const char *key)
{
    TOMLX_ENTRY(tomlx_table_ref);
    TOMLX_LOG_DEBUG("\tkey: %s", key);

    // STRING
    toml_datum_t datum = toml_string_in(tt, key);
    if (datum.ok) {
        /* TOMLX_LOG_DEBUG("STRING: %s", datum.u.s); */
        struct tomlx_item_s *rv = tomlx_make_item(datum.u.s, TOML_STRING);
        /* TOMLX_LOG_DEBUG("STRINGx: %s", rv->u.s); */
        return rv;
    }
    // INT
    datum = toml_int_in(tt, key);
    if (datum.ok) {
        /* TOMLX_LOG_DEBUG("INT: %lld", datum.u.i); */
        struct tomlx_item_s *rv = tomlx_make_item(&datum.u.i, TOML_INT);
        /* TOMLX_LOG_DEBUG("INT x: %lld", rv->u.i); */
        return rv;
    }
    // BOOL
    datum = toml_bool_in(tt, key);
    if (datum.ok) {
        struct tomlx_item_s *rv = tomlx_make_item(&datum.u.b, TOML_BOOL);
        return rv;
    }
    // DOUBLE
    datum = toml_double_in(tt, key);
    if (datum.ok) {
        /* TOMLX_LOG_DEBUG("DOUBLE: %lf", datum.u.d); */
        struct tomlx_item_s *rv = tomlx_make_item(&datum.u.d, TOML_DOUBLE);
        /* TOMLX_LOG_DEBUG("DBL: %lf", rv->u.d); */
        return rv;
    }
    // TIMESTAMP
    datum = toml_timestamp_in(tt, key);
    if (datum.ok) {
        /* log_debug("ref: TIMESTAMP", ""); */
        struct tomlx_item_s *rv = tomlx_make_item(datum.u.ts, TOML_TIMESTAMP);
        return rv;
    }
    // ARRAY
    toml_array_t *a = toml_array_in(tt, key);
    if (a) {
        /* TOMLX_LOG_DEBUG("ARRAY", ""); */
        struct tomlx_item_s *rv = tomlx_make_item(a, TOML_ARRAY);
        return rv;
    }
    // TABLE
    toml_table_t *t = toml_table_in(tt, key);
    if (t) {
        /* TOMLX_LOG_DEBUG("TABLE", ""); */
        struct tomlx_item_s *rv = tomlx_make_item(t, TOML_TABLE);
        /* log_debug("returning table-ref"); */
        return rv;
    }
    /* log_debug("table ref not found: %s", key); */
    return NULL; //FIXME
}

/* precon: o->type == TOML_ARRAY  */
struct tomlx_item_s *tomlx_array_ref(toml_array_t *ta,
                                           //struct tomlx_item_s *obj,
                                           int idx)
{
    TOMLX_ENTRY(tomlx_array_ref);
    TOMLX_LOG_DEBUG("\tidx: %s", idx);

    toml_datum_t datum;

    // STRING
    datum = toml_string_at(ta, idx);
    if (datum.ok) {
        /* TOMLX_LOG_DEBUG("STRING: %s", datum.u.s); */
        struct tomlx_item_s *rv = tomlx_make_item(datum.u.s, TOML_STRING);
        /* TOMLX_LOG_DEBUG("STRINGx: %s", rv->u.s); */
        return rv;
    }
    // INT
    datum = toml_int_at(ta, idx);
    if (datum.ok) {
        /* TOMLX_LOG_DEBUG("INT: %lld", datum.u.i); */
        struct tomlx_item_s *rv = tomlx_make_item(&datum.u.i, TOML_INT);
        /* TOMLX_LOG_DEBUG("INT x: %lld", rv->u.i); */
        return rv;
    }
    // BOOL
    datum = toml_bool_at(ta, idx);
    if (datum.ok) {
        struct tomlx_item_s *rv = tomlx_make_item(&datum.u.b, TOML_BOOL);
        return rv;
    }
    // DOUBLE
    datum = toml_double_at(ta, idx);
    if (datum.ok) {
        /* TOMLX_LOG_DEBUG("DOUBLE: %lf", datum.u.d); */
        struct tomlx_item_s *rv = tomlx_make_item(&datum.u.d, TOML_DOUBLE);
        /* TOMLX_LOG_DEBUG("DBL: %lf", rv->u.d); */
        return rv;
    }
    // ARRAY
    toml_array_t *a = toml_array_at(ta, idx);
    if (a) {
        /* TOMLX_LOG_DEBUG("ARRAY", ""); */
        struct tomlx_item_s *rv = tomlx_make_item(a, TOML_ARRAY);
        return rv;
    }
    // TABLE
    toml_table_t *t = toml_table_at(ta, idx);
    if (t) {
        /* TOMLX_LOG_DEBUG("TABLE", ""); */
        struct tomlx_item_s *rv = tomlx_make_item(t, TOML_TABLE);
        /* log_debug("returning table-ref"); */
        return rv;
    }
    // TIMESTAMP
    datum = toml_timestamp_at(ta, idx);
    if (datum.ok) {
        return NULL;
    }
    /* log_debug("array ref not found: %s", idx); */
    return NULL; //FIXME
}

/* **************************************************************** */
/* caller frees result */
/* toml_table_t *tomlx_read(const char *s) */
toml_table_t *tomlx_read_string(const char *s)
{
    TOMLX_ENTRY(tomlx_read_string);
    char errbuff[200];
    toml_table_t *tt = toml_parse((char*)s, errbuff, sizeof(errbuff));
    if (tt == NULL) {
        log_error("toml:read failure: %s", errbuff);
        return NULL;
   }
    return tt;
}

toml_table_t *tomlx_read_fp(FILE *instream)
/* toml_table_t *tomlx_read_stream(FILE *instream) */
{
    TOMLX_ENTRY(tomlx_fread);
    /* log_debug("s: %s", fname); */

    char errbuff[200];
    //WARNING: this toml_table_t must be freed by client
    toml_table_t *tt = toml_parse_file(instream, errbuff, sizeof(errbuff));
    fclose(instream);
    if (tt == NULL) {
        log_error("toml:read failure: %s", errbuff);
        return NULL;
    } else {
        return tt;
    }
}

toml_table_t *tomlx_read_file(const char *fname)
{
    TOMLX_ENTRY(tomlx_read_file);
    FILE *instream = fopen(fname, "r");
    if (instream == NULL) {
        /* Handle error */
        log_debug("fopen failure: %s", fname);
        perror(NULL);
        return NULL;
    }

    char errbuff[200];

    //WARNING: this toml_table_t must be freed by client
    toml_table_t *tt = toml_parse_file(instream, errbuff, sizeof(errbuff));
    fclose(instream);
    if (tt == NULL) {
        log_error("toml:read failure: %s", errbuff);
        return NULL;
    } else {
        return tt;
    }
}
