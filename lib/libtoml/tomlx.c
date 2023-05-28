#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>

#include "log.h"
#include "toml.h"
#include "tomlx.h"
#include "trace.h"

toml_datum_t tomlx_array_datum_for_idx(toml_array_t *ta, int idx, int *typ)
{
    TRACE_ENTRY(tomlx_array_datum_for_idx);
    toml_datum_t datum;

    datum = toml_string_at(ta, idx);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: s", "");
        *typ = TOML_STRING;
        return datum;
    }

    datum = toml_bool_at(ta, idx);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: b", "");
        *typ = TOML_BOOL;
        return datum;
    }

    datum = toml_int_at(ta, idx);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: i", "");
        *typ = TOML_INT;
        return datum;
    }

    datum = toml_double_at(ta, idx);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: d", "");
        *typ = TOML_DOUBLE;
        return datum;
    }

    datum = toml_timestamp_at(ta, idx);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: ts", "");
        *typ = TOML_TIMESTAMP;
        /* not yet supported */
        return datum;
    }
    TRACE_LOG_DEBUG("datum: NULL", "");
    *typ = TOML_NONDATUM;
    return datum;
}

void *tomlx_array_seq_for_idx(toml_array_t *ta, int idx, int *typ)
{
    TRACE_ENTRY(tomlx_array_seq_for_idx);

    toml_array_t *a = toml_array_at(ta, idx);
    if (a) {
        TRACE_LOG_DEBUG("array", "");
        *typ = TOML_ARRAY;
        return a;
    } else {
        TRACE_LOG_DEBUG("not array", "");
    }

    toml_table_t *subt = toml_table_at(ta, idx);
    if (subt) {
        TRACE_LOG_DEBUG("table: %p", subt);
        *typ = TOML_TABLE;
        return subt;
    } else {
        TRACE_LOG_DEBUG("not table", "");
    }
    return NULL;
}

char *tomlx_array_to_string(toml_array_t *ta, bool use_write)
{
    TRACE_ENTRY(tomlx_array_to_string);
    toml_datum_t datum;
    int typ;

    const int BUFSZ = 4096;
    char *buf;          /* WARNING: malloc */
    buf = calloc(BUFSZ, sizeof(char));
    if (!buf) {
        log_error("OOM");
        return NULL;
    } else {
        TRACE_LOG_DEBUG("callocated %d chars for buffer", BUFSZ);
    }
    size_t bufsz = BUFSZ;
    size_t char_ct = 0;
    int ct;
    (void)ct;

    // print header
    /* { */
        errno = 0;
        TRACE_LOG_DEBUG("snprintfing header", "");
        ct = snprintf(buf, 2, "%s", "[");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        } else {
            TRACE_LOG_DEBUG("snprintf hdr ct: %d", ct);
        }
        char_ct += 1; // do not include terminating '\0'
        TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
        TRACE_LOG_DEBUG("buf: %s", buf);
    /* } */

    // print elements
    int idx_ct = toml_array_nelem(ta);
    /* char *k, *v; */
    int len;
    (void)ct;                   /* set-but-not-used warning */
    for (int i = 0; i < idx_ct; i++) {
        // print comma
        if (i > 0) {
            if ((char_ct + 3) > bufsz) {
                log_error("realloc for comma");
            } else {
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing comma", "");
                ct = snprintf(buf+char_ct, 3, "%s", ", ");
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf comma ct: %d", ct);
                }
                char_ct += 2; // do not include terminating '\0'
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
            }
        }

        // print value
        datum = tomlx_array_datum_for_idx(ta, i, &typ);
        char *seq_str;
        TRACE_LOG_DEBUG("datum typ: %d", typ);
        if (typ == TOML_NONDATUM) {
            void *seq = tomlx_array_seq_for_idx(ta, i, &typ);
            switch(typ) {
            case TOML_ARRAY:
                TRACE_LOG_DEBUG("array seq: %p", seq);
                seq_str = tomlx_array_to_string((toml_array_t*)seq, use_write);
                TRACE_LOG_DEBUG("ARRAY str: %s", seq_str);
                len = strlen(seq_str) + 1;  // + 1 for '\0'
                if ((char_ct + len) > bufsz) {
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing array len %d", len);
                ct = snprintf(buf+char_ct, len, "%s = ", seq_str);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1; // do not include terminating '\0'
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_TABLE:
                TRACE_LOG_DEBUG("table seq: %p", seq);
                seq_str = tomlx_table_to_string((toml_table_t*)seq, use_write);
                TRACE_LOG_DEBUG("TABLE: %s", seq_str);
                len = strlen(seq_str) + 1;  // + 1 for '\0'
                if ((char_ct + len) > bufsz) {
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing table len %d", len);
                ct = snprintf(buf+char_ct, len, "%s = ", seq_str);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1; // do not include terminating '\0'
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                break;
            default:
                log_error("Bad toml seq type: %d", typ);
            }
        } else {
            switch(typ) {
            case TOML_INT:
                TRACE_LOG_DEBUG("toml datum val: %d", datum.u.i);
                len = snprintf(NULL, 0, "%lld", datum.u.i);
                len++; // for terminating '\0';
                TRACE_LOG_DEBUG("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing len %d", len);
                ct = snprintf(buf+char_ct, len, "%lld", datum.u.i);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_STRING:
                TRACE_LOG_DEBUG("toml datum val: %s", datum.u.s);
                // add 2 for quotes
                len = snprintf(NULL, 0, "%s", datum.u.s) + 2;
                len++; // for terminating '\0';
                TRACE_LOG_DEBUG("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing string, len %d", len);
                ct = snprintf(buf+char_ct, len, "\"%s\"", datum.u.s);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                free(datum.u.s);
                break;
            case TOML_BOOL:
                // tomlc99 bool val is int
                TRACE_LOG_DEBUG("toml datum val: %d", datum.u.b);
                if (datum.u.b) {
                    len = 5; // "true" + \0
                } else {
                    len = 6; // "false" + \0
                }
                TRACE_LOG_DEBUG("bool str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing len %d", len);
                if (datum.u.b) {
                    ct = snprintf(buf+char_ct, len, "%s", "true");
                } else {
                    ct = snprintf(buf+char_ct, len, "%s", "false");
                }
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_DOUBLE:
                TRACE_LOG_DEBUG("toml datum val: %g", datum.u.d);
                len = snprintf(NULL, 0, "%g", datum.u.d);
                len++; // for terminating '\0';
                TRACE_LOG_DEBUG("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing len %d", len);
                ct = snprintf(buf+char_ct, len, "%g", datum.u.d);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_TIMESTAMP:
                log_error("toml timestamp (not yet)");
                if ((char_ct + 18) > bufsz) {
                    // realloc
                } else {
                    errno = 0;
                    snprintf(buf, 18, "%s", "<#toml-timestamp>");
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
    {
        errno = 0;
        TRACE_LOG_DEBUG("snprintfing footer", "");
        ct = snprintf(buf+char_ct, 2, "%s", "]");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        } else {
            TRACE_LOG_DEBUG("snprintf hdr ct: %d", ct);
        }
        char_ct += 1; // do not include terminating '\0'
        TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
        TRACE_LOG_DEBUG("buf: %s", buf);
    }
    TRACE_LOG_DEBUG("tomlx_array_to_string returning: %s", buf);
    return buf;
}

toml_datum_t tomlx_table_datum_for_key(toml_table_t *tt, char *key, int *typ)
{
    TRACE_ENTRY_STR(tomlx_table_datum_for_key, key);
    toml_datum_t datum;

    datum = toml_string_in(tt, key);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: string", "");
        *typ = TOML_STRING;
        return datum;
    }

    datum = toml_bool_in(tt, key);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: bool", "");
        *typ = TOML_BOOL;
        return datum;
    }

    datum = toml_int_in(tt, key);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: int", "");
        *typ = TOML_INT;
        return datum;
    }

    datum = toml_double_in(tt, key);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: double", "");
        *typ = TOML_DOUBLE;
        return datum;
    }

    datum = toml_timestamp_in(tt, key);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: ts", "");
        *typ = TOML_TIMESTAMP;
        return datum;
    }
    TRACE_LOG_DEBUG("datum: NULL", "");
    *typ = TOML_NONDATUM;
    return datum;
}

void *tomlx_table_seq_for_key(toml_table_t *tt, char *key, int *typ)
{
    TRACE_ENTRY(tomlx_table_seq_for_key);

    toml_array_t *a = toml_array_in(tt, key);
    if (a) {
        TRACE_LOG_DEBUG("array", "");
        *typ = TOML_ARRAY;
        return a;
    /* } else { */
    /*     TRACE_LOG_DEBUG("not array", ""); */
    }

    toml_table_t *subt = toml_table_in(tt, key);
    if (subt) {
        TRACE_LOG_DEBUG("table: %p", subt);
        *typ = TOML_TABLE;
        return subt;
    /* } else { */
    /*     TRACE_LOG_DEBUG("not table", ""); */
    }
    return NULL;
}

char *tomlx_table_to_string(toml_table_t *tt, bool use_write)
{
    TRACE_ENTRY(tomlx_table_to_string);
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
        TRACE_LOG_DEBUG("callocated %d chars for buffer", BUFSZ);
    }
    size_t bufsz = BUFSZ;
    size_t char_ct = 0;
    int ct;
    (void)ct;                   /* elim unused-but-set-variable warning */

    // print header
    {
        errno = 0;
        TRACE_LOG_DEBUG("snprintfing header", "");
        ct = snprintf(buf, 14, "%s", "<#toml-table ");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        } else {
            TRACE_LOG_DEBUG("snprintf hdr ct: %d", ct);
        }
        char_ct += 13; // do not include terminating '\0'
        TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
        TRACE_LOG_DEBUG("buf: %s", buf);
    }

    // print fields
    int ntab = toml_table_ntab(tt);
    int narr = toml_table_narr(tt);
    int nkv = toml_table_nkval(tt);
    int key_ct = ntab + narr + nkv;

    TRACE_LOG_DEBUG("key ct: %d", key_ct);

    const char *k; //, *v;
    int len;
    for (int i = 0; i < key_ct; i++) {
        k = toml_key_in(tt, i);
        if (!k) {
            log_error("toml_key_in failure for idx: %d", i);
            return NULL;
        }
        TRACE_LOG_DEBUG("table key: %s", k);

        // print comma
        if (i > 0) {
            if ((char_ct + 3) > bufsz) {
                log_error("realloc for comma");
            } else {
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing comma", "");
                ct = snprintf(buf+char_ct, 3, "%s", ", ");
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf comma ct: %d", ct);
                }
                char_ct += 2; // do not include terminating '\0'
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
            }
        }

        // print key to buf
        {
            len = strlen(k) + 3; // for " = "
            len++; // terminating '\0'
            if ((char_ct + len) > bufsz) { // + 1 for '\0'
                log_error("exceeded bufsz: %d", char_ct + len);
                // expand buf
            }
            errno = 0;
            TRACE_LOG_DEBUG("snprintfing key len %d", len);
            ct = snprintf(buf+char_ct, len, "%s = ", k);
            if (errno) {
                log_error("snprintf: %s", strerror(errno));
                break;
            } else {
                TRACE_LOG_DEBUG("snprintf ct: %d", ct);
            }
            char_ct += len - 1; // do not include terminating '\0'
            TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
            TRACE_LOG_DEBUG("buf: %s", buf);
        }

        // print value
        datum = tomlx_table_datum_for_key(tt, (char*)k, &typ);
        char *seq_str;
        TRACE_LOG_DEBUG("datum typ: %d", typ);
        if (typ == TOML_NONDATUM) {
            void *seq = tomlx_table_seq_for_key(tt, (char*)k, &typ);
            switch(typ) {
            case TOML_ARRAY:
                TRACE_LOG_DEBUG("array seq: %p", seq);
                seq_str = tomlx_array_to_string((toml_array_t*)seq, use_write);
                TRACE_LOG_DEBUG("ARRAY: %s", seq_str);
                len = strlen(seq_str) + 1;  // + 1 for '\0'
                if ((char_ct + len) > bufsz) {
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing array len %d", len);
                ct = snprintf(buf+char_ct, len, "%s = ", seq_str);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1; // do not include terminating '\0'
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_TABLE:
                TRACE_LOG_DEBUG("table seq: %p", seq);
                seq_str = tomlx_table_to_string((toml_table_t*)seq, use_write);
                TRACE_LOG_DEBUG("TABLE: %s", seq_str);
                len = strlen(seq_str) + 1;  // + 1 for '\0'
                if ((char_ct + len) > bufsz) {
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing table len %d", len);
                ct = snprintf(buf+char_ct, len, "%s = ", seq_str);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1; // do not include terminating '\0'
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                break;
            default:
                log_error("Bad toml seq type: %d", typ);
            }
        } else {
            switch(typ) {
            case TOML_INT:
                TRACE_LOG_DEBUG("toml datum val: %d", datum.u.i);
                len = snprintf(NULL, 0, "%lld", datum.u.i);
                len++; // for terminating '\0';
                TRACE_LOG_DEBUG("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing len %d", len);
                ct = snprintf(buf+char_ct, len, "%lld", datum.u.i);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_STRING:
                TRACE_LOG_DEBUG("toml datum val: %s", datum.u.s);
                // add 2 for quotes
                len = snprintf(NULL, 0, "%s", datum.u.s) + 2;
                len++; // for terminating '\0';
                TRACE_LOG_DEBUG("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing string, len %d", len);
                ct = snprintf(buf+char_ct, len, "'%s'", datum.u.s);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                free(datum.u.s);
                break;
            case TOML_BOOL:
                // tomlc99 bool val is int
                TRACE_LOG_DEBUG("toml datum val: %d", datum.u.b);
                if (datum.u.b) {
                    len = 5; // "true" + \0
                } else {
                    len = 6; // "false" + \0
                }
                TRACE_LOG_DEBUG("bool str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing len %d", len);
                if (datum.u.b) {
                    ct = snprintf(buf+char_ct, len, "%s", "true");
                } else {
                    ct = snprintf(buf+char_ct, len, "%s", "false");
                }
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_DOUBLE:
                TRACE_LOG_DEBUG("toml datum val: %g", datum.u.d);
                len = snprintf(NULL, 0, "%g", datum.u.d);
                len++; // for terminating '\0';
                TRACE_LOG_DEBUG("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing len %d", len);
                ct = snprintf(buf+char_ct, len, "%g", datum.u.d);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_TIMESTAMP:
                seq_str = tomlx_datetime_to_string((toml_timestamp_t*)datum.u.ts, use_write);
                TRACE_LOG_DEBUG("TS: %s", seq_str);
                len = strlen(seq_str) + 1;  // + 1 for '\0'
                if ((char_ct + len) > bufsz) {
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing timestamp len %d", len);
                ct = snprintf(buf+char_ct, len, "%s = ", seq_str);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1; // do not include terminating '\0'
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
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
    {
        errno = 0;
        TRACE_LOG_DEBUG("snprintfing footer", "");
        ct = snprintf(buf+char_ct, 2, "%s", ">");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        } else {
            TRACE_LOG_DEBUG("snprintf hdr ct: %d", ct);
        }
        char_ct += 1; // do not include terminating '\0'
        TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
        TRACE_LOG_DEBUG("buf: %s", buf);
    }
    TRACE_LOG_DEBUG("tomlx_table_to_string returning: %s", buf);
    return buf;
}



char *tomlx_datetime_to_string(toml_timestamp_t *ts, bool use_write)
{
    TRACE_ENTRY(tomlx_datetime_to_string);
    TRACE_LOG_DEBUG("use_write: %d", use_write);
    const int BUFSZ = 4096;
    char *buf;          /* WARNING: malloc */
    buf = calloc(BUFSZ, sizeof(char));
    if (!buf) {
        log_error("OOM");
        return NULL;
    } else {
        TRACE_LOG_DEBUG("callocated %d chars for buffer", BUFSZ);
    }
    /* size_t bufsz = BUFSZ; */
    size_t char_ct = 0;
    int ct;
    (void)ct;

    // print leading "
    if (use_write) {
        errno = 0;
        TRACE_LOG_DEBUG("snprintfing header", "");
        // FIXME: check buf sz
        ct = snprintf(buf, 2, "%s", "\"");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        } else {
            TRACE_LOG_DEBUG("snprintf hdr ct: %d", ct);
        }
        char_ct += 1; // do not include terminating '\0'
    }

    int zlen;
    if (ts->z == NULL) {
        zlen = 0;
    } else {
        zlen = strlen(ts->z);
        log_debug("z: %s", ts->z);
    }
    log_debug("zlen %d", zlen);

    if (ts->year == NULL) {
        // local time
    }
    else if (ts->hour == NULL) {
        // local date
    }
    else if (ts->millisec == NULL) {
        log_debug("NO MILLIS");
        // e.g. 1979-05-27T07:32:00
        snprintf(buf + char_ct, 20 + zlen, "%.4d-%0.2d-%0.2dT%0.2d:%02.d:%0.2d%s",
                 *ts->year, *ts->month, *ts->day,
                 *ts->hour, *ts->minute, *ts->second,
                 (zlen>0)? ts->z : "X");
        char_ct += 20 +  zlen - 1;
    }
    else {
        log_debug("MILLIS");
        // tomlc99: only 3 decimal places for millis
        // e.g. 1979-05-27T00:32:00.999999
        snprintf(buf + char_ct, 24 + zlen, "%.4d-%0.2d-%0.2dT%0.2d:%02.d:%0.2d.%d%s",
                 *ts->year, *ts->month, *ts->day,
                 *ts->hour, *ts->minute, *ts->second,
                 *ts->millisec,
                 (zlen>0)? ts->z : "");
        char_ct += 24 + zlen - 1;
    }
    log_debug("buf: %s", buf);

    // print footer
    if (use_write) {
        errno = 0;
        TRACE_LOG_DEBUG("snprintfing datetime footer", "");
        ct = snprintf(buf+char_ct, 2, "%s", "\"");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        } else {
            TRACE_LOG_DEBUG("snprintf footer ct: %d", ct);
        }
        char_ct += 1; // do not include terminating '\0'
        TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
        TRACE_LOG_DEBUG("buf: %s", buf);
    }
    TRACE_LOG_DEBUG("tomlx_datetime_to_string returning: %s", buf);
    return buf;
}



