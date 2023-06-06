#ifndef _CJSONX_H_
#define _CJSONX_H_

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/errno.h>

#include "config.h"
#include "cJSON.h"
#include "cJSONx.h"

/* caller frees result */
cJSON *cjsonx_read_string(const char *s)
{
    /* log_debug("cjsonx_read"); */
    TRACE_ENTRY(cjsonx_read);
    cJSON *json_c = cJSON_Parse(s);
    if (json_c == NULL) {
        //FIXME: return an error code? errno_t? errno?
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            log_error("cJSON_Parse failure: %s", error_ptr);
        } else {
            log_error("cJSON_Parse failure w/o error_ptr");
        }
        // cJSON_Delete(json_c);
        return NULL;
    }
    return json_c;
}

/* from tomlc99 toml.c */
static void *_expand(void *p, int sz, int newsz)
{
    void *s = malloc(newsz);
    if (!s)
        return 0;

    memcpy(s, p, sz);
    free(p);
    return s;
}

cJSON *cjsonx_read_fp(FILE *fp)
{
    TRACE_LOG_DEBUG("cjsonx_read_fp", "");
    // code borrowed from tomlc99 toml_parse_file
    int bufsz = 0;
    char *json_str = 0;
    int off = 0;

    /* read from fp into json_str */
    while (!feof(fp)) {

        if (off == bufsz) {
            int xsz = bufsz + MALLOC_CHUNK;
            char *x = _expand(json_str, bufsz, xsz);
            if (!x) {
                /* snprintf(errbuf, errbufsz, "out of memory"); */
                log_error("cjsonx_fread: OOM");
                free(json_str);
                return NULL;
            }
            json_str = x;
            bufsz = xsz;
        }

        errno = 0;
        int n = fread(json_str + off, 1, bufsz - off, fp);
        if (ferror(fp)) {
            log_error("fread error: %s", strerror(errno));
            /* snprintf(errbuf, errbufsz, "%s", */
            /*          errno ? strerror(errno) : "Error reading file"); */
            free(json_str);
            return NULL;
        }
        off += n;
    }

    /* tag on a NUL to cap the string */
    if (off == bufsz) {
        int xsz = bufsz + 1;
        char *x = _expand(json_str, bufsz, xsz);
        if (!x) {
            log_error("OOM"); //FIXME
            /* snprintf(errbuf, errbufsz, "out of memory"); */
            free(json_str);
            return NULL;
        }
        json_str = x;
        bufsz = xsz;
    }
    json_str[off] = 0;

    cJSON *json_c = cJSON_Parse(json_str);
    if (json_c == NULL) {
        //FIXME: return an error code? errno_t? errno?
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            log_error("cJSON_Parse failure: %s", error_ptr);
        } else {
            log_error("cJSON_Parse failure w/o error_ptr");
        }
        // cJSON_Delete(json_c);
        return NULL;
    }

    free((void*)json_str);
    return json_c;
}

/* caller frees result */
cJSON *cjsonx_read_file(const char *fname)
{
    TRACE_LOG_DEBUG("cjsonx_read_file: %s", fname);
    FILE *instream = fopen(fname, "r");
    if (instream == NULL) {
        /* Handle error */
        log_debug("fopen failure: %s", fname);
        perror(NULL);
        return NULL;
    }
    cJSON *jmap = cjsonx_read_fp(instream);
    if (jmap == NULL) {
        log_error("cjsonx_read_file error: %s", fname);
        fclose(instream);
        return NULL;
    }
    fclose(instream);
    return jmap;
}

#endif
