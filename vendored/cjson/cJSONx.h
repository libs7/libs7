#ifndef _CJSONX_H_
#define _CJSONX_H_

#include <stdio.h>
#include "cJSON.h"

cJSON *cjsonx_read_string(const char *s);

cJSON *cjsonx_read_fp(FILE *f);

cJSON *cjsonx_read_file(const char *fname);

#endif
