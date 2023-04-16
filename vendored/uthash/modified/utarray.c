#include <stdio.h>
#include "utarray.h"

/* last we pre-define a few icd for common utarrays of ints and strings */
/* static */
void utarray_str_cpy(void *dst, const void *src) {
    /* printf("UTARRAY_STR_CPY: %s\n", (char*)src); */
  char *const *srcc = (char *const *)src;
  char **dstc = (char**)dst;
  *dstc = (*srcc == NULL) ? NULL : strdup(*srcc);
}
/* static */
void utarray_str_dtor(void *elt) {
  char **eltc = (char**)elt;
  if (*eltc != NULL) free(*eltc);
}
/* static */
const UT_icd ut_str_icd UTARRAY_UNUSED = {sizeof(char*),NULL,utarray_str_cpy,utarray_str_dtor};
/* static */
const UT_icd ut_int_icd UTARRAY_UNUSED = {sizeof(int),NULL,NULL,NULL};
/* static */
const UT_icd ut_ptr_icd UTARRAY_UNUSED = {sizeof(void*),NULL,NULL,NULL};

