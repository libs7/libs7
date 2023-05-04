#include <errno.h>

#include <inttypes.h> // PRI format macros
#define ld64 PRId64
#define p64 PRIdPTR

#include <locale.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <unistd.h>

#include "s7.h"

/* --------------------------------------------------------------------------------
 * local versions of some standard C library functions
 * timing tests involving these are very hard to interpret, local_memset is faster using int64_t than int32_t
 */

/* #if WITH_GCC */
/*   #define Sentinel __attribute__((sentinel)) */
/* #else */
/*   #define Sentinel */
/* #endif */

#ifndef S7_ALIGNED
  #define S7_ALIGNED 0
  /* memclr, local_strcmp and local_memset */
#endif

#ifndef S7_DEBUGGING
  #define S7_DEBUGGING 0
#endif

#define LOOP_4(Code) do {Code; Code; Code; Code;} while (0)

/*static*/ void local_memset(void *s, uint8_t val, size_t n)
{
  uint8_t *s2;
#if S7_ALIGNED
  s2 = (uint8_t *)s;
#else
#if (defined(__x86_64__) || defined(__i386__))
  if (n >= 8)
    {
      int64_t *s1 = (int64_t *)s;
      size_t n8 = n >> 3;
      int64_t ival = val | (val << 8) | (val << 16) | (((uint64_t)val) << 24); /* uint64_t casts make gcc/clang/fsanitize happy */
      ival = (((uint64_t)ival) << 32) | ival;
      if ((n8 & 0x3) == 0)
	while (n8 > 0) {LOOP_4(*s1++ = ival); n8 -= 4;}
      else do {*s1++ = ival;} while (--n8 > 0);
      n &= 7;
      s2 = (uint8_t *)s1;
    }
  else s2 = (uint8_t *)s;
#else
  s2 = (uint8_t *)s;
#endif
#endif
  while (n > 0)
    {
      *s2++ = val;
      n--;
    }
}

static inline s7_int safe_strlen(const char *str) /* this is safer than strlen, and slightly faster */
{
  const char *tmp = str;
  if ((!tmp) || (!(*tmp))) return(0);
  for (; *tmp; ++tmp);
  return(tmp - str);
}

#define Malloc malloc

/*static*/ char *copy_string_with_length(const char *str, s7_int len)
{
  char *newstr;
  if ((S7_DEBUGGING) && ((len <= 0) || (!str))) fprintf(stderr, "%s[%d]: len: %" ld64 ", str: %s\n", __func__, __LINE__, len, str);
  if (len > (1LL << 48)) return(NULL); /* squelch an idiotic warning */
  newstr = (char *)Malloc(len + 1);
  memcpy((void *)newstr, (const void *)str, len); /* we check len != 0 above -- 24-Jan-22 */
  newstr[len] = '\0';
  return(newstr);
}

/*static*/ char *copy_string(const char *str) {return(copy_string_with_length(str, safe_strlen(str)));}

#if 0
/*static*/ bool local_strcmp(const char *s1, const char *s2)
{
  while (true)
    {
      if (*s1 != *s2++) return(false);
      if (*s1++ == 0) return(true);
    }
  return(true);
}
#else
#define local_strcmp(S1, S2) (strcmp(S1, S2) == 0)
/* I think libc strcmp is much faster than it used to be, and beats the code above */
#endif

#define c_strings_are_equal(Str1, Str2) (local_strcmp(Str1, Str2))
/* scheme strings can have embedded nulls */

/*static*/ bool safe_strcmp(const char *s1, const char *s2)
{
  if ((!s1) || (!s2)) return(s1 == s2);
  return(local_strcmp(s1, s2));
}

/*static*/ bool local_strncmp(const char *s1, const char *s2, size_t n) /* not strncmp because scheme strings can have embedded nulls */
{
#if ((!S7_ALIGNED) && (defined(__x86_64__) || defined(__i386__))) /* unaligned accesses are safe on i386 hardware, sez everyone */
  if (n >= 8)
    {
      size_t n8 = n >> 3;
      int64_t *is1 = (int64_t *)s1, *is2 = (int64_t *)s2;
      do {if (*is1++ != *is2++) return(false);} while (--n8 > 0); /* in tbig LOOP_4 is slower? */
      s1 = (const char *)is1;
      s2 = (const char *)is2;
      n &= 7;
    }
#endif
  while (n > 0)
    {
      if (*s1++ != *s2++) return(false);
      n--;
    }
  return(true);
}

