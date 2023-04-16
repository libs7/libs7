#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <utf8proc.h>
#include "s7.h"

static s7_pointer c_pointer_string, string_string, character_string, boolean_string, real_string, complex_string, integer_string;


/* -------- utf8proc_version -------- */
static s7_pointer s7__utf8proc_version(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, (char*)utf8proc_version()));
}


/* -------- utf8proc_errmsg -------- */
static s7_pointer s7__utf8proc_errmsg(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__utf8proc_errmsg_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__utf8proc_errmsg_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libc* 'utf8proc_errmsg)", 25), 0, arg, integer_string));
  return(s7_make_string(sc, (char*)utf8proc_errmsg(s7__utf8proc_errmsg_0)));
}


/* -------- utf8proc_tolower -------- */
static s7_pointer s7__utf8proc_tolower(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  utf8proc_int32_t s7__utf8proc_tolower_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__utf8proc_tolower_0 = (utf8proc_int32_t)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libc* 'utf8proc_tolower)", 26), 0, arg, integer_string));
  return(s7_make_integer(sc, (s7_int)utf8proc_tolower(s7__utf8proc_tolower_0)));
}


/* -------- utf8proc_toupper -------- */
static s7_pointer s7__utf8proc_toupper(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  utf8proc_int32_t s7__utf8proc_toupper_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__utf8proc_toupper_0 = (utf8proc_int32_t)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libc* 'utf8proc_toupper)", 26), 0, arg, integer_string));
  return(s7_make_integer(sc, (s7_int)utf8proc_toupper(s7__utf8proc_toupper_0)));
}


/* -------- utf8proc_charwidth -------- */
static s7_pointer s7__utf8proc_charwidth(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  utf8proc_int32_t s7__utf8proc_charwidth_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__utf8proc_charwidth_0 = (utf8proc_int32_t)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libc* 'utf8proc_charwidth)", 28), 0, arg, integer_string));
  return(s7_make_integer(sc, (s7_int)utf8proc_charwidth(s7__utf8proc_charwidth_0)));
}


/* -------- utf8proc_category -------- */
static s7_pointer s7__utf8proc_category(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  utf8proc_int32_t s7__utf8proc_category_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__utf8proc_category_0 = (utf8proc_int32_t)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libc* 'utf8proc_category)", 27), 0, arg, integer_string));
  return(s7_make_integer(sc, (s7_int)utf8proc_category(s7__utf8proc_category_0)));
}


/* -------- utf8proc_category_string -------- */
static s7_pointer s7__utf8proc_category_string(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  utf8proc_int32_t s7__utf8proc_category_string_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__utf8proc_category_string_0 = (utf8proc_int32_t)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libc* 'utf8proc_category_string)", 34), 0, arg, integer_string));
  return(s7_make_string(sc, (char*)utf8proc_category_string(s7__utf8proc_category_string_0)));
}


/* -------- utf8proc_codepoint_valid -------- */
static s7_pointer s7__utf8proc_codepoint_valid(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  utf8proc_int32_t s7__utf8proc_codepoint_valid_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__utf8proc_codepoint_valid_0 = (utf8proc_int32_t)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libc* 'utf8proc_codepoint_valid)", 34), 0, arg, integer_string));
  return(s7_make_boolean(sc, (bool)utf8proc_codepoint_valid(s7__utf8proc_codepoint_valid_0)));
}


/* -------- utf8proc_grapheme_break -------- */
static s7_pointer s7__utf8proc_grapheme_break(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  utf8proc_int32_t s7__utf8proc_grapheme_break_0;
  utf8proc_int32_t s7__utf8proc_grapheme_break_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__utf8proc_grapheme_break_0 = (utf8proc_int32_t)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libc* 'utf8proc_grapheme_break)", 33), 1, arg, integer_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__utf8proc_grapheme_break_1 = (utf8proc_int32_t)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libc* 'utf8proc_grapheme_break)", 33), 2, arg, integer_string));
  return(s7_make_boolean(sc, (bool)utf8proc_grapheme_break(s7__utf8proc_grapheme_break_0, s7__utf8proc_grapheme_break_1)));
}


/* -------- utf8proc_NFD -------- */
static s7_pointer s7__utf8proc_NFD(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__utf8proc_NFD_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__utf8proc_NFD_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libc* 'utf8proc_NFD)", 22), 0, arg, string_string));
  return(s7_make_string(sc, (char*)utf8proc_NFD(s7__utf8proc_NFD_0)));
}


/* -------- utf8proc_NFC -------- */
static s7_pointer s7__utf8proc_NFC(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__utf8proc_NFC_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__utf8proc_NFC_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libc* 'utf8proc_NFC)", 22), 0, arg, string_string));
  return(s7_make_string(sc, (char*)utf8proc_NFC(s7__utf8proc_NFC_0)));
}


/* -------- utf8proc_NFKD -------- */
static s7_pointer s7__utf8proc_NFKD(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__utf8proc_NFKD_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__utf8proc_NFKD_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libc* 'utf8proc_NFKD)", 23), 0, arg, string_string));
  return(s7_make_string(sc, (char*)utf8proc_NFKD(s7__utf8proc_NFKD_0)));
}


/* -------- utf8proc_NFKC -------- */
static s7_pointer s7__utf8proc_NFKC(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__utf8proc_NFKC_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__utf8proc_NFKC_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libc* 'utf8proc_NFKC)", 23), 0, arg, string_string));
  return(s7_make_string(sc, (char*)utf8proc_NFKC(s7__utf8proc_NFKC_0)));
}

static s7_pointer g_utf8proc_iterate(s7_scheme *sc, s7_pointer args)
                  {
                    utf8proc_int32_t code_ref = 0;
                    int len, res;
                    char *str;
                    str = (char *)s7_string(s7_car(args));
                    len = s7_string_length(s7_car(args));
                    res = utf8proc_iterate(str, len, &code_ref);
                    return(s7_list(sc, 2, s7_make_integer(sc, code_ref), s7_make_integer(sc, res)));
                   }
static s7_pointer g_utf8proc_encode_char(s7_scheme *sc, s7_pointer args)
                  {
                    ssize_t res;
                    utf8proc_uint8_t buf[8];
                    res = utf8proc_encode_char((utf8proc_int32_t)s7_integer(s7_car(args)), buf);
                    return(s7_list(sc, 2, s7_make_string_with_length(sc, buf, res), s7_make_integer(sc, res)));
                   }
static s7_pointer g_utf8proc_reencode(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer buffer, codepoints, options;
                    ssize_t res;
                    buffer = s7_car(args);
                    codepoints = s7_cadr(args);
                    options = s7_caddr(args);
                    res = utf8proc_reencode((utf8proc_int32_t *)s7_string(buffer), 
                                            (utf8proc_ssize_t)s7_integer(codepoints), 
                                            (utf8proc_option_t)s7_integer(options));
                    return(s7_make_integer(sc, res));
                   }
static s7_pointer g_utf8proc_get_property(s7_scheme *sc, s7_pointer args)
                  {
	            const utf8proc_property_t *info;
                    info = utf8proc_get_property((utf8proc_int32_t)s7_integer(s7_car(args)));
                    return(s7_inlet(sc, s7_list(sc, 30,
                             s7_make_symbol(sc, "category"),           s7_make_integer(sc, info->category),
                             s7_make_symbol(sc, "combining_class"),    s7_make_integer(sc, info->combining_class),
                             s7_make_symbol(sc, "bidi_class"),         s7_make_integer(sc, info->bidi_class),
                             s7_make_symbol(sc, "decomp_type"),        s7_make_integer(sc, info->decomp_type),
                 #if (UTF8PROC_VERSION_MAJOR >= 2)
                             s7_make_symbol(sc, "uppercase_seqindex"), s7_make_integer(sc, info->uppercase_seqindex),
                             s7_make_symbol(sc, "lowercase_seqindex"), s7_make_integer(sc, info->lowercase_seqindex),
                             s7_make_symbol(sc, "titlecase_seqindex"), s7_make_integer(sc, info->titlecase_seqindex),
                             s7_make_symbol(sc, "casefold_seqindex"),  s7_make_integer(sc, info->casefold_seqindex),
                             s7_make_symbol(sc, "comb_index"),         s7_make_integer(sc, info->comb_index),
                 #else
                             s7_make_symbol(sc, "uppercase_mapping"),  s7_make_integer(sc, info->uppercase_mapping),
                             s7_make_symbol(sc, "lowercase_mapping"),  s7_make_integer(sc, info->lowercase_mapping),
                             s7_make_symbol(sc, "titlecase_mapping"),  s7_make_integer(sc, info->titlecase_mapping),
                             s7_make_symbol(sc, "comb1st_index"),      s7_make_integer(sc, info->comb1st_index),
                             s7_make_symbol(sc, "comb2nd_index"),      s7_make_integer(sc, info->comb2nd_index),
                 #endif
                             s7_make_symbol(sc, "bidi_mirrored"),      s7_make_integer(sc, info->bidi_mirrored),
                             s7_make_symbol(sc, "comp_exclusion"),     s7_make_integer(sc, info->comp_exclusion),
                             s7_make_symbol(sc, "ignorable"),          s7_make_integer(sc, info->ignorable),
                             s7_make_symbol(sc, "control_boundary"),   s7_make_integer(sc, info->control_boundary),
                             s7_make_symbol(sc, "boundclass"),         s7_make_integer(sc, info->boundclass),
                             s7_make_symbol(sc, "charwidth"),          s7_make_integer(sc, info->charwidth))));
                   }
static s7_pointer g_utf8proc_decompose_char(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer code, opt, str;
                    int last_boundclass;
                    utf8proc_ssize_t size;
                    utf8proc_int32_t *dst;
                    ssize_t res;
                    code = s7_car(args);
                    str = s7_cadr(args);
                    opt = s7_caddr(args);
                    dst = (utf8proc_int32_t *)s7_string(str);
                    size = (utf8proc_ssize_t)s7_string_length(str);
                    res = utf8proc_decompose_char((utf8proc_int32_t)s7_integer(code), dst, size, (utf8proc_option_t)s7_integer(opt), &last_boundclass);
                    return(s7_make_integer(sc, res));
                  }
static s7_pointer g_utf8proc_map(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer opt, str, p;
                    ssize_t res;
                    utf8proc_uint8_t *dst;
                    str = s7_car(args);
                    opt = s7_cadr(args);
                    res = utf8proc_map((utf8proc_uint8_t *)s7_string(str), s7_string_length(str), &dst, (utf8proc_option_t)s7_integer(opt));
                    if (res < 0) return(s7_make_integer(sc, res));
                    p = s7_make_string_with_length(sc, (const char*)dst, res);
                    free(dst);
                    return(p);
                  }
static s7_pointer g_utf8proc_decompose(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer opt, str;
                    int len;
                    ssize_t res;
                    utf8proc_int32_t *dst;
                    str = s7_car(args);
                    opt = s7_cadr(args);
                    len = s7_string_length(str);
                    dst = (utf8proc_int32_t *)malloc(len * 4);
                    res = utf8proc_decompose((const utf8proc_uint8_t *)s7_string(str), len, dst, len, (utf8proc_option_t)s7_integer(opt));
                    if (res < 0) return(s7_make_integer(sc, res));
                    return(s7_make_string_with_length(sc, (char *)dst, res));
                  }
void utf8proc_s7_init(s7_scheme *sc);
void utf8proc_s7_init(s7_scheme *sc)
{
  s7_pointer cur_env;
  s7_pointer pcl_s, pcl_i, pcl_bi, pl_bi, pl_si, pl_st;
  {
    s7_pointer t, s, b, i;
    t = s7_t(sc);
    s = s7_make_symbol(sc, "string?");
    b = s7_make_symbol(sc, "boolean?");
    i = s7_make_symbol(sc, "integer?");

    pcl_s = s7_make_circular_signature(sc, 0, 1, s);
    pcl_i = s7_make_circular_signature(sc, 0, 1, i);
    pcl_bi = s7_make_circular_signature(sc, 1, 2, b, i);
    pl_bi = s7_make_signature(sc, 2, b, i);
    pl_si = s7_make_signature(sc, 2, s, i);
    pl_st = s7_make_signature(sc, 2, s, t);
  }

  string_string = s7_make_semipermanent_string(sc, "a string");
  c_pointer_string = s7_make_semipermanent_string(sc, "a c-pointer");
  character_string = s7_make_semipermanent_string(sc, "a character");
  boolean_string = s7_make_semipermanent_string(sc, "a boolean");
  real_string = s7_make_semipermanent_string(sc, "a real");
  complex_string = s7_make_semipermanent_string(sc, "a complex number");
  integer_string = s7_make_semipermanent_string(sc, "an integer");
  cur_env = s7_curlet(sc);

  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BOUNDCLASS_SPACINGMARK"), s7_make_integer(sc, (s7_int)UTF8PROC_BOUNDCLASS_SPACINGMARK));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BOUNDCLASS_REGIONAL_INDICATOR"), s7_make_integer(sc, (s7_int)UTF8PROC_BOUNDCLASS_REGIONAL_INDICATOR));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BOUNDCLASS_LVT"), s7_make_integer(sc, (s7_int)UTF8PROC_BOUNDCLASS_LVT));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BOUNDCLASS_LV"), s7_make_integer(sc, (s7_int)UTF8PROC_BOUNDCLASS_LV));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BOUNDCLASS_T"), s7_make_integer(sc, (s7_int)UTF8PROC_BOUNDCLASS_T));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BOUNDCLASS_V"), s7_make_integer(sc, (s7_int)UTF8PROC_BOUNDCLASS_V));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BOUNDCLASS_L"), s7_make_integer(sc, (s7_int)UTF8PROC_BOUNDCLASS_L));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BOUNDCLASS_EXTEND"), s7_make_integer(sc, (s7_int)UTF8PROC_BOUNDCLASS_EXTEND));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BOUNDCLASS_CONTROL"), s7_make_integer(sc, (s7_int)UTF8PROC_BOUNDCLASS_CONTROL));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BOUNDCLASS_LF"), s7_make_integer(sc, (s7_int)UTF8PROC_BOUNDCLASS_LF));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BOUNDCLASS_CR"), s7_make_integer(sc, (s7_int)UTF8PROC_BOUNDCLASS_CR));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BOUNDCLASS_OTHER"), s7_make_integer(sc, (s7_int)UTF8PROC_BOUNDCLASS_OTHER));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BOUNDCLASS_START"), s7_make_integer(sc, (s7_int)UTF8PROC_BOUNDCLASS_START));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_COMPAT"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_COMPAT));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_FRACTION"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_FRACTION));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_SQUARE"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_SQUARE));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_SMALL"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_SMALL));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_NARROW"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_NARROW));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_WIDE"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_WIDE));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_VERTICAL"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_VERTICAL));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_SUB"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_SUB));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_SUPER"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_SUPER));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_CIRCLE"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_CIRCLE));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_ISOLATED"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_ISOLATED));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_FINAL"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_FINAL));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_MEDIAL"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_MEDIAL));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_INITIAL"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_INITIAL));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_NOBREAK"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_NOBREAK));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMP_TYPE_FONT"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMP_TYPE_FONT));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_PDI"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_PDI));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_FSI"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_FSI));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_RLI"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_RLI));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_LRI"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_LRI));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_ON"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_ON));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_WS"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_WS));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_S"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_S));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_B"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_B));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_BN"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_BN));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_NSM"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_NSM));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_CS"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_CS));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_AN"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_AN));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_ET"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_ET));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_ES"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_ES));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_EN"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_EN));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_PDF"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_PDF));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_RLO"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_RLO));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_RLE"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_RLE));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_AL"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_AL));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_R"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_R));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_LRO"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_LRO));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_LRE"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_LRE));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_BIDI_CLASS_L"), s7_make_integer(sc, (s7_int)UTF8PROC_BIDI_CLASS_L));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_CO"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_CO));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_CS"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_CS));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_CF"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_CF));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_CC"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_CC));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_ZP"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_ZP));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_ZL"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_ZL));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_ZS"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_ZS));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_SO"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_SO));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_SK"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_SK));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_SC"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_SC));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_SM"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_SM));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_PO"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_PO));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_PF"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_PF));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_PI"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_PI));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_PE"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_PE));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_PS"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_PS));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_PD"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_PD));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_PC"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_PC));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_NO"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_NO));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_NL"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_NL));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_ND"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_ND));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_ME"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_ME));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_MC"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_MC));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_MN"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_MN));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_LO"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_LO));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_LM"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_LM));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_LT"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_LT));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_LL"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_LL));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_LU"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_LU));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CATEGORY_CN"), s7_make_integer(sc, (s7_int)UTF8PROC_CATEGORY_CN));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_STRIPMARK"), s7_make_integer(sc, (s7_int)UTF8PROC_STRIPMARK));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_LUMP"), s7_make_integer(sc, (s7_int)UTF8PROC_LUMP));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CHARBOUND"), s7_make_integer(sc, (s7_int)UTF8PROC_CHARBOUND));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_CASEFOLD"), s7_make_integer(sc, (s7_int)UTF8PROC_CASEFOLD));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_STRIPCC"), s7_make_integer(sc, (s7_int)UTF8PROC_STRIPCC));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_NLF2LF"), s7_make_integer(sc, (s7_int)UTF8PROC_NLF2LF));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_NLF2PS"), s7_make_integer(sc, (s7_int)UTF8PROC_NLF2PS));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_NLF2LS"), s7_make_integer(sc, (s7_int)UTF8PROC_NLF2LS));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_REJECTNA"), s7_make_integer(sc, (s7_int)UTF8PROC_REJECTNA));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_IGNORE"), s7_make_integer(sc, (s7_int)UTF8PROC_IGNORE));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_DECOMPOSE"), s7_make_integer(sc, (s7_int)UTF8PROC_DECOMPOSE));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_COMPOSE"), s7_make_integer(sc, (s7_int)UTF8PROC_COMPOSE));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_COMPAT"), s7_make_integer(sc, (s7_int)UTF8PROC_COMPAT));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_STABLE"), s7_make_integer(sc, (s7_int)UTF8PROC_STABLE));
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_NULLTERM"), s7_make_integer(sc, (s7_int)UTF8PROC_NULLTERM));

#ifdef UTF8PROC_ERROR_INVALIDOPTS
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_ERROR_INVALIDOPTS"), s7_make_integer(sc, (s7_int)UTF8PROC_ERROR_INVALIDOPTS));
#endif
#ifdef UTF8PROC_ERROR_NOTASSIGNED
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_ERROR_NOTASSIGNED"), s7_make_integer(sc, (s7_int)UTF8PROC_ERROR_NOTASSIGNED));
#endif
#ifdef UTF8PROC_ERROR_INVALIDUTF8
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_ERROR_INVALIDUTF8"), s7_make_integer(sc, (s7_int)UTF8PROC_ERROR_INVALIDUTF8));
#endif
#ifdef UTF8PROC_ERROR_OVERFLOW
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_ERROR_OVERFLOW"), s7_make_integer(sc, (s7_int)UTF8PROC_ERROR_OVERFLOW));
#endif
#ifdef UTF8PROC_ERROR_NOMEM
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_ERROR_NOMEM"), s7_make_integer(sc, (s7_int)UTF8PROC_ERROR_NOMEM));
#endif
#ifdef UTF8PROC_VERSION_PATCH
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_VERSION_PATCH"), s7_make_integer(sc, (s7_int)UTF8PROC_VERSION_PATCH));
#endif
#ifdef UTF8PROC_VERSION_MINOR
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_VERSION_MINOR"), s7_make_integer(sc, (s7_int)UTF8PROC_VERSION_MINOR));
#endif
#ifdef UTF8PROC_VERSION_MAJOR
  s7_define(sc, cur_env, s7_make_symbol(sc, "UTF8PROC_VERSION_MAJOR"), s7_make_integer(sc, (s7_int)UTF8PROC_VERSION_MAJOR));
#endif

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_decompose"),
            s7_make_typed_function(sc, "utf8proc_decompose", g_utf8proc_decompose, 2, 0, false, "utf8proc_decompose", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_map"),
            s7_make_typed_function(sc, "utf8proc_map", g_utf8proc_map, 2, 0, false, "utf8proc_map", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_decompose_char"),
            s7_make_typed_function(sc, "utf8proc_decompose_char", g_utf8proc_decompose_char, 3, 0, false, "utf8proc_decompose_char", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_get_property"),
            s7_make_typed_function(sc, "utf8proc_get_property", g_utf8proc_get_property, 1, 0, false, "utf8proc_get_property", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_reencode"),
            s7_make_typed_function(sc, "utf8proc_reencode", g_utf8proc_reencode, 1, 0, false, "utf8proc_reencode", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_encode_char"),
            s7_make_typed_function(sc, "utf8proc_encode_char", g_utf8proc_encode_char, 1, 0, false, "utf8proc_encode_char", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_iterate"),
            s7_make_typed_function(sc, "utf8proc_iterate", g_utf8proc_iterate, 1, 0, false, "utf8proc_iterate", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_NFKC"),
            s7_make_typed_function(sc, "utf8proc_NFKC", s7__utf8proc_NFKC, 1, 0, false, "char* utf8proc_NFKC(char*)", pcl_s));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_NFKD"),
            s7_make_typed_function(sc, "utf8proc_NFKD", s7__utf8proc_NFKD, 1, 0, false, "char* utf8proc_NFKD(char*)", pcl_s));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_NFC"),
            s7_make_typed_function(sc, "utf8proc_NFC", s7__utf8proc_NFC, 1, 0, false, "char* utf8proc_NFC(char*)", pcl_s));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_NFD"),
            s7_make_typed_function(sc, "utf8proc_NFD", s7__utf8proc_NFD, 1, 0, false, "char* utf8proc_NFD(char*)", pcl_s));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_grapheme_break"),
            s7_make_typed_function(sc, "utf8proc_grapheme_break", s7__utf8proc_grapheme_break, 2, 0, false, "bool utf8proc_grapheme_break((utf8proc_int32_t int) (utf8proc_int32_t int))", pcl_bi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_codepoint_valid"),
            s7_make_typed_function(sc, "utf8proc_codepoint_valid", s7__utf8proc_codepoint_valid, 1, 0, false, "bool utf8proc_codepoint_valid((utf8proc_int32_t int))", pl_bi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_category_string"),
            s7_make_typed_function(sc, "utf8proc_category_string", s7__utf8proc_category_string, 1, 0, false, "char* utf8proc_category_string((utf8proc_int32_t int))", pl_si));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_category"),
            s7_make_typed_function(sc, "utf8proc_category", s7__utf8proc_category, 1, 0, false, "int utf8proc_category((utf8proc_int32_t int))", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_charwidth"),
            s7_make_typed_function(sc, "utf8proc_charwidth", s7__utf8proc_charwidth, 1, 0, false, "int utf8proc_charwidth((utf8proc_int32_t int))", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_toupper"),
            s7_make_typed_function(sc, "utf8proc_toupper", s7__utf8proc_toupper, 1, 0, false, "int utf8proc_toupper((utf8proc_int32_t int))", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_tolower"),
            s7_make_typed_function(sc, "utf8proc_tolower", s7__utf8proc_tolower, 1, 0, false, "int utf8proc_tolower((utf8proc_int32_t int))", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_errmsg"),
            s7_make_typed_function(sc, "utf8proc_errmsg", s7__utf8proc_errmsg, 1, 0, false, "char* utf8proc_errmsg(int)", pl_si));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utf8proc_version"),
            s7_make_typed_function(sc, "utf8proc_version", s7__utf8proc_version, 0, 0, false, "char* utf8proc_version(void)", pl_st));
}
