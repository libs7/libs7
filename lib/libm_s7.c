#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "s7.h"

static s7_pointer c_pointer_string, string_string, character_string, boolean_string, real_string, complex_string, integer_string;


/* -------- j0 -------- */
static s7_pointer s7__j0(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__j0_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__j0_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'j0)", 12), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)j0(s7__j0_0)));
}
static s7_double j0_d_d(s7_double x) {return(j0(x));}


/* -------- j1 -------- */
static s7_pointer s7__j1(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__j1_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__j1_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'j1)", 12), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)j1(s7__j1_0)));
}
static s7_double j1_d_d(s7_double x) {return(j1(x));}


/* -------- jn -------- */
static s7_pointer s7__jn(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__jn_0;
  double s7__jn_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__jn_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'jn)", 12), 1, arg, integer_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__jn_1 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'jn)", 12), 2, arg, real_string));
  return(s7_make_real(sc, (s7_double)jn(s7__jn_0, s7__jn_1)));
}
static s7_double jn_d_id(s7_int x1, s7_double x2) {return(jn(x1, x2));}


/* -------- erf -------- */
static s7_pointer s7__erf(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__erf_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__erf_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'erf)", 13), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)erf(s7__erf_0)));
}
static s7_double erf_d_d(s7_double x) {return(erf(x));}


/* -------- erfc -------- */
static s7_pointer s7__erfc(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__erfc_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__erfc_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'erfc)", 14), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)erfc(s7__erfc_0)));
}
static s7_double erfc_d_d(s7_double x) {return(erfc(x));}


/* -------- lgamma -------- */
static s7_pointer s7__lgamma(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__lgamma_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__lgamma_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'lgamma)", 16), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)lgamma(s7__lgamma_0)));
}
static s7_double lgamma_d_d(s7_double x) {return(lgamma(x));}


/* -------- fabs -------- */
static s7_pointer s7__fabs(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__fabs_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__fabs_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'fabs)", 14), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)fabs(s7__fabs_0)));
}
static s7_double fabs_d_d(s7_double x) {return(fabs(x));}


/* -------- ceil -------- */
static s7_pointer s7__ceil(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__ceil_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__ceil_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'ceil)", 14), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)ceil(s7__ceil_0)));
}
static s7_double ceil_d_d(s7_double x) {return(ceil(x));}


/* -------- nearbyint -------- */
static s7_pointer s7__nearbyint(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__nearbyint_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__nearbyint_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'nearbyint)", 19), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)nearbyint(s7__nearbyint_0)));
}
static s7_double nearbyint_d_d(s7_double x) {return(nearbyint(x));}


/* -------- scalbln -------- */
static s7_pointer s7__scalbln(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__scalbln_0;
  int s7__scalbln_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__scalbln_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'scalbln)", 17), 1, arg, real_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__scalbln_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'scalbln)", 17), 2, arg, integer_string));
  return(s7_make_real(sc, (s7_double)scalbln(s7__scalbln_0, s7__scalbln_1)));
}


/* -------- fma -------- */
static s7_pointer s7__fma(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__fma_0;
  double s7__fma_1;
  double s7__fma_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__fma_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'fma)", 13), 1, arg, real_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__fma_1 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'fma)", 13), 2, arg, real_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__fma_2 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'fma)", 13), 3, arg, real_string));
  return(s7_make_real(sc, (s7_double)fma(s7__fma_0, s7__fma_1, s7__fma_2)));
}
static s7_double fma_d_ddd(s7_double x1, s7_double x2, s7_double x3) {return(fma(x1, x2, x3));}


/* -------- rint -------- */
static s7_pointer s7__rint(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__rint_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__rint_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'rint)", 14), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)rint(s7__rint_0)));
}
static s7_double rint_d_d(s7_double x) {return(rint(x));}


/* -------- llrint -------- */
static s7_pointer s7__llrint(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__llrint_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__llrint_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'llrint)", 16), 0, arg, real_string));
  return(s7_make_integer(sc, (s7_int)llrint(s7__llrint_0)));
}
static s7_int llrint_i_7d(s7_scheme *sc, s7_double x) {return(llrint(x));}


/* -------- llround -------- */
static s7_pointer s7__llround(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__llround_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__llround_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'llround)", 17), 0, arg, real_string));
  return(s7_make_integer(sc, (s7_int)llround(s7__llround_0)));
}
static s7_int llround_i_7d(s7_scheme *sc, s7_double x) {return(llround(x));}


/* -------- trunc -------- */
static s7_pointer s7__trunc(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__trunc_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__trunc_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'trunc)", 15), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)trunc(s7__trunc_0)));
}
static s7_double trunc_d_d(s7_double x) {return(trunc(x));}


/* -------- fmod -------- */
static s7_pointer s7__fmod(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__fmod_0;
  double s7__fmod_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__fmod_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'fmod)", 14), 1, arg, real_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__fmod_1 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'fmod)", 14), 2, arg, real_string));
  return(s7_make_real(sc, (s7_double)fmod(s7__fmod_0, s7__fmod_1)));
}
static s7_double fmod_d_dd(s7_double x1, s7_double x2) {return(fmod(x1, x2));}


/* -------- ldexp -------- */
static s7_pointer s7__ldexp(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__ldexp_0;
  int s7__ldexp_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__ldexp_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'ldexp)", 15), 1, arg, real_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__ldexp_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'ldexp)", 15), 2, arg, integer_string));
  return(s7_make_real(sc, (s7_double)ldexp(s7__ldexp_0, s7__ldexp_1)));
}


/* -------- scalbn -------- */
static s7_pointer s7__scalbn(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__scalbn_0;
  int s7__scalbn_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__scalbn_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'scalbn)", 16), 1, arg, real_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__scalbn_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'scalbn)", 16), 2, arg, integer_string));
  return(s7_make_real(sc, (s7_double)scalbn(s7__scalbn_0, s7__scalbn_1)));
}


/* -------- exp2 -------- */
static s7_pointer s7__exp2(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__exp2_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__exp2_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'exp2)", 14), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)exp2(s7__exp2_0)));
}
static s7_double exp2_d_d(s7_double x) {return(exp2(x));}


/* -------- expm1 -------- */
static s7_pointer s7__expm1(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__expm1_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__expm1_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'expm1)", 15), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)expm1(s7__expm1_0)));
}
static s7_double expm1_d_d(s7_double x) {return(expm1(x));}


/* -------- log10 -------- */
static s7_pointer s7__log10(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__log10_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__log10_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'log10)", 15), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)log10(s7__log10_0)));
}
static s7_double log10_d_d(s7_double x) {return(log10(x));}


/* -------- log1p -------- */
static s7_pointer s7__log1p(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__log1p_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__log1p_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'log1p)", 15), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)log1p(s7__log1p_0)));
}
static s7_double log1p_d_d(s7_double x) {return(log1p(x));}


/* -------- log2 -------- */
static s7_pointer s7__log2(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__log2_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__log2_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'log2)", 14), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)log2(s7__log2_0)));
}
static s7_double log2_d_d(s7_double x) {return(log2(x));}


/* -------- ilogb -------- */
static s7_pointer s7__ilogb(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__ilogb_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__ilogb_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'ilogb)", 15), 0, arg, real_string));
  return(s7_make_integer(sc, (s7_int)ilogb(s7__ilogb_0)));
}
static s7_int ilogb_i_7d(s7_scheme *sc, s7_double x) {return(ilogb(x));}


/* -------- cbrt -------- */
static s7_pointer s7__cbrt(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__cbrt_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__cbrt_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'cbrt)", 14), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)cbrt(s7__cbrt_0)));
}
static s7_double cbrt_d_d(s7_double x) {return(cbrt(x));}


/* -------- hypot -------- */
static s7_pointer s7__hypot(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__hypot_0;
  double s7__hypot_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__hypot_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'hypot)", 15), 1, arg, real_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__hypot_1 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'hypot)", 15), 2, arg, real_string));
  return(s7_make_real(sc, (s7_double)hypot(s7__hypot_0, s7__hypot_1)));
}
static s7_double hypot_d_dd(s7_double x1, s7_double x2) {return(hypot(x1, x2));}


/* -------- pow -------- */
static s7_pointer s7__pow(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__pow_0;
  double s7__pow_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__pow_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'pow)", 13), 1, arg, real_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__pow_1 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'pow)", 13), 2, arg, real_string));
  return(s7_make_real(sc, (s7_double)pow(s7__pow_0, s7__pow_1)));
}
static s7_double pow_d_dd(s7_double x1, s7_double x2) {return(pow(x1, x2));}


/* -------- fdim -------- */
static s7_pointer s7__fdim(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__fdim_0;
  double s7__fdim_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__fdim_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'fdim)", 14), 1, arg, real_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__fdim_1 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'fdim)", 14), 2, arg, real_string));
  return(s7_make_real(sc, (s7_double)fdim(s7__fdim_0, s7__fdim_1)));
}
static s7_double fdim_d_dd(s7_double x1, s7_double x2) {return(fdim(x1, x2));}


/* -------- tgamma -------- */
static s7_pointer s7__tgamma(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__tgamma_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__tgamma_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'tgamma)", 16), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)tgamma(s7__tgamma_0)));
}
static s7_double tgamma_d_d(s7_double x) {return(tgamma(x));}


/* -------- copysign -------- */
static s7_pointer s7__copysign(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__copysign_0;
  double s7__copysign_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__copysign_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'copysign)", 18), 1, arg, real_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__copysign_1 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'copysign)", 18), 2, arg, real_string));
  return(s7_make_real(sc, (s7_double)copysign(s7__copysign_0, s7__copysign_1)));
}
static s7_double copysign_d_dd(s7_double x1, s7_double x2) {return(copysign(x1, x2));}


/* -------- nan -------- */
static s7_pointer s7__nan(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__nan_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__nan_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'nan)", 13), 0, arg, string_string));
  return(s7_make_real(sc, (s7_double)nan(s7__nan_0)));
}


/* -------- nextafter -------- */
static s7_pointer s7__nextafter(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__nextafter_0;
  double s7__nextafter_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__nextafter_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'nextafter)", 19), 1, arg, real_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__nextafter_1 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'nextafter)", 19), 2, arg, real_string));
  return(s7_make_real(sc, (s7_double)nextafter(s7__nextafter_0, s7__nextafter_1)));
}
static s7_double nextafter_d_dd(s7_double x1, s7_double x2) {return(nextafter(x1, x2));}


/* -------- nexttoward -------- */
static s7_pointer s7__nexttoward(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__nexttoward_0;
  double s7__nexttoward_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__nexttoward_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'nexttoward)", 20), 1, arg, real_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__nexttoward_1 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'nexttoward)", 20), 2, arg, real_string));
  return(s7_make_real(sc, (s7_double)nexttoward(s7__nexttoward_0, s7__nexttoward_1)));
}
static s7_double nexttoward_d_dd(s7_double x1, s7_double x2) {return(nexttoward(x1, x2));}


/* -------- fpclassify -------- */
static s7_pointer s7__fpclassify(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__fpclassify_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__fpclassify_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'fpclassify)", 20), 0, arg, real_string));
  return(s7_make_integer(sc, (s7_int)fpclassify(s7__fpclassify_0)));
}
static s7_int fpclassify_i_7d(s7_scheme *sc, s7_double x) {return(fpclassify(x));}


/* -------- isfinite -------- */
static s7_pointer s7__isfinite(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__isfinite_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__isfinite_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'isfinite)", 18), 0, arg, real_string));
  return(s7_make_integer(sc, (s7_int)isfinite(s7__isfinite_0)));
}
static s7_int isfinite_i_7d(s7_scheme *sc, s7_double x) {return(isfinite(x));}


/* -------- isinf -------- */
static s7_pointer s7__isinf(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__isinf_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__isinf_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'isinf)", 15), 0, arg, real_string));
  return(s7_make_integer(sc, (s7_int)isinf(s7__isinf_0)));
}
static s7_int isinf_i_7d(s7_scheme *sc, s7_double x) {return(isinf(x));}


/* -------- isnan -------- */
static s7_pointer s7__isnan(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__isnan_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__isnan_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'isnan)", 15), 0, arg, real_string));
  return(s7_make_integer(sc, (s7_int)isnan(s7__isnan_0)));
}
static s7_int isnan_i_7d(s7_scheme *sc, s7_double x) {return(isnan(x));}


/* -------- isnormal -------- */
static s7_pointer s7__isnormal(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__isnormal_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__isnormal_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'isnormal)", 18), 0, arg, real_string));
  return(s7_make_integer(sc, (s7_int)isnormal(s7__isnormal_0)));
}
static s7_int isnormal_i_7d(s7_scheme *sc, s7_double x) {return(isnormal(x));}


/* -------- signbit -------- */
static s7_pointer s7__signbit(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__signbit_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__signbit_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'signbit)", 17), 0, arg, real_string));
  return(s7_make_integer(sc, (s7_int)signbit(s7__signbit_0)));
}
static s7_int signbit_i_7d(s7_scheme *sc, s7_double x) {return(signbit(x));}


/* -------- floor -------- */
static s7_pointer s7__floor(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__floor_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__floor_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'floor)", 15), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)floor(s7__floor_0)));
}


/* -------- round -------- */
static s7_pointer s7__round(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__round_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__round_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'round)", 15), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)round(s7__round_0)));
}


/* -------- remainder -------- */
static s7_pointer s7__remainder(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__remainder_0;
  double s7__remainder_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__remainder_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'remainder)", 19), 1, arg, real_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__remainder_1 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'remainder)", 19), 2, arg, real_string));
  return(s7_make_real(sc, (s7_double)remainder(s7__remainder_0, s7__remainder_1)));
}


/* -------- exp -------- */
static s7_pointer s7__exp(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__exp_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__exp_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'exp)", 13), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)exp(s7__exp_0)));
}


/* -------- log -------- */
static s7_pointer s7__log(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__log_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__log_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'log)", 13), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)log(s7__log_0)));
}


/* -------- sqrt -------- */
static s7_pointer s7__sqrt(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__sqrt_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__sqrt_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'sqrt)", 14), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)sqrt(s7__sqrt_0)));
}


/* -------- cos -------- */
static s7_pointer s7__cos(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__cos_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__cos_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'cos)", 13), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)cos(s7__cos_0)));
}


/* -------- sin -------- */
static s7_pointer s7__sin(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__sin_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__sin_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'sin)", 13), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)sin(s7__sin_0)));
}


/* -------- tan -------- */
static s7_pointer s7__tan(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__tan_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__tan_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'tan)", 13), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)tan(s7__tan_0)));
}


/* -------- cosh -------- */
static s7_pointer s7__cosh(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__cosh_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__cosh_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'cosh)", 14), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)cosh(s7__cosh_0)));
}


/* -------- sinh -------- */
static s7_pointer s7__sinh(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__sinh_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__sinh_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'sinh)", 14), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)sinh(s7__sinh_0)));
}


/* -------- tanh -------- */
static s7_pointer s7__tanh(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__tanh_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__tanh_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'tanh)", 14), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)tanh(s7__tanh_0)));
}


/* -------- acos -------- */
static s7_pointer s7__acos(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__acos_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__acos_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'acos)", 14), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)acos(s7__acos_0)));
}


/* -------- asin -------- */
static s7_pointer s7__asin(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__asin_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__asin_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'asin)", 14), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)asin(s7__asin_0)));
}


/* -------- atan -------- */
static s7_pointer s7__atan(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__atan_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__atan_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'atan)", 14), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)atan(s7__atan_0)));
}


/* -------- atan2 -------- */
static s7_pointer s7__atan2(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__atan2_0;
  double s7__atan2_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__atan2_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'atan2)", 15), 1, arg, real_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__atan2_1 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'atan2)", 15), 2, arg, real_string));
  return(s7_make_real(sc, (s7_double)atan2(s7__atan2_0, s7__atan2_1)));
}
static s7_double atan2_d_dd(s7_double x1, s7_double x2) {return(atan2(x1, x2));}


/* -------- acosh -------- */
static s7_pointer s7__acosh(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__acosh_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__acosh_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'acosh)", 15), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)acosh(s7__acosh_0)));
}


/* -------- asinh -------- */
static s7_pointer s7__asinh(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__asinh_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__asinh_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'asinh)", 15), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)asinh(s7__asinh_0)));
}


/* -------- atanh -------- */
static s7_pointer s7__atanh(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  double s7__atanh_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_real(arg))
    s7__atanh_0 = (double)s7_number_to_real_with_caller(sc, arg, __func__);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'atanh)", 15), 0, arg, real_string));
  return(s7_make_real(sc, (s7_double)atanh(s7__atanh_0)));
}


static s7_pointer g_remquo(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_real(s7_car(args)))
    {
      if (s7_is_real(s7_cadr(args)))
        {
          int quo = 0;
          double rem;
          rem = remquo(s7_number_to_real_with_caller(sc, s7_car(args), __func__), s7_number_to_real_with_caller(sc, s7_cadr(args), __func__), &quo);
          return(s7_list(sc, 2, s7_make_real(sc, rem), s7_make_integer(sc, quo)));
        }
      return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'remquo)", 16), 2, s7_cadr(args), real_string));
     }
  return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'remquo)", 16), 1, s7_car(args), real_string));
}
static s7_pointer g_frexp(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_real(s7_car(args)))
    {
      int ex = 0;
      double frac;
      frac = frexp(s7_number_to_real_with_caller(sc, s7_car(args), __func__), &ex);
      return(s7_list(sc, 2, s7_make_real(sc, frac), s7_make_integer(sc, ex)));
     }
  return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'frexp)", 15), 0, s7_car(args), real_string));
}
static s7_pointer g_modf(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_real(s7_car(args)))
    {
      double frac, ip = 0.0;
      frac = modf(s7_number_to_real_with_caller(sc, s7_car(args), __func__), &ip);
      return(s7_list(sc, 2, s7_make_real(sc, frac), s7_make_real(sc, ip)));
     }
  return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libm* 'modf)", 14), 0, s7_car(args), real_string));
}

void libm_s7_init(s7_scheme *sc);
void libm_s7_init(s7_scheme *sc)
{
  s7_pointer cur_env;
  s7_pointer pl_ds, pl_ir, pcl_dr, pl_dr, pl_dri, pl_dir;
  {
    s7_pointer s, d, r, i;
    s = s7_make_symbol(sc, "string?");
    d = s7_make_symbol(sc, "float?");
    r = s7_make_symbol(sc, "real?");
    i = s7_make_symbol(sc, "integer?");

    pl_ds = s7_make_signature(sc, 2, d, s);
    pl_ir = s7_make_signature(sc, 2, i, r);
    pcl_dr = s7_make_circular_signature(sc, 1, 2, d, r);
    pl_dr = s7_make_signature(sc, 2, d, r);
    pl_dri = s7_make_signature(sc, 3, d, r, i);
    pl_dir = s7_make_signature(sc, 3, d, i, r);
  }

  string_string = s7_make_semipermanent_string(sc, "a string");
  c_pointer_string = s7_make_semipermanent_string(sc, "a c-pointer");
  character_string = s7_make_semipermanent_string(sc, "a character");
  boolean_string = s7_make_semipermanent_string(sc, "a boolean");
  real_string = s7_make_semipermanent_string(sc, "a real");
  complex_string = s7_make_semipermanent_string(sc, "a complex number");
  integer_string = s7_make_semipermanent_string(sc, "an integer");
  cur_env = s7_curlet(sc);

  s7_define(sc, cur_env, s7_make_symbol(sc, "M_SQRT1_2"), s7_make_real(sc, (s7_double)M_SQRT1_2));
  s7_define(sc, cur_env, s7_make_symbol(sc, "M_SQRT2"), s7_make_real(sc, (s7_double)M_SQRT2));
  s7_define(sc, cur_env, s7_make_symbol(sc, "M_2_SQRTPI"), s7_make_real(sc, (s7_double)M_2_SQRTPI));
  s7_define(sc, cur_env, s7_make_symbol(sc, "M_2_PI"), s7_make_real(sc, (s7_double)M_2_PI));
  s7_define(sc, cur_env, s7_make_symbol(sc, "M_1_PI"), s7_make_real(sc, (s7_double)M_1_PI));
  s7_define(sc, cur_env, s7_make_symbol(sc, "M_PI_4"), s7_make_real(sc, (s7_double)M_PI_4));
  s7_define(sc, cur_env, s7_make_symbol(sc, "M_PI_2"), s7_make_real(sc, (s7_double)M_PI_2));
  s7_define(sc, cur_env, s7_make_symbol(sc, "M_PI"), s7_make_real(sc, (s7_double)M_PI));
  s7_define(sc, cur_env, s7_make_symbol(sc, "M_LN10"), s7_make_real(sc, (s7_double)M_LN10));
  s7_define(sc, cur_env, s7_make_symbol(sc, "M_LN2"), s7_make_real(sc, (s7_double)M_LN2));
  s7_define(sc, cur_env, s7_make_symbol(sc, "M_LOG10E"), s7_make_real(sc, (s7_double)M_LOG10E));
  s7_define(sc, cur_env, s7_make_symbol(sc, "M_LOG2E"), s7_make_real(sc, (s7_double)M_LOG2E));
  s7_define(sc, cur_env, s7_make_symbol(sc, "M_E"), s7_make_real(sc, (s7_double)M_E));
  s7_define(sc, cur_env, s7_make_symbol(sc, "FP_NORMAL"), s7_make_integer(sc, (s7_int)FP_NORMAL));
  s7_define(sc, cur_env, s7_make_symbol(sc, "FP_SUBNORMAL"), s7_make_integer(sc, (s7_int)FP_SUBNORMAL));
  s7_define(sc, cur_env, s7_make_symbol(sc, "FP_ZERO"), s7_make_integer(sc, (s7_int)FP_ZERO));
  s7_define(sc, cur_env, s7_make_symbol(sc, "FP_INFINITE"), s7_make_integer(sc, (s7_int)FP_INFINITE));
  s7_define(sc, cur_env, s7_make_symbol(sc, "FP_NAN"), s7_make_integer(sc, (s7_int)FP_NAN));

#ifdef __LDBL_DENORM_MIN__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__LDBL_DENORM_MIN__"), s7_make_real(sc, (s7_double)__LDBL_DENORM_MIN__));
#endif
#ifdef __FLT_MAX__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__FLT_MAX__"), s7_make_real(sc, (s7_double)__FLT_MAX__));
#endif
#ifdef __FLT_DENORM_MIN__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__FLT_DENORM_MIN__"), s7_make_real(sc, (s7_double)__FLT_DENORM_MIN__));
#endif
#ifdef __DBL_EPSILON__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__DBL_EPSILON__"), s7_make_real(sc, (s7_double)__DBL_EPSILON__));
#endif
#ifdef __LDBL_EPSILON__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__LDBL_EPSILON__"), s7_make_real(sc, (s7_double)__LDBL_EPSILON__));
#endif
#ifdef __DBL_MIN__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__DBL_MIN__"), s7_make_real(sc, (s7_double)__DBL_MIN__));
#endif
#ifdef __DBL_MAX__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__DBL_MAX__"), s7_make_real(sc, (s7_double)__DBL_MAX__));
#endif
#ifdef __LDBL_MIN__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__LDBL_MIN__"), s7_make_real(sc, (s7_double)__LDBL_MIN__));
#endif
#ifdef __FLT_EPSILON__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__FLT_EPSILON__"), s7_make_real(sc, (s7_double)__FLT_EPSILON__));
#endif
#ifdef __LDBL_MAX__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__LDBL_MAX__"), s7_make_real(sc, (s7_double)__LDBL_MAX__));
#endif
#ifdef __DBL_DENORM_MIN__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__DBL_DENORM_MIN__"), s7_make_real(sc, (s7_double)__DBL_DENORM_MIN__));
#endif
#ifdef __FLT_MIN__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__FLT_MIN__"), s7_make_real(sc, (s7_double)__FLT_MIN__));
#endif
#ifdef __LDBL_DIG__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__LDBL_DIG__"), s7_make_integer(sc, (s7_int)__LDBL_DIG__));
#endif
#ifdef __LDBL_MIN_10_EXP__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__LDBL_MIN_10_EXP__"), s7_make_integer(sc, (s7_int)__LDBL_MIN_10_EXP__));
#endif
#ifdef __DBL_MAX_10_EXP__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__DBL_MAX_10_EXP__"), s7_make_integer(sc, (s7_int)__DBL_MAX_10_EXP__));
#endif
#ifdef __FLT_MIN_10_EXP__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__FLT_MIN_10_EXP__"), s7_make_integer(sc, (s7_int)__FLT_MIN_10_EXP__));
#endif
#ifdef __INTMAX_MAX__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__INTMAX_MAX__"), s7_make_integer(sc, (s7_int)__INTMAX_MAX__));
#endif
#ifdef __LDBL_MAX_10_EXP__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__LDBL_MAX_10_EXP__"), s7_make_integer(sc, (s7_int)__LDBL_MAX_10_EXP__));
#endif
#ifdef __LDBL_MIN_EXP__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__LDBL_MIN_EXP__"), s7_make_integer(sc, (s7_int)__LDBL_MIN_EXP__));
#endif
#ifdef __DBL_MANT_DIG__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__DBL_MANT_DIG__"), s7_make_integer(sc, (s7_int)__DBL_MANT_DIG__));
#endif
#ifdef __FLT_MAX_EXP__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__FLT_MAX_EXP__"), s7_make_integer(sc, (s7_int)__FLT_MAX_EXP__));
#endif
#ifdef __INT_MAX__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__INT_MAX__"), s7_make_integer(sc, (s7_int)__INT_MAX__));
#endif
#ifdef __FLT_DIG__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__FLT_DIG__"), s7_make_integer(sc, (s7_int)__FLT_DIG__));
#endif
#ifdef __LDBL_MANT_DIG__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__LDBL_MANT_DIG__"), s7_make_integer(sc, (s7_int)__LDBL_MANT_DIG__));
#endif
#ifdef __LONG_MAX__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__LONG_MAX__"), s7_make_integer(sc, (s7_int)__LONG_MAX__));
#endif
#ifdef __FLT_MAX_10_EXP__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__FLT_MAX_10_EXP__"), s7_make_integer(sc, (s7_int)__FLT_MAX_10_EXP__));
#endif
#ifdef __FLT_RADIX__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__FLT_RADIX__"), s7_make_integer(sc, (s7_int)__FLT_RADIX__));
#endif
#ifdef __FLT_MANT_DIG__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__FLT_MANT_DIG__"), s7_make_integer(sc, (s7_int)__FLT_MANT_DIG__));
#endif
#ifdef __FLT_MIN_EXP__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__FLT_MIN_EXP__"), s7_make_integer(sc, (s7_int)__FLT_MIN_EXP__));
#endif
#ifdef __LONG_LONG_MAX__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__LONG_LONG_MAX__"), s7_make_integer(sc, (s7_int)__LONG_LONG_MAX__));
#endif
#ifdef __DBL_MAX_EXP__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__DBL_MAX_EXP__"), s7_make_integer(sc, (s7_int)__DBL_MAX_EXP__));
#endif
#ifdef __BIGGEST_ALIGNMENT__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__BIGGEST_ALIGNMENT__"), s7_make_integer(sc, (s7_int)__BIGGEST_ALIGNMENT__));
#endif
#ifdef __DECIMAL_DIG__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__DECIMAL_DIG__"), s7_make_integer(sc, (s7_int)__DECIMAL_DIG__));
#endif
#ifdef __DBL_DIG__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__DBL_DIG__"), s7_make_integer(sc, (s7_int)__DBL_DIG__));
#endif
#ifdef __LDBL_MAX_EXP__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__LDBL_MAX_EXP__"), s7_make_integer(sc, (s7_int)__LDBL_MAX_EXP__));
#endif
#ifdef __DBL_MIN_10_EXP__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__DBL_MIN_10_EXP__"), s7_make_integer(sc, (s7_int)__DBL_MIN_10_EXP__));
#endif
#ifdef __DBL_MIN_EXP__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__DBL_MIN_EXP__"), s7_make_integer(sc, (s7_int)__DBL_MIN_EXP__));
#endif
#ifdef __CHAR_BIT__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__CHAR_BIT__"), s7_make_integer(sc, (s7_int)__CHAR_BIT__));
#endif
#ifdef __SIZEOF_DOUBLE__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__SIZEOF_DOUBLE__"), s7_make_integer(sc, (s7_int)__SIZEOF_DOUBLE__));
#endif
#ifdef __SIZEOF_SHORT__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__SIZEOF_SHORT__"), s7_make_integer(sc, (s7_int)__SIZEOF_SHORT__));
#endif
#ifdef __SIZEOF_FLOAT__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__SIZEOF_FLOAT__"), s7_make_integer(sc, (s7_int)__SIZEOF_FLOAT__));
#endif
#ifdef __SIZEOF_SIZE_T__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__SIZEOF_SIZE_T__"), s7_make_integer(sc, (s7_int)__SIZEOF_SIZE_T__));
#endif
#ifdef __SIZEOF_LONG_DOUBLE__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__SIZEOF_LONG_DOUBLE__"), s7_make_integer(sc, (s7_int)__SIZEOF_LONG_DOUBLE__));
#endif
#ifdef __SIZEOF_LONG__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__SIZEOF_LONG__"), s7_make_integer(sc, (s7_int)__SIZEOF_LONG__));
#endif
#ifdef __SIZEOF_POINTER__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__SIZEOF_POINTER__"), s7_make_integer(sc, (s7_int)__SIZEOF_POINTER__));
#endif
#ifdef __SIZEOF_INT__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__SIZEOF_INT__"), s7_make_integer(sc, (s7_int)__SIZEOF_INT__));
#endif
#ifdef __SIZEOF_LONG_LONG__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__SIZEOF_LONG_LONG__"), s7_make_integer(sc, (s7_int)__SIZEOF_LONG_LONG__));
#endif
#ifdef __VERSION__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__VERSION__"), s7_make_string(sc, (char*)__VERSION__));
#endif

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "modf"),
            s7_make_typed_function(sc, "modf", g_modf, 1, 0, false, "(modf x) returns a list: (int-part frac-part) -- this is not the same as fmod!", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "frexp"),
            s7_make_typed_function(sc, "frexp", g_frexp, 1, 0, false, "(frexp x) returns a list: (fraction exponent)", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "remquo"),
            s7_make_typed_function(sc, "remquo", g_remquo, 2, 0, false, "(remquo x y) returns a list: (remainder messed-up-quotient)", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "atanh"),
            s7_make_typed_function(sc, "atanh", s7__atanh, 1, 0, false, "double atanh(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "asinh"),
            s7_make_typed_function(sc, "asinh", s7__asinh, 1, 0, false, "double asinh(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "acosh"),
            s7_make_typed_function(sc, "acosh", s7__acosh, 1, 0, false, "double acosh(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "atan2"),
            s7_make_typed_function(sc, "atan2", s7__atan2, 2, 0, false, "double atan2(double double)", pcl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "atan"),
            s7_make_typed_function(sc, "atan", s7__atan, 1, 0, false, "double atan(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "asin"),
            s7_make_typed_function(sc, "asin", s7__asin, 1, 0, false, "double asin(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "acos"),
            s7_make_typed_function(sc, "acos", s7__acos, 1, 0, false, "double acos(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "tanh"),
            s7_make_typed_function(sc, "tanh", s7__tanh, 1, 0, false, "double tanh(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sinh"),
            s7_make_typed_function(sc, "sinh", s7__sinh, 1, 0, false, "double sinh(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "cosh"),
            s7_make_typed_function(sc, "cosh", s7__cosh, 1, 0, false, "double cosh(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "tan"),
            s7_make_typed_function(sc, "tan", s7__tan, 1, 0, false, "double tan(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sin"),
            s7_make_typed_function(sc, "sin", s7__sin, 1, 0, false, "double sin(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "cos"),
            s7_make_typed_function(sc, "cos", s7__cos, 1, 0, false, "double cos(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sqrt"),
            s7_make_typed_function(sc, "sqrt", s7__sqrt, 1, 0, false, "double sqrt(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "log"),
            s7_make_typed_function(sc, "log", s7__log, 1, 0, false, "double log(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "exp"),
            s7_make_typed_function(sc, "exp", s7__exp, 1, 0, false, "double exp(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "remainder"),
            s7_make_typed_function(sc, "remainder", s7__remainder, 2, 0, false, "double remainder(double double)", pcl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "round"),
            s7_make_typed_function(sc, "round", s7__round, 1, 0, false, "double round(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "floor"),
            s7_make_typed_function(sc, "floor", s7__floor, 1, 0, false, "double floor(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "signbit"),
            s7_make_typed_function(sc, "signbit", s7__signbit, 1, 0, false, "int signbit(double)", pl_ir));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "isnormal"),
            s7_make_typed_function(sc, "isnormal", s7__isnormal, 1, 0, false, "int isnormal(double)", pl_ir));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "isnan"),
            s7_make_typed_function(sc, "isnan", s7__isnan, 1, 0, false, "int isnan(double)", pl_ir));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "isinf"),
            s7_make_typed_function(sc, "isinf", s7__isinf, 1, 0, false, "int isinf(double)", pl_ir));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "isfinite"),
            s7_make_typed_function(sc, "isfinite", s7__isfinite, 1, 0, false, "int isfinite(double)", pl_ir));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fpclassify"),
            s7_make_typed_function(sc, "fpclassify", s7__fpclassify, 1, 0, false, "int fpclassify(double)", pl_ir));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "nexttoward"),
            s7_make_typed_function(sc, "nexttoward", s7__nexttoward, 2, 0, false, "double nexttoward(double double)", pcl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "nextafter"),
            s7_make_typed_function(sc, "nextafter", s7__nextafter, 2, 0, false, "double nextafter(double double)", pcl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "nan"),
            s7_make_typed_function(sc, "nan", s7__nan, 1, 0, false, "double nan(char*)", pl_ds));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "copysign"),
            s7_make_typed_function(sc, "copysign", s7__copysign, 2, 0, false, "double copysign(double double)", pcl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "tgamma"),
            s7_make_typed_function(sc, "tgamma", s7__tgamma, 1, 0, false, "double tgamma(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fdim"),
            s7_make_typed_function(sc, "fdim", s7__fdim, 2, 0, false, "double fdim(double double)", pcl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "pow"),
            s7_make_typed_function(sc, "pow", s7__pow, 2, 0, false, "double pow(double double)", pcl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "hypot"),
            s7_make_typed_function(sc, "hypot", s7__hypot, 2, 0, false, "double hypot(double double)", pcl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "cbrt"),
            s7_make_typed_function(sc, "cbrt", s7__cbrt, 1, 0, false, "double cbrt(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ilogb"),
            s7_make_typed_function(sc, "ilogb", s7__ilogb, 1, 0, false, "int ilogb(double)", pl_ir));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "log2"),
            s7_make_typed_function(sc, "log2", s7__log2, 1, 0, false, "double log2(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "log1p"),
            s7_make_typed_function(sc, "log1p", s7__log1p, 1, 0, false, "double log1p(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "log10"),
            s7_make_typed_function(sc, "log10", s7__log10, 1, 0, false, "double log10(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "expm1"),
            s7_make_typed_function(sc, "expm1", s7__expm1, 1, 0, false, "double expm1(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "exp2"),
            s7_make_typed_function(sc, "exp2", s7__exp2, 1, 0, false, "double exp2(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "scalbn"),
            s7_make_typed_function(sc, "scalbn", s7__scalbn, 2, 0, false, "double scalbn(double int)", pl_dri));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ldexp"),
            s7_make_typed_function(sc, "ldexp", s7__ldexp, 2, 0, false, "double ldexp(double int)", pl_dri));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fmod"),
            s7_make_typed_function(sc, "fmod", s7__fmod, 2, 0, false, "double fmod(double double)", pcl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "trunc"),
            s7_make_typed_function(sc, "trunc", s7__trunc, 1, 0, false, "double trunc(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "llround"),
            s7_make_typed_function(sc, "llround", s7__llround, 1, 0, false, "int llround(double)", pl_ir));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "llrint"),
            s7_make_typed_function(sc, "llrint", s7__llrint, 1, 0, false, "int llrint(double)", pl_ir));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rint"),
            s7_make_typed_function(sc, "rint", s7__rint, 1, 0, false, "double rint(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fma"),
            s7_make_typed_function(sc, "fma", s7__fma, 3, 0, false, "double fma(double double double)", pcl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "scalbln"),
            s7_make_typed_function(sc, "scalbln", s7__scalbln, 2, 0, false, "double scalbln(double int)", pl_dri));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "nearbyint"),
            s7_make_typed_function(sc, "nearbyint", s7__nearbyint, 1, 0, false, "double nearbyint(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ceil"),
            s7_make_typed_function(sc, "ceil", s7__ceil, 1, 0, false, "double ceil(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fabs"),
            s7_make_typed_function(sc, "fabs", s7__fabs, 1, 0, false, "double fabs(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "lgamma"),
            s7_make_typed_function(sc, "lgamma", s7__lgamma, 1, 0, false, "double lgamma(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "erfc"),
            s7_make_typed_function(sc, "erfc", s7__erfc, 1, 0, false, "double erfc(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "erf"),
            s7_make_typed_function(sc, "erf", s7__erf, 1, 0, false, "double erf(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "jn"),
            s7_make_typed_function(sc, "jn", s7__jn, 2, 0, false, "double jn(int double)", pl_dir));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "j1"),
            s7_make_typed_function(sc, "j1", s7__j1, 1, 0, false, "double j1(double)", pl_dr));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "j0"),
            s7_make_typed_function(sc, "j0", s7__j0, 1, 0, false, "Bessel j0", pl_dr));

  /* double optimizer connections */
  s7_set_d_dd_function(sc, s7_name_to_value(sc, "atan2"), atan2_d_dd);
  s7_set_d_dd_function(sc, s7_name_to_value(sc, "nexttoward"), nexttoward_d_dd);
  s7_set_d_dd_function(sc, s7_name_to_value(sc, "nextafter"), nextafter_d_dd);
  s7_set_d_dd_function(sc, s7_name_to_value(sc, "copysign"), copysign_d_dd);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "tgamma"), tgamma_d_d);
  s7_set_d_dd_function(sc, s7_name_to_value(sc, "fdim"), fdim_d_dd);
  s7_set_d_dd_function(sc, s7_name_to_value(sc, "pow"), pow_d_dd);
  s7_set_d_dd_function(sc, s7_name_to_value(sc, "hypot"), hypot_d_dd);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "cbrt"), cbrt_d_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "log2"), log2_d_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "log1p"), log1p_d_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "log10"), log10_d_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "expm1"), expm1_d_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "exp2"), exp2_d_d);
  s7_set_d_dd_function(sc, s7_name_to_value(sc, "fmod"), fmod_d_dd);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "trunc"), trunc_d_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "rint"), rint_d_d);
  s7_set_d_ddd_function(sc, s7_name_to_value(sc, "fma"), fma_d_ddd);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "nearbyint"), nearbyint_d_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "ceil"), ceil_d_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "fabs"), fabs_d_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "lgamma"), lgamma_d_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "erfc"), erfc_d_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "erf"), erf_d_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "j1"), j1_d_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, "j0"), j0_d_d);

  /* int optimizer connections */
  s7_set_i_7d_function(sc, s7_name_to_value(sc, "signbit"), signbit_i_7d);
  s7_set_i_7d_function(sc, s7_name_to_value(sc, "isnormal"), isnormal_i_7d);
  s7_set_i_7d_function(sc, s7_name_to_value(sc, "isnan"), isnan_i_7d);
  s7_set_i_7d_function(sc, s7_name_to_value(sc, "isinf"), isinf_i_7d);
  s7_set_i_7d_function(sc, s7_name_to_value(sc, "isfinite"), isfinite_i_7d);
  s7_set_i_7d_function(sc, s7_name_to_value(sc, "fpclassify"), fpclassify_i_7d);
  s7_set_i_7d_function(sc, s7_name_to_value(sc, "ilogb"), ilogb_i_7d);
  s7_set_i_7d_function(sc, s7_name_to_value(sc, "llround"), llround_i_7d);
  s7_set_i_7d_function(sc, s7_name_to_value(sc, "llrint"), llrint_i_7d);

  /* double-int optimizer connections */
  s7_set_d_id_function(sc, s7_name_to_value(sc, "jn"), jn_d_id);
}
