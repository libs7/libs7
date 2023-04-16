/* s7 ffi tester
 *
 * gcc -o ffitest ffitest.c -g3 -Wall s7.o -lm -I. -ldl -Wl,-export-dynamic
 * gcc -o ffitest ffitest.c -g3 -Wall s7.o -DWITH_GMP -lgmp -lmpfr -lmpc -lm -I. -ldl -Wl,-export-dynamic
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>

#if WITH_GMP
#include <gmp.h>
#include <mpfr.h>
#include <mpc.h>
#endif

#include "s7.h"

#define ld64 PRId64

#ifndef WITH_GMP
  #define WITH_GMP 0
#endif

#define ld64 PRId64

#define TO_STR(x) s7_object_to_c_string(sc, x)
#define TO_S7_INT(x) s7_make_integer(sc, x)

static s7_pointer a_function(s7_scheme *sc, s7_pointer args)
{
  return(s7_car(args));
}

static s7_pointer test_hook_function(s7_scheme *sc, s7_pointer args)
{
  s7_pointer val;
  val = s7_symbol_local_value(sc, s7_make_symbol(sc, "a"), s7_car(args));
  if ((!s7_is_integer(val)) ||
      (s7_integer(val) != 1))
    {
      char *s1;
      s1 = TO_STR(val);
      fprintf(stderr, "%d: (hook 'a) is %s\n", __LINE__, s1);
      free(s1);
    }
  return(val);
}

static char last_c;
static void my_print(s7_scheme *sc, unsigned char c, s7_pointer port)
{
  last_c = c;
}

static s7_pointer my_read(s7_scheme *sc, s7_read_t peek, s7_pointer port)
{
  return(s7_make_character(sc, '0'));
}

static s7_pointer my_error(s7_scheme *sc, s7_pointer args)
{
  s7_error(sc, s7_make_symbol(sc, "my-error"), s7_list(sc, 1, s7_make_integer(sc, 60)));
  return(s7_f(sc));
}

static s7_pointer my_no_error(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, 30));
}

static s7_pointer my_error_handler(s7_scheme *sc, s7_pointer args)
{
  return(s7_car(s7_cadr(args)));
}

static bool tested_begin_hook = false;
static void test_begin_hook(s7_scheme *sc, bool *val)
{
  tested_begin_hook = true;
}

static s7_pointer test_error_handler(s7_scheme *sc, s7_pointer args)
{
  s7_display(sc, s7_make_symbol(sc, "error!"), s7_current_error_port(sc));
  return(s7_f(sc));
}

static s7_pointer set_sym, set_val;
static s7_pointer scheme_set_notification(s7_scheme *sc, s7_pointer args)
{
  set_sym = s7_car(args);
  set_val = s7_cadr(args);
  return(set_val);
}


typedef struct {
  s7_double x;
  s7_pointer data;
} dax;

static s7_int dax_type_tag = 0, dx_type_tag = 0;

static s7_pointer dax_to_string(s7_scheme *sc, s7_pointer args)
{
  char *data_str, *str;
  s7_pointer result;
  int data_str_len;
  dax *o = (dax *)s7_c_object_value(s7_car(args));
  data_str = s7_object_to_c_string(sc, o->data);
  data_str_len = strlen(data_str);
  str = (char *)calloc(data_str_len + 32, sizeof(char));
  snprintf(str, data_str_len + 32, "#<dax %.3f %s>", o->x, data_str);
  free(data_str);
  result = s7_make_string(sc, str);
  free(str);
  return(result);
}

static void free_dax(void *val)
{
  if (val) free(val);
}

static void mark_dax(void *val)
{
  dax *o = (dax *)val;
  if (o) s7_mark(o->data);
}

static s7_pointer make_dax(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)malloc(sizeof(dax));
  o->x = s7_real(s7_car(args));
  if (s7_cdr(args) != s7_nil(sc))
    o->data = s7_car(s7_cdr(args));
  else o->data = s7_nil(sc);
  return(s7_make_c_object(sc, dax_type_tag, (void *)o));
}

static s7_pointer is_dax(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc,
			 s7_is_c_object(s7_car(args)) &&
			 s7_c_object_type(s7_car(args)) == dax_type_tag));
}

static s7_pointer dax_x(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)s7_c_object_value(s7_car(args));
  return(s7_make_real(sc, o->x));
}

static s7_pointer set_dax_x(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)s7_c_object_value(s7_car(args));
  o->x = s7_real(s7_car(s7_cdr(args)));
  return(s7_car(s7_cdr(args)));
}

static s7_pointer dax_data(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)s7_c_object_value(s7_car(args));
  return(o->data);
}

static s7_pointer set_dax_data(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)s7_c_object_value(s7_car(args));
  o->data = s7_car(s7_cdr(args));
  return(o->data);
}

static bool equal_dax(void *val1, void *val2) /* this is the old form of equal? */
{
  dax *d1, *d2;
  if (val1 == val2)
    return(true);
  d1 = (dax *)val1;
  d2 = (dax *)val2;
  return((d1->x == d2->x) &&
	 (d1->data == d2->data));  /* we want s7_is_equal here, but the interpreter is not passed to us */
}

static s7_pointer equality_dax(s7_scheme *sc, s7_pointer args) /* this is the new form of equal? */
{
  s7_pointer p1, p2;
  dax *d1, *d2;
  p1 = s7_car(args);      /* we know this is a dax object */
  p2 = s7_cadr(args);
  if (p1 == p2)
    return(s7_t(sc));
  if ((!s7_is_c_object(p2)) ||
      (s7_c_object_type(p2) != dax_type_tag))
    return(s7_f(sc));
  d1 = (dax *)s7_c_object_value(p1);
  d2 = (dax *)s7_c_object_value(p2);
  return(s7_make_boolean(sc,       /* here we can call s7_is_equal */
			 (d1->x == d2->x) &&
			 (s7_is_equal(sc, d1->data, d2->data))));
}


static s7_pointer free_dx(s7_scheme *sc, s7_pointer obj)
{
  free(s7_c_object_value(obj));
  return(NULL);
}

static s7_pointer mark_dx(s7_scheme *sc, s7_pointer obj)
{
  dax *o = (dax *)s7_c_object_value(obj);
  if (o) s7_mark(o->data);
  return(NULL);
}

static s7_pointer dx_to_list(s7_scheme *sc, s7_pointer obj)
{
  return(s7_nil(sc));
}


static s7_pointer plus(s7_scheme *sc, s7_pointer args)
{
  /* (define* (plus (red 32) blue) (+ (* 2 red) blue)) */
  return(TO_S7_INT(2 * s7_integer(s7_car(args)) + s7_integer(s7_car(s7_cdr(args)))));
}

static s7_pointer plus1(s7_scheme *sc, s7_pointer args) /* check recursion in "unsafe" case */
{
  s7_pointer d;
  if (s7_integer(s7_car(args)) == 4)
    d = s7_apply_function_star(sc, s7_name_to_value(sc, "plus1"),
			       s7_list(sc, 3,
				       s7_make_integer(sc, 1),
				       s7_make_integer(sc, 2),
				       s7_make_integer(sc, 3)));
  else d = s7_make_integer(sc, 0);
  return(s7_make_integer(sc, s7_integer(s7_car(args)) +
			 s7_integer(s7_cadr(args)) +
			 s7_integer(s7_caddr(args)) +
			 s7_integer(d)));
}

s7_pointer fs1(s7_scheme* sc, s7_pointer args) {if (s7_is_pair(args)) return(s7_car(args)); return s7_nil(sc);}
s7_pointer fs2(s7_scheme* sc, s7_pointer args) {if (s7_is_pair(args)) return(s7_car(args)); return s7_nil(sc);}
s7_pointer fs3(s7_scheme* sc, s7_pointer args) {if (s7_is_pair(args)) return(s7_car(args)); return s7_nil(sc);}
s7_pointer fs31(s7_scheme* sc, s7_pointer args) {if (s7_is_pair(args)) return(s7_car(args)); return s7_nil(sc);}

s7_pointer fs4(s7_scheme* sc, s7_pointer args) {if (s7_is_pair(args)) return(s7_car(args)); return s7_nil(sc);}
s7_pointer fs5(s7_scheme* sc, s7_pointer args) {if (s7_is_pair(args)) return(s7_car(args)); return s7_nil(sc);}
s7_pointer fs6(s7_scheme* sc, s7_pointer args) {if (s7_is_pair(args)) return(s7_car(args)); return s7_nil(sc);}
s7_pointer fs61(s7_scheme* sc, s7_pointer args) {if (s7_is_pair(args)) return(s7_car(args)); return s7_nil(sc);}

static s7_pointer mac_plus(s7_scheme *sc, s7_pointer args)
{
  /* (define-macro (plus a b) `(+ ,a ,b)) */
  s7_pointer a, b;
  a = s7_car(args);
  b = s7_cadr(args);
  return(s7_list(sc, 3, s7_make_symbol(sc, "+"),  a, b));
}

static s7_pointer mac_plus_mv(s7_scheme *sc, s7_pointer args)
{
  /* (define-macro (plus-mv a b) (values `(define a ,a) `(define b ,b))) */
  return(s7_values(sc, args));
}

static s7_pointer open_plus(s7_scheme *sc, s7_pointer args)
{
  #define plus_help "(plus obj ...) applies obj's plus method to obj and any trailing arguments."
  s7_pointer obj, method;
  obj = s7_car(args);
  if (s7_is_openlet(obj))
    {
      method = s7_method(sc, obj, s7_make_symbol(sc, "plus"));
      if (s7_is_procedure(method))
	return(s7_apply_function(sc, method, s7_cdr(args)));
    }
  return(s7_f(sc));
}

static s7_pointer g_car(s7_scheme *sc, s7_pointer args) {return(s7_car(s7_car(args)));}


typedef struct {
  size_t size;
  double *data;
} g_block;

static s7_int g_block_type = 0;
static s7_pointer g_block_methods;

static s7_pointer g_make_block(s7_scheme *sc, s7_pointer args)
{
  #define g_make_block_help "(make-block size) returns a new block of the given size"
  g_block *g;
  s7_pointer new_g;
  g = (g_block *)calloc(1, sizeof(g_block));
  g->size = (size_t)s7_integer(s7_car(args));
  g->data = (double *)calloc(g->size, sizeof(double));
  new_g = s7_make_c_object(sc, g_block_type, (void *)g);
  s7_c_object_set_let(sc, new_g, g_block_methods);
  s7_openlet(sc, new_g);
  return(new_g);
}

static s7_pointer g_to_block(s7_scheme *sc, s7_pointer args)
{
#define g_block_help "(block ...) returns a block c_object with the arguments as its contents."
  s7_pointer p, b;
  size_t i, len;
  g_block *gb;
  len = s7_list_length(sc, args);
  b = g_make_block(sc, s7_cons(sc, s7_make_integer(sc, len), s7_nil(sc)));
  gb = (g_block *)s7_c_object_value(b);
  for (i = 0, p = args; i < len; i++, p = s7_cdr(p))
    gb->data[i] = s7_number_to_real(sc, s7_car(p));
  return(b);
}

static s7_pointer g_block_to_string(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, "<block>"));
}

static void g_block_free(void *value)
{
  g_block *g = (g_block *)value;
  free(g->data);
  free(g);
}

static bool g_blocks_are_eql(void *val1, void *val2)
{
  s7_int i, len;
  g_block *b1 = (g_block *)val1;
  g_block *b2 = (g_block *)val2;
  if (val1 == val2) return(true);
  len = b1->size;
  if (len != b2->size) return(false);
  for (i = 0; i < len; i++)
    if (b1->data[i] != b2->data[i])
      return(false);
  return(true);
}

static s7_pointer g_blocks_are_equal(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, g_blocks_are_eql((void *)s7_c_object_value(s7_car(args)), (void *)s7_c_object_value(s7_cadr(args)))));
}

static s7_pointer g_blocks_are_equivalent(s7_scheme *sc, s7_pointer args)
{
  s7_pointer v1, v2, arg1, arg2;
  g_block *g1, *g2;
  bool result;
  uint32_t gc1, gc2;
  size_t len;
  arg1 = s7_car(args);
  arg2 = s7_cadr(args);
  if (!s7_is_c_object(arg2))
    return(s7_f(sc));
  if (arg1 == arg2)
    return(s7_make_boolean(sc, true));
  if (s7_is_let(arg1))             /* (block-let (block)) */
    return(s7_make_boolean(sc, false));    /* checked == above */
  g1 = (g_block *)s7_c_object_value(arg1);
  if (s7_c_object_type(arg2) != g_block_type)
    return(s7_make_boolean(sc, false));
  g2 = (g_block *)s7_c_object_value(arg2);
  len = g1->size;
  if (len != g2->size)
    return(s7_make_boolean(sc, false));
  v1 = s7_make_float_vector_wrapper(sc, len, g1->data, 1, NULL, false);
  gc1 = s7_gc_protect(sc, v1);
  v2 = s7_make_float_vector_wrapper(sc, len, g2->data, 1, NULL, false);
  gc2 = s7_gc_protect(sc, v2);
  result = s7_is_equivalent(sc, v1, v2);
  s7_gc_unprotect_at(sc, gc1);
  s7_gc_unprotect_at(sc, gc2);
  return(s7_make_boolean(sc, result));
}

static void g_block_mark(void *val)
{
  /* nothing to mark */
}

static s7_pointer g_block_ref(s7_scheme *sc, s7_pointer args)
{
  g_block *g;
  size_t index;
  g = (g_block *)s7_c_object_value(s7_car(args));
  index = (size_t)s7_integer(s7_cadr(args));
  if (index < g->size)
    return(s7_make_real(sc, g->data[index]));
  return(s7_out_of_range_error(sc, "block-ref", 2, s7_cadr(args), "should be less than block length"));
}

static s7_pointer g_block_set(s7_scheme *sc, s7_pointer args)
{
  g_block *g;
  s7_int index;
  if (s7_list_length(sc, args) != 3)
    return(s7_wrong_number_of_args_error(sc, "block-set! takes 3 arguments: ~S", args));
  g = (g_block *)s7_c_object_value(s7_car(args));
  index = s7_integer(s7_cadr(args));
  if ((index >= 0) && (index < g->size))
    {
      g->data[index] = s7_number_to_real(sc, s7_caddr(args));
      return(s7_caddr(args));
    }
  return(s7_out_of_range_error(sc, "block-set", 2, s7_cadr(args), "should be less than block length"));
}

static s7_pointer g_block_length(s7_scheme *sc, s7_pointer args)
{
  g_block *g = (g_block *)s7_c_object_value(s7_car(args));
  return(s7_make_integer(sc, g->size));
}

static s7_pointer g_block_copy(s7_scheme *sc, s7_pointer args)
{
  s7_pointer obj, new_g;
  g_block *g, *g1;
  obj = s7_car(args);
  g = (g_block *)s7_c_object_value(obj);
  new_g = g_make_block(sc, s7_cons(sc, s7_make_integer(sc, g->size), s7_nil(sc)));
  g1 = (g_block *)s7_c_object_value(new_g);
  memcpy((void *)(g1->data), (void *)(g->data), g->size * sizeof(double));
  return(new_g);
}

static s7_pointer g_block_reverse(s7_scheme *sc, s7_pointer args)
{
  size_t i, j;
  s7_pointer obj, new_g;
  g_block *g, *g1;
  obj = s7_car(args);
  g = (g_block *)s7_c_object_value(obj);
  new_g = g_make_block(sc, s7_cons(sc, s7_make_integer(sc, g->size), s7_nil(sc)));
  g1 = (g_block *)s7_c_object_value(new_g);
  for (i = 0, j = g->size - 1; i < g->size; i++, j--)
    g1->data[i] = g->data[j];
  return(new_g);
}

static s7_pointer g_block_fill(s7_scheme *sc, s7_pointer args)
{
  s7_pointer obj;
  size_t i;
  double fill_val;
  g_block *g;
  obj = s7_car(args);
  g = (g_block *)s7_c_object_value(obj);
  fill_val = s7_number_to_real(sc, s7_cadr(args));
  for (i = 0; i < g->size; i++)
    g->data[i] = fill_val;
  return(obj);
}

static bool symbol_func(const char *symbol_name, void *data)
{
  return(false);
}

static bool symbol_func_1(const char *symbol_name, void *data)
{
  return(false);
}

static s7_scheme *cur_sc;

static const char *pretty_print(s7_scheme *sc, s7_pointer obj) /* (pretty-print obj) */
{
  return(s7_string(
          s7_eval_c_string_with_environment(sc,
            "(catch #t                                \
               (lambda ()                             \
                 (unless (defined? 'pp)		      \
                   (load \"write.scm\"))	      \
                 (pp obj))			      \
               (lambda (type info)		      \
                 (apply format #f info)))",
	   s7_inlet(sc, s7_list(sc, 1, s7_cons(sc, s7_make_symbol(sc, "obj"), obj))))));
}

#if WITH_GMP
static s7_pointer big_add_1(s7_scheme *sc, s7_pointer args)
{
  /* add 1 to either a normal number or a bignum */
  s7_pointer x, n;
  x = s7_car(args);
  if (s7_is_big_integer(x))
    {
      mpz_t big_n;
      mpz_init_set(big_n, *s7_big_integer(x));
      mpz_add_ui(big_n, big_n, 1);
      n = s7_make_big_integer(sc, &big_n);
      mpz_clear(big_n);
      return(n);
    }
  if (s7_is_big_ratio(x))
    {
      mpq_t big_q;
      mpq_init(big_q);
      mpq_set_si(big_q, 1, 1);
      mpq_add(big_q, *s7_big_ratio(x), big_q);
      mpq_canonicalize(big_q);
      n = s7_make_big_ratio(sc, &big_q);
      mpq_clear(big_q);
      return(n);
    }
  if (s7_is_big_real(x))
    {
      mpfr_t big_x;
      mpfr_init_set(big_x, *s7_big_real(x), MPFR_RNDN);
      mpfr_add_ui(big_x, big_x, 1, MPFR_RNDN);
      n = s7_make_big_real(sc, &big_x);
      mpfr_clear(big_x);
      return(n);
    }
  if (s7_is_big_complex(x))
    {
      mpc_t big_z;
      mpc_init2(big_z, mpc_get_prec(*s7_big_complex(x)));
      mpc_add_ui(big_z, *s7_big_complex(x), 1, MPC_RNDNN);
      n = s7_make_big_complex(sc, &big_z);
      mpc_clear(big_z);
      return(n);
    }
  if (s7_is_integer(x))
    return(s7_make_integer(sc, 1 + s7_integer(x)));
  if (s7_is_rational(x))
    return(s7_make_ratio(sc, s7_numerator(x) + s7_denominator(x), s7_denominator(x)));
  if (s7_is_real(x))
    return(s7_make_real(sc, 1.0 + s7_real(x)));
  if (s7_is_complex(x))
    return(s7_make_complex(sc, 1.0 + s7_real_part(x), s7_imag_part(x)));
  return(s7_wrong_type_arg_error(sc, "add-1", 0, x, "a number"));
}
#endif

static const char *snd_names[8] = {
    "*clm-array-print-length*", "ws.scm",
    "*clm-channels*", "ws.scm",
    "zone-tailed-hawk", "animals.scm",
    "zoom-spectrum", "examp.scm",
};


static s7_double opt_d_func(void) {return(1.0);}
static s7_pointer g_d_func(s7_scheme *sc, s7_pointer args) {return(s7_make_real(sc, 1.0));}

static s7_double opt_d_d_func(s7_double x) {return(x + 1.0);}
static s7_pointer g_d_d_func(s7_scheme *sc, s7_pointer args) {return(s7_make_real(sc, s7_real(s7_car(args)) + 1.0));}

static s7_double opt_d_dd_func(s7_double x, s7_double y) {return(x + y);}
static s7_pointer g_d_dd_func(s7_scheme *sc, s7_pointer args) {return(s7_make_real(sc, s7_real(s7_car(args)) + s7_real(s7_cadr(args))));}

static s7_double opt_d_ddd_func(s7_double x, s7_double y, s7_double z) {return(x + y + z);}
static s7_pointer g_d_ddd_func(s7_scheme *sc, s7_pointer args) {return(s7_make_real(sc, s7_real(s7_car(args)) + s7_real(s7_cadr(args)) + s7_real(s7_caddr(args))));}

static s7_double opt_d_dddd_func(s7_double w, s7_double x, s7_double y, s7_double z) {return(w + x + y + z);}
static s7_pointer g_d_dddd_func(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_real(sc, s7_real(s7_car(args)) + s7_real(s7_cadr(args)) + s7_real(s7_caddr(args)) + s7_real(s7_cadddr(args))));
}

static s7_double opt_d_p_func(s7_pointer x) {return(s7_real(x));}
static s7_pointer g_d_p_func(s7_scheme *sc, s7_pointer args) {return(s7_make_real(sc, s7_real(s7_car(args))));}

static s7_int opt_i_i_func(s7_int x) {return(x + 1);}
static s7_pointer g_i_i_func(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, s7_integer(s7_car(args)) + 1));}

static s7_int opt_i_ii_func(s7_int x, s7_int y) {return(x + y);}
static s7_pointer g_i_ii_func(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, s7_integer(s7_car(args)) + s7_integer(s7_cadr(args))));}

static s7_double opt_d_id_func(s7_int x, s7_double y) {return(x + y);}
static s7_pointer g_d_id_func(s7_scheme *sc, s7_pointer args) {return(s7_make_real(sc, s7_integer(s7_car(args)) + s7_real(s7_cadr(args))));}

static s7_double opt_d_ip_func(s7_int x, s7_pointer y) {return(x + s7_real(y));}
static s7_pointer g_d_ip_func(s7_scheme *sc, s7_pointer args) {return(s7_make_real(sc, s7_integer(s7_car(args)) + s7_real(s7_cadr(args))));}

static s7_int opt_i_7d_func(s7_scheme *sc, s7_double x) {return((s7_int)x);}
static s7_pointer g_i_7d_func(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, (s7_int)s7_real(s7_car(args))));}

static s7_int opt_i_7p_func(s7_scheme *sc, s7_pointer x) {return(s7_integer(x));}
static s7_pointer g_i_7p_func(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, s7_integer(s7_car(args))));}

static s7_double opt_d_pd_func(s7_pointer x, s7_double y) {return(s7_real(x) + y);}
static s7_pointer g_d_pd_func(s7_scheme *sc, s7_pointer args) {return(s7_make_real(sc, s7_real(s7_car(args)) + s7_real(s7_cadr(args))));}

static s7_double opt_d_7pi_func(s7_scheme *sc, s7_pointer x, s7_int y) {return(s7_real(x) + y);}
static s7_pointer g_d_7pi_func(s7_scheme *sc, s7_pointer args) {return(s7_make_real(sc, s7_real(s7_car(args)) + s7_integer(s7_cadr(args))));}

static s7_double opt_d_7pid_func(s7_scheme *sc, s7_pointer x, s7_int y, s7_double z) {return(s7_real(x) + y + z);}
static s7_pointer g_d_7pid_func(s7_scheme *sc, s7_pointer args) {return(s7_make_real(sc, s7_real(s7_car(args)) + s7_integer(s7_cadr(args)) + s7_real(s7_caddr(args))));}

static bool opt_b_p_func(s7_pointer x) {return(s7_is_real(x));}
static s7_pointer g_b_p_func(s7_scheme *sc, s7_pointer args) {return(s7_make_boolean(sc, s7_is_real(s7_car(args))));}

static s7_pointer opt_p_d_func(s7_scheme *sc, s7_double x) {return(s7_make_real(sc, x));}
static s7_pointer g_p_d_func(s7_scheme *sc, s7_pointer args) {return(s7_car(args));}

static s7_double opt_d_v_func(void *x) {g_block *g = (g_block *)x; return(g->data[0]);}
static s7_pointer g_d_v_func(s7_scheme *sc, s7_pointer args) {g_block *g; g = (g_block *)s7_c_object_value(s7_car(args)); return(s7_make_real(sc, g->data[0]));}

static s7_double opt_d_vd_func(void *x, s7_double y) {g_block *g = (g_block *)x; return(y + g->data[0]);}
static s7_pointer g_d_vd_func(s7_scheme *sc, s7_pointer args)
{
  g_block *g;
  g = (g_block *)s7_c_object_value(s7_car(args));
  return(s7_make_real(sc, s7_real(s7_cadr(args)) + g->data[0]));
}

static s7_double opt_d_vid_func(void *x, s7_int y, s7_double z) {g_block *g = (g_block *)x; return(z + g->data[y]);}
static s7_pointer g_d_vid_func(s7_scheme *sc, s7_pointer args)
{
  g_block *g;
  g = (g_block *)s7_c_object_value(s7_car(args));
  return(s7_make_real(sc, s7_real(s7_caddr(args)) + g->data[s7_integer(s7_cadr(args))]));
}

static s7_double opt_d_vdd_func(void *x, s7_double y, s7_double z) {g_block *g = (g_block *)x; return(y + z + g->data[0]);}
static s7_pointer g_d_vdd_func(s7_scheme *sc, s7_pointer args)
{
  g_block *g;
  g = (g_block *)s7_c_object_value(s7_car(args));
  return(s7_make_real(sc, s7_real(s7_cadr(args)) + s7_real(s7_caddr(args)) + g->data[0]));
}

/* s7_call_with_catch */
static s7_pointer make_func, catcher1, catcher2;
static s7_pointer ter_bad_func(s7_scheme *sc, s7_pointer args) {s7_eval_c_string(sc, "(/ 10 0)"); return(s7_t(sc));}
static s7_pointer ter_error_handler(s7_scheme *sc, s7_pointer args) {return s7_f(sc);}
static s7_pointer ter1_bad_func(s7_scheme *sc, s7_pointer args) {return(s7_call_with_catch(sc, s7_t(sc), make_func, catcher1));}
static s7_pointer ter1_error_handler(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, 123));}

static s7_pointer ter2_bad_func(s7_scheme *sc, s7_pointer args) {return(s7_wrong_type_error(sc, s7_make_symbol(sc, "ter2"), 0, args, s7_make_string(sc, "oops")));}
static s7_pointer ter2_error_handler(s7_scheme *sc, s7_pointer args) {return(s7_apply_function(sc, s7_name_to_value(sc, "format"), s7_cons(sc, s7_f(sc), s7_cadr(args))));}

/* another s7_call_with_catch case thanks to Woody Douglass */
static int32_t wd_val = 0;
static s7_pointer wd_inner_test(s7_scheme *s, s7_pointer args)
{
  s7_error(s, s7_make_symbol(s, "test-error"), s7_list(s, 1, s7_make_string(s, "TEST ERROR")));
  return s7_nil(s);
}
static s7_pointer wd_test_fn(s7_scheme *s, s7_pointer args)
{
  wd_val = 1;
  s7_call_with_catch(s, s7_t(s), s7_name_to_value(s, "wd-inner-test"), s7_name_to_value(s, "wd-inner-test-handler"));
  wd_val = 2;
  return s7_nil(s);
}
static s7_pointer wd_inner_test_handler(s7_scheme *s, s7_pointer args) {return s7_nil(s);}


static int32_t wd1_val = 0, wd2_val = 0;
static s7_pointer wd1_test_fn(s7_scheme *s, s7_pointer args)
{
  wd1_val = 1;
  s7_call_with_catch(s, s7_t(s), s7_name_to_value(s, "wd1-inner-fn"), s7_name_to_value(s, "wd-inner-test-handler"));
  wd1_val = 2;
  return s7_nil(s);
}
static s7_pointer wd1_inner_fn(s7_scheme *s, s7_pointer args)
{
  wd2_val = 11;
  s7_call_with_catch(s, s7_t(s), s7_name_to_value(s, "wd-inner-test"), s7_name_to_value(s, "wd-inner-test-handler"));
  wd2_val = 12;
  return s7_nil(s);
}



int main(int argc, char **argv)
{
  s7_scheme *sc;
  s7_pointer p, p1;
  s7_int i, gc_loc;
  char *s1, *s2;

  sc = s7_init();
  cur_sc = sc;

  {
    char b1[32];
    snprintf(b1, 32, "%d.%d", S7_MAJOR_VERSION, S7_MINOR_VERSION);
    if (strcmp(b1, S7_VERSION) != 0) fprintf(stderr, "version mismatch: %d.%d != %s\n", S7_MAJOR_VERSION, S7_MINOR_VERSION, S7_VERSION);
    if (strlen(S7_DATE) < 6) fprintf(stderr, "S7_DATE: %s\n", S7_DATE);
  }

  s7_set_history_enabled(sc, false);
  if (s7_history_enabled(sc)) fprintf(stderr, "%d: history is enabled\n", __LINE__);

  /* try each straight (no errors) case */

  if (!s7_is_null(sc, s7_nil(sc)))
    {fprintf(stderr, "%d: %s is not null?\n", __LINE__, s1 = TO_STR(s7_nil(sc))); free(s1);}

  if (s7_is_pair(s7_nil(sc)))
    {fprintf(stderr, "%d: %s is a pair?\n", __LINE__, s1 = TO_STR(s7_nil(sc))); free(s1);}

  if (!s7_is_boolean(s7_t(sc)))
    {fprintf(stderr, "%d: %s is not boolean?\n", __LINE__, s1 = TO_STR(s7_t(sc))); free(s1);}

  if (!s7_is_boolean(s7_f(sc)))
    {fprintf(stderr, "%d: %s is not boolean?\n", __LINE__, s1 = TO_STR(s7_f(sc))); free(s1);}

  if (s7_boolean(sc, s7_f(sc)))
    {fprintf(stderr, "%d: %s is #t?\n", __LINE__, s1 = TO_STR(s7_f(sc))); free(s1);}

  if (!s7_boolean(sc, s7_t(sc)))
    {fprintf(stderr, "%d: %s is #f?\n", __LINE__, s1 = TO_STR(s7_t(sc))); free(s1);}

  p = s7_make_boolean(sc, true);
  if (p != s7_t(sc))
    {fprintf(stderr, "%d: %s is not #t?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_make_boolean(sc, false);
  if (p != s7_f(sc))
    {fprintf(stderr, "%d: %s is not #f?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_eq(s7_f(sc), s7_f(sc)))
    {fprintf(stderr, "%d: (eq? %s %s) -> #f?\n", __LINE__, s1 = TO_STR(s7_f(sc)), s2 = TO_STR(s7_f(sc))); free(s1); free(s2);}

  if (!s7_is_eqv(sc, s7_f(sc), s7_f(sc)))
    {fprintf(stderr, "%d: (eqv? %s %s) -> #f?\n", __LINE__, s1 = TO_STR(s7_f(sc)), s2 = TO_STR(s7_f(sc))); free(s1); free(s2);}

  if (!s7_is_equal(sc, s7_f(sc), s7_f(sc)))
    {fprintf(stderr, "%d: (equal? %s %s) -> #f?\n", __LINE__, s1 = TO_STR(s7_f(sc)), s2 = TO_STR(s7_f(sc))); free(s1); free(s2);}

  if (!s7_is_unspecified(sc, s7_unspecified(sc)))
    {fprintf(stderr, "%d: %s is not #<unspecified>?\n", __LINE__, s1 = TO_STR(s7_unspecified(sc))); free(s1);}

  if (s7_is_eq(s7_eof_object(sc), s7_undefined(sc)))
    {fprintf(stderr, "%d: (eq? %s %s) -> #t?\n", __LINE__, s1 = TO_STR(s7_eof_object(sc)), s2 = TO_STR(s7_undefined(sc))); free(s1); free(s2);}

  if (s7_is_eqv(sc, s7_eof_object(sc), s7_undefined(sc)))
    {fprintf(stderr, "%d: (eqv? %s %s) -> #t?\n", __LINE__, s1 = TO_STR(s7_eof_object(sc)), s2 = TO_STR(s7_undefined(sc))); free(s1); free(s2);}

  if (s7_is_equal(sc, s7_eof_object(sc), s7_undefined(sc)))
    {fprintf(stderr, "%d: (equal? %s %s) -> #t?\n", __LINE__, s1 = TO_STR(s7_eof_object(sc)), s2 = TO_STR(s7_undefined(sc))); free(s1); free(s2);}

  if (!s7_is_valid(sc, s7_t(sc)))
    {fprintf(stderr, "%d: %s is not valid?\n", __LINE__, s1 = TO_STR(s7_t(sc))); free(s1);}
  {
    typedef struct fake_cell {
      union {
	uint64_t flag;
	uint8_t type_field;
      } tf;
      int64_t hloc, i1, i2, i3;
    } fake_cell;
    fake_cell *x;
    x = calloc(1, sizeof(fake_cell));
    x->tf.flag = 53 + (1 << 11);
    if (s7_is_valid(sc, (s7_pointer)x))
      fprintf(stderr, "fake_cell is ok?\n");
    if (!s7_is_provided(sc, "debugging"))
      s7_object_to_c_string(sc, (s7_pointer)x);
    free(x);
  }
  if (s7_is_c_pointer(s7_t(sc)))
    {fprintf(stderr, "%d: %s is a raw c pointer?\n", __LINE__, s1 = TO_STR(s7_t(sc))); free(s1);}

  i = 32;
  p = s7_make_c_pointer(sc, (void *)(&i));
  if (!s7_is_c_pointer(p))
    {fprintf(stderr, "%d: %s is not a raw c pointer?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  i = (*((int *)s7_c_pointer(p)));
  if (i != 32)
    fprintf(stderr, "%d: 32 -> %" ld64 " via raw c pointer?\n", __LINE__, i);

  s7_provide(sc, "ffitest");
  if (!s7_is_provided(sc, "ffitest"))
    {fprintf(stderr, "%d: *features* %s doesn't provide 'ffitest?\n", __LINE__, s1 = TO_STR(s7_name_to_value(sc, "*features*"))); free(s1);}

  p = s7_cons(sc, s7_f(sc), s7_t(sc));
  gc_loc = s7_gc_protect(sc, p);
  if (p != s7_gc_protected_at(sc, gc_loc))
    {fprintf(stderr, "%d: %s is not gc protected at %" ld64 ": %s?\n", __LINE__, s1 = TO_STR(p), gc_loc, s2 = TO_STR(s7_gc_protected_at(sc, gc_loc))); free(s1); free(s2);}

  if (s7_car(p) != s7_f(sc))
    {fprintf(stderr, "%d: (car %s) is not #f?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_cdr(p) != s7_t(sc))
    {fprintf(stderr, "%d: (cdr %s) is not #t?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_pair(p))
    {fprintf(stderr, "%d: %s is not a pair?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_set_car(p, s7_eof_object(sc));
  if (s7_car(p) != s7_eof_object(sc))
    {fprintf(stderr, "%d: (car %s) is not #<eof>?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_set_cdr(p, s7_unspecified(sc));
  if (s7_cdr(p) != s7_unspecified(sc))
    {fprintf(stderr, "%d: (cdr %s) is not #<unspecified>?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);

  { /* force realloc of protected_objects array */
    s7_int gc_locs[20];
    for (i = 0; i < 18; i++) /* initial size = 16 */
      gc_locs[i] = s7_gc_protect(sc, s7_cons(sc, s7_f(sc), s7_t(sc)));
    for (i = 0; i < 18; i++)
      s7_gc_unprotect_at(sc, gc_locs[i]);
  }

  p = TO_S7_INT(123);
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_integer(p))
    {fprintf(stderr, "%d: %s is not integral?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_rational(p))
    {fprintf(stderr, "%d: %s is not rational?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_ratio(p))
    {fprintf(stderr, "%d: %s is a ratio?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_real(p))
    {fprintf(stderr, "%d: %s is not real?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_complex(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_number(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_integer(p) != 123)
    {fprintf(stderr, "%d: %s is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s2 = s7_number_to_string(sc, p, 10);
  if (strcmp(s2, "123") != 0)
    {fprintf(stderr, "%d: (number->string %s) is not \"123\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  free(s2);

  if (s7_number_to_integer(sc, p) != 123)
    {fprintf(stderr, "%d: s7_number_to_integer %s is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_number_to_integer_with_caller(sc, p, "ffitest") != 123)
    {fprintf(stderr, "%d: s7_number_to_integer_with_caller %s is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_string(s7_object_to_string(sc, p, false)))
    fprintf(stderr, "%s is not a string\n", s7_object_to_c_string(sc, s7_object_to_string(sc, p, false)));
  if (strcmp(s7_string(s7_object_to_string(sc, p, true)), "123") != 0)
    fprintf(stderr, "%s is not \"123\"", s7_string(s7_object_to_string(sc, p, true)));

  s7_gc_unprotect_at(sc, gc_loc);


  p = s7_make_ratio(sc, 123, 5);
  gc_loc = s7_gc_protect(sc, p);

  if (s7_is_integer(p))
    {fprintf(stderr, "%d: %s is integral?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_rational(p))
    {fprintf(stderr, "%d: %s is not rational?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_ratio(p))
    {fprintf(stderr, "%d: %s is not a ratio?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_real(p))
    {fprintf(stderr, "%d: %s is not real?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_complex(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_number(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_numerator(p) != 123)
    {fprintf(stderr, "%d: (numerator %s) is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_denominator(p) != 5)
    {fprintf(stderr, "%d: (denominator %s) is not 5?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s2 = s7_number_to_string(sc, p, 10);
  if (strcmp(s2, "123/5") != 0)
    {fprintf(stderr, "%d: (number->string %s) is not \"123/5\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  free(s2);

  s7_gc_unprotect_at(sc, gc_loc);


  p = s7_make_real(sc, 1.5);
  gc_loc = s7_gc_protect(sc, p);

  if (s7_is_integer(p))
    {fprintf(stderr, "%d: %s is integral?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_rational(p))
    {fprintf(stderr, "%d: %s is rational?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_ratio(p))
    {fprintf(stderr, "%d: %s is a ratio?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_real(p))
    {fprintf(stderr, "%d: %s is not real?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_complex(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_number(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_real(p) != 1.5)
    {fprintf(stderr, "%d: %s is not 1.5?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s2 = s7_number_to_string(sc, p, 10);
  if (strcmp(s2, "1.5") != 0)
    {fprintf(stderr, "%d: (number->string %s) is not \"1.5\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  free(s2);

  if (s7_number_to_real(sc, p) != 1.5)
    {fprintf(stderr, "%d: s7_number_to_real %s is not 1.5?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_number_to_real_with_caller(sc, p, "ffitest") != 1.5)
    {fprintf(stderr, "%d: s7_number_to_real_with_caller %s is not 1.5?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_number_to_real_with_location(sc, p, s7_make_symbol(sc, "ffitest")) != 1.5)
    {fprintf(stderr, "%d: s7_number_to_real_with_location %s is not 1.5?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);

  p = s7_make_mutable_real(sc, 1.5);
  if (!s7_is_real(p))
    {fprintf(stderr, "%d: %s is not real?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_make_complex(sc, 1.0, 1.0);
  gc_loc = s7_gc_protect(sc, p);

  if (s7_is_integer(p))
    {fprintf(stderr, "%d: %s is integral?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_rational(p))
    {fprintf(stderr, "%d: %s is rational?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_ratio(p))
    {fprintf(stderr, "%d: %s is a ratio?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_real(p))
    {fprintf(stderr, "%d: %s is real?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_complex(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_number(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_real_part(p) != 1.0)
    {fprintf(stderr, "%d: (real-part %s) is not 1.0?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_imag_part(p) != 1.0)
    {fprintf(stderr, "%d: (imag-part %s) is not 1.0?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s2 = s7_number_to_string(sc, p, 10);
  if (strcmp(s2, "1.0+1.0i") != 0)
    {fprintf(stderr, "%d: (number->string %s) is not \"1.0+1.0i\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  free(s2);

  s7_immutable(p);
  if (!s7_is_immutable(p))
    fprintf(stderr, "s7_immutable failed?\n");
  s7_gc_unprotect_at(sc, gc_loc);

  p = s7_signature(sc, s7_name_to_value(sc, "abs"));
  if (!s7_is_pair(p))
    {fprintf(stderr, "%d: %s is not a pair?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  {
    s7_pointer p;
    p = s7_shadow_rootlet(sc);
    if ((!s7_is_null(sc, p)) &&
	(!s7_is_let(p)))
      fprintf(stderr, "shadow rootlet is %s\n", s7_object_to_c_string(sc, p));
    s7_set_shadow_rootlet(sc, p);
  }

  if (s7_c_pointer(s7_make_c_pointer(sc, NULL)))
    fprintf(stderr, "s7_c_pointer 0 is not null\n");
  if (s7_c_pointer_type(s7_make_c_pointer_with_type(sc, NULL, s7_nil(sc), s7_f(sc))) != s7_nil(sc))
    fprintf(stderr, "s7_c_pointer_type is not ()\n");
  {
    s7_pointer csc;
    csc = s7_make_c_pointer_with_type(sc, (void *)sc, s7_make_symbol(sc, "s7_scheme*"), s7_f(sc));
    if (!s7_is_c_pointer_of_type(csc, s7_make_symbol(sc, "s7_scheme*")))
      fprintf(stderr, "c-pointer type %s != s7_scheme*\n", s7_object_to_c_string(sc, s7_c_pointer_type(csc)));
    s7_c_pointer_with_type(sc, csc, s7_make_symbol(sc, "s7_scheme*"), "ffitest", __LINE__);
  }
  if (!s7_is_int_vector(s7_make_int_vector(sc, 3, 1, NULL)))
    fprintf(stderr, "s7_make_int_vector did not make an int-vector\n");
  if (s7_is_float_vector(s7_make_int_vector(sc, 3, 1, NULL)))
    fprintf(stderr, "s7_make_int_vector made a float-vector?\n");

  {
    s7_int* dims;
    s7_pointer p;
    s7_double *els;
    uint8_t *bels;
    dims = (s7_int *)malloc(2 * sizeof(s7_int));
    dims[0] = 2;
    dims[1] = 3;

    p = s7_make_int_vector(sc, 6, 2, dims);
    if (s7_vector_rank(p) != 2) fprintf(stderr, "int vector rank not 2?\n");
    p = s7_make_float_vector(sc, 6, 2, dims);
    if (s7_vector_rank(p) != 2) fprintf(stderr, "float vector rank not 2?\n");
    if (s7_vector_dimension(p, 0) != 2) fprintf(stderr, "%d: s7_vector_dimension 0: %" ld64 "\n", __LINE__, s7_vector_dimension(p, 0));
    if (s7_vector_dimension(p, 1) != 3) fprintf(stderr, "%d: s7_vector_dimension 1: %" ld64 "\n", __LINE__, s7_vector_dimension(p, 1));

    p = s7_make_float_vector(sc, 6, 1, NULL);
    s7_float_vector_set(p, 1, 32.0);
    if (s7_float_vector_ref(p, 1) != 32.0) fprintf(stderr, "float_vector[1] not 32.0?\n");
    els = s7_float_vector_elements(p);
    if (els[1] != 32.0) fprintf(stderr, "float_vector els[1] not 32.0?\n");
    if (!s7_is_float_vector(p)) fprintf(stderr, "not a float_vector?\n");

    p = s7_make_byte_vector(sc, 6, 2, dims);
    s7_byte_vector_set(p, 1, 32);
    if (s7_byte_vector_ref(p, 1) != 32) fprintf(stderr, "byte_vector[1] not 32?\n");
    bels = s7_byte_vector_elements(p);
    if (bels[1] != 32) fprintf(stderr, "byte_vector bels[1] not 32?\n");
    if (!s7_is_byte_vector(p)) fprintf(stderr, "not a byte_vector?\n");
    free(dims); /* ?? */
  }

  {
    s7_pointer p, q;
    s7_int *els;
    p = s7_make_int_vector(sc, 6, 1, NULL);
    s7_int_vector_set(p, 1, 32);
    if (s7_int_vector_ref(p, 1) != 32) fprintf(stderr, "int_vector[1] not 32?\n");
    els = s7_int_vector_elements(p);
    if (els[1] != 32) fprintf(stderr, "int_vector els[1] not 32?\n");
    if (!s7_is_int_vector(p)) fprintf(stderr, "not an int_vector?\n");
    q = s7_vector_to_list(sc, p);
    if (!s7_is_pair(q)) fprintf(stderr, "%d vector->list is not a list %s\n", __LINE__, TO_STR(q));
    if (s7_list_length(sc, q) != 6) fprintf(stderr, "%d vector->list len != 6 %s\n", __LINE__, TO_STR(q));
    if (s7_vector_dimension(p, 0) != 6) fprintf(stderr, "%d: s7_vector_dimension: %" ld64 "\n", __LINE__, s7_vector_dimension(p, 0));
  }

  {
    s7_int* dims;
    s7_pointer p;
    s7_pointer *els;
    dims = (s7_int *)malloc(2 * sizeof(s7_int));
    dims[0] = 2;
    dims[1] = 3;
    p = s7_make_normal_vector(sc, 6, 2, dims);
    if (s7_vector_rank(p) != 2) fprintf(stderr, "vector rank not 2?\n");
    if (s7_vector_dimension(p, 0) != 2) fprintf(stderr, "%d: s7_vector_dimension 0: %" ld64 "\n", __LINE__, s7_vector_dimension(p, 0));
    if (s7_vector_dimension(p, 1) != 3) fprintf(stderr, "%d: s7_vector_dimension 1: %" ld64 "\n", __LINE__, s7_vector_dimension(p, 1));
    s7_vector_set(sc, p, 1, s7_make_integer(sc, 1));
    if (s7_integer(s7_vector_ref(sc, p, 1)) != 1) fprintf(stderr, "vector[1] not 1?\n");
    els = s7_vector_elements(p);
    if (s7_integer(els[1]) != 1) fprintf(stderr, "vector els[1] not 1?\n");
    if (!s7_is_vector(p)) fprintf(stderr, "not a vector?\n");
    free(dims); /* ?? */
  }

  {
    s7_int len;
    len = s7_integer(s7_starlet_ref(sc, s7_make_symbol(sc, "print-length")));
    s7_starlet_set(sc, s7_make_symbol(sc, "print-length"), s7_make_integer(sc, len));
  }

  p = s7_rationalize(sc, 1.5, 1e-12);
  gc_loc = s7_gc_protect(sc, p);
  s1 = TO_STR(p);
  if (strcmp(s1, "3/2") != 0)
    fprintf(stderr, "%d: ratio is %s?\n", __LINE__, s1);
  free(s1);
  s7_gc_unprotect_at(sc, gc_loc);

  s7_set_default_random_state(sc, 1234, 5678);
  s7_random(sc, NULL);
  s7_stacktrace(sc);

  if (s7_list(sc, 0) != s7_nil(sc))
    fprintf(stderr, "s7_list 0 is not ()\n");
  if (s7_list_nl(sc, 0, NULL) != s7_nil(sc))
    fprintf(stderr, "s7_list_nl 0 is not ()\n");

  p = s7_make_vector(sc, 12);
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_vector(p))
    {fprintf(stderr, "%d: %s is not a vector?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_type_of(sc, p) != s7_make_symbol(sc, "vector?"))
    fprintf(stderr, "type-of(vector) confused?\n");

  if (s7_vector_rank(p) != 1)
    {fprintf(stderr, "%d: (dimensions %s) is not 1?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_vector_set(sc, p, 1, s7_t(sc));
  if (s7_vector_ref(sc, p, 1) != s7_t(sc))
    {fprintf(stderr, "%d: (%s 1) is not #t?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_vector_fill(sc, p, TO_S7_INT(123));
  if (s7_integer(s7_vector_ref(sc, p, 1)) != 123)
    {fprintf(stderr, "%d: (%s 1) is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);

  p = s7_make_and_fill_vector(sc, 3, TO_S7_INT(3));
  gc_loc = s7_gc_protect(sc, p);

  if (s7_integer(s7_vector_ref(sc, p, 1)) != 3)
    {fprintf(stderr, "%d: (%s 1) is not 3?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p1 = s7_vector_copy(sc, p);
  if ((p == p1) ||
      (!s7_is_vector(p1)))
    {fprintf(stderr, "%d: copied vector: %s\n", __LINE__, s1 = TO_STR(p1)); free(s1);}
  s7_gc_unprotect_at(sc, gc_loc);


  p = s7_make_string(sc, "1234");
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_string(p))
    {fprintf(stderr, "%d: %s is not a string?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_string_length(p) != 4)
    {fprintf(stderr, "%d: (length %s) is 4?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (strcmp(s7_string(p), "1234") != 0)
    {fprintf(stderr, "%d: %s is not \"1234\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (strcmp(s7_string(s7_copy(sc, s7_cons(sc, p, s7_nil(sc)))), "1234") != 0)
    {fprintf(stderr, "%d: copy(%s) is not \"1234\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_fill(sc, s7_cons(sc, p, s7_cons(sc, s7_make_character(sc, 'c'), s7_nil(sc))));
  if (strcmp(s7_string(p), "cccc") != 0)
    {fprintf(stderr, "%d: fill(%s) is not \"cccc\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);

  p = s7_make_permanent_string(sc, "asdf");
  if (!s7_is_string(p))
    {fprintf(stderr, "%d: %s is not a string?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_string_length(p) != 4)
    {fprintf(stderr, "%d: (length %s) is 4?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  {
    s7_int gc_loc;
    s7_pointer q;
    p = s7_make_string_with_length(sc, "asdf", 4);
    gc_loc = s7_gc_protect(sc, p);
    if (!s7_is_string(p))
      {fprintf(stderr, "%d: %s is not a string?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    if (s7_string_length(p) != 4)
      {fprintf(stderr, "%d: (length %s) is 4?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    s7_gc_protect_via_location(sc, q = s7_make_string(sc, "fdsa"), gc_loc);
    if (q != s7_gc_protected_at(sc, gc_loc))
      fprintf(stderr, "%d: wrong thing at gc_loc? %s\n", __LINE__, TO_STR(s7_gc_protected_at(sc, gc_loc)));
    s7_gc_unprotect_via_location(sc, gc_loc);
    p = s7_make_string_wrapper(sc, "hiho");
    if (!s7_is_string(p))
      {fprintf(stderr, "%d: %s is not a string?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    if (s7_string_length(p) != 4)
      {fprintf(stderr, "%d: (length %s) is 4?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  }

  p = s7_make_character(sc, 65);
  if (!s7_is_character(p))
    {fprintf(stderr, "%d: %s is not a character?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_character(p) != 'A')
    {fprintf(stderr, "%d: %s is not #\\A?\n", __LINE__, s1 = TO_STR(p)); free(s1);}


  p = s7_list(sc, 3, TO_S7_INT(1), TO_S7_INT(2), TO_S7_INT(3));
  gc_loc = s7_gc_protect(sc, p);
  if (s7_tree_memq(sc, s7_make_symbol(sc, "oops"), p))
    fprintf(stderr, "'oops is in the list?\n");

  if (!s7_is_list(sc, p))
    {fprintf(stderr, "%d: %s is not a list?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_list_length(sc, p) != 3)
    {fprintf(stderr, "%d: (length %s) is not 3?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_integer(s7_list_ref(sc, p, 1)) != 2)
    {fprintf(stderr, "%d: (%s 1) is not 2?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_integer(s7_car(p)) != 1)
    {fprintf(stderr, "%d: (car %s) is not 1?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_integer(s7_cadr(p)) != 2)
    {fprintf(stderr, "%d: (cadr %s) is not 2?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_integer(s7_caddr(p)) != 3)
    {fprintf(stderr, "%d: (caddr %s) is not 2?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_integer(s7_car(s7_cddr(p))) != 3)
    {fprintf(stderr, "%d: (car (cddr %s)) is not 2?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_list_set(sc, p, 1, s7_f(sc));
  if (s7_list_ref(sc, p, 1) != s7_f(sc))
    {fprintf(stderr, "%d: (%s 1) is not #f?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);

  p = s7_make_list(sc, 3, s7_make_integer(sc, 123));
  if (!s7_is_list(sc, p))
    {fprintf(stderr, "%d: %s is not a list?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_list_length(sc, p) != 3)
    {fprintf(stderr, "%d: (length %s) is not 3?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_integer(s7_list_ref(sc, p, 1)) != 123)
    {fprintf(stderr, "%d: (%s 1) is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_list_nl(sc, 3, TO_S7_INT(1), TO_S7_INT(2), TO_S7_INT(3), NULL);
  gc_loc = s7_gc_protect(sc, p);
  if (!s7_is_list(sc, p))
    {fprintf(stderr, "%d: %s is not a list?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_list_length(sc, p) != 3)
    {fprintf(stderr, "%d: (length %s) is not 3?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_integer(s7_list_ref(sc, p, 1)) != 2)
    {fprintf(stderr, "%d: (%s 1) is not 2?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  s7_gc_unprotect_at(sc, gc_loc);

  {
    s7_pointer array[3];
    for (i = 1; i < 4; i++) array[i - 1] = s7_make_integer(sc, i);
    p = s7_array_to_list(sc, 3, array);
    gc_loc = s7_gc_protect(sc, p);
    if (!s7_is_list(sc, p))
      {fprintf(stderr, "%d: %s is not a list?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    if (s7_list_length(sc, p) != 3)
      {fprintf(stderr, "%d: (length %s) is not 3?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    if (s7_integer(s7_list_ref(sc, p, 1)) != 2)
      {fprintf(stderr, "%d: (%s 1) is not 2?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    s7_gc_unprotect_at(sc, gc_loc);
  }
  p = s7_array_to_list(sc, 0, NULL);
  if (!s7_is_null(sc, p))
    {fprintf(stderr, "%d: %s is not null?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  {
    s7_pointer c1, c2, c3, c12, c23, c123, c1234, c1d2, c2d3, c3d4, c12d3, c23d4, c123d4, c1234d5;
    s7_gc_on(sc, false);
    c1 = s7_list(sc, 1, TO_S7_INT(1));                                              /* (1) */
    c2 = s7_list(sc, 1, TO_S7_INT(2));                                              /* (2) */
    c3 = s7_list(sc, 1, TO_S7_INT(3));                                              /* (3) */
    c12 = s7_list(sc, 2, TO_S7_INT(1), TO_S7_INT(2));                               /* (1 2) */
    c23 = s7_list(sc, 2, TO_S7_INT(2), TO_S7_INT(3));                               /* (2 3) */
    c123 = s7_list(sc, 3, TO_S7_INT(1), TO_S7_INT(2), TO_S7_INT(3));                /* (1 2 3) */
    c1234 = s7_list(sc, 4, TO_S7_INT(1), TO_S7_INT(2), TO_S7_INT(3), TO_S7_INT(4)); /* (1 2 3 4) */
    c1d2 = s7_cons(sc, TO_S7_INT(1), TO_S7_INT(2));                                 /* (1 . 2) */
    c2d3 = s7_cons(sc, TO_S7_INT(2), TO_S7_INT(3));                                 /* (2 . 3) */
    c3d4 = s7_cons(sc, TO_S7_INT(3), TO_S7_INT(4));                                 /* (3 . 4) */
    c12d3 = s7_cons(sc, TO_S7_INT(1), s7_cons(sc, TO_S7_INT(2), TO_S7_INT(3)));     /* (1 2 . 3) */
    c23d4 = s7_cons(sc, TO_S7_INT(2), s7_cons(sc, TO_S7_INT(3), TO_S7_INT(4)));     /* (2 3 . 4) */
    c123d4 = s7_cons(sc, TO_S7_INT(1), s7_cons(sc, TO_S7_INT(2), s7_cons(sc, TO_S7_INT(3), TO_S7_INT(4))));                             /* (1 2 3 . 4) */
    c1234d5 = s7_cons(sc, TO_S7_INT(1), s7_cons(sc, TO_S7_INT(2), s7_cons(sc, TO_S7_INT(3), s7_cons(sc, TO_S7_INT(4), TO_S7_INT(5))))); /* (1 2 3 4 . 5) */

    if (s7_integer(p = s7_caar(s7_list(sc, 1, c1))) != 1)
      {fprintf(stderr, "%d: caar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cadr(c12)) != 2)
      {fprintf(stderr, "%d: cadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cdar(s7_list(sc, 1, c1d2))) != 2)
      {fprintf(stderr, "%d: cdar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cddr(c12d3)) != 3)
      {fprintf(stderr, "%d: cddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_caaar(s7_list(sc, 1, s7_list(sc, 1, c1)))) != 1)
      {fprintf(stderr, "%d: caaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_caadr(s7_list(sc, 2, TO_S7_INT(1), c2))) != 2)
      {fprintf(stderr, "%d: caadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cadar(s7_list(sc, 1, c12))) != 2)
      {fprintf(stderr, "%d: cadar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cdaar(s7_list(sc, 1, s7_list(sc, 1, c1d2)))) != 2)
      {fprintf(stderr, "%d: cdaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_caddr(c123)) != 3)
      {fprintf(stderr, "%d: caddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cdddr(c123d4)) != 4)
      {fprintf(stderr, "%d: cdddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cdadr(s7_list(sc, 2, TO_S7_INT(1), c2d3))) != 3)
      {fprintf(stderr, "%d: cdadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cddar(s7_list(sc, 1, c12d3))) != 3)
      {fprintf(stderr, "%d: cddar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_caaaar(s7_list(sc, 1, s7_list(sc, 1, s7_list(sc, 1, c1))))) != 1)
      {fprintf(stderr, "%d: caaaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_caaadr(s7_list(sc, 2, TO_S7_INT(1), s7_list(sc, 1, c2)))) != 2)
      {fprintf(stderr, "%d: caaadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_caadar(s7_list(sc, 1, s7_list(sc, 2, TO_S7_INT(1), c2)))) != 2)
      {fprintf(stderr, "%d: caadar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cadaar(s7_list(sc, 1, s7_list(sc, 1, c12)))) != 2)
      {fprintf(stderr, "%d: cadaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_caaddr(s7_list(sc, 3, TO_S7_INT(1), TO_S7_INT(2), c3))) != 3)
      {fprintf(stderr, "%d: caaddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cadddr(c1234)) != 4)
      {fprintf(stderr, "%d: cadddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cadadr(s7_list(sc, 2, TO_S7_INT(1), c23))) != 3)
      {fprintf(stderr, "%d: cadadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_caddar(s7_list(sc, 1, c123))) != 3)
      {fprintf(stderr, "%d: caddar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cdaaar(s7_list(sc, 1, s7_list(sc, 1, s7_list(sc, 1, c1d2))))) != 2)
      {fprintf(stderr, "%d: cdaaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cdaadr(s7_list(sc, 2, TO_S7_INT(1), s7_list(sc, 1, c2d3)))) != 3)
      {fprintf(stderr, "%d: cdaadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cdadar(s7_list(sc, 1, s7_list(sc, 2, TO_S7_INT(1), c2d3)))) != 3)
      {fprintf(stderr, "%d: cdadar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cddaar(s7_list(sc, 1, s7_list(sc, 1, c12d3)))) != 3)
      {fprintf(stderr, "%d: cddaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cdaddr(s7_list(sc, 3, TO_S7_INT(1), TO_S7_INT(2), c3d4))) != 4)
      {fprintf(stderr, "%d: cdaddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cddddr(c1234d5)) != 5)
      {fprintf(stderr, "%d: cdddd is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cddadr(s7_list(sc, 2, TO_S7_INT(1), c23d4))) != 4)
      {fprintf(stderr, "%d: cddadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_integer(p = s7_cdddar(s7_list(sc, 1, c123d4))) != 4)
      {fprintf(stderr, "%d: cdddar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    p = s7_reverse(sc, c123);
    s1 = TO_STR(p);
    if (strcmp(s1, "(3 2 1)") != 0)
      {fprintf(stderr, "%d: (reverse '(1 2 3)) is %s?\n", __LINE__, s1);}
    free(s1);

    p = s7_append(sc, c1, c2);
    s1 = TO_STR(p);
    if (strcmp(s1, "(1 2)") != 0)
      {fprintf(stderr, "%d: (append '(1) '(2)) is %s?\n", __LINE__, s1);}
    free(s1);

    p = s7_list(sc, 2, s7_cons(sc, s7_make_symbol(sc, "a"), TO_S7_INT(32)), s7_cons(sc, s7_make_symbol(sc, "b"), TO_S7_INT(1)));
    p1 = s7_assq(sc, s7_make_symbol(sc, "a"), p);
    s1 = TO_STR(p1);
    if (strcmp(s1, "(a . 32)") != 0)
      {fprintf(stderr, "%d: (assq 'a '((a . 32) (b . 1)))) is %s?\n", __LINE__, s1);}
    free(s1);

    p1 = s7_assoc(sc, s7_make_symbol(sc, "b"), p);
    s1 = TO_STR(p1);
    if (strcmp(s1, "(b . 1)") != 0)
      {fprintf(stderr, "%d: (assoc 'b '((a . 32) (b . 1))) is %s?\n", __LINE__, s1);}
    free(s1);

    p = s7_member(sc, TO_S7_INT(2), c1234);
    s1 = TO_STR(p);
    if (strcmp(s1, "(2 3 4)") != 0)
      {fprintf(stderr, "%d: (member 2 '(1 2 3 4)) is %s?\n", __LINE__, s1);}
    free(s1);

    p = s7_list(sc, 2, s7_make_symbol(sc, "a"), s7_make_symbol(sc, "b"));
    p1 = s7_memq(sc, s7_make_symbol(sc, "b"), p);
    s1 = TO_STR(p1); /* s1 is "(b)" here but valgrind complains */
    if (strcmp(s1, "(b)") != 0)
      {fprintf(stderr, "%d: (memq 'b '(a b)) is %s?\n", __LINE__, s1);}
    free(s1);

    s7_set_car(c1234, s7_make_symbol(sc, "+"));
    p = s7_eval(sc, c1234, s7_sublet(sc, s7_rootlet(sc), s7_nil(sc)));
    if (s7_integer(p) != 9)
      {fprintf(stderr, "%d: (eval '(+ 2 3 4)) is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    s7_gc_on(sc, true);

    p = s7_eval(sc, s7_cons(sc, s7_make_symbol(sc, "+"), /* s7.html example */
               s7_cons(sc, s7_make_integer(sc, 1),
                  s7_cons(sc, s7_make_integer(sc, 2), s7_nil(sc)))),
               s7_nil(sc));
    if (s7_integer(p) != 3)
       {fprintf(stderr, "%d: (eval '(+ 1 2)) is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    p = s7_eval_with_location(sc, s7_cons(sc, s7_make_symbol(sc, "+"),
					  s7_cons(sc, s7_make_integer(sc, 1),
						  s7_cons(sc, s7_make_integer(sc, 2), s7_nil(sc)))),
			      s7_nil(sc), "ffitest", __FILE__, __LINE__);
    if (s7_integer(p) != 3)
       {fprintf(stderr, "%d: (eval '(+ 1 2)) is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    p = s7_eval(sc, s7_cons(sc, s7_make_symbol(sc, "+"), /* s7.html example */
               s7_cons(sc, s7_make_integer(sc, 1),
                  s7_cons(sc, s7_make_integer(sc, 3), s7_nil(sc)))),
               s7_rootlet(sc));
    if (s7_integer(p) != 4)
       {fprintf(stderr, "%d: (eval '(+ 1 3)) is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    p = s7_eval_c_string(sc, "(+ 2 3)");
    if (s7_integer(p) != 5)
       {fprintf(stderr, "%d: (eval-string '(+ 2 3)) is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  }

  s7_for_each_symbol_name(sc, symbol_func, NULL);
  s7_for_each_symbol(sc, symbol_func_1, NULL);
  s7_symbol_name(s7_make_symbol(sc, "a_symbol"));

  p = s7_make_hash_table(sc, 255);
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_hash_table(p))
    {fprintf(stderr, "%d: %s is not a hash-table?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_hash_table_ref(sc, p, s7_eof_object(sc)) != s7_f(sc))
    {fprintf(stderr, "%d: (hash-table-ref %s #<eof>) is not #f?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_hash_table_set(sc, p, s7_eof_object(sc), s7_unspecified(sc));
  if (s7_hash_table_ref(sc, p, s7_eof_object(sc)) != s7_unspecified(sc))
    {fprintf(stderr, "%d: (hash-table-ref %s #<eof>) is not #<unspecified>?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  s7_gc_unprotect_at(sc, gc_loc);

  p = s7_current_input_port(sc);
  if (!s7_is_input_port(sc, p))
    {fprintf(stderr, "%d: %s is not an input port?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  s7_port_line_number(sc, p);
  s7_add_to_history(sc, s7_nil(sc));
  s7_history(sc);

  p = s7_current_output_port(sc);
  if (!s7_is_output_port(sc, p))
    {fprintf(stderr, "%d: %s is not an output port?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_name_to_value(sc, "abs");
  if (!s7_is_procedure(p))
    {fprintf(stderr, "%d: %s is not a procedure?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_make_symbol(sc, "abs");
  if (!s7_is_symbol(p))
    {fprintf(stderr, "%d: %s is not a symbol?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_syntax(p))
    {fprintf(stderr, "%d: %s is syntax?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_gensym(sc, "abs");
  if (!s7_is_symbol(p))
    {fprintf(stderr, "%d: %s is not a symbol?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_make_keyword(sc, "key");
  if (!s7_is_keyword(p))
    {fprintf(stderr, "%d: %s is not a keyword?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_keyword_to_symbol(sc, p) != s7_make_symbol(sc, "key"))
    fprintf(stderr, "%d: key->sym != sym?\n", __LINE__);

  if (!s7_is_eq(p, p))
    {fprintf(stderr, "%d: %s is not a self-eq??\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_rootlet(sc);
  if (!s7_is_let(p))
    {fprintf(stderr, "%d: %s is not an environment?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_curlet(sc);
  if ((!s7_is_null(sc, p)) && (!s7_is_let(p)))
    {fprintf(stderr, "%d: %s is not an environment?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_define_constant(sc, "a_constant", s7_t(sc));
  if (!s7_is_immutable(s7_name_to_value(sc, "a_constant")))
    {fprintf(stderr, "%d: a_constant is not a constant?\n", __LINE__);}
  if (!s7_is_defined(sc, "a_constant"))
    {fprintf(stderr, "%d: a_constant is not defined?\n", __LINE__);}
  p = s7_symbol_table_find_name(sc, "a_constant");
  if (!s7_is_symbol(p))
    {fprintf(stderr, "%d: %s is not a symbol (from s7_symbol_find_name)?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_define_constant_with_documentation(sc, "another_constant", s7_t(sc), "another constant");
  if (s7_name_to_value(sc, "another_constant") != s7_t(sc))
    {fprintf(stderr, "%d: another_constant: %s?\n", __LINE__, s1 = TO_STR(s7_name_to_value(sc, "another_constant"))); free(s1);}
  {
    const char *s;
    s = s7_documentation(sc, s7_make_symbol(sc, "another_constant"));
    if ((!s) || (strcmp(s, "another constant") != 0))
      fprintf(stderr, "%d: another_constant doc: %s\n", __LINE__, s);
  }
  s7_define_variable_with_documentation(sc, "another_variable", s7_t(sc), "another variable");
  if (s7_name_to_value(sc, "another_variable") != s7_t(sc))
    {fprintf(stderr, "%d: another_variable: %s?\n", __LINE__, s1 = TO_STR(s7_name_to_value(sc, "another_variable"))); free(s1);}
  {
    const char *s;
    s = s7_documentation(sc, s7_make_symbol(sc, "another_variable"));
    if ((!s) || (strcmp(s, "another variable") != 0))
      fprintf(stderr, "%d: another_variable doc: %s\n", __LINE__, s);
  }

  s7_define_safe_function(sc, "a_function", a_function, 1, 0, false, "a function");
  if (!s7_is_defined(sc, "a_function"))
    {fprintf(stderr, "%d: a_function is not defined?\n", __LINE__);}
  if (!s7_is_function(s7_name_to_value(sc, "a_function")))
    {fprintf(stderr, "%d: a_function is not a function?\n", __LINE__);}

  p = s7_apply_function(sc, s7_name_to_value(sc, "a_function"), s7_cons(sc, TO_S7_INT(32), s7_nil(sc)));
  if (!s7_is_integer(p))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_integer(p) != 32)
    {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p)); free(s1);}


  dax_type_tag = s7_make_c_type(sc, "dax");
  s7_c_type_set_free(sc, dax_type_tag, free_dax);
  s7_c_type_set_equal(sc, dax_type_tag, equal_dax);
  s7_c_type_set_is_equal(sc, dax_type_tag, equality_dax);
  s7_c_type_set_mark(sc, dax_type_tag, mark_dax);
  s7_c_type_set_to_string(sc, dax_type_tag, dax_to_string);

  s7_define_function(sc, "make-dax", make_dax, 2, 0, false, "(make-dax x data) makes a new dax");
  s7_define_typed_function(sc, "dax?", is_dax, 1, 0, false, "(dax? anything) returns #t if its argument is a dax object", s7_make_signature(sc, 1, s7_t(sc)));
  if (s7_car(s7_signature(sc, s7_name_to_value(sc, "dax?"))) != s7_t(sc))
    fprintf(stderr, "%d: dax? signature: %s\n", __LINE__, TO_STR(s7_signature(sc, s7_name_to_value(sc, "dax?"))));
  p = s7_make_function(sc, "make-dax", make_dax, 2, 0, false, "(make-dax x data) makes a new dax");
  if (!s7_is_procedure(p)) fprintf(stderr, "%d: make-dax is not a procedure\n", __LINE__);
  p = s7_make_safe_function(sc, "make-dax", make_dax, 2, 0, false, "(make-dax x data) makes a new dax");
  if (!s7_is_procedure(p)) fprintf(stderr, "%d: make-dax is not a (safe) procedure\n", __LINE__);

  s7_define_variable(sc, "dax-x",
                     s7_dilambda(sc, "dax-x", dax_x, 1, 0, set_dax_x, 2, 0, "dax x field (a real)"));

  s7_define_variable(sc, "dax-xx",
                     s7_typed_dilambda(sc, "dax-x", dax_x, 1, 0, set_dax_x, 2, 0, "dax x field (a real)",
				       s7_make_signature(sc, 1, s7_make_symbol(sc, "real?")),
				       s7_make_circular_signature(sc, 0, 1, s7_make_symbol(sc, "real?"))));

  s7_define_variable(sc, "dax-data",
                     s7_dilambda_with_environment(sc, s7_nil(sc), "dax-data", dax_data, 1, 0, set_dax_data, 2, 0, "dax data field"));

  if (!s7_is_dilambda(s7_name_to_value(sc, "dax-x")))
    {fprintf(stderr, "%d: dax-x is not a pws?\n", __LINE__);}

  p = make_dax(sc, s7_cons(sc, s7_make_real(sc, 1.0), s7_cons(sc, TO_S7_INT(2), s7_nil(sc))));
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_c_object(p))
    {fprintf(stderr, "%d: %s is not a c_object?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p1 = s7_apply_function(sc, s7_name_to_value(sc, "dax?"), s7_cons(sc, p, s7_nil(sc)));
  if (p1 != s7_t(sc))
    {fprintf(stderr, "%d: %s is not a dax c_object?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s1 = TO_STR(p);
  if (strcmp(s1, "#<dax 1.000 2>") != 0)
    {fprintf(stderr, "%d: dax prints as %s?\n", __LINE__, s2 = TO_STR(p)); free(s2);}
  free(s1);

  p1 = s7_apply_function(sc, s7_name_to_value(sc, "dax-data"), s7_cons(sc, p, s7_nil(sc)));
  if (!s7_is_integer(p1))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}
  if (s7_integer(p1) != 2)
    {fprintf(stderr, "%d: %s is not 2?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}

  s7_apply_function(sc, s7_setter(sc, s7_name_to_value(sc, "dax-data")), s7_cons(sc, p, s7_cons(sc, TO_S7_INT(32), s7_nil(sc))));
  p1 = s7_apply_function(sc, s7_name_to_value(sc, "dax-data"), s7_cons(sc, p, s7_nil(sc)));
  if (!s7_is_integer(p1))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}
  if (s7_integer(p1) != 32)
    {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);

  s7_define_function(sc, "dx->list", dx_to_list, 1, 0, false, NULL);
  dx_type_tag = s7_make_c_type(sc, "dx");
  s7_c_type_set_gc_free(sc, dx_type_tag, free_dx);
  s7_c_type_set_gc_mark(sc, dx_type_tag, mark_dx);
  s7_c_type_set_getter(sc, dx_type_tag, s7_name_to_value(sc, "abs"));
  s7_c_type_set_setter(sc, dx_type_tag, s7_name_to_value(sc, "list-ref"));
  s7_c_type_set_to_list(sc, dx_type_tag, dx_to_list);

  s7_define_function_star(sc, "plus", plus, "(red 32) blue", "an example of define* from C");
  if (!s7_is_procedure(s7_name_to_value(sc, "plus")))
    {fprintf(stderr, "%d: plus is not a function?\n", __LINE__);}

  p = s7_apply_function(sc, s7_name_to_value(sc, "plus"), s7_cons(sc, TO_S7_INT(1), s7_cons(sc, TO_S7_INT(2), s7_nil(sc))));
  if (!s7_is_integer(p))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_integer(p) != 4)
    {fprintf(stderr, "%d: %s is not 4?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_define_function_star(sc, "plus1", plus1, "a b c", "an example of define* from C");
  {
    s7_int val;
    val = s7_integer(s7_apply_function_star(sc, s7_name_to_value(sc, "plus1"),
					    s7_list(sc, 3,
						    s7_make_integer(sc, 4),
						    s7_make_integer(sc, 5),
						    s7_make_integer(sc, 6))));
    if (val != 21)
      fprintf(stderr, "plus1: %" ld64 "\n", val);

    p = s7_make_c_object_without_gc(sc, dax_type_tag, (void *)malloc(sizeof(dax)));
    {
      dax *o;
      o = (dax *)malloc(sizeof(dax));
      o->x = 1.0;
      o->data = s7_nil(sc);
      p = s7_make_c_object_with_let(sc, dax_type_tag, (void *)o, s7_sublet(sc, s7_curlet(sc), s7_nil(sc)));
    }
  }

  {
    s7_pointer old_port, val;
    const char *doc;
    old_port = s7_current_error_port(sc);

    s7_define_function_star(sc, "fs1", fs1, "(opts (inlet 'f \"b\"))", NULL);
    p = s7_make_symbol(sc, "fs1");
    s7_set_documentation(sc, p, "new doc");
    doc = s7_documentation(sc, p);
    if ((!doc) || (strcmp(s7_documentation(sc, p), "new doc") != 0))
      fprintf(stderr, "%d: s7_set_documentation: %s\n", __LINE__, s7_documentation(sc, p));
    s7_define_typed_function_star(sc, "fs2", fs2, "", NULL, s7_make_signature(sc, 1, s7_t(sc)));
    if (s7_car(s7_signature(sc, s7_name_to_value(sc, "fs2"))) != s7_t(sc))
      fprintf(stderr, "%d: fs2 signature: %s\n", __LINE__, TO_STR(s7_signature(sc, s7_name_to_value(sc, "fs2"))));
    s7_set_current_error_port(sc, s7_f(sc));
    s7_define_function_star(sc, "fs3", fs3, ":allow-other-keys", NULL);
    s7_set_current_error_port(sc, old_port);
    s7_define_function_star(sc, "fs31", fs31, "(a 32) :allow-other-keys", NULL);

    s7_define_safe_function_star(sc, "fs4", fs4, "(opts (inlet 'f \"b\"))", NULL);
    s7_define_safe_function_star(sc, "fs5", fs5, "", NULL);
    s7_set_current_error_port(sc, s7_f(sc));
    s7_define_safe_function_star(sc, "fs6", fs6, ":allow-other-keys", NULL);
    s7_set_current_error_port(sc, old_port);
    s7_define_safe_function_star(sc, "fs61", fs61, "(a #(0)) :allow-other-keys", NULL);
    val = s7_make_function_star(sc, "fs4", fs4, "(opts (inlet 'f \"b\"))", NULL);
    if (!s7_is_procedure(val)) fprintf(stderr, "%d: fs4 is not a procedure\n", __LINE__);
    val = s7_make_safe_function_star(sc, "fs4", fs4, "(opts (inlet 'f \"b\"))", NULL);
    if (!s7_is_procedure(val)) fprintf(stderr, "%d: fs4 is not a (safe) procedure\n", __LINE__);

    val = s7_eval_c_string(sc, "(fs1)");
    if (!s7_is_let(val)) fprintf(stderr, "(fs1): %s\n", s7_object_to_c_string(sc, val));
    val = s7_eval_c_string(sc, "(fs1 #f)");
    if (!s7_is_eq(val, s7_f(sc))) fprintf(stderr, "(fs1 #f): %s\n", s7_object_to_c_string(sc, val));
    val = s7_eval_c_string(sc, "(fs2)");
    if (!s7_is_null(sc, val)) fprintf(stderr, "(fs2): %s\n", s7_object_to_c_string(sc, val));

    val = s7_eval_c_string(sc, "(fs31)");
    if (s7_integer(val) != 32) fprintf(stderr, "(fs31): %s\n", s7_object_to_c_string(sc, val));
    val = s7_eval_c_string(sc, "(fs31 32)");
    if (s7_integer(val) != 32) fprintf(stderr, "(fs31 32): %s\n", s7_object_to_c_string(sc, val));
    val = s7_eval_c_string(sc, "(fs31 :a 31)");
    if (s7_integer(val) != 31) fprintf(stderr, "(fs31 :a 31): %s\n", s7_object_to_c_string(sc, val));
    val = s7_eval_c_string(sc, "(fs31 :ignored #f)");
    if (s7_integer(val) != 32) fprintf(stderr, "(fs31 :ignored #f): %s\n", s7_object_to_c_string(sc, val));
    val = s7_eval_c_string(sc, "(fs31 :a 30 :ignored #f)");
    if (s7_integer(val) != 30) fprintf(stderr, "(fs31 :a 30 :ignored #f): %s\n", s7_object_to_c_string(sc, val));
    val = s7_eval_c_string(sc, "(fs31 :ignored #f :a 29)");
    if (s7_integer(val) != 29) fprintf(stderr, "(fs31 :ignored #f :a 29): %s\n", s7_object_to_c_string(sc, val));

    val = s7_eval_c_string(sc, "(fs4)");
    if (!s7_is_let(val)) fprintf(stderr, "(fs4): %s\n", s7_object_to_c_string(sc, val));
    val = s7_eval_c_string(sc, "(fs4 #f)");
    if (!s7_is_eq(val, s7_f(sc))) fprintf(stderr, "(fs4 #f): %s\n", s7_object_to_c_string(sc, val));
    val = s7_eval_c_string(sc, "(fs5)");
    if (!s7_is_null(sc, val)) fprintf(stderr, "(fs5): %s\n", s7_object_to_c_string(sc, val));

    val = s7_eval_c_string(sc, "(fs61)");
    if (!s7_is_vector(val)) fprintf(stderr, "(fs61): %s\n", s7_object_to_c_string(sc, val));
    val = s7_eval_c_string(sc, "(fs61 32)");
    if (s7_integer(val) != 32) fprintf(stderr, "(fs61 32): %s\n", s7_object_to_c_string(sc, val));
    val = s7_eval_c_string(sc, "(fs61 :a 31)");
    if (s7_integer(val) != 31) fprintf(stderr, "(fs61 :a 31): %s\n", s7_object_to_c_string(sc, val));
    val = s7_eval_c_string(sc, "(fs61 :ignored #f)");
    if (!s7_is_vector(val)) fprintf(stderr, "(fs61 :ignored #f): %s\n", s7_object_to_c_string(sc, val));
    val = s7_eval_c_string(sc, "(fs61 :a 30 :ignored #f)");
    if (s7_integer(val) != 30) fprintf(stderr, "(fs61 :a 30 :ignored #f): %s\n", s7_object_to_c_string(sc, val));
    val = s7_eval_c_string(sc, "(fs61 :ignored #f :a 29)");
    if (s7_integer(val) != 29) fprintf(stderr, "(fs61 :ignored #f :a 29): %s\n", s7_object_to_c_string(sc, val));
  }

  p = s7_apply_function(sc, s7_name_to_value(sc, "plus"), s7_cons(sc, s7_make_keyword(sc, "blue"), s7_cons(sc, TO_S7_INT(2), s7_nil(sc))));
  if (!s7_is_integer(p))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_integer(p) != 66)
    {fprintf(stderr, "%d: %s is not 66?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_define_variable(sc, "my-1", s7_make_integer(sc, 1));
  p = s7_name_to_value(sc, "my-1");
  if (!s7_is_integer(p))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_integer(p) != 1)
    {fprintf(stderr, "%d: %s is not 1?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_symbol_set_value(sc, s7_make_symbol(sc, "my-1"), s7_make_integer(sc, 32));
  p = s7_name_to_value(sc, "my-1");
  if (s7_integer(p) != 32)
    {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p)); free(s1);}


  s7_define_macro(sc, "mac-plus", mac_plus, 2, 0, false, "plus adds its two arguments");
  p = s7_eval_c_string(sc, "(mac-plus 2 3)");
  if (s7_integer(p) != 5)
    {fprintf(stderr, "%d: %s is not 5?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  p1 = s7_apply_function(sc,
	s7_name_to_value(sc, "mac-plus"),
	s7_list(sc, 2, s7_make_integer(sc, 3), s7_make_integer(sc, 4)));
  p = s7_eval(sc, p1, s7_rootlet(sc));
  if ((!s7_is_integer(p)) ||
      (s7_integer(p) != 7))
    {char *s2; fprintf(stderr, "%d: %s -> %s is not 7?\n", __LINE__, s1 = TO_STR(p1), s2 = TO_STR(p)); free(s1); free(s2);}

  s7_define_macro(sc, "mac-plus-mv", mac_plus_mv, 2, 0, false, "macro values test");
  p = s7_eval_c_string(sc, "(let () (+ (mac-plus-mv 2 3)))");
  if (s7_integer(p) != 5)
    {fprintf(stderr, "%d: %s is not 5?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_values(sc, s7_list(sc, 2, s7_make_integer(sc, 1), s7_make_integer(sc, 2)));
  if (!s7_is_multiple_value(p))
    {fprintf(stderr, "%d: %s is not a multiple-values object?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  p = s7_values(sc, s7_nil(sc));
  if ((!s7_is_unspecified(sc, p)) || (p == s7_unspecified(sc)))
    {fprintf(stderr, "%d: %s is not a no-values object?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  p = s7_values(sc, s7_list(sc, 1, s7_f(sc)));
  if (s7_is_multiple_value(p))
    {fprintf(stderr, "%d: %s is a multiple-values object?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_define_semisafe_typed_function(sc, "open-plus", open_plus, 1, 0, true, plus_help, s7_make_circular_signature(sc, 1, 2, s7_make_symbol(sc, "number?"), s7_t(sc)));
  p = s7_sublet(sc, s7_nil(sc), s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "plus"), s7_name_to_value(sc, "plus")), s7_nil(sc)));
  s7_openlet(sc, p);
  p1 = s7_apply_function(sc, s7_name_to_value(sc, "open-plus"), s7_list(sc, 3, p, s7_make_integer(sc, 2), s7_make_integer(sc, 3)));
  if ((!s7_is_integer(p1)) ||
      (s7_integer(p1) != 7))
    {fprintf(stderr, "%d: %s is not 7?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}


  s7_eval_c_string(sc,  "(define my-vect (make-vector '(2 3 4) 0))");
  s7_eval_c_string(sc,  "(set! (my-vect 1 1 1) 32)");
  p1 = s7_name_to_value(sc, "my-vect");

  p = s7_vector_ref_n(sc,  p1, 3, 0LL, 0LL, 0LL);
  if (s7_integer(p) != 0)
    {fprintf(stderr, "%d: %s is not 0?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_vector_ref_n(sc,  p1, 3, 0LL, 0LL, 0LL);
  if (s7_integer(p) != 0)
    {fprintf(stderr, "%d: %s is not 0?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_vector_ref_n(sc,  p1, 3, 1LL, 1LL, 1LL);
  if (s7_integer(p) != 32)
    {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_vector_ref_n(sc,  p1, 3, 1LL, 1LL, 1LL);
  if (s7_integer(p) != 32)
    {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_vector_set_n(sc,  p1, TO_S7_INT(12), 3, 1LL, 1LL, 2LL);
  p = s7_vector_ref_n(sc,  p1, 3, 1LL, 1LL, 2LL);
  if (s7_integer(p) != 12)
    {fprintf(stderr, "%d: %s is not 12?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_vector_length(p1) != 24)
    {fprintf(stderr, "%d: (length %s) is not 24?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}
  if (s7_vector_rank(p1) != 3)
    {fprintf(stderr, "%d: (vector-dimensions %s) is not 3?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}

  {
    s7_int *dims, *offs;
    s7_pointer *els;
    s7_int ndims;

    ndims = s7_vector_rank(p1);
    dims = (s7_int *)malloc(ndims * sizeof(s7_int));
    offs = (s7_int *)malloc(ndims * sizeof(s7_int));
    s7_vector_dimensions(p1, dims, ndims);
    s7_vector_offsets(p1, offs, ndims);
    els = s7_vector_elements(p1);

    if (dims[0] != 2) fprintf(stderr, "%d: dims[0]: %" ld64 "?\n", __LINE__, dims[0]);
    if (dims[1] != 3) fprintf(stderr, "%d: dims[1]: %" ld64 "?\n", __LINE__, dims[1]);
    if (dims[2] != 4) fprintf(stderr, "%d: dims[2]: %" ld64 "?\n", __LINE__, dims[2]);
    if (offs[0] != 12) fprintf(stderr, "%d: offs[0]: %" ld64 "?\n", __LINE__, offs[0]);
    if (offs[1] != 4) fprintf(stderr, "%d: offs[1]: %" ld64 "?\n", __LINE__, offs[1]);
    if (s7_integer(p = els[12 + 4 + 1]) != 32)
      {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    free(dims);
    free(offs);
  }

  s7_vector_fill(sc, p1, s7_t(sc));
  p = s7_vector_ref_n(sc,  p1, 3, 1LL, 1LL, 1LL);
  if (p != s7_t(sc))
    {fprintf(stderr, "%d: %s is not #t?\n", __LINE__, s1 = TO_STR(p)); free(s1);}


  {
    s7_pointer new_env, old_env;
    new_env = s7_sublet(sc, old_env = s7_curlet(sc), s7_nil(sc));
    gc_loc = s7_gc_protect(sc, new_env);

    s7_define(sc, new_env, s7_make_symbol(sc, "var1"), s7_make_integer(sc, 32));

    if (new_env == s7_curlet(sc))
      {fprintf(stderr, "%d: %s is the current env?\n", __LINE__, s1 = TO_STR(new_env)); free(s1);}

    s1 = TO_STR(s7_let_to_list(sc, new_env));
    if (strcmp(s1, "((var1 . 32))") != 0)
      {fprintf(stderr, "%d: new-env is %s?\n", __LINE__, s1);}
    free(s1);

    p = s7_let_ref(sc, new_env, s7_make_symbol(sc, "var1"));
    if (s7_integer(p) != 32)
      {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    s7_let_set(sc, new_env, s7_make_symbol(sc, "var1"), TO_S7_INT(3));
    p = s7_let_ref(sc, new_env, s7_make_symbol(sc, "var1"));
    if (s7_integer(p) != 3)
      {fprintf(stderr, "%d: %s is not 3?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    s7_set_curlet(sc, new_env);
    p = s7_slot(sc, s7_make_symbol(sc, "var1"));
    if (s7_integer(s7_slot_value(p)) != 3)
      {fprintf(stderr, "%d: slot-value %s is not 3?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    s7_slot_set_value(sc, p, s7_f(sc));
    p = s7_let_ref(sc, new_env, s7_make_symbol(sc, "var1"));
    if (p != s7_f(sc))
      {fprintf(stderr, "%d: set slot-value %s is not #f?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if ((s7_outlet(sc, new_env) != old_env) && (old_env != s7_nil(sc)))
      {fprintf(stderr, "%d: outer-env %s?\n", __LINE__, s1 = TO_STR(old_env)); free(s1);}

    s7_make_slot(sc, new_env, s7_make_symbol(sc, "var2"), TO_S7_INT(-1));
    p = s7_let_ref(sc, new_env, s7_make_symbol(sc, "var2"));
    if (s7_integer(p) != -1)
      {fprintf(stderr, "%d: make_slot %s is not -1?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    s7_symbol_set_value(sc, s7_make_symbol(sc, "var2"), s7_t(sc));
    p = s7_symbol_local_value(sc, s7_make_symbol(sc, "var2"), new_env);
    if (p != s7_t(sc))
      {fprintf(stderr, "%d: set symbol-value %s is not #t?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    p = s7_let_to_list(sc, new_env);
    {
      int gloc;
      gloc = s7_gc_protect(sc, p);
      s1 = TO_STR(p);
      if ((strcmp(s1, "((var2 . #t) (var1 . #f))") != 0) && (strcmp(s1, "((var1 . #f) (var2 . #t))") != 0))
	{fprintf(stderr, "%d: env->list: %s\n", __LINE__, s1);}
      free(s1);
      s7_gc_unprotect_at(sc, gloc);
    }

    s7_define_constant_with_environment(sc, new_env, "new-env-var", s7_make_integer(sc, 123));
    if (s7_integer(s7_name_to_value(sc, "new-env-var")) != 123)
      fprintf(stderr, "%d: constant: %s\n", __LINE__, TO_STR(s7_name_to_value(sc, "new-env-var")));

    s7_set_curlet(sc, old_env);
    s7_gc_unprotect_at(sc, gc_loc);
  }

  {
    s7_pointer e, yp, old_e, arg;
    e = s7_sublet(sc, s7_curlet(sc), s7_nil(sc));
    s7_gc_protect_via_stack(sc, e);
    old_e = s7_set_curlet(sc, e);
    arg = s7_make_symbol(sc, "arg");
    yp = s7_make_slot(sc, e, arg, s7_make_mutable_real(sc, 1.0));
    if (s7_real(s7_slot_value(yp)) != 1.0)
      {fprintf(stderr, "%d: mutable real slot-value %s is not 1.0?\n", __LINE__, s1 = TO_STR(s7_slot_value(yp))); free(s1);}
    s7_slot_set_real_value(sc, yp, 2.0);
    if (s7_real(s7_slot_value(yp)) != 2.0)
      {fprintf(stderr, "%d: mutable real slot-value %s is not 2.0?\n", __LINE__, s1 = TO_STR(s7_slot_value(yp))); free(s1);}
    s7_varlet(sc, e, s7_make_symbol(sc, "new-var"), s7_make_integer(sc, 123));
    if (s7_integer(s7_name_to_value(sc, "new-var")) != 123)
      fprintf(stderr, "%d: new-var: %s\n", __LINE__, TO_STR(s7_name_to_value(sc, "new-var")));
    s7_set_curlet(sc, old_e);
    s7_gc_unprotect_via_stack(sc, e);
  }

  if (!s7_is_list(sc, p = s7_load_path(sc)))
    {fprintf(stderr, "%d: %s is not a list?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  s7_add_to_load_path(sc, "/home/bil/");
  if (!s7_is_pair(s7_member(sc, s7_make_string(sc, "/home/bil/"), s7_load_path(sc))))
    fprintf(stderr, "/home/bil/ not in *load-path*: %s\n", TO_STR(s7_load_path(sc)));

  {
    s7_pointer port;
    port = s7_open_output_file(sc, "ffitest.scm", "w");

    if (!s7_is_output_port(sc, port))
      {fprintf(stderr, "%d: %s is not an output port?\n", __LINE__, s1 = TO_STR(port)); free(s1);}
    else
      {
	/* (define loaded_var 321) hopefully */
	gc_loc = s7_gc_protect(sc, port);
	s7_write_char(sc, s7_make_character(sc, (uint8_t)'('), port);
	s7_write(sc, s7_make_symbol(sc, "define"), port);
	s7_write_char(sc, s7_make_character(sc, (uint8_t)' '), port);
	s7_display(sc, s7_make_symbol(sc, "loaded_var"), port);
	s7_write_char(sc, s7_make_character(sc, (uint8_t)' '), port);
	s7_format(sc, s7_list(sc, 3, port, s7_make_string(sc, "~A)"), TO_S7_INT(321)));
	s7_newline(sc, port);
	s7_flush_output_port(sc, port);
	s7_close_output_port(sc, port);
	s7_gc_unprotect_at(sc, gc_loc);

	s7_load(sc, "ffitest.scm");
	if (!s7_is_defined(sc, "loaded_var"))
	  {fprintf(stderr, "%d: load ffitest.scm unhappy?\n", __LINE__);}
	else
	  {
	    if (s7_integer(p = s7_name_to_value(sc, "loaded_var")) != 321)
	      {fprintf(stderr, "%d: %s is not 321?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

	    port = s7_open_input_file(sc, "ffitest.scm", "r");
	    if (!s7_is_input_port(sc, port))
	      {fprintf(stderr, "%d: %s is not an input port?\n", __LINE__, s1 = TO_STR(port)); free(s1);}
	    else
	      {
		uint8_t c;
		const char *filename;
		gc_loc = s7_gc_protect(sc, port);
		c = s7_character(s7_peek_char(sc, port));
		if (c != (int)'(')
		  {fprintf(stderr, "%d: peek-char sees %c?\n", __LINE__, (unsigned char)c);}

		c = s7_character(s7_read_char(sc, port));
		if (c != (uint8_t)'(')
		  {fprintf(stderr, "%d: read-char sees %c?\n", __LINE__, (unsigned char)c);}

		filename = s7_port_filename(sc, port);
		if (strcmp(filename, "ffitest.scm") != 0)
		  fprintf(stderr, "%d: s7_port_filename: %s\n", __LINE__, filename);

		s7_close_input_port(sc, port);
		s7_gc_unprotect_at(sc, gc_loc);

		port = s7_open_input_file(sc, "ffitest.scm", "r");
		gc_loc = s7_gc_protect(sc, port);

		p = s7_read(sc, port);
		s1 = TO_STR(p);
		if (strcmp(s1, "(define loaded_var 321)") != 0)
		  {fprintf(stderr, "%d: read file sees %s?\n", __LINE__, s1);}
		free(s1);

		s7_close_input_port(sc, port);
		s7_gc_unprotect_at(sc, gc_loc);
	      }
	  }
      }

    {
      s7_pointer e, val;
      e = s7_inlet(sc, s7_nil(sc));
      gc_loc = s7_gc_protect(sc, e);
      val = s7_load_with_environment(sc, "~/ffitest.scm", e);
      if (val)
	fprintf(stderr, "%d: load ~/ffitest.scm found!?\n", __LINE__);
      val = s7_load_with_environment(sc, "~/cl/ffitest.scm", e);
      if (!val)
	fprintf(stderr, "%d: load ~/cl/ffitest.scm not found\n", __LINE__);
      else
	{
	  if (s7_symbol_local_value(sc, s7_make_symbol(sc, "loaded_var"), e) == s7_undefined(sc))
	    {fprintf(stderr, "%d: load ~/ffitest.scm unhappy? %s\n", __LINE__, s1 = TO_STR(e)); free(s1);}
	}
      val = s7_load(sc, "/home/bil/snd-motif/");
      if (val)
	fprintf(stderr, "s7_load(directory) did not fail?\n");
      s7_gc_unprotect_at(sc, gc_loc);
    }

    {
      s7_pointer e;
      unsigned char another_var[] = {0x28, 0x64, 0x65, 0x66, 0x69, 0x6e, 0x65, 0x20, 0x61, 0x6e, 0x6f, 0x74, /* (define another-var 123)\n */
				     0x68, 0x65, 0x72, 0x2d, 0x76, 0x61, 0x72, 0x20, 0x31, 0x32, 0x33, 0x29,
				     0x0a, 0};
      unsigned int another_var_len = 25;
      unsigned char yet_another_var[] = {0x28, 0x64, 0x65, 0x66, 0x69, 0x6e, 0x65, 0x20, 0x79, 0x65, 0x74, 0x2d, /* (define yet-another-var 123)\n */
					 0x61, 0x6e, 0x6f, 0x74, 0x68, 0x65, 0x72, 0x2d, 0x76, 0x61, 0x72, 0x20,
					 0x31, 0x32, 0x33, 0x29, 0x0a, 0};
      unsigned int yet_another_var_len = 29;
      unsigned char a_global_var[] = {0x28, 0x64, 0x65, 0x66, 0x69, 0x6e, 0x65, 0x20, 0x61, 0x2d, 0x67, 0x6c, /* (define a_global_var 321)\n */
				      0x6f, 0x62, 0x61, 0x6c, 0x2d, 0x76, 0x61, 0x72, 0x20, 0x33, 0x32, 0x31,
				      0x29, 0x0a, 0};
      unsigned int a_global_var_len = 26;

      unsigned char a_test_var[] = {0x28, 0x64, 0x65, 0x66, 0x69, 0x6e, 0x65, 0x20, 0x61, 0x2d, 0x74, 0x65,
				    0x73, 0x74, 0x2d, 0x76, 0x61, 0x72, 0x20, 0x33, 0x32, 0x31, 0x29, 0x0a, 0};
      unsigned int a_test_var_len = 24;

      s7_load_c_string(sc, (const char *)another_var, another_var_len);
      p = s7_symbol_value(sc, s7_make_symbol(sc, "another-var"));
      if (s7_integer(p) != 123)
	fprintf(stderr, "load_c_string: %s\n", TO_STR(p));

      e = s7_inlet(sc, s7_nil(sc));
      gc_loc = s7_gc_protect(sc, e);
      s7_load_c_string_with_environment(sc, (const char *)yet_another_var, yet_another_var_len, e);
      p = s7_symbol_local_value(sc, s7_make_symbol(sc, "yet-another-var"), e);
      if (s7_integer(p) != 123)
	fprintf(stderr, "load_c_string_with_environment: %s\n", TO_STR(p));
      s7_gc_unprotect_at(sc, gc_loc);

      s7_load_c_string_with_environment(sc, (const char *)a_global_var, a_global_var_len, s7_nil(sc));
      p = s7_symbol_value(sc, s7_make_symbol(sc, "a-global-var"));
      if (s7_integer(p) != 321)
	fprintf(stderr, "load_c_string_with_environment nil: %s\n", TO_STR(p));

      s7_load_c_string_with_environment(sc, (const char *)a_test_var, a_test_var_len, s7_rootlet(sc));
      p = s7_symbol_value(sc, s7_make_symbol(sc, "a-test-var"));
      if (s7_integer(p) != 321)
	fprintf(stderr, "load_c_string_with_environment rootlet: %s\n", TO_STR(p));

      s7_load_with_environment(sc, "~/cl/ffitest.scm", s7_rootlet(sc));  /* rootlet=segfault 10-Jul-21 */
    }

    port = s7_open_input_string(sc, "(+ 1 2)");
    if (!s7_is_input_port(sc, port))
      {fprintf(stderr, "%d: %s is not an input port?\n", __LINE__, s1 = TO_STR(port)); free(s1);}
    gc_loc = s7_gc_protect(sc, port);
    p = s7_read(sc, port);
    s1 = TO_STR(p);
    if (strcmp(s1, "(+ 1 2)") != 0)
      {fprintf(stderr, "%d: read string sees %s?\n", __LINE__, s1);}
    free(s1);
    s7_close_input_port(sc, port);
    s7_gc_unprotect_at(sc, gc_loc);

    /* make sure s7_read does not ignore #<eof> */
    port = s7_open_input_string(sc, "(define aaa 32)\n(define bbb 33)\n");
    if (!s7_is_input_port(sc, port))
      {fprintf(stderr, "%d: %s is not an input port?\n", __LINE__, s1 = TO_STR(port)); free(s1);}
    gc_loc = s7_gc_protect(sc, port);
    while(true)
      {
	s7_pointer code;
	code = s7_read(sc, port);
	if (code == s7_eof_object(sc)) break;
	s7_eval(sc, code, s7_nil(sc));
      }
    s7_close_input_port(sc, port);
    s7_gc_unprotect_at(sc, gc_loc);

    port = s7_open_input_string(sc, "(define ccc 34)\n(define ddd 35)");
    if (!s7_is_input_port(sc, port))
      {fprintf(stderr, "%d: %s is not an input port?\n", __LINE__, s1 = TO_STR(port)); free(s1);}
    gc_loc = s7_gc_protect(sc, port);
    while(true)
      {
	s7_pointer code;
	code = s7_read(sc, port);
	if (code == s7_eof_object(sc)) break;
	s7_eval(sc, code, s7_nil(sc));
      }
    s7_close_input_port(sc, port);
    s7_gc_unprotect_at(sc, gc_loc);
    {
      s7_pointer val;
      val = s7_name_to_value(sc, "aaa");
      if ((!s7_is_integer(val)) || (s7_integer(val) != 32))
	fprintf(stderr, "aaa: %s\n", s7_object_to_c_string(sc, val));
      val = s7_name_to_value(sc, "bbb");
      if ((!s7_is_integer(val)) || (s7_integer(val) != 33))
	fprintf(stderr, "bbb: %s\n", s7_object_to_c_string(sc, val));
      val = s7_name_to_value(sc, "ccc");
      if ((!s7_is_integer(val)) || (s7_integer(val) != 34))
	fprintf(stderr, "ccc: %s\n", s7_object_to_c_string(sc, val));
      val = s7_name_to_value(sc, "ddd");
      if ((!s7_is_integer(val)) || (s7_integer(val) != 35))
	fprintf(stderr, "ddd: %s\n", s7_object_to_c_string(sc, val));
    }

    port = s7_open_output_string(sc);
    if (!s7_is_output_port(sc, port))
      {fprintf(stderr, "%d: %s is not an output port?\n", __LINE__, s1 = TO_STR(port)); free(s1);}
    gc_loc = s7_gc_protect(sc, port);
    s7_display(sc, s7_make_string(sc, "(+ 2 3)"), port);
    {
      const char *s2;
      s7_pointer s3;
      s2 = s7_get_output_string(sc, port);
      if (strcmp(s2, "(+ 2 3)") != 0)
	{fprintf(stderr, "%d: s7_get_output_string returns %s?\n", __LINE__, s2);}
      s3 = s7_output_string(sc, port);
      if ((!s7_is_string(s3)) ||
	  (strcmp(s7_string(s3), "(+ 2 3)") != 0))
	{fprintf(stderr, "%d: s7_output_string returns %s?\n", __LINE__, s2);}
    }
    s7_close_output_port(sc, port);
    s7_gc_unprotect_at(sc, gc_loc);

    p = s7_set_current_output_port(sc, s7_open_output_function(sc, my_print));
    p1 = s7_open_input_function(sc, my_read);
    gc_loc = s7_gc_protect(sc, p1);

    s7_display(sc, s7_make_character(sc, '3'), s7_current_output_port(sc));
    if (last_c != '3')
      {fprintf(stderr, "%d: last_c: %c, c: %c\n", __LINE__, last_c, '3');}
    last_c = s7_character(s7_read_char(sc, p1));
    if (last_c != '0')
      {fprintf(stderr, "%d: last_c: %c\n", __LINE__, last_c);}
    s7_set_current_output_port(sc, p);
    s7_gc_unprotect_at(sc, gc_loc);
  }

  {
    s7_pointer port, val;
    s7_autoload(sc, s7_make_symbol(sc, "auto_var"), s7_make_string(sc, "ffitest.scm"));
    port = s7_open_output_file(sc, "ffitest.scm", "w");
    gc_loc = s7_gc_protect(sc, port);
    s7_display(sc, s7_make_string(sc, "(define auto_var 123)"), port);
    s7_newline(sc, port);
    s7_close_output_port(sc, port);
    s7_gc_unprotect_at(sc, gc_loc);
    val = s7_eval_c_string(sc, "(+ auto_var 1)");
    if ((!s7_is_integer(val)) ||
	(s7_integer(val) != 124))
      {fprintf(stderr, "%d: auto_var+1 = %s?\n", __LINE__, s1 = TO_STR(val)); free(s1);}
    s7_autoload_set_names(sc, snd_names, 8);
  }

  {
    s7_pointer test_hook;
    test_hook = s7_eval_c_string(sc, "(make-hook 'a 'b)");
    s7_define_constant(sc, "test-hook", test_hook);
    s7_hook_set_functions(sc, test_hook,
			  s7_cons(sc, s7_make_function(sc, "test-hook-function", test_hook_function, 1, 0, false, "a test-hook function"),
				  s7_hook_functions(sc, test_hook)));
    s7_call(sc, test_hook, s7_list(sc, 2, TO_S7_INT(1), TO_S7_INT(2)));
    s7_call_with_location(sc, test_hook, s7_list(sc, 2, TO_S7_INT(1), TO_S7_INT(2)), "ffitest", "ffitest.c", __LINE__);
  }

  {
    s7_pointer x, kar; /* from s7.h */
    kar = s7_make_function(sc, "kar", g_car, 1, 0, false, "(car obj)");
    x = s7_call(sc, kar, s7_cons(sc, s7_cons(sc, s7_make_integer(sc, 123), s7_nil(sc)), s7_nil(sc)));
    if ((!s7_is_integer(x)) ||
	(s7_integer(x) != 123))
      fprintf(stderr, "s7_call x: %s\n", s7_object_to_c_string(sc, x));
  }

  {
    s7_pointer x, y, funcs;
    funcs = s7_eval_c_string(sc, "(let ((x 0)) (list (lambda () (set! x 1)) (lambda () (set! x (+ x 1))) (lambda () (set! x (+ x 1))) (lambda () x)))");
    gc_loc = s7_gc_protect(sc, funcs);
    y = s7_dynamic_wind(sc, s7_car(funcs), s7_cadr(funcs), s7_caddr(funcs));
    x = s7_call(sc, s7_cadddr(funcs), s7_nil(sc));
    if ((!s7_is_integer(x)) ||
	(!s7_is_integer(y)) ||
	(s7_integer(x) != 3) ||
	(s7_integer(y) != 2))
      fprintf(stderr, "s7_dynamic_wind: x: %s, y: %s\n", s7_object_to_c_string(sc, x), s7_object_to_c_string(sc, y));
    y = s7_dynamic_wind(sc, s7_f(sc), s7_car(funcs), s7_cadr(funcs));
    x = s7_call(sc, s7_cadddr(funcs), s7_nil(sc));
    if ((!s7_is_integer(x)) ||
	(!s7_is_integer(y)) ||
	(s7_integer(x) != 2) ||
	(s7_integer(y) != 1))
      fprintf(stderr, "s7_dynamic_wind (init #f): x: %s, y: %s\n", s7_object_to_c_string(sc, x), s7_object_to_c_string(sc, y));
    y = s7_dynamic_wind(sc, s7_f(sc), s7_cadr(funcs), s7_f(sc));
    x = s7_call(sc, s7_cadddr(funcs), s7_nil(sc));
    if ((!s7_is_integer(x)) ||
	(!s7_is_integer(y)) ||
	(s7_integer(x) != 3) ||
	(s7_integer(y) != 3))
      fprintf(stderr, "s7_dynamic_wind (init #f, finish #f): x: %s, y: %s\n", s7_object_to_c_string(sc, x), s7_object_to_c_string(sc, y));
    y = s7_dynamic_wind(sc, s7_cadr(funcs), s7_cadr(funcs), s7_f(sc));
    x = s7_call(sc, s7_cadddr(funcs), s7_nil(sc));
    if ((!s7_is_integer(x)) ||
	(!s7_is_integer(y)) ||
	(s7_integer(x) != 5) ||
	(s7_integer(y) != 5))
      fprintf(stderr, "s7_dynamic_wind (finish #f): x: %s, y: %s\n", s7_object_to_c_string(sc, x), s7_object_to_c_string(sc, y));
    s7_gc_unprotect_at(sc, gc_loc);
  }

  if (s7_begin_hook(sc))
    {fprintf(stderr, "%d: begin_hook is not null?\n", __LINE__);}
  tested_begin_hook = false;
  s7_set_begin_hook(sc, test_begin_hook);
  s7_eval_c_string(sc, "(begin #f (+ 1 2))");
  if (!tested_begin_hook)
    {fprintf(stderr, "%d: begin_hook not called?\n", __LINE__);}
  if (s7_begin_hook(sc) != test_begin_hook)
    {fprintf(stderr, "%d: begin_hook was not set?\n", __LINE__);}
  s7_set_begin_hook(sc, NULL);


  p1 = s7_name_to_value(sc, "abs");
  if (!s7_is_procedure(p1))
    {fprintf(stderr, "%d: (procedure? abs) = #f?\n", __LINE__);}
  if (s7_is_macro(sc, p1))
    {fprintf(stderr, "%d: (macro? abs) = #t?\n", __LINE__);}

  if (!s7_is_aritable(sc, p1, 1))
    {fprintf(stderr, "%d: (aritable? abs 1) = #f?\n", __LINE__);}
  if (s7_is_aritable(sc, p1, 2))
    {fprintf(stderr, "%d: (aritable? abs 2) = #t?\n", __LINE__);}
  p = s7_arity(sc, p1);
  if ((!s7_is_pair(p)) || (s7_integer(s7_car(p)) != 1) || (s7_integer(s7_cdr(p)) != 1))
    {fprintf(stderr, "%d: (arity abs) = %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_is_proper_list(sc, p))
    fprintf(stderr, "%d: arity is a proper-list?\n", __LINE__);

  p = s7_funclet(sc, p1);
  if (p != s7_rootlet(sc))
    {fprintf(stderr, "%d: (funclet abs) = %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  {
    const char *s3;
    s3 = s7_documentation(sc, p1);
    if (strcmp(s3, "(abs x) returns the absolute value of the real number x") != 0)
      {fprintf(stderr, "%d: (documentation abs) = %s?\n", __LINE__, s3);}

    s3 = s7_help(sc, p1);
    if (strcmp(s3, "(abs x) returns the absolute value of the real number x") != 0)
      {fprintf(stderr, "%d: (help abs) = %s?\n", __LINE__, s3);}

    s3 = s7_documentation(sc, s7_make_symbol(sc, "abs"));
    if (strcmp(s3, "(abs x) returns the absolute value of the real number x") != 0)
      {fprintf(stderr, "%d: (documentation 'abs) = %s?\n", __LINE__, s3);}
  }

  p = s7_eval_c_string(sc, "(lambda (a b . c) (+ a b (apply * c)))");
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_procedure(p))
    {fprintf(stderr, "%d: %s is not a procedure?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s1 = TO_STR(s7_closure_body(sc, p));
  if (strcmp(s1, "((+ a b (apply * c)))") != 0)
    {fprintf(stderr, "%d: s7_closure_body is %s?\n", __LINE__, s1);}
  free(s1);

  s1 = TO_STR(s7_closure_args(sc, p));
  if (strcmp(s1, "(a b . c)") != 0)
    {fprintf(stderr, "%d: s7_closure_args is %s?\n", __LINE__, s1);}
  free(s1);

  s1 = TO_STR(s7_closure_let(sc, p));
  if (strcmp(s1, "()") != 0)
    {fprintf(stderr, "%d: s7_closure_let is %s?\n", __LINE__, s1);}
  free(s1);

  if (s7_closure_body(sc, s7_name_to_value(sc, "abs")) != s7_nil(sc))
    fprintf(stderr, "closure_body(abs) is not nil?\n");
  if (s7_closure_args(sc, s7_name_to_value(sc, "abs"))  != s7_nil(sc))
    fprintf(stderr, "closure_args(abs) is not nil?\n");
  if (s7_closure_let(sc, s7_name_to_value(sc, "abs"))  != s7_nil(sc))
    fprintf(stderr, "closure_let(abs) is not nil?\n");

  if (!s7_is_aritable(sc, p, 2))
    {fprintf(stderr, "%d: aritable? lambda 2 = #f?\n", __LINE__);}
  if (s7_is_aritable(sc, p, 1))
    {fprintf(stderr, "%d: aritable? lambda 1 = #t?\n", __LINE__);}

  s7_gc_unprotect_at(sc, gc_loc);

  {
    /* iterators */
    s7_pointer iter, hash, x;
    s7_int gc1, gc2;

    /* iterate over list */
    iter = s7_make_iterator(sc, s7_list(sc, 3, TO_S7_INT(1), TO_S7_INT(2), TO_S7_INT(3)));
    if (!s7_is_iterator(iter))
      fprintf(stderr, "%d: %s is not an iterator\n", __LINE__, TO_STR(iter));
    if (s7_iterator_is_at_end(sc, iter))
      fprintf(stderr, "%d: %s is prematurely done\n", __LINE__, TO_STR(iter));
    x = s7_iterate(sc, iter);
    if ((!s7_is_integer(x)) || (s7_integer(x) != 1))
      fprintf(stderr, "%d: %s should be 1\n", __LINE__, TO_STR(x));
    x = s7_iterate(sc, iter);
    if ((!s7_is_integer(x)) || (s7_integer(x) != 2))
      fprintf(stderr, "%d: %s should be 2\n", __LINE__, TO_STR(x));
    x = s7_iterate(sc, iter);
    if ((!s7_is_integer(x)) || (s7_integer(x) != 3))
      fprintf(stderr, "%d: %s should be 3\n", __LINE__, TO_STR(x));
    x = s7_iterate(sc, iter);
    if ((x != s7_eof_object(sc)) || (!s7_iterator_is_at_end(sc, iter)))
      fprintf(stderr, "%d: %s should be #<eof> and iter should be done\n", __LINE__, TO_STR(x));

    /* iterate over hash table */
    hash = s7_make_hash_table(sc, 8);
    gc1 = s7_gc_protect(sc, hash);
    s7_hash_table_set(sc, hash, s7_make_symbol(sc, "a"), s7_make_integer(sc, 1));
    s7_hash_table_set(sc, hash, s7_make_symbol(sc, "b"), s7_make_integer(sc, 2));
    iter = s7_make_iterator(sc, hash);
    gc2 = s7_gc_protect(sc, iter);
    x = s7_iterate(sc, iter);
    if (!s7_is_pair(x))
      fprintf(stderr, "x: %s\n", s7_object_to_c_string(sc, x));
    x = s7_iterate(sc, iter);
    if (!s7_is_pair(x))
      fprintf(stderr, "x: %s\n", s7_object_to_c_string(sc, x));
    x = s7_iterate(sc, iter);
    if (!s7_is_eq(s7_eof_object(sc), x))
      fprintf(stderr, "x: %s\n", s7_object_to_c_string(sc, x));
    s7_gc_unprotect_at(sc, gc1);
    s7_gc_unprotect_at(sc, gc2);
  }

  g_block_type = s7_make_c_type(sc, "<block>");
  s7_c_type_set_free(sc, g_block_type, g_block_free);
  s7_c_type_set_equal(sc, g_block_type, g_blocks_are_eql);
  s7_c_type_set_is_equal(sc, g_block_type, g_blocks_are_equal);
  s7_c_type_set_is_equivalent(sc, g_block_type, g_blocks_are_equivalent);
  s7_c_type_set_mark(sc, g_block_type, g_block_mark);
  s7_c_type_set_ref(sc, g_block_type, g_block_ref);
  s7_c_type_set_set(sc, g_block_type, g_block_set);
  s7_c_type_set_length(sc, g_block_type, g_block_length);
  s7_c_type_set_copy(sc, g_block_type, g_block_copy);
  s7_c_type_set_reverse(sc, g_block_type, g_block_reverse);
  s7_c_type_set_fill(sc, g_block_type, g_block_fill);
  s7_c_type_set_to_string(sc, g_block_type, g_block_to_string);

  s7_define_function(sc, "make-block", g_make_block, 1, 0, false, g_make_block_help);
  s7_define_function(sc, "block", g_to_block, 0, 0, true, g_block_help);

  g_block_methods = s7_eval_c_string(sc, "(inlet (cons 'vector? (lambda (p) #t)))");
  s7_gc_protect(sc, g_block_methods);

  {
    g_block *g;
    s7_pointer gp;

    gp = g_make_block(sc, s7_list(sc, 1, TO_S7_INT(32)));
    gc_loc = s7_gc_protect(sc, gp);
    if (!s7_is_c_object(gp))
      {fprintf(stderr, "%d: g_block %s is not a c_object?\n", __LINE__, s1 = TO_STR(gp)); free(s1);}
    g = (g_block *)s7_c_object_value(gp);
    if (s7_c_object_type(gp) != g_block_type)
      {fprintf(stderr, "%d: g_block types: %" ld64 " %" ld64 "\n", __LINE__, g_block_type, s7_c_object_type(gp));}
    if (s7_c_object_value_checked(gp, g_block_type) != g)
      {fprintf(stderr, "%d: checked g_block types: %" ld64 " %" ld64 "\n", __LINE__, g_block_type, s7_c_object_type(gp));}
    if (s7_c_object_let(gp) != g_block_methods)
      fprintf(stderr, "%d: s7_c_object_let trouble\n", __LINE__);
    s7_gc_unprotect_at(sc, gc_loc);
  }

  {
    s7_pointer old_port;
    const char *errmsg = NULL;

    old_port = s7_set_current_error_port(sc, s7_open_output_string(sc));
    gc_loc = s7_gc_protect(sc, old_port);

    s7_eval_c_string(sc, "(+ 1 #\\c)");
    errmsg = s7_get_output_string(sc, s7_current_error_port(sc));
    if (!errmsg)
      fprintf(stderr, "%d: no error!\n", __LINE__);

    s7_close_output_port(sc, s7_current_error_port(sc));
    s7_set_current_error_port(sc, old_port);
    s7_gc_unprotect_at(sc, gc_loc);
  }

  {
    int gc_loc1;
    s7_pointer old_port, result, func;
    const char *errmsg = NULL;

    s7_define_unsafe_typed_function(sc, "error-handler", test_error_handler, 1, 0, false, "our error handler", s7_make_signature(sc, 2, s7_t(sc), s7_t(sc)));

    s7_eval_c_string(sc, "(set! (hook-functions *error-hook*)                                 \n\
                            (list (lambda (hook)                                              \n\
                                    (error-handler                                            \n\
				     (string-append \"hook: \" (apply format #f (hook 'data)))) \n\
                                    (set! (hook 'result) 'our-error))))");

    old_port = s7_set_current_error_port(sc, s7_open_output_string(sc));
    gc_loc = s7_gc_protect(sc, old_port);

    result = s7_eval_c_string(sc, "(+ 1 #\\c)");
    if (result != s7_make_symbol(sc, "our-error"))
      {fprintf(stderr, "%d: error hook result: %s\n", __LINE__, s1 = TO_STR(result)); free(s1);}
    errmsg = s7_get_output_string(sc, s7_current_error_port(sc));
    if ((errmsg) && (*errmsg))
      {
	if (strcmp(errmsg, "error!") != 0)
	  fprintf(stderr, "%d: error: %s\n", __LINE__, errmsg);
      }
    else fprintf(stderr, "%d: no error!\n", __LINE__);

    s7_close_output_port(sc, s7_current_error_port(sc));
    s7_set_current_error_port(sc, old_port);
    s7_gc_unprotect_at(sc, gc_loc);


    old_port = s7_set_current_error_port(sc, s7_open_output_string(sc));
    gc_loc = s7_gc_protect(sc, old_port);

    func = s7_eval_c_string(sc, "(lambda (x) (+ x 1))");
    result = s7_call(sc, func, s7_list(sc, 1, s7_make_integer(sc, 2)));
    if ((!s7_is_integer(result)) || (s7_integer(result) != 3))
      {fprintf(stderr, "%d: s7_call (x+1) result: %s\n", __LINE__, s1 = TO_STR(result)); free(s1);}

    result = s7_call(sc, func, s7_list(sc, 1, s7_make_vector(sc, 0)));
    if (result != s7_make_symbol(sc, "our-error"))
      {fprintf(stderr, "%d: s7_call error hook result: %s\n", __LINE__, s1 = TO_STR(result)); free(s1);}
    errmsg = s7_get_output_string(sc, s7_current_error_port(sc));
    if ((errmsg) && (*errmsg))
      {
	if (strcmp(errmsg, "error!") != 0)
	  fprintf(stderr, "%d: error: %s\n", __LINE__, errmsg);
      }
    else fprintf(stderr, "%d: no error!\n", __LINE__);

    s7_close_output_port(sc, s7_current_error_port(sc));
    s7_set_current_error_port(sc, old_port);
    s7_gc_unprotect_at(sc, gc_loc);


    old_port = s7_set_current_error_port(sc, s7_open_output_string(sc));
    gc_loc = s7_gc_protect(sc, old_port);

    func = s7_eval_c_string(sc, "(let ((x 0)) (list (lambda () (set! x 1)) (lambda () (set! x (+ x #()))) (lambda () (set! x (+ x 1))) (lambda () x)))");
    gc_loc1 = s7_gc_protect(sc, func);
    result = s7_dynamic_wind(sc, s7_car(func), s7_cadr(func), s7_caddr(func));

    if (result != s7_make_symbol(sc, "our-error"))
      {fprintf(stderr, "%d: s7_dynamic_wind error hook result: %s\n", __LINE__, s1 = TO_STR(result)); free(s1);}
    errmsg = s7_get_output_string(sc, s7_current_error_port(sc));
    if ((errmsg) && (*errmsg))
      {
	if (strcmp(errmsg, "error!") != 0)
	  fprintf(stderr, "%d: error: %s\n", __LINE__, errmsg);
      }
    else fprintf(stderr, "%d: no error!\n", __LINE__);

    s7_close_output_port(sc, s7_current_error_port(sc));
    s7_set_current_error_port(sc, old_port);
    s7_gc_unprotect_at(sc, gc_loc);
    s7_gc_unprotect_at(sc, gc_loc1);


    s7_eval_c_string(sc, "(set! (hook-functions *error-hook*) ())");
  }

#if WITH_GMP
  s7_define_function(sc, "add-1", big_add_1, 1, 0, false, "(add-1 num) adds 1 to num");
  p = s7_eval_c_string(sc, "(add-1 (*s7* 'most-positive-fixnum))");
  if ((!s7_is_bignum(p)) || (!s7_is_big_integer(p))) {fprintf(stderr, "add-1: %s\n", s1 = TO_STR(p)); free(s1);}
  {
    mpz_t val, val1;
    mpz_init_set(val, *s7_big_integer(p));
    mpz_init(val1);
    mpz_set_si(val1, s7_integer(s7_starlet_ref(sc, s7_make_symbol(sc, "most-positive-fixnum"))));
    mpz_add_ui(val1, val1, 1);
    if (mpz_cmp(val, val1) != 0) {fprintf(stderr, "add-1: %s\n", s1 = TO_STR(p)); free(s1);}
    mpz_clear(val);
    mpz_clear(val1);
  }
#endif

  s7_define_function(sc, "notify-C", scheme_set_notification, 2, 0, false, "called if notified-var is set!");
  s7_define_variable(sc, "notified-var", s7_make_integer(sc, 0));
  s7_set_setter(sc, s7_make_symbol(sc, "notified-var"), s7_name_to_value(sc, "notify-C"));
  s7_eval_c_string(sc, "(set! notified-var 32)");
  p = s7_name_to_value(sc, "notified-var");
  if (s7_integer(p) != 32)
    {fprintf(stderr, "%d: sym set: %s\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_integer(set_val) != 32)
    {fprintf(stderr, "%d: sym val: %s\n", __LINE__, s1 = TO_STR(set_val)); free(s1);}
  if (set_sym != s7_make_symbol(sc, "notified-var"))
    {fprintf(stderr, "%d: sym: %s\n", __LINE__, s1 = TO_STR(set_sym)); free(s1);}

  {
    s7_pointer e, val;
    e = s7_inlet(sc, s7_list(sc, 2, s7_make_symbol(sc, "init_func"), s7_make_symbol(sc, "block_init")));
    gc_loc = s7_gc_protect(sc, e);
    val = s7_load_with_environment(sc, "s7test-block.so", e);
    if (!val)
      fprintf(stderr, "can't load s7test-block.so\n");
    s7_gc_unprotect_at(sc, gc_loc);
  }

  {
    s7_pointer body, err, result;
    s7_int gc_body, gc_err;
    body = s7_eval_c_string(sc, "(lambda () (+ 1 2))");
    gc_body = s7_gc_protect(sc, body);
    err = s7_eval_c_string(sc, "(lambda (type info) 'oops)");
    gc_err = s7_gc_protect(sc, err);
    result = s7_call_with_catch(sc, s7_t(sc), body, err);                                                     /* Ln L */
    if ((!s7_is_integer(result)) || (s7_integer(result) != 3))
      {fprintf(stderr, "catch (3): %s\n", s1 = TO_STR(result)); free(s1);}
    s7_gc_unprotect_at(sc, gc_body);
    s7_gc_unprotect_at(sc, gc_err);

    body = s7_eval_c_string(sc, "(lambda () (+ #f 2))");                                                      /* Le L */
    gc_body = s7_gc_protect(sc, body);
    result = s7_call_with_catch(sc, s7_t(sc), body, err);
    if (result != s7_make_symbol(sc, "oops"))
      {fprintf(stderr, "catch (oops): %s\n", s1 = TO_STR(result)); free(s1);}
    s7_gc_unprotect_at(sc, gc_body);

    body = s7_make_function(sc, "my-error", my_error, 0, 0, false, "call s7_error");
    gc_body = s7_gc_protect(sc, body);
    result = s7_call_with_catch(sc, s7_t(sc), body, s7_eval_c_string(sc, "(lambda (type info) (car info))")); /* Ce L */
    if (s7_integer(result) != 60)
      /* '(60) returned from my_error via its error handler, car(info) -> 60 above */
      fprintf(stderr, "%d: catch my_error via car: %s\n", __LINE__, TO_STR(result));
    s7_gc_unprotect_at(sc, gc_body);

    err = s7_make_function(sc, "my-error-handler", my_error_handler, 2, 0, false, "handle error");
    gc_err = s7_gc_protect(sc, err);
    result = s7_call_with_catch(sc, s7_t(sc), body, err);                                                     /* Ce C */
    if (s7_integer(result) != 60)
      fprintf(stderr, "%d: catch my_error via my_error_handler: %s\n", __LINE__, TO_STR(result));

    body = s7_make_function(sc, "my-no-error", my_no_error, 0, 0, false, "don't call s7_error");
    gc_body = s7_gc_protect(sc, body);
    result = s7_call_with_catch(sc, s7_t(sc), body, s7_eval_c_string(sc, "(lambda (type info) (car info))")); /* Cn L */
    if (s7_integer(result) != 30)
      fprintf(stderr, "%d: catch my_no_error: %s\n", __LINE__, TO_STR(result));

    result = s7_call_with_catch(sc, s7_t(sc), s7_eval_c_string(sc, "(lambda () (+ #f 2))"), err);
    if ((!s7_is_string(result)) || (strcmp(s7_string(result), "~A ~:D argument, ~S, is ~A but should be ~A") != 0))
      fprintf(stderr, "%d: catch (+ #f 2) via my_error_handler: %s\n", __LINE__, TO_STR(result));
    s7_gc_unprotect_at(sc, gc_err);

    err = s7_make_function(sc, "my-error-handler", my_error_handler, 2, 0, false, "handle error");
    gc_err = s7_gc_protect(sc, err);
    result = s7_call_with_catch(sc, s7_t(sc), body, err);                                                     /* Cn C */
    if (s7_integer(result) != 30)
      fprintf(stderr, "%d: catch my_error via my_error_handler: %s\n", __LINE__, TO_STR(result));
    s7_gc_unprotect_at(sc, gc_body);

    body = s7_eval_c_string(sc, "(lambda () (+ 1 2))");
    gc_body = s7_gc_protect(sc, body);
    result = s7_call_with_catch(sc, s7_t(sc), body, err);                                                     /* Ln C */
    if ((!s7_is_integer(result)) || (s7_integer(result) != 3))
      {fprintf(stderr, "%d: catch (3): %s\n", __LINE__, s1 = TO_STR(result)); free(s1);}
    s7_gc_unprotect_at(sc, gc_body);
    s7_gc_unprotect_at(sc, gc_err);
  }

  {
    s7_pointer make_func2, catcher2;
    make_func2 = s7_make_function(sc, "bad-func-define", ter_bad_func, 0, 0, false, NULL);
    catcher2 = s7_make_function(sc, "error-handler", ter_error_handler, 2, 0, false, NULL);
    s7_call_with_catch(sc, s7_t(sc), make_func2, catcher2);
    s7_call_with_catch(sc, s7_t(sc), make_func2, catcher2);
  }

  {
    s7_pointer result1, result2;
    s7_define_function(sc, "bad-func", ter_bad_func, 0, 0, false, NULL);
    s7_define_function(sc, "error-handler", ter_error_handler, 2, 0, false, NULL);
    result1 = s7_call_with_catch(sc, s7_t(sc), s7_name_to_value(sc, "bad-func"), s7_name_to_value(sc, "error-handler"));
    result2 = s7_eval_c_string(sc, "(catch #t bad-func error-handler)");
    if (result1 != result2)
      fprintf(stderr, "%d: %s != %s\n", __LINE__, s7_object_to_c_string(sc, result1), s7_object_to_c_string(sc, result2));
  }

  {
    s7_pointer catcher, make_func1, val;
    make_func = s7_make_function(sc, "bad", ter_bad_func, 0, 0, false, NULL);
    catcher = s7_make_function(sc, "catcher", ter_error_handler, 2, 0, false, NULL);
    make_func1 = s7_make_function(sc, "bad1", ter1_bad_func, 0, 0, false, NULL);
    catcher1 = s7_make_function(sc, "catcher1", ter1_error_handler, 2, 0, false, NULL);
    val = s7_call_with_catch(sc, s7_t(sc), make_func, catcher);
    if (val != s7_f(sc)) fprintf(stderr, "%d: %s should be #f\n", __LINE__, s7_object_to_c_string(sc, val));
    val = s7_call_with_catch(sc, s7_t(sc), make_func, catcher);
    if (val != s7_f(sc)) fprintf(stderr, "%d: %s should be #f\n", __LINE__, s7_object_to_c_string(sc, val));
    val = s7_call_with_catch(sc, s7_t(sc), make_func1, catcher1);
    if (s7_integer(val) != 123) fprintf(stderr, "%d: %s should be 123\n", __LINE__, s7_object_to_c_string(sc, val));
    val = s7_call_with_catch(sc, s7_t(sc), make_func1, catcher1);
    if (s7_integer(val) != 123) fprintf(stderr, "%d: %s should be 123\n", __LINE__, s7_object_to_c_string(sc, val));
    val = s7_call_with_catch(sc, s7_t(sc), make_func, catcher);
    if (val != s7_f(sc)) fprintf(stderr, "%d: %s should be #f\n", __LINE__, s7_object_to_c_string(sc, val));

    make_func = s7_make_function(sc, "bad2", ter2_bad_func, 0, 0, false, NULL);
    catcher2 = s7_make_function(sc, "catcher2", ter2_error_handler, 2, 0, false, NULL);
    val = s7_call_with_catch(sc, s7_t(sc), make_func, catcher2);
    if (strcmp(s7_string(val), "ter2 argument, (), is nil but should be oops") != 0)
      fprintf(stderr, "%d: %s is unexpected\n", __LINE__, s7_string(val));
  }

  {
    s7_define_function(sc, "wd-test-fn", wd_test_fn, 1, 0, false, "call the inner test");
    s7_define_function(sc, "wd-inner-test", wd_inner_test, 0, 0, false, "throw");
    s7_define_function(sc, "wd-inner-test-handler", wd_inner_test_handler, 2, 0, false, "do nothing");
    s7_eval_c_string(sc, "(wd-test-fn #f)");
    if (wd_val != 2) fprintf(stderr, "%d: s7_call_with_catch wd_val(1): %d\n", __LINE__, wd_val);
    s7_eval_c_string(sc, "(catch #t wd-test-fn (lambda (t i) 'oops))");
    if (wd_val != 2) fprintf(stderr, "%d: s7_call_with_catch wd_val(2): %d\n", __LINE__, wd_val);
    s7_eval_c_string(sc, "(call-with-exit wd-test-fn)");
    if (wd_val != 2) fprintf(stderr, "%d: s7_call_with_catch wd_val(3): %d\n", __LINE__, wd_val);
    s7_define_function(sc, "wd1-test-fn", wd1_test_fn, 1, 0, false, "call the inner test");
    s7_define_function(sc, "wd1-inner-fn", wd1_inner_fn, 1, 0, false, "call the inner test");
    s7_eval_c_string(sc, "(wd1-test-fn #f)");
    if (wd1_val != 2) fprintf(stderr, "%d: s7_call_with_catch wd1_val: %d\n", __LINE__, wd1_val);
    if (wd2_val != 12) fprintf(stderr, "%d: s7_call_with_catch wd2_val: %d\n", __LINE__, wd2_val);
  }

  {
    const char *str;
    s7_pointer obj;
    obj = s7_eval_c_string(sc, "'(* 3 (+ 1 2))");
    gc_loc = s7_gc_protect(sc, obj);
    str = pretty_print(sc, obj);
    s7_gc_unprotect_at(sc, gc_loc);
    if ((!str) || (strcmp(str, "(* 3 (+ 1 2))") != 0))
      fprintf(stderr, "pretty_print: \"%s\"\n", str);
  }

  {
    s7_int size = 256, gc_loc, loc, code;
    s7_pointer hasher, key, result;
    hasher = s7_make_and_fill_vector(sc, size, s7_nil(sc));
    gc_loc = s7_gc_protect(sc, hasher);
    key = s7_make_integer(sc, 3);
    code = s7_hash_code(sc, key, s7_f(sc));
    loc = code % size;
    s7_vector_set(sc, hasher, loc, s7_cons(sc, s7_cons(sc, key, s7_make_symbol(sc, "abc")), s7_vector_ref(sc, hasher, loc)));
    result = s7_cdr(s7_assoc(sc, key, s7_vector_ref(sc, hasher, loc)));
    if (result != s7_make_symbol(sc, "abc"))
      fprintf(stderr, "hash-code: %s\n", s7_object_to_c_string(sc, result));
    s7_gc_unprotect_at(sc, gc_loc);
  }

  {
    s7_pointer p;
    p = s7_random_state(sc, s7_cons(sc, s7_make_integer(sc, 123456), s7_cons(sc, s7_make_integer(sc, 654321), s7_nil(sc))));
    if (!s7_is_random_state(p))
      fprintf(stderr, "%d: s7_random_state returned %s\n", __LINE__, TO_STR(p));
    if (s7_type_of(sc, p) != s7_make_symbol(sc, "random-state?"))
      fprintf(stderr, "%d: s7_random_state returned %s\n", __LINE__, TO_STR(p));
#if (!WITH_GMP)
    {
      s7_pointer q;
      q = s7_random_state_to_list(sc, s7_cons(sc, p, s7_nil(sc)));
      if (!s7_is_pair(q))
	fprintf(stderr, "%d: s7_random_state_to_list is %s\n", __LINE__, TO_STR(q));
    }
#endif
  }

  {
    s7_pointer lst;
    s7_pointer arr[3];
    lst = s7_list(sc, 3, s7_make_integer(sc, 1), s7_make_integer(sc, 2), s7_make_integer(sc, 3));
    s7_list_to_array(sc, lst, arr, 3);
    if (s7_integer(arr[1]) != 2) fprintf(stderr, "arr[1]: %s\n", TO_STR(arr[1]));
  }

  {
    s7_float_function func;
    s7_pfunc sfunc;
    s7_pointer symbol;
    symbol = s7_define_safe_function(sc, "d-func", g_d_func, 0, 0, false, "d opt func");
    s7_set_d_function(sc, s7_name_to_value(sc, "d-func"), opt_d_func);
    func = s7_float_optimize(sc, s7_list(sc, 1, s7_list(sc, 1, symbol))); /* ((d-func)) -- the extra list layer is an historical artifact */
    if ((!WITH_GMP) && (!func)) fprintf(stderr, "%d: d-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "d-d-func", g_d_d_func, 1, 0, false, "d-d opt func");
    s7_set_d_d_function(sc, s7_name_to_value(sc, "d-d-func"), opt_d_d_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 2, symbol, s7_make_real(sc, 2.0)))); /* ((d-d-func 2.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: d-d-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "d-dd-func", g_d_dd_func, 2, 0, false, "d-dd opt func");
    s7_set_d_dd_function(sc, s7_name_to_value(sc, "d-dd-func"), opt_d_dd_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 3, symbol, s7_make_real(sc, 2.0), s7_make_real(sc, 3.0)))); /* ((d-dd-func 2.0 3.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: d-dd-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "d-ddd-func", g_d_ddd_func, 3, 0, false, "d-ddd opt func");
    s7_set_d_ddd_function(sc, s7_name_to_value(sc, "d-ddd-func"), opt_d_ddd_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 4, symbol, s7_make_real(sc, 2.0), s7_make_real(sc, 3.0), s7_make_real(sc, 4.0)))); /* ((d-ddd-func 2.0 3.0 4.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: d-ddd-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "d-dddd-func", g_d_dddd_func, 4, 0, false, "d-dddd opt func");
    s7_set_d_dddd_function(sc, s7_name_to_value(sc, "d-dddd-func"), opt_d_dddd_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1,
       s7_list(sc, 5, symbol, s7_make_real(sc, 2.0), s7_make_real(sc, 3.0), s7_make_real(sc, 4.0), s7_make_real(sc, 5.0)))); /* ((d-dddd-func 2.0 3.0 4.0 5.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: d-dddd-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "i-i-func", g_i_i_func, 1, 0, false, "i-i opt func");
    s7_set_i_i_function(sc, s7_name_to_value(sc, "i-i-func"), opt_i_i_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 2, symbol, s7_make_integer(sc, 2)))); /* ((i-i-func 2)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: i-i-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "i-ii-func", g_i_ii_func, 2, 0, false, "i-ii opt func");
    s7_set_i_ii_function(sc, s7_name_to_value(sc, "i-ii-func"), opt_i_ii_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 3, symbol, s7_make_integer(sc, 2), s7_make_integer(sc, 3)))); /* ((i-ii-func 2 3)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: i-ii-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "d-id-func", g_d_id_func, 2, 0, false, "d-id opt func");
    s7_set_d_id_function(sc, s7_name_to_value(sc, "d-id-func"), opt_d_id_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 3, symbol, s7_make_integer(sc, 2), s7_make_real(sc, 3.0)))); /* ((d-id-func 2 3.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: d-id-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "d-ip-func", g_d_ip_func, 2, 0, false, "d-ip opt func");
    s7_set_d_ip_function(sc, s7_name_to_value(sc, "d-ip-func"), opt_d_ip_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 3, symbol, s7_make_integer(sc, 2), s7_make_real(sc, 3.0)))); /* ((d-ip-func 2 3.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: d-ip-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "i-7d-func", g_i_7d_func, 1, 0, false, "i-7d opt func");
    s7_set_i_7d_function(sc, s7_name_to_value(sc, "i-7d-func"), opt_i_7d_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 2, symbol, s7_make_real(sc, 2.0)))); /* ((i-7d-func 2.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: i-7d-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "i-7p-func", g_i_7p_func, 1, 0, false, "i-7p opt func");
    s7_set_i_7p_function(sc, s7_name_to_value(sc, "i-7p-func"), opt_i_7p_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 2, symbol, s7_make_integer(sc, 2)))); /* ((i-7p-func 2)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: i-7p-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "d-p-func", g_d_p_func, 1, 0, false, "d-p opt func");
    s7_set_d_p_function(sc, s7_name_to_value(sc, "d-p-func"), opt_d_p_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 2, symbol, s7_make_real(sc, 2.0)))); /* ((d-p-func 2.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: d-p-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "d-pd-func", g_d_pd_func, 2, 0, false, "d-pd opt func");
    s7_set_d_pd_function(sc, s7_name_to_value(sc, "d-pd-func"), opt_d_pd_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 3, symbol, s7_make_real(sc, 2.0), s7_make_real(sc, 3.0)))); /* ((d-pd-func 2.0 3.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: d-pd-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "d-7pi-func", g_d_7pi_func, 2, 0, false, "d-7pi opt func");
    s7_set_d_7pi_function(sc, s7_name_to_value(sc, "d-7pi-func"), opt_d_7pi_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 3, symbol, s7_make_real(sc, 2.0), s7_make_integer(sc, 3)))); /* ((d-7pi-func 2.0 3)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: d-7pi-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "d-7pid-func", g_d_7pid_func, 3, 0, false, "d-7pid opt func");
    s7_set_d_7pid_function(sc, s7_name_to_value(sc, "d-7pid-func"), opt_d_7pid_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 4, symbol, s7_make_real(sc, 2.0), s7_make_integer(sc, 3), s7_make_real(sc, 4.0)))); /* ((d-7pid-func 2.0 3 4.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: d-7pid-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "b-p-func", g_b_p_func, 1, 0, false, "b-p opt func");
    s7_set_b_p_function(sc, s7_name_to_value(sc, "b-p-func"), opt_b_p_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 2, symbol, s7_make_real(sc, 2.0)))); /* ((b-p-func 2.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: b-p-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "p-d-func", g_p_d_func, 1, 0, false, "p-d opt func");
    s7_set_p_d_function(sc, s7_name_to_value(sc, "p-d-func"), opt_p_d_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 2, symbol, s7_make_real(sc, 2.0)))); /* ((p-d-func 2.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: p-d-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "d-v-func", g_d_v_func, 1, 0, false, "d-v opt func");
    s7_set_d_v_function(sc, s7_name_to_value(sc, "d-v-func"), opt_d_v_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1, s7_list(sc, 2, symbol, s7_list(sc, 2, s7_make_symbol(sc, "block"), s7_make_integer(sc, 2))))); /* ((d-v-func (block 2))) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: d-v-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "d-vd-func", g_d_vd_func, 2, 0, false, "d-vd opt func");
    s7_set_d_vd_function(sc, s7_name_to_value(sc, "d-vd-func"), opt_d_vd_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1,
                              s7_list(sc, 3, symbol,
                                             s7_list(sc, 2, s7_make_symbol(sc, "block"), s7_make_integer(sc, 2)),
				             s7_make_real(sc, 0.0)))); /* ((d-vd-func (block 2) 0.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: d-vd-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "d-vid-func", g_d_vid_func, 3, 0, false, "d-vid opt func");
    s7_set_d_vid_function(sc, s7_name_to_value(sc, "d-vid-func"), opt_d_vid_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1,
                              s7_list(sc, 4, symbol,
                                             s7_list(sc, 2, s7_make_symbol(sc, "block"), s7_make_integer(sc, 2)),
				             s7_make_integer(sc, 0),
				             s7_make_real(sc, 0.0)))); /* ((d-vid-func (block 2) 0 0.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: d-vid-func not optimized\n", __LINE__);

    symbol = s7_define_safe_function(sc, "d-vdd-func", g_d_vdd_func, 3, 0, false, "d-vdd opt func");
    s7_set_d_vdd_function(sc, s7_name_to_value(sc, "d-vdd-func"), opt_d_vdd_func);
    sfunc = s7_optimize(sc, s7_list(sc, 1,
                              s7_list(sc, 4, symbol,
                                             s7_list(sc, 2, s7_make_symbol(sc, "block"), s7_make_integer(sc, 2)),
				             s7_make_real(sc, 0.0),
				             s7_make_real(sc, 0.0)))); /* ((d-vdd-func (block 2) 0.0 0.0)) */
    if ((!WITH_GMP) && (!sfunc)) fprintf(stderr, "%d: d-vdd-func not optimized\n", __LINE__);
  }

  {
    s7_pointer old_port, new_port, res;
    new_port = s7_open_input_string(sc, "01234");
    old_port = s7_set_current_input_port(sc, new_port);
    res = s7_eval_c_string(sc, "(read-char)");
    if (s7_character(res) != '0') fprintf(stderr, "%d: read-char: %s\n", __LINE__, TO_STR(res));
    s7_set_current_input_port(sc, old_port);
  }

  s7_make_continuation(sc);

  s7_quit(sc);
  s7_free(sc);

  return(0);
}
