;; shared library loader timing test

(call-with-output-file "add1.c"
  (lambda (oport)
    (format oport "
#include <stdlib.h>
#include \"s7.h\"
static s7_pointer add1(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_integer(s7_car(args)))
    return(s7_make_integer(sc, 1 + s7_integer(s7_car(args))));
  return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, \"add1\", 4), 0, s7_car(args), 
                                 s7_make_string_wrapper_with_length(sc, \"an integer\", 10)));
}
void add1_init(s7_scheme *sc);
void add1_init(s7_scheme *sc)
{
  s7_define_function(sc, \"add1\", add1, 1, 0, false, \"(add1 int) adds 1 to int\");
}
")))

(system "gcc -fpic -c add1.c")
(system "gcc -shared -Wl,-soname,libadd1.so -o libadd1.so add1.o -lm -lc")
(load "libadd1.so" (inlet 'init_func 'add1_init))

(display (add1 2)) (newline)

;;; --------------------------------------------------------------------------------

(call-with-output-file "tlib.c"
  (lambda (oport)
    (format oport "
#include <stdio.h>
#include <stdlib.h>
#include \"s7.h\"
static s7_pointer a_function(s7_scheme *sc, s7_pointer args)
{
  return(s7_car(args));
}
s7_pointer tlib_init(s7_scheme *sc, s7_pointer args);
s7_pointer tlib_init(s7_scheme *sc, s7_pointer args)
{
  s7_define_function(sc, \"a-function\", a_function, 1, 0, true, NULL);
  return(s7_cons(sc, s7_car(args), s7_nil(sc)));
}
")))

(system "gcc -fPIC -c tlib.c")
(system "gcc tlib.o -shared -o tlib.so -ldl -lm -Wl,-export-dynamic")

(define tinit (load "tlib.so" (inlet 'init_func 'tlib_init 'init_args (list 1 2 3))))
(display (apply a-function tinit)) (newline)

;;; --------------------------------------------------------------------------------

(unless (file-exists? "s7test-block.so")
  (system "gcc -fPIC -c s7test-block.c -I. -g -O2")
  (system "gcc s7test-block.o -shared -o s7test-block.so -ldl -lm -Wl,-export-dynamic"))

(load "s7test-block.so" (sublet (curlet) (cons 'init_func 'block_init)))


(define (f)
  (do ((i 0 (+ i 1)))
      ((= i 20000))
    (let ()
      (load "libadd1.so" (inlet 'init_func 'add1_init))
      (load "tlib.so" (inlet 'init_func 'tlib_init 'init_args (list 1 2 3)))
      (load "s7test-block.so" (sublet (curlet) (cons 'init_func 'block_init)))
      (block-release-methods))))
(f)

;;; --------------------------------------------------------------------------------

(require libc.scm)
(require libm.scm)
(require libgsl.scm)
(require libgdbm.scm)
(require libdl.scm)
(require libutf8proc.scm)

(define username (getenv "USER"))

(define (g)
  (do ((i 0 (+ i 1)))
      ((= i 3))
    (load (append "/home/" username "/cl/libc_s7.so") (inlet 'init_func 'libc_s7_init))
    (load (append "/home/" username "/cl/libm_s7.so") (inlet 'init_func 'libm_s7_init))
    (load (append "/home/" username "/cl/libgsl_s7.so") (inlet 'init_func 'libgsl_s7_init))
    (load (append "/home/" username "/cl/libgdbm_s7.so") (inlet 'init_func 'libgdbm_s7_init))
    (load (append "/home/" username "/cl/libdl_s7.so") (inlet 'init_func 'libdl_s7_init))))
(g)

;;; --------------------------------------------------------------------------------

(call-with-output-file "dax1.c"
  (lambda (oport)
    (format oport "

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include \"s7.h\"
typedef struct {
  s7_double x;
  s7_pointer data;
} dax;

static int dax_type_tag = 0;

static s7_pointer dax_to_string(s7_scheme *sc, s7_pointer args)
{
  char *data_str, *str;
  s7_pointer result;
  int data_str_len;
  dax *o = (dax *)s7_c_object_value(s7_car(args));
  data_str = s7_object_to_c_string(sc, o->data);
  data_str_len = strlen(data_str);
  str = (char *)calloc(data_str_len + 32, sizeof(char));
  snprintf(str, data_str_len + 32, \"<dax %.3f %s>\", o->x, data_str);
  free(data_str);
  result = s7_make_string(sc, str);
  free(str);
  return(result);
}

static s7_pointer free_dax(s7_scheme *sc, s7_pointer obj)
{
  free(s7_c_object_value(obj));
  return(NULL);
}

static s7_pointer mark_dax(s7_scheme *sc, s7_pointer obj)
{
  dax *o;
  o = (dax *)s7_c_object_value(obj);
  s7_mark(o->data);
  return(NULL);
}

static s7_pointer make_dax(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)malloc(sizeof(dax));
  o->x = s7_real(s7_car(args));
  if (s7_cdr(args) != s7_nil(sc))
    o->data = s7_cadr(args);
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
  o->x = s7_real(s7_cadr(args));
  return(s7_cadr(args));
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
  o->data = s7_cadr(args);
  return(o->data);
}

static s7_pointer dax_is_equal(s7_scheme *sc, s7_pointer args) 
{
  s7_pointer p1, p2;
  dax *d1, *d2;
  p1 = s7_car(args);
  p2 = s7_cadr(args);
  if (p1 == p2) 
    return(s7_t(sc));
  if ((!s7_is_c_object(p2)) ||
      (s7_c_object_type(p2) != dax_type_tag))
    return(s7_f(sc));
  d1 = (dax *)s7_c_object_value(p1);
  d2 = (dax *)s7_c_object_value(p2);
  return(s7_make_boolean(sc,
			 (d1->x == d2->x) &&
			 (s7_is_equal(sc, d1->data, d2->data))));
}

static s7_pointer make_and_free(s7_scheme *sc, s7_pointer args)
{
  s7_scheme *s7;
  s7 = s7_init();
  
  dax_type_tag = s7_make_c_type(s7, \"dax\");
  s7_c_type_set_gc_free(s7, dax_type_tag, free_dax);
  s7_c_type_set_gc_mark(s7, dax_type_tag, mark_dax);
  s7_c_type_set_is_equal(s7, dax_type_tag, dax_is_equal);
  s7_c_type_set_to_string(s7, dax_type_tag, dax_to_string);
  
  s7_define_function(s7, \"make-dax\", make_dax, 2, 0, false, \"(make-dax x data) makes a new dax\");
  s7_define_function(s7, \"dax?\", is_dax, 1, 0, false, \"(dax? anything) returns #t if its argument is a dax object\");
  
  s7_define_variable(s7, \"dax-x\", 
		     s7_dilambda(s7, \"dax-x\", dax_x, 1, 0, set_dax_x, 2, 0, \"dax x field\"));
  
  s7_define_variable(s7, \"dax-data\", 
		     s7_dilambda(s7, \"dax-data\", dax_data, 1, 0, set_dax_data, 2, 0, \"dax data field\"));
  
  s7_free(s7);
  return(s7_f(sc));
}

void dax_init(s7_scheme *sc);
void dax_init(s7_scheme *sc)
{
  s7_define_function(sc, \"dax\", make_and_free, 0, 0, false, NULL);
}
")))

(system "gcc -fpic dax1.c -c dax1.c")
(system "gcc dax1.o -shared -o dax1.so -ldl -lm -Wl,-export-dynamic")

(load "dax1.so" (inlet 'init_func 'dax_init))
(do ((i 0 (+ i 1)))
    ((= i 200))
  (dax))


(exit)
