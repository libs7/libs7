/* bindings for tomlc99 array ops that are NOT exposed by libtoml_s7 */

s7_pointer pl_txi, pl_xxi;

    pl_txi = s7_make_signature(sc, 3, t, x, i);
    pl_xxi = s7_make_signature(sc, 3, x, x, i);

/* -------- toml_string_at -------- */
static s7_pointer toml_toml_string_at(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_string_at_0;
  int toml_toml_string_at_1;
  p = args;
  arg = s7_car(p);
  toml_toml_string_at_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_string_at_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:string-at", 14), 2, arg, integer_string));
  toml_string_at(toml_toml_string_at_0, toml_toml_string_at_1);
  return(s7_unspecified(sc));
}


/* -------- toml_bool_at -------- */
static s7_pointer toml_toml_bool_at(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_bool_at_0;
  int toml_toml_bool_at_1;
  p = args;
  arg = s7_car(p);
  toml_toml_bool_at_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_bool_at_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:bool-at", 12), 2, arg, integer_string));
  toml_bool_at(toml_toml_bool_at_0, toml_toml_bool_at_1);
  return(s7_unspecified(sc));
}


/* -------- toml_int_at -------- */
static s7_pointer toml_toml_int_at(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_int_at_0;
  int toml_toml_int_at_1;
  p = args;
  arg = s7_car(p);
  toml_toml_int_at_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_int_at_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:int-at", 11), 2, arg, integer_string));
  toml_int_at(toml_toml_int_at_0, toml_toml_int_at_1);
  return(s7_unspecified(sc));
}


/* -------- toml_double_at -------- */
static s7_pointer toml_toml_double_at(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_double_at_0;
  int toml_toml_double_at_1;
  p = args;
  arg = s7_car(p);
  toml_toml_double_at_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_double_at_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:double-at", 14), 2, arg, integer_string));
  toml_double_at(toml_toml_double_at_0, toml_toml_double_at_1);
  return(s7_unspecified(sc));
}


/* -------- toml_timestamp_at -------- */
static s7_pointer toml_toml_timestamp_at(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_timestamp_at_0;
  int toml_toml_timestamp_at_1;
  p = args;
  arg = s7_car(p);
  toml_toml_timestamp_at_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_timestamp_at_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:timestamp-at", 17), 2, arg, integer_string));
  toml_timestamp_at(toml_toml_timestamp_at_0, toml_toml_timestamp_at_1);
  return(s7_unspecified(sc));
}


/* -------- toml_array_at -------- */
static s7_pointer toml_toml_array_at(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_array_at_0;
  int toml_toml_array_at_1;
  p = args;
  arg = s7_car(p);
  toml_toml_array_at_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_array_at_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:array-at", 13), 2, arg, integer_string));
  return(s7_make_c_pointer_with_type(sc, (void*)toml_array_at(toml_toml_array_at_0, toml_toml_array_at_1), toml_array_t__symbol, s7_f(sc)));
}


/* -------- toml_table_at -------- */
static s7_pointer toml_toml_table_at(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_table_at_0;
  int toml_toml_table_at_1;
  p = args;
  arg = s7_car(p);
  toml_toml_table_at_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_table_at_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:table-at", 13), 2, arg, integer_string));
  return(s7_make_c_pointer_with_type(sc, (void*)toml_table_at(toml_toml_table_at_0, toml_toml_table_at_1), toml_table_t__symbol, s7_f(sc)));
}


// in init fn:

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:table-at"),
            s7_make_typed_function(sc, "toml:table-at", toml_toml_table_at, 2, 0, false,
                                   "(toml:table-at a i) value of array a at index i, if val is a table",
                                   pl_xxi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:array-at"),
            s7_make_typed_function(sc, "toml:array-at", toml_toml_array_at, 2, 0, false, "toml_array_t* toml_array_at(toml_array_t* int)", pl_xxi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:timestamp-at"),
            s7_make_typed_function(sc, "toml:timestamp-at", toml_toml_timestamp_at, 2, 0, false, "toml_datum_t toml_timestamp_at(toml_array_t* int)", pl_txi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:double-at"),
            s7_make_typed_function(sc, "toml:double-at", toml_toml_double_at, 2, 0, false, "toml_datum_t toml_double_at(toml_array_t* int)", pl_txi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:int-at"),
            s7_make_typed_function(sc, "toml:int-at", toml_toml_int_at, 2, 0, false, "toml_datum_t toml_int_at(toml_array_t* int)", pl_txi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:bool-at"),
            s7_make_typed_function(sc, "toml:bool-at", toml_toml_bool_at, 2, 0, false, "toml_datum_t toml_bool_at(toml_array_t* int)", pl_txi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:string-at"),
            s7_make_typed_function(sc, "toml:string-at", toml_toml_string_at, 2, 0, false, "toml_datum_t toml_string_at(toml_array_t* int)", pl_txi));

