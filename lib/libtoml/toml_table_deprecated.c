/* bindings for tomlc99 table ops that are NOT exposed by libtoml_s7 */

s7_pointer pl_txs;

    pl_txs = s7_make_signature(sc, 3, t, x, s);

/* -------- toml_string_in -------- */
static s7_pointer toml_toml_string_in(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_string_in_0;
  char* toml_toml_string_in_1;
  p = args;
  arg = s7_car(p);
  toml_toml_string_in_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_string_in_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:string-in", 14), 2, arg, string_string));
  toml_datum_t d = toml_string_in(toml_toml_string_in_0, toml_toml_string_in_1);
  if (d.ok)
      return(s7_make_c_pointer_with_type(sc, d.u.s, toml_datum_t__symbol, s7_f(sc)));
  else {
      log_error("toml:string-in failure");
      return(NULL);
  }
}


/* -------- toml_bool_in -------- */
static s7_pointer toml_toml_bool_in(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_bool_in_0;
  char* toml_toml_bool_in_1;
  p = args;
  arg = s7_car(p);
  toml_toml_bool_in_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_bool_in_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:bool-in", 12), 2, arg, string_string));
  toml_bool_in(toml_toml_bool_in_0, toml_toml_bool_in_1);
  return(s7_unspecified(sc));
}


/* -------- toml_int_in -------- */
static s7_pointer toml_toml_int_in(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_int_in_0;
  char* toml_toml_int_in_1;
  p = args;
  arg = s7_car(p);
  toml_toml_int_in_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_int_in_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:int-in", 11), 2, arg, string_string));
  toml_int_in(toml_toml_int_in_0, toml_toml_int_in_1);
  return(s7_unspecified(sc));
}


/* -------- toml_double_in -------- */
static s7_pointer toml_toml_double_in(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_double_in_0;
  char* toml_toml_double_in_1;
  p = args;
  arg = s7_car(p);
  toml_toml_double_in_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_double_in_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:double-in", 14), 2, arg, string_string));
  toml_double_in(toml_toml_double_in_0, toml_toml_double_in_1);
  return(s7_unspecified(sc));
}


/* -------- toml_timestamp_in -------- */
static s7_pointer toml_toml_timestamp_in(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_timestamp_in_0;
  char* toml_toml_timestamp_in_1;
  p = args;
  arg = s7_car(p);
  toml_toml_timestamp_in_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_timestamp_in_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:timestamp-in", 17), 2, arg, string_string));
  toml_timestamp_in(toml_toml_timestamp_in_0, toml_toml_timestamp_in_1);
  return(s7_unspecified(sc));
}


/* -------- toml_array_in -------- */
static s7_pointer toml_toml_array_in(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_array_in_0;
  char* toml_toml_array_in_1;
  p = args;
  arg = s7_car(p);
  toml_toml_array_in_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_array_in_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:array-in", 13), 2, arg, string_string));
  return(s7_make_c_pointer_with_type(sc, (void*)toml_array_in(toml_toml_array_in_0, toml_toml_array_in_1), toml_array_t__symbol, s7_f(sc)));
}


/* -------- toml_table_in -------- */
static s7_pointer toml_toml_table_in(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_table_in_0;
  char* toml_toml_table_in_1;
  p = args;
  arg = s7_car(p);
  toml_toml_table_in_0 = (toml_table_t*)s7_c_object_value(arg);

  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_table_in_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:table-in", 13), 2, arg, string_string));

  toml_table_t *t = toml_table_in(toml_toml_table_in_0, toml_toml_table_in_1);

  return(s7_make_c_object(sc, toml_table_type_tag, (void*)t));
  /* return(s7_make_c_pointer_with_type(sc, t, toml_table_t__symbol, s7_f(sc))); */
}


// in init fn:
  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:table-in"),
            s7_make_typed_function(sc, "toml:table-in", toml_toml_table_in, 2, 0, false,
                                   "(toml:table-in t k) returns value of t at k, if val is a table", pl_xxs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:array-in"),
            s7_make_typed_function(sc, "toml:array-in", toml_toml_array_in, 2, 0, false,
                                   "(toml:array-in t k) returns value of t at k, if val is an array", pl_xxs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:timestamp-in"),
            s7_make_typed_function(sc, "toml:timestamp-in", toml_toml_timestamp_in, 2, 0, false,
                                   "(toml:timestamp-in t k) returns value of t at k, if val is a timestamp", pl_txs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:double-in"),
            s7_make_typed_function(sc, "toml:double-in", toml_toml_double_in, 2, 0, false,
                                   "(toml:double-in t k) returns value of t at k, if val is a double", pl_txs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:int-in"),
            s7_make_typed_function(sc, "toml:int-in", toml_toml_int_in, 2, 0, false,
                                   "(toml:int-in t k) returns value of t at k, if val is an int",
                                   pl_txs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:bool-in"),
            s7_make_typed_function(sc, "toml:bool-in", toml_toml_bool_in, 2, 0, false,
                                   "(toml:bool-in t k) returns value of t at k, if val is a bool",
                                   pl_txs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:string-in"),
            s7_make_typed_function(sc, "toml:string-in", toml_toml_string_in, 2, 0, false,
                                   "(toml:string-in t k) returns value of t at k, if val is a string",
                                   pl_xxs));

