;;; libcjson_clib.scm
;;;
;;; generate libjson_s7.c, s7 bindings for libcjson
;;; https://github.com/likle/cjson

(require clibgen.scm) ;; cload.scm)
(provide 'libcjson_clibgen.scm)

;; if loading from a different directory, pass that info to C
(let ((directory (let ((current-file (port-filename)))
		   (and (memv (current-file 0) '(#\/ #\~))
			(substring current-file 0 (- (length current-file) 9))))))
  (when (and directory (not (member directory *load-path*)))
    (set! *load-path* (cons directory *load-path*))))


(if (not (defined? '*libjson*))
    ;; (define *libjson*
      (with-let (unlet)
	(set! *libraries* (cons (cons "libjson.scm" (curlet)) *libraries*))
        ;; IMPORTANT! if *cload-library-name* is defined, then clibgen will
	;; (set! *cload-library-name* "*libjson*")
	(c-define
	 '(
;; typedef struct cJSON
;; {
;; } cJSON;

;; typedef struct cJSON_Hooks
;; {
;; } cJSON_Hooks;

;; typedef int cJSON_bool;

           ;; CJSON_PUBLIC(const char*) cJSON_Version(void);
           (char* cJSON_Version (void))

           ;; CJSON_PUBLIC(void) cJSON_InitHooks(cJSON_Hooks* hooks);

           ;; FIXME: scheme name: string->json or string->cjson
           (cJSON* cJSON_Parse (char*))
           ;; CJSON_PUBLIC(cJSON *) cJSON_Parse(const char *value);

           ;; CJSON_PUBLIC(cJSON *) cJSON_ParseWithLength(const char *value, size_t buffer_length);
           (cJSON* cJSON_ParseWithLength (char* size_t))

           ;; CJSON_PUBLIC(cJSON *) cJSON_ParseWithOpts(const char *value, const char **return_parse_end, cJSON_bool require_null_terminated);
           (cJSON* cJSON_ParseWithOpts (char* char** cJSON_bool))

           ;; CJSON_PUBLIC(cJSON *) cJSON_ParseWithLengthOpts(const char *value, size_t buffer_length, const char **return_parse_end, cJSON_bool require_null_terminated);
           (cJSON* cJSON_ParseWithLengthOpts (char* size_t char** cJSON_bool))


           ;; /* Render a cJSON entity to text for transfer/storage. */
           ;; CJSON_PUBLIC(char *) cJSON_Print(const cJSON *item);
           (char* cJSON_Print (cJSON*))

           ;; /* Render a cJSON entity to text for transfer/storage without any formatting. */
           ;; CJSON_PUBLIC(char *) cJSON_PrintUnformatted(const cJSON *item);
           ;; /* Render a cJSON entity to text using a buffered strategy. prebuffer is a guess at the final size. guessing well reduces reallocation. fmt=0 gives unformatted, =1 gives formatted */
           ;; CJSON_PUBLIC(char *) cJSON_PrintBuffered(const cJSON *item, int prebuffer, cJSON_bool fmt);
           ;; /* Render a cJSON entity to text using a buffer already allocated in memory with given length. Returns 1 on success and 0 on failure. */
           ;; /* NOTE: cJSON is not always 100% accurate in estimating how much memory it will use, so to be safe allocate 5 bytes more than you actually need */
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_PrintPreallocated(cJSON *item, char *buffer, const int length, const cJSON_bool format);
           ;; /* Delete a cJSON entity and all subentities. */
           ;; CJSON_PUBLIC(void) cJSON_Delete(cJSON *item);

           ;; /* Returns the number of items in an array (or object). */
           (int cJSON_GetArraySize (cJSON*))
           ;; CJSON_PUBLIC(int) cJSON_GetArraySize(const cJSON *array);

           ;; /* Retrieve item number "index" from array "array". Returns NULL if unsuccessful. */
           (cJSON* cJSON_GetArrayItem (cJSON* int))
           ;; CJSON_PUBLIC(cJSON *) cJSON_GetArrayItem(const cJSON *array, int index);

           ;; /* Get item "string" from object. Case insensitive. */
           (cJSON* cJSON_GetObjectItem (cJSON* char*))
           ;; CJSON_PUBLIC(cJSON *) cJSON_GetObjectItem(const cJSON * const object, const char * const string);

           (cJSON* cJSON_GetObjectItemCaseSensitive (cJSON* char*))
           ;; CJSON_PUBLIC(cJSON *) cJSON_GetObjectItemCaseSensitive(const cJSON * const object, const char * const string);

           (cJSON_bool cJSON_HasObjectItem (cJSON* char*))
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_HasObjectItem(const cJSON *object, const char *string);

           ;; /* For analysing failed parses. This returns a pointer to the parse error. You'll probably need to look a few chars back to make sense of it. Defined when cJSON_Parse() returns 0. 0 when cJSON_Parse() succeeds. */
           (char* cJSON_GetErrorPtr (void))
           ;; CJSON_PUBLIC(const char *) cJSON_GetErrorPtr(void);

           ;; /* Check item type and return its value */
           (char* cJSON_GetStringValue (cJSON*))
           ;; CJSON_PUBLIC(char *) cJSON_GetStringValue(const cJSON * const item);
           (double cJSON_GetNumberValue (cJSON*))
           ;; CJSON_PUBLIC(double) cJSON_GetNumberValue(const cJSON * const item);

           ;; /* These functions check the type of an item */
           (cJSON_bool cJSON_IsInvalid (cJSON*))
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_IsInvalid(const cJSON * const item);
           (cJSON_bool cJSON_IsFalse (cJSON*))
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_IsFalse(const cJSON * const item);
           (cJSON_bool cJSON_IsTrue (cJSON*))
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_IsTrue(const cJSON * const item);
           (cJSON_bool cJSON_IsBool (cJSON*))
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_IsBool(const cJSON * const item);
           (cJSON_bool cJSON_IsNull (cJSON*))
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_IsNull(const cJSON * const item);
           (cJSON_bool cJSON_IsNumber (cJSON*))
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_IsNumber(const cJSON * const item);
           (cJSON_bool cJSON_IsString (cJSON*))
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_IsString(const cJSON * const item);
           (cJSON_bool cJSON_IsArray (cJSON*))
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_IsArray(const cJSON * const item);
           (cJSON_bool cJSON_IsObject (cJSON*))
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_IsObject(const cJSON * const item);
           (cJSON_bool cJSON_IsRaw (cJSON*))
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_IsRaw(const cJSON * const item);

           ;; /* These calls create a cJSON item of the appropriate type. */
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateNull(void);
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateTrue(void);
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateFalse(void);
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateBool(cJSON_bool boolean);
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateNumber(double num);
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateString(const char *string);
           ;; /* raw json */
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateRaw(const char *raw);
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateArray(void);
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateObject(void);

           ;; /* Create a string where valuestring references a string so
           ;;  * it will not be freed by cJSON_Delete */
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateStringReference(const char *string);
           ;; /* Create an object/array that only references it's elements so
           ;;  * they will not be freed by cJSON_Delete */
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateObjectReference(const cJSON *child);
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateArrayReference(const cJSON *child);

           ;; /* These utilities create an Array of count items.
           ;;  * The parameter count cannot be greater than the number of elements in the number array, otherwise array access will be out of bounds.*/
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateIntArray(const int *numbers, int count);
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateFloatArray(const float *numbers, int count);
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateDoubleArray(const double *numbers, int count);
           ;; CJSON_PUBLIC(cJSON *) cJSON_CreateStringArray(const char *const *strings, int count);

           ;; /* Append item to the specified array/object. */
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_AddItemToArray(cJSON *array, cJSON *item);
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_AddItemToObject(cJSON *object, const char *string, cJSON *item);
           ;; /* Use this when string is definitely const (i.e. a literal, or as good as), and will definitely survive the cJSON object.
           ;;  * WARNING: When this function was used, make sure to always check that (item->type & cJSON_StringIsConst) is zero before
           ;;  * writing to `item->string` */
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_AddItemToObjectCS(cJSON *object, const char *string, cJSON *item);
           ;; /* Append reference to item to the specified array/object. Use this when you want to add an existing cJSON to a new cJSON, but don't want to corrupt your existing cJSON. */
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_AddItemReferenceToArray(cJSON *array, cJSON *item);
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_AddItemReferenceToObject(cJSON *object, const char *string, cJSON *item);

           ;; /* Remove/Detach items from Arrays/Objects. */
           ;; CJSON_PUBLIC(cJSON *) cJSON_DetachItemViaPointer(cJSON *parent, cJSON * const item);
           ;; CJSON_PUBLIC(cJSON *) cJSON_DetachItemFromArray(cJSON *array, int which);
           ;; CJSON_PUBLIC(void) cJSON_DeleteItemFromArray(cJSON *array, int which);
           ;; CJSON_PUBLIC(cJSON *) cJSON_DetachItemFromObject(cJSON *object, const char *string);
           ;; CJSON_PUBLIC(cJSON *) cJSON_DetachItemFromObjectCaseSensitive(cJSON *object, const char *string);
           ;; CJSON_PUBLIC(void) cJSON_DeleteItemFromObject(cJSON *object, const char *string);
           ;; CJSON_PUBLIC(void) cJSON_DeleteItemFromObjectCaseSensitive(cJSON *object, const char *string);

           ;; /* Update array items. */
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_InsertItemInArray(cJSON *array, int which, cJSON *newitem); /* Shifts pre-existing items to the right. */
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_ReplaceItemViaPointer(cJSON * const parent, cJSON * const item, cJSON * replacement);
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_ReplaceItemInArray(cJSON *array, int which, cJSON *newitem);
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_ReplaceItemInObject(cJSON *object,const char *string,cJSON *newitem);
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_ReplaceItemInObjectCaseSensitive(cJSON *object,const char *string,cJSON *newitem);

           ;; /* Duplicate a cJSON item */
           ;; CJSON_PUBLIC(cJSON *) cJSON_Duplicate(const cJSON *item, cJSON_bool recurse);
           ;; /* Duplicate will create a new, identical cJSON item to the one you pass, in new memory that will
           ;;  * need to be released. With recurse!=0, it will duplicate any children connected to the item.
           ;;  * The item->next and ->prev pointers are always zero on return from Duplicate. */
           ;; /* Recursively compare two cJSON items for equality. If either a or b is NULL or invalid, they will be considered unequal.
           ;;  * case_sensitive determines if object keys are treated case sensitive (1) or case insensitive (0) */
           ;; CJSON_PUBLIC(cJSON_bool) cJSON_Compare(const cJSON * const a, const cJSON * const b, const cJSON_bool case_sensitive);

           ;; /* Minify a strings, remove blank characters(such as ' ', '\t', '\r', '\n') from strings.
           ;;  * The input pointer json cannot point to a read-only address area, such as a string constant, 
           ;;  * but should point to a readable and writable address area. */
           ;; CJSON_PUBLIC(void) cJSON_Minify(char *json);

           ;; /* Helper functions for creating and adding items to an object at the same time.
           ;;  * They return the added item or NULL on failure. */
           ;; CJSON_PUBLIC(cJSON*) cJSON_AddNullToObject(cJSON * const object, const char * const name);
           ;; CJSON_PUBLIC(cJSON*) cJSON_AddTrueToObject(cJSON * const object, const char * const name);
           ;; CJSON_PUBLIC(cJSON*) cJSON_AddFalseToObject(cJSON * const object, const char * const name);
           ;; CJSON_PUBLIC(cJSON*) cJSON_AddBoolToObject(cJSON * const object, const char * const name, const cJSON_bool boolean);
           ;; CJSON_PUBLIC(cJSON*) cJSON_AddNumberToObject(cJSON * const object, const char * const name, const double number);
           ;; CJSON_PUBLIC(cJSON*) cJSON_AddStringToObject(cJSON * const object, const char * const name, const char * const string);
           ;; CJSON_PUBLIC(cJSON*) cJSON_AddRawToObject(cJSON * const object, const char * const name, const char * const raw);
           ;; CJSON_PUBLIC(cJSON*) cJSON_AddObjectToObject(cJSON * const object, const char * const name);
           ;; CJSON_PUBLIC(cJSON*) cJSON_AddArrayToObject(cJSON * const object, const char * const name);

           ;; /* When assigning an integer value, it needs to be propagated to valuedouble too. */
           ;; #define cJSON_SetIntValue(object, number) ((object) ? (object)->valueint = (object)->valuedouble = (number) : (number))
           ;; /* helper for the cJSON_SetNumberValue macro */
           ;; CJSON_PUBLIC(double) cJSON_SetNumberHelper(cJSON *object, double number);
           ;; #define cJSON_SetNumberValue(object, number) ((object != NULL) ? cJSON_SetNumberHelper(object, (double)number) : (number))
           ;; /* Change the valuestring of a cJSON_String object, only takes effect when type of object is cJSON_String */
           ;; CJSON_PUBLIC(char*) cJSON_SetValuestring(cJSON *object, const char *valuestring);

           ;; /* If the object is not a boolean type this does nothing and returns cJSON_Invalid else it returns the new type*/
           ;; #define cJSON_SetBoolValue(object, boolValue) ( \
           ;;     (object != NULL && ((object)->type & (cJSON_False|cJSON_True))) ? \
           ;;     (object)->type=((object)->type &(~(cJSON_False|cJSON_True)))|((boolValue)?cJSON_True:cJSON_False) : \
           ;;     cJSON_Invalid\
           ;; )

           ;; /* Macro for iterating over an array or object */
           ;; #define cJSON_ArrayForEach(element, array) for(element = (array != NULL) ? (array)->child : NULL; element != NULL; element = element->next)

           ;; /* malloc/free objects using the malloc/free functions that have been set with cJSON_InitHooks */
           ;; CJSON_PUBLIC(void *) cJSON_malloc(size_t size);
           ;; CJSON_PUBLIC(void) cJSON_free(void *object);

           )

	 "json" ;; prefix to add
         "cJSON_" ;; strip-prefix
         (list "cJSON.h" "stddef.h")
         ;; "" ""
         "libcjson_s7")
	(curlet))
      ;; )
)

;; *libjson*
;; the loader will return *libjson*
