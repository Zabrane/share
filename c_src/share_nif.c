/* share_nif.c */

#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

#include "erl_nif.h"
#include "erl_driver.h"

#include "../include/share_type.h"
#include "share_big.h"

// #define DEBUG
// #define NIF_TRACE

// Dirty optional since 2.7 and mandatory since 2.12
#if (ERL_NIF_MAJOR_VERSION > 2) || ((ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION >= 7))
#ifdef USE_DIRTY_SCHEDULER
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(0)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#endif
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#endif

#define UNUSED(a) ((void) a)

#ifdef DEBUG
#define DEBUGF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#else
#define DEBUGF(f,a...)
#endif
#define INFOF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#define ERRORF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#define BADARG(env) enif_fprintf(stderr, "%s: badarg line=%d\r\n", __FILE__, __LINE__), enif_make_badarg((env))

#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

// require env in context (ugly)
#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define NIF_LIST \
    NIF("info", 0, share_info)			\
    NIF("info", 1, share_info)			\
    NIF("new_type", 1, share_new_type)		\
    NIF("new", 1, share_new)			\
    NIF("new", 2, share_new)			\
    NIF("setelement", 3, share_setelement)	\
    NIF("element",  2, share_element)		\
    NIF("resize", 3, share_resize)		\
    NIF("size",  1, share_size)			\
    NIF("size",  2, share_size)			\
    NIF("alignment",  1, share_alignment)	\
    NIF("alignment",  2, share_alignment)	\
    NIF("sizeof",  1, share_sizeof)		\
    NIF("sizeof",  2, share_sizeof)		\
    NIF("offsetof",  2, share_offsetof)		\
    NIF("typeof",  1, share_typeof)		\
    NIF("typeof",  2, share_typeof)

typedef struct {
    unsigned int num_types;
    unsigned int num_objects;
} nif_ctx_t;

DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(true);
DECL_ATOM(false);

// api names
DECL_ATOM(object);
DECL_ATOM(type);

// function names
DECL_ATOM(element);
DECL_ATOM(setelement);
// sample types
DECL_ATOM(uint);
DECL_ATOM(uchar);
DECL_ATOM(ushort);
DECL_ATOM(ulong);
DECL_ATOM(uintptr_t);
DECL_ATOM(size_t);
DECL_ATOM(uint8_t);
DECL_ATOM(uint16_t);
DECL_ATOM(uint32_t);
DECL_ATOM(uint64_t);
DECL_ATOM(uint128_t);
DECL_ATOM(uint8);
DECL_ATOM(uint16);
DECL_ATOM(uint32);
DECL_ATOM(uint64);
DECL_ATOM(uint128);
DECL_ATOM(char);
DECL_ATOM(short);
DECL_ATOM(long);
DECL_ATOM(int);
DECL_ATOM(intptr_t);
DECL_ATOM(ssize_t);
DECL_ATOM(int8_t);
DECL_ATOM(int16_t);
DECL_ATOM(int32_t);
DECL_ATOM(int64_t);
DECL_ATOM(int128_t);
DECL_ATOM(int8);
DECL_ATOM(int16);
DECL_ATOM(int32);
DECL_ATOM(int64);
DECL_ATOM(int128);
DECL_ATOM(float);
DECL_ATOM(double);
DECL_ATOM(float16_t);
DECL_ATOM(float32_t);
DECL_ATOM(float64_t);
DECL_ATOM(float128_t);
DECL_ATOM(complex);
DECL_ATOM(complex64_t);
DECL_ATOM(complex128_t);
DECL_ATOM(atm);
// structured types
DECL_ATOM(array);
DECL_ATOM(struct);
DECL_ATOM(dict_ent);
// array options
DECL_ATOM(rowmajor);
DECL_ATOM(size);
DECL_ATOM(stride);
DECL_ATOM(alignment);
DECL_ATOM(offset);

#define PATH_FLAG_NONE    0x00
#define PATH_FLAG_RESIZE  0x01
#define PATH_FLAG_OFFSET  0x02

#define FIXED_SIZE 1024


ErlNifResourceType *type_r;
ErlNifResourceType *object_r;
// ErlNifResourceType *array_r;

#ifdef DEBUG
static const char* sht_type_name(share_type_t* type)
{
    switch(*type) {
    case SHT_UINT8: return "uint8_t";
    case SHT_UINT16: return "uint16_t";
    case SHT_UINT32: return "uint32_t";
    case SHT_UINT64: return "uint64_t";
    case SHT_UINT128: return "uint128_t";
    case SHT_INT8: return "int8_t";
    case SHT_INT16: return "int16_t";
    case SHT_INT32: return "int32_t";
    case SHT_INT64: return "int64_t";
    case SHT_INT128: return "int128_t";
    case SHT_FLOAT16: return "float16_t";
    case SHT_FLOAT32: return "float32_t";
    case SHT_FLOAT64: return "float64_t";
    case SHT_FLOAT128: return "float128_t";
    case SHT_COMPLEX64: return "complex64_t";
    case SHT_COMPLEX128: return "complex128_t";
    case SHT_ARRAY: return "array";
    case SHT_STRUCT: return "struct";
    case SHT_DICT_ENT: return "dict_ent";
    case SHT_ATM: return "atm";
    default: return "unknown";
    }
}
#endif

// uint() | int() | flt()
static int build_type0(ErlNifEnv* env, ERL_NIF_TERM arg, share_type_t* type_ptr)
{
    share_type_t type;

    // unsigned 
    if (arg == ATOM(uchar)) type = SHT_UCHAR;
    else if (arg == ATOM(ushort)) type = SHT_USHORT;
    else if (arg == ATOM(uint)) type = SHT_UINT;
    else if (arg == ATOM(ulong)) type = SHT_ULONG;
    else if (arg == ATOM(size_t)) type = SHT_SIZE;
    else if (arg == ATOM(uintptr_t)) type = SHT_UINTPTR;
    else if (arg == ATOM(uint8_t)) type = SHT_UINT8;
    else if (arg == ATOM(uint16_t)) type = SHT_UINT16;
    else if (arg == ATOM(uint32_t)) type = SHT_UINT32;
    else if (arg == ATOM(uint64_t)) type = SHT_UINT64;
    else if (arg == ATOM(uint128_t)) type = SHT_UINT128;
    else if (arg == ATOM(uint8)) type = SHT_UINT8;
    else if (arg == ATOM(uint16)) type = SHT_UINT16;
    else if (arg == ATOM(uint32)) type = SHT_UINT32;
    else if (arg == ATOM(uint64)) type = SHT_UINT64;
    else if (arg == ATOM(uint128)) type = SHT_UINT128;    

    // signed 
    else if (arg == ATOM(char)) type = SHT_CHAR;
    else if (arg == ATOM(short)) type = SHT_SHORT;
    else if (arg == ATOM(int)) type = SHT_INT;
    else if (arg == ATOM(long)) type = SHT_LONG;
    else if (arg == ATOM(ssize_t)) type = SHT_SSIZE;
    else if (arg == ATOM(intptr_t)) type = SHT_INTPTR;
    else if (arg == ATOM(int8_t)) type = SHT_INT8;
    else if (arg == ATOM(int16_t)) type = SHT_INT16;
    else if (arg == ATOM(int32_t)) type = SHT_INT32;
    else if (arg == ATOM(int64_t)) type = SHT_INT64;
    else if (arg == ATOM(int128_t)) type = SHT_INT128;
    else if (arg == ATOM(int8)) type = SHT_INT8;
    else if (arg == ATOM(int16)) type = SHT_INT16;
    else if (arg == ATOM(int32)) type = SHT_INT32;
    else if (arg == ATOM(int64)) type = SHT_INT64;
    else if (arg == ATOM(int128)) type = SHT_INT128;    
    // float
    else if (arg == ATOM(float)) type = SHT_FLOAT;
    else if (arg == ATOM(double)) type = SHT_DOUBLE;    
    else if (arg == ATOM(float32_t)) type = SHT_FLOAT32;
    else if (arg == ATOM(float64_t)) type = SHT_FLOAT64;
    else if (arg == ATOM(float128_t)) type = SHT_FLOAT128;
    else if (arg == ATOM(complex)) type = SHT_COMPLEX64;
    else if (arg == ATOM(complex64_t)) type = SHT_COMPLEX64;
    else if (arg == ATOM(complex128_t)) type = SHT_COMPLEX128;
    else if (arg == ATOM(atm)) type = SHT_ATM;
    else return 0;
    *type_ptr = type;
    return 1;
}

static int get_size_t(ErlNifEnv* env, ERL_NIF_TERM arg, size_t* sizep)
{
    unsigned long sz;
    if (enif_get_ulong(env, arg, &sz)) {
	*sizep = sz;
	return 1;
    }
    return 0;
}

static int get_bool(ErlNifEnv* env, ERL_NIF_TERM arg, bool_t* bp)
{
    int ival;
    if (arg == ATOM(true))
	*bp = true;
    else if (arg == ATOM(false))
	*bp = false;
    else if (enif_get_int(env, arg, &ival))
	*bp = ival ? true : false;
    else
	return 0;
    return 1;
}

static int make_bool(ErlNifEnv* env, bool_t val)
{
    return val ? ATOM(true) : ATOM(false);
}

// building dynamic type
typedef struct
{
    size_t size;          // size of base array
    size_t alloc;         // allocated size of base array
    unsigned int cur;     // current position
    size_t size0;         // size inital base array    
    share_type_t* base0;  // inital optional fixed data
    share_type_t* base;
} dyn_build_t;

static int dyn_build_need(dyn_build_t* dp, size_t need)
{
    share_type_t* ptr;
    size_t cur_size = dp->size;
    size_t new_size = dp->cur + need;
    if (new_size <= cur_size)
	return 1;
    if (!dp->alloc) {
	if (!(ptr = enif_alloc(new_size*sizeof(share_type_t))))
	    return 0;
	memcpy(ptr, dp->base, cur_size*sizeof(share_type_t));
    }
    else {
	if (!(ptr = enif_realloc(dp->base, new_size*sizeof(share_type_t))))
	    return 0;
    }
    dp->base = ptr;
    dp->size = new_size;
    dp->alloc = new_size;
    return 1;
}

static void dyn_build_clean(dyn_build_t* dp)
{
    if (dp->alloc) {
	enif_free(dp->base);
	dp->base = dp->base0;
	dp->size = dp->size0;
	dp->alloc = 0;
    }
    dp->cur = 0;
}

static int dyn_build_push(dyn_build_t* dp, share_type_t type)
{
    if (!dyn_build_need(dp, 1))
	return 0;
    dp->base[dp->cur++] = type;
    return 1;
}

static void* dyn_build_struct(dyn_build_t* dp, size_t size)
{
    size_t n = size / sizeof(share_type_t);
    void* ptr;
    if (!dyn_build_need(dp, n))
	return NULL;
    ptr = &dp->base[dp->cur];
    dp->cur += n;
    return ptr;
}

static size_t sht_sizeof(share_type_t* tptr)
{
    size_t size = 0;
    switch(*tptr) {
    case SHT_ARRAY: {
	sht_array_t* sp = (sht_array_t*) tptr;
	if ((sp->s == 1) && (sp->size == 0))
	    size = sizeof(share_type_t);  // pointer to array!
	else
	    size = sp->e_size;
	break;
    }
    case SHT_STRUCT: {
	sht_struct_t* sp = (sht_struct_t*) tptr;
	size = sp->e_size;
	break;
    }
    case SHT_DICT_ENT: {
	sht_dict_ent_t* sp = (sht_dict_ent_t*) tptr;
	size += sht_sizeof(sp->spec);              // key size
	size += sht_sizeof(sp->spec+sp->t_offset); // value size
	break;
    }
    case SHT_UINT8:   size = 1; break;
    case SHT_UINT16:  size = 2; break;
    case SHT_UINT32:  size = 4; break;
    case SHT_UINT64:  size = 8; break;
    case SHT_UINT128: size = 16; break;
    case SHT_INT8:    size = 1; break;
    case SHT_INT16:   size = 2; break;
    case SHT_INT32:   size = 4; break;
    case SHT_INT64:   size = 8; break;
    case SHT_INT128:  size = 16; break;
    case SHT_FLOAT32: size = 4; break;
    case SHT_FLOAT64: size = 8; break;
    case SHT_FLOAT128: size = 16; break;
    case SHT_COMPLEX64: size = 8; break;
    case SHT_COMPLEX128: size = 16; break;
    case SHT_ATM:     size = sizeof(share_type_t); break;
    default: return 0;
    }
    return size;
}

static ERL_NIF_TERM sht_array_opts(ErlNifEnv* env, sht_array_t* sp)
{
    ERL_NIF_TERM list = enif_make_list(env, 0);
    ERL_NIF_TERM list2;
    ERL_NIF_TERM elem;
    sht_array_t* shape = (sht_array_t*) sp;
    int i;

    // FIXME: remove option set to default?

    // rowmajor option
    elem = enif_make_tuple2(env, ATOM(rowmajor),
			    make_bool(env, sp->rowmajor));
    list = enif_make_list_cell(env, elem, list);

    // alignment option
    elem = enif_make_tuple2(env, ATOM(alignment),
			    enif_make_ulong(env, sp->alignment));
    list = enif_make_list_cell(env, elem, list);
    
    // stride option
    list2 = enif_make_list(env, 0);
    for (i = sp->s-1; i >= 0; i--) {
	ERL_NIF_TERM hd = enif_make_ulong(env, shape[i].stride);
	list2 = enif_make_list_cell(env, hd, list2);
    }
    elem = enif_make_tuple2(env, ATOM(stride), list2);
    list = enif_make_list_cell(env, elem, list);

    // size option
    list2 = enif_make_list(env, 0);
    for (i = sp->s-1; i >= 0; i--) {
	ERL_NIF_TERM hd = enif_make_ulong(env, shape[i].size);
	list2 = enif_make_list_cell(env, hd, list2);
    }
    elem = enif_make_tuple2(env, ATOM(size), list2);
    list = enif_make_list_cell(env, elem, list);

    return list;
}

static ERL_NIF_TERM sht_typeof(ErlNifEnv* env, share_type_t* tptr)
{
    switch(*tptr) {
    case SHT_ARRAY: {
	sht_array_t* sp = (sht_array_t*) tptr;
	ERL_NIF_TERM elem_type = sht_typeof(env, sht_array_base_type(sp));
	ERL_NIF_TERM array_opts;
	if (elem_type == 0)
	    return 0;
	array_opts = sht_array_opts(env, sp);
	return enif_make_tuple3(env, ATOM(array), array_opts, elem_type);
    }
    case SHT_STRUCT: {
	sht_struct_t* sp = (sht_struct_t*) tptr;
	share_size_t n = sp->n;
	ERL_NIF_TERM list = enif_make_list(env, 0);
	int i;
	
	for (i = n-1; i >= 0; i--) {
	    ERL_NIF_TERM field_type;
	    ERL_NIF_TERM field_name;
	    ERL_NIF_TERM field;
	    sht_field_t* fp = &sp->spec[i];

	    field_name = (ERL_NIF_TERM) fp->name;
	    if (!enif_is_atom(env, field_name))
		return 0;
	    if ((field_type = sht_typeof(env, fp->spec + fp->t_offset)) == 0)
		return 0;
	    field = enif_make_tuple2(env, field_name, field_type);
	    list = enif_make_list_cell(env, field, list);
	}
	return enif_make_tuple2(env, ATOM(struct), list);
    }
    case SHT_DICT_ENT: {
	sht_dict_ent_t* sp = (sht_dict_ent_t*) tptr;
	ERL_NIF_TERM key, value;
	if ((key = sht_typeof(env, sp->spec)) == 0)
	    return 0;
	if ((value = sht_typeof(env, sp->spec+sp->t_offset)) == 0)
	    return 0;
	return enif_make_tuple3(env, ATOM(dict_ent), key, value);
    }
    case SHT_UINT8:   return ATOM(uint8_t);
    case SHT_UINT16:  return ATOM(uint16_t);
    case SHT_UINT32:  return ATOM(uint32_t);
    case SHT_UINT64:  return ATOM(uint64_t);
    case SHT_UINT128: return ATOM(uint128_t);
    case SHT_INT8:    return ATOM(int8_t);
    case SHT_INT16:   return ATOM(int16_t);
    case SHT_INT32:   return ATOM(int32_t);
    case SHT_INT64:   return ATOM(int64_t);
    case SHT_INT128:  return ATOM(int128_t);
    case SHT_FLOAT32: return ATOM(float32_t);
    case SHT_FLOAT64: return ATOM(float64_t);
    case SHT_FLOAT128: return ATOM(float128_t);
    case SHT_COMPLEX64: return ATOM(complex64_t);
    case SHT_COMPLEX128: return ATOM(complex128_t);
    case SHT_ATM:  return ATOM(atm);
    default: return 0;
    }
}

// return "natural" alignment in bytes
static size_t sht_align(share_type_t* tptr)
{
    switch(*tptr) {
    case SHT_ARRAY: {
	sht_array_t* sp = (sht_array_t*) tptr;
	return sp->alignment;
    }
    case SHT_STRUCT: {
	sht_struct_t* sp = (sht_struct_t*) tptr;
	return sp->alignment;
    }
    case SHT_DICT_ENT: {
	sht_dict_ent_t* sp = (sht_dict_ent_t*) tptr;
	return sp->alignment;
    }
    case SHT_UINT8:   return 1;
    case SHT_UINT16:  return 2;
    case SHT_UINT32:  return 4;
    case SHT_UINT64:  return 8;
    case SHT_UINT128: return 16;
    case SHT_INT8:    return 1;
    case SHT_INT16:   return 2;
    case SHT_INT32:   return 4;
    case SHT_INT64:   return 8;
    case SHT_INT128:  return 16;
    case SHT_FLOAT32: return 4;
    case SHT_FLOAT64: return 8;
    case SHT_FLOAT128: return 16;
    case SHT_COMPLEX64:  return 8;
    case SHT_COMPLEX128: return 16;
    case SHT_ATM: return sizeof(share_type_t); // word_t?
    default: return 0;
    }
}

// binary size of (internal) type spec (number of share_type_t elements)
static size_t sht_sizeof_spec(share_type_t* tptr)
{
    switch(*tptr) {
    case SHT_ARRAY: {
	sht_array_t* sp = (sht_array_t*) tptr;
	size_t asize = sizeof(sht_array_t)/sizeof(share_type_t);
	size_t size;
	size = sp->s*asize; // shape array(s)
	size += sht_sizeof_spec(sht_array_base_type(sp));
	return size;
    }
    case SHT_STRUCT: {
	sht_struct_t* sp = (sht_struct_t*) tptr;
	size_t size = sizeof(sht_struct_t)/sizeof(share_type_t);
	int i;
	for (i = 0; i < sp->n; i++) {
	    sht_field_t* fp = &sp->spec[i];
	    size += sizeof(sht_field_t)/sizeof(share_type_t);
	    size += sht_sizeof_spec(fp->spec + fp->t_offset);
	}
	return size;
    }
    case SHT_DICT_ENT: {
	sht_dict_ent_t* sp = (sht_dict_ent_t*) tptr;
	size_t size = sizeof(sht_dict_ent_t)/sizeof(share_type_t);

	size += sht_sizeof_spec(sp->spec);
	size += sht_sizeof_spec(sp->spec + sp->t_offset);
	return size;
    }
    default:
	return 1;
    }
}

static int get_number(ErlNifEnv* env, ERL_NIF_TERM term, double* val_ptr)
{
    if (enif_get_double(env, term, val_ptr))
	return 1;
    else {
	int64_t v;
	if (enif_get_int64(env, term, &v)) {
	    *val_ptr = (double) v;
	    return 1;
	}
    }
    return 0;
}

// FIXME: erlang128 bit float?
static ERL_NIF_TERM make_float128(ErlNifEnv* env, void* ptr)
{
    double d = *((long double*) ptr);
    return enif_make_double(env, d);
}

static ERL_NIF_TERM make_complex64(ErlNifEnv* env, void* ptr)
{
    complex z = *((complex*) ptr);
    return enif_make_list_cell(env,
			       enif_make_double(env, creal(z)),
			       enif_make_double(env, cimag(z)));
}

static ERL_NIF_TERM make_complex128(ErlNifEnv* env, void* ptr)
{
    double complex z = *((double complex*) ptr);
    return enif_make_list_cell(env,
			       enif_make_double(env, creal(z)),
			       enif_make_double(env, cimag(z)));
}

static ERL_NIF_TERM make_uint128(ErlNifEnv* env, void* ptr)
{
    return enif_make_ubig(env, (ERL_NIF_TERM*) ptr, 16/sizeof(ERL_NIF_TERM));
}

static ERL_NIF_TERM make_int128(ErlNifEnv* env, void* ptr)
{
    return enif_make_big(env, (ERL_NIF_TERM*) ptr, 16/sizeof(ERL_NIF_TERM));
}


static ERL_NIF_TERM make_value(ErlNifEnv* env, share_type_t* type,
			       void* ptr)
{
    switch(*type) {
    case SHT_DICT_ENT: {
	sht_dict_ent_t* sp = (sht_dict_ent_t*) type;
	ERL_NIF_TERM key, value;
	key = make_value(env, sp->spec, ptr);
	value = make_value(env, sp->spec+sp->t_offset, ptr+sp->e_offset);
	return enif_make_tuple2(env, key, value);
    }
    case SHT_ATM: {
	ERL_NIF_TERM name = *((ERL_NIF_TERM*) ptr);
	if (!enif_is_atom(env, name))
	    return 0;
	return name;
    }
    case SHT_ARRAY: {
	sht_array_t* sp = (sht_array_t*) type;
	ERL_NIF_TERM list = enif_make_list(env, 0);
	share_type_t* elem_type = sht_array_elem_type(sp);
	int i;
	// build reverse
	for (i = sp->size-1; i >= 0; i--) {
	    ERL_NIF_TERM elem = make_value(env, elem_type,
					   ptr + i*sp->stride);
	    list = enif_make_list_cell(env, elem, list);
	}
	return list;
    }
    case SHT_STRUCT: {
	sht_struct_t* sp = (sht_struct_t*) type;
	ERL_NIF_TERM list = enif_make_list(env, 0);
	int i;
	// buld reverse
	for (i = sp->n-1; i >= 0; i--) {
	    sht_field_t* fp = &sp->spec[i];
	    ERL_NIF_TERM elem = make_value(env, fp->spec + fp->t_offset,
					   ptr + fp->e_offset);
	    list = enif_make_list_cell(env, elem, list);
	}
	return list;
    }
    case SHT_UINT8:   return enif_make_ulong(env, *((uint8_t*)ptr));
    case SHT_UINT16:  return enif_make_ulong(env, *((uint16_t*)ptr));
    case SHT_UINT32:  return enif_make_ulong(env, *((uint32_t*)ptr));
    case SHT_UINT64:  return enif_make_uint64(env, *((ErlNifUInt64*)ptr));
    case SHT_UINT128: return make_uint128(env, ptr);	
    case SHT_INT8:    return enif_make_long(env, *((int8_t*)ptr));
    case SHT_INT16:   return enif_make_long(env, *((int16_t*)ptr));
    case SHT_INT32:   return enif_make_long(env, *((int32_t*)ptr));
    case SHT_INT64:   return enif_make_int64(env, *((ErlNifSInt64*)ptr));
    case SHT_INT128:  return make_int128(env, ptr);
    case SHT_FLOAT32: return enif_make_double(env, *((float*)ptr));
    case SHT_FLOAT64: return enif_make_double(env, *((double*)ptr));
    case SHT_FLOAT128: return make_float128(env, ptr);
    case SHT_COMPLEX64:  return make_complex64(env, ptr);
    case SHT_COMPLEX128:  return make_complex128(env, ptr);
    default: break;
    }
    return enif_make_badarg(env);
}

// type() =
//   uint() | int() | flt() | dict()
//   {array, array_opts(), type()}
//   {struct, [{Name::atom(), type()}]}
// dict() =
//   {array, array_opts(), {dict_ent, type(), type()}}
//
//  array_opts() = size_t() | [size_t()] | [array_opt()]
//  array_opt() = {size, size_t()|[size_t()]} |
//                {alignment, size_t()} |
//                {offset,    size_t()} |
//                {stride, [size_t()]} |
//                {rowmajor, bool()} |
//

// process array size opts  size_t() | [size_t()] |
//                          [...{size,size_t()|[size_t]}...]
// process stride() = [size_t()] (size of each dimension)
// process alignment() = size_t()|[size_t()] (same size or each dimension)
//

static int set_array_alignment(ErlNifEnv* env, ERL_NIF_TERM arg,
			       sht_array_t* sp)
{
    sht_array_t* shape = (sht_array_t*) sp;
    ERL_NIF_TERM hd, tl;
    ERL_NIF_TERM list = arg;
    unsigned int len = 0;
    size_t align;
    int i;

    if (get_size_t(env, arg, &align)) {
	for (i = 0; i < sp->s; i++)
	    shape[i].alignment = align;
	return 1;
    }
    if (!enif_is_list(env, arg))
	return 0;
    if (enif_is_empty_list(env, list))
	return 0;
    if (!enif_get_list_length(env, list, &len) || (len != sp->s))
	return 0;

    i = 0;
    while (enif_get_list_cell(env, list, &hd, &tl)) {
	if (!get_size_t(env, hd, &align))
	    return 0;
	shape[i].alignment = align;
	i++;
	list = tl;
    }
    return 1;
}

static int set_default_alignment(ErlNifEnv* env, sht_array_t* sp)
{
    sht_array_t* shape = (sht_array_t*) sp;
    size_t align = sizeof(uintptr_t);
    int i;

    for (i = 0; i < sp->s; i++)
	shape[i].alignment = align;
    return 1;
}

static int set_array_stride(ErlNifEnv* env, ERL_NIF_TERM arg, sht_array_t* sp)
{
    sht_array_t* shape;
    ERL_NIF_TERM hd, tl;
    ERL_NIF_TERM list = arg;
    unsigned int len = 0;
    int i;
    
    if (!enif_is_list(env, arg))
	return 0;
    if (enif_is_empty_list(env, list))
	return 0;
    if (!enif_get_list_length(env, list, &len) || (len != sp->s))
	return 0;

    i = 0;
    shape = (sht_array_t*) sp;
    while (enif_get_list_cell(env, list, &hd, &tl)) {
	size_t stride;
	size_t pad;
	size_t align = shape[i].alignment;
	if (!get_size_t(env, hd, &stride))
	    return 0;
	pad = (align - (stride % align)) % align;
	stride += pad;
	shape[i].stride = stride;
	i++;
	list = tl;
    }
    return 1;
}

static int set_default_stride(ErlNifEnv* env, sht_array_t* sp,
			      size_t element_size)
{
    int i;
    size_t stride = element_size;
    sht_array_t* shape = (sht_array_t*) sp;
    size_t align = sp->alignment;
    i = sp->s-1;
    shape[i].stride = stride;
    stride = stride * shape[i].size;
    i--;
    while(i >= 0) {
	size_t pad = (align - (stride % align)) % align;
	stride += pad;
	shape[i].stride = stride;
	stride = stride * shape[i].size;
	i--;
    }
    return 1;
}


// process array_size() = size_t() | [size_t()]
static int build_array_size(ErlNifEnv* env, ERL_NIF_TERM arg, sht_array_t* sp,
			    dyn_build_t* dp)
{
    size_t n;

    if (get_size_t(env, arg, &n)) { // size_t()
	sp->type = SHT_ARRAY;
	sp->s = 1;  // one dimension
	sp->alignment = sizeof(uintptr_t);
	sp->rowmajor = true;
	sp->size = n;
	sp->stride = 0;
	return 1;
    }
    else if (enif_is_list(env, arg)) {
	ERL_NIF_TERM hd, tl;
	ERL_NIF_TERM list = arg;
	size_t len = 0;
	sht_array_t* shape;
	int i;

	if (enif_is_empty_list(env, list))
	    return 0;
	n = 0;
	// check list of unsigned integers
	while (enif_get_list_cell(env, list, &hd, &tl)) {
	    size_t val;
	    if (!get_size_t(env, hd, &val))
		return 0;
	    len++;
	    list = tl;
	}
	if (!enif_is_empty_list(env, list))
	    return 0;
	if (!(shape = dyn_build_struct(dp, (len-1)*sizeof(sht_array_t))))
	    return 0;
	shape = (sht_array_t*) sp;
	
	// now build the array sizes
	// build shapes (size, stripe) for each dimension
	list = arg;  // restart processing
	i = 0;
	while (enif_get_list_cell(env, list, &hd, &tl)) {
	    size_t val = 0;
	    get_size_t(env, hd, &val);
	    shape[i].type = SHT_ARRAY;
	    shape[i].s = len;
	    shape[i].alignment = sizeof(uintptr_t);
	    shape[i].rowmajor = true;
	    shape[i].size = val;
	    shape[i].stride = 0;
	    len--;
	    i++;
	    list = tl;
	}
	return 1;
    }
    return 0;
}


// process array_size() | [{size, array_size()}]
static int build_array_size_opt(ErlNifEnv* env, ERL_NIF_TERM arg,
				sht_array_t* sp, dyn_build_t* dp,
				int* is_option_list)
{
    // first check for simple array size = no other options
    *is_option_list = 0;
    if (build_array_size(env, arg, sp, dp))
	return 1;
    else if (enif_is_list(env, arg)) { // look for 'size' option
	ERL_NIF_TERM hd, tl;
	ERL_NIF_TERM list = arg;

	*is_option_list = 1;
	while (enif_get_list_cell(env, list, &hd, &tl)) {
	    const ERL_NIF_TERM* elem;
	    int arity;

	    if (!enif_get_tuple(env, hd, &arity, &elem) || (arity != 2))
		return 0;
	    if (elem[0] == ATOM(size))
		return build_array_size(env, elem[1], sp, dp);
	    list = tl;
	}
	return 0;  // no size found
    }
    return 0;
}

static int build_type(ErlNifEnv* env, ERL_NIF_TERM arg, bool_t is_array,
		      dyn_build_t* dp);

static int build_struct_type(ErlNifEnv* env, ERL_NIF_TERM list,
			     bool_t is_array, dyn_build_t* dp)
{
    ERL_NIF_TERM hd, tl;
    unsigned int len;
    sht_struct_t* sp;
    int i;
    int cur0;
    int offset;
    int pad;
    int max_align = 0;  // max alignment of struct fields
    size_t size;
    
    if (!enif_get_list_length(env, list, &len))
	return 0;
    size = sizeof(sht_struct_t)+len*sizeof(sht_field_t);
    if ((sp = dyn_build_struct(dp, size)) == 0)
	return 0;
    sp->type = SHT_STRUCT;
    sp->n    = len;
    cur0 = dp->cur;  // current build position
    offset = 0;
    i = 0;
    while(enif_get_list_cell(env, list, &hd, &tl)) {
	const ERL_NIF_TERM* felem;
	sht_field_t* fp = &sp->spec[i];
	int farity;
	int align;
	int fcur;
		
	if (!enif_get_tuple(env, hd, &farity, &felem) || (farity!=2))
	    return 0;
	if (!enif_is_atom(env, felem[0]))
	    return 0;

	fp->name = felem[0];
	fcur = dp->cur;
	fp->t_offset = (dp->base+fcur) - fp->spec;
	if (!build_type(env, felem[1], false, dp))
	    return 0;	
	if ((size = sht_sizeof(&dp->base[fcur])) == 0)
	    return 0;
	if ((align  = sht_align(&dp->base[fcur])) == 0)
	    return 0;

	if (align > max_align)
	    max_align = align;
	pad = (align - (offset % align)) % align;
	fp->e_offset = offset + pad;
	// enif_fprintf(stdout, "field %d: offset=%d, align=%d, size=%d, pad=%d\r\n", i, offset, align, size, pad);
	offset += (pad+size);
	i++;
	list = tl;
    }
    sp->t_size = dp->cur - cur0;
    pad = (max_align - (offset % max_align)) % max_align;
    sp->e_size = offset+pad;
    sp->alignment = max_align;
    return 1;
}


static int build_array_type(ErlNifEnv* env,
			    ERL_NIF_TERM options, ERL_NIF_TERM base_type,
			    bool_t is_array,
			    dyn_build_t* dp)
{
    sht_array_t* sp;
    sht_array_t* shape;
    int cur;
    ERL_NIF_TERM stride_elem = 0;
    ERL_NIF_TERM alignment_elem = 0;
    size_t elem_size;
    size_t type_size;
    int is_option_list;
    int i;
	    
    if (!(sp = dyn_build_struct(dp, sizeof(sht_array_t))))
	return 0;

    // build and init array with sub arrays
    if (!build_array_size_opt(env, options, sp, dp, &is_option_list))
	return 0;

    if (is_option_list && enif_is_list(env, options)) {
	ERL_NIF_TERM hd, tl;
	ERL_NIF_TERM list = options;
	
	while (enif_get_list_cell(env, list, &hd, &tl)) {
	    const ERL_NIF_TERM* opt_elem;
	    int arity;
	    // may have bad options since we only processed size
	    // option following that are not check by
	    // build_array_size_opt
	    if (!enif_get_tuple(env, hd, &arity, &opt_elem) ||
		(arity != 2))
		return 0;  // bad option
	    
	    if (opt_elem[0] == ATOM(size))
		;
	    else if (opt_elem[0] == ATOM(alignment)) {
		alignment_elem = opt_elem[1];		       
	    }
	    else if (opt_elem[0] == ATOM(stride)) {
		stride_elem = opt_elem[1];
	    }
	    else if (opt_elem[0] == ATOM(rowmajor)) {
		bool_t val;
		if (!get_bool(env, opt_elem[1], &val))
		    return 0;
		sp->rowmajor = val;
	    }
	    else
		return 0;  // bad option
	    list = tl;
	}
    }
    cur = dp->cur;
    if (!build_type(env, base_type, true, dp))
	return 0;
    elem_size = sht_sizeof(&dp->base[cur]);
    // enif_fprintf(stdout, "elem_size=%d\r\n", elem_size);
    
    if (alignment_elem) {
	if (!set_array_alignment(env, alignment_elem, sp))
	    return 0;
    }
    else
	set_default_alignment(env, sp);
    
    if (stride_elem) {
	if (!set_array_stride(env, stride_elem, sp))
	    return 0;
    }
    else
	set_default_stride(env, sp, elem_size);

    shape = (sht_array_t*) sp;
    type_size = dp->cur - cur;
    for (i = sp->s-1; i >= 0; i--) {
	shape[i].e_size = shape[i].size * shape[i].stride;
	shape[i].t_size = type_size;
	type_size += sizeof(sht_array_t)/sizeof(share_type_t);
    }
    return 1;
}

static int build_dict_ent(ErlNifEnv* env,
			  ERL_NIF_TERM key, ERL_NIF_TERM value,
			  bool_t is_array,
			  dyn_build_t* dp)
{
    sht_dict_ent_t* sp;
    int cur;
	    
    if (!is_array)
	return 0;
    if (!(sp = dyn_build_struct(dp, sizeof(sht_dict_ent_t))))
	return 0;
    sp->type = SHT_DICT_ENT;
    cur = dp->cur;
    if (!build_type(env, key, false, dp))
	return 0;
    sp->t_offset = dp->cur - cur;
    sp->e_offset = sht_sizeof(&dp->base[cur]);
    if (!build_type(env, value, false, dp))
	return 0;
    sp->alignment = sizeof(uintptr_t);
    return 1;
}

static int build_type(ErlNifEnv* env, ERL_NIF_TERM arg, bool_t is_array,
		      dyn_build_t* dp)
{
    if (enif_is_atom(env, arg)) {
	share_type_t type;
	if (!build_type0(env, arg, &type))
	    return 0;
	return dyn_build_push(dp, type);
    }
    else {
	const ERL_NIF_TERM* elem;	
	int arity;
	
	if (!enif_get_tuple(env, arg, &arity, &elem))
	    return 0;
	if ((arity == 3) && (elem[0] == ATOM(array))) {
	    return build_array_type(env, elem[1], elem[2], is_array, dp);
	}
	else if ((arity == 2) && (elem[0] == ATOM(struct))) {
	    return build_struct_type(env, elem[1], is_array, dp);
	}
	else if ((arity == 3) && (elem[0] == ATOM(dict_ent))) {
	    return build_dict_ent(env, elem[1], elem[2], is_array, dp);
	}
	else
	    return 0;
    }
    return 1;
}

// [1...] array index 1 OR dict key=1
// [x...] field name x  OR dict key=x
//
// FIXME: allow simple index like 1, x for simple elements
//

static share_array_t* share_array_resize(share_array_t* obj, size_t new_size)
{
    if (obj == NULL) {
	obj = enif_alloc(sizeof(share_object_t)+new_size);
	obj->alloc = new_size;
	memset(obj->data, 0, new_size);
    }
    else if (new_size > obj->alloc) {
	obj = enif_realloc(obj, sizeof(share_object_t)+new_size);
	memset(obj->data+obj->alloc, 0, new_size - obj->alloc);
	obj->alloc = new_size;
    }
    obj->size = new_size;    
    return obj;
}

// return the ith dict element
static int get_dict_ent_by_index(ErlNifEnv* env, unsigned int index,
				 share_type_t* tptr, share_type_t** type_ret,
				 uint8_t* ptr, uint8_t** ptr_ret)
{
    sht_array_t* sp = (sht_array_t*) tptr;
    sht_dict_ent_t* ep = (sht_dict_ent_t*) sht_array_elem_type(sp);
    size_t elem_size;

    if (index >= sp->size)
	return 0;

    elem_size = sht_sizeof((share_type_t*) ep); // size of dict_ent
    ptr += index*elem_size;
    tptr = (share_type_t*) ep;

    *type_ret = tptr;
    *ptr_ret = ptr;
    return 1;    
}

static int get_dict_val_by_atom(ErlNifEnv* env, ERL_NIF_TERM key,
				 share_type_t* tptr, share_type_t** type_ret,
				 uint8_t* ptr, uint8_t** ptr_ret)
{
    sht_array_t* sp = (sht_array_t*) tptr;
    sht_dict_ent_t* ep = (sht_dict_ent_t*) sht_array_elem_type(sp);
    size_t elem_size;
    int i;

    if (!sht_is_atom(ep->spec[0]))  // must be atom dict
	return 0;
    elem_size = sht_sizeof(sht_array_elem_type(sp)); // size of dict_ent
    
    for (i = 0; i < (int)sp->size; i++) {
	// FIXME: do not? store atom in data?
	if (*((share_type_t*) ptr) == key) {
	    ptr += ep->e_offset;          // skip to value
	    tptr = ep->spec+ep->t_offset; // and value spec
	    break;
	}
	ptr += elem_size;  // next dict entry
    }
    *type_ret = tptr;
    *ptr_ret = ptr;
    return 1;
}


static int get_dict_val_by_int(ErlNifEnv* env, unsigned int index,
			       share_type_t* tptr, share_type_t** type_ret,
				 uint8_t* ptr, uint8_t** ptr_ret)
{
    sht_array_t* sp = (sht_array_t*) tptr;
    sht_dict_ent_t* ep = (sht_dict_ent_t*) sht_array_elem_type(sp);
    size_t elem_size;
    int i;

    if (!sht_is_integer(ep->spec[0]))
	return 0;
    elem_size = sht_sizeof(sht_array_elem_type(sp)); // size of dict_ent
    
    for (i = 0; i < (int)sp->size; i++) {
	uint64_t key;
	if (share_get_uint64(ep->spec, ptr, &key) &&
	    (key == (uint64_t) index)) {
	    ptr += ep->e_offset;            // skip to value
	    tptr = ep->spec + ep->t_offset; // and value spec
	    break;
	}
	ptr += elem_size;  // next dict entry
    }
    *type_ret = tptr;
    *ptr_ret = ptr;
    return 1;    
}


static int get_array_ent_by_index(ErlNifEnv* env, unsigned flags,
				  unsigned int index,
				  share_type_t* tptr, share_type_t** type_ret,
				  uint8_t* ptr, uint8_t** ptr_ret)
{
    sht_array_t* sp = (sht_array_t*) tptr;
    share_type_t* elem_type = sht_array_elem_type(sp);
    // size_t elem_size = sht_sizeof(elem_type);
    size_t elem_offset = index*sp->stride;
    if (sp->size == 0) {  // dynamic pointer
	share_array_t** app = (share_array_t**) ptr;
	share_array_t* ap = *app;
	if (!(flags & PATH_FLAG_RESIZE)) {
	    if (ap == NULL) return 0;
	    if (ap->size < elem_offset+sp->stride) return 0;
	}
	else {
	    ap = share_array_resize(ap, elem_offset+sp->stride);
	    *app = ap;
	}
	ptr = ap->data + elem_offset;
    }
    else if (index >= sp->size)
	return 0;
    else {
	ptr += elem_offset;
    }
    *type_ret = elem_type;
    *ptr_ret = ptr;
    return 1;
}

// FIXME: hash and precompute key offsets and store in sht_struct
static int get_struct_field_by_atom(ErlNifEnv* env, ERL_NIF_TERM key,
				    share_type_t* tptr, share_type_t** type_ret,
				    uint8_t* ptr, uint8_t** ptr_ret)
{
    sht_struct_t* sp = (sht_struct_t*) tptr;
    int i;

    for (i = 0; i < (int)sp->n; i++) {
	sht_field_t* fp = &sp->spec[i];
	if (fp->name == key) {
	    ptr = ptr + fp->e_offset;
	    tptr = fp->spec + fp->t_offset;
	    break;
	}
    }
    *type_ret = tptr;
    *ptr_ret = ptr;
    return 1;
}

//
// retrieve entry pointer for path
// [3]    9 for array 9 {6,7,9,9} (0-bases)
// [1,x]  7 for struct array { #{x=8,y=1}, #{x=7,y=2}, #{x=6,y=3}}
// [{2}]  {z,17} for SPECIAL #{ x => 12, y => 12, z => 17 }
// [{2,0}]  z for SPECIAL #{ x => 12, y => 12, z => 17 }
// [{2,1}]  17 for SPECIAL #{ x => 12, y => 12, z => 17 }
// [z]      17 for SPECIAL #{ x => 12, y => 12, z => 17 }
// [x]    12 for  #{ x => 12, y => 12 }
// [2]    13 for  #{ 1 => 12, 2 => 13 }
//
// FIXME
// [[2,3],x] x component from a 2 dimensional array
//
static int get_path(ErlNifEnv* env, ERL_NIF_TERM path,
		    unsigned flags,
		    share_type_t* tptr, share_type_t** type_ret,
		    uint8_t* ptr, uint8_t** ptr_ret)
{
    ERL_NIF_TERM hd, tl;
    
    while(enif_get_list_cell(env, path, &hd, &tl)) {
	unsigned int index;

	if (enif_get_uint(env, hd, &index)) {  // array index or key=index
	    if (sht_is_dict(tptr)) {
		if (flags & PATH_FLAG_OFFSET)
		    return 0;
		if (!get_dict_val_by_int(env, index, tptr, &tptr, ptr, &ptr))
		    return 0;
	    }
	    else if (sht_is_array(tptr)) {
		if (!get_array_ent_by_index(env, flags,
					    index, tptr, &tptr, ptr, &ptr))
		    return 0;
	    }	    
	    else if (sht_is_dict_ent(tptr)) {
		sht_dict_ent_t* ep = (sht_dict_ent_t*) tptr;
		// path like [{1}, 0] or [{1}, 1]
		if (index == 0) // key
		    tptr = ep->spec;
		else if (index == 1) { // value
		    tptr += ep->t_offset;
		    ptr += ep->e_offset;
		}
		else
		    return 0;
	    }
	    else
		return 0;
	}	
	else if (enif_is_atom(env, hd)) {  // access field x | key=x
	    if (sht_is_struct(tptr)) {
		if (!get_struct_field_by_atom(env,hd,tptr,&tptr,ptr,&ptr))
		    return 0;
	    }
	    else if (sht_is_dict(tptr)) {
		if (flags & PATH_FLAG_OFFSET)
		    return 0;		
		if (!get_dict_val_by_atom(env, hd, tptr, &tptr, ptr, &ptr))
		    return 0;
	    }
	    else if (sht_is_dict_ent(tptr)) {
		sht_dict_ent_t* ep = (sht_dict_ent_t*) tptr;
		if (sht_is_atom(ep->spec[0])) {
		    if (hd == *((share_atom_t*) ptr)) {
			tptr = ep->spec + ep->t_offset;
			ptr += ep->e_offset;
		    }
		    else
			return 0;
		}
		else
		    return 0;
	    }
	    else
		return 0;
	}
	else if (enif_is_tuple(env, hd)) {
	    const ERL_NIF_TERM* elem;
	    int arity;
	    unsigned int subind;

	    if (!enif_is_empty_list(env, tl))  // must be last element
		return 0;
	    enif_get_tuple(env, hd, &arity, &elem);
	    switch(arity) {
	    case 1: // access the dict entry it self
		if (!enif_get_uint(env, elem[0], &index))
		    return 0;
		if (!get_dict_ent_by_index(env, index, tptr, &tptr, ptr, &ptr))
		    return 0;
		break;
	    case 2: // access the key (subind=0) or value (subind=1)
		if (!enif_get_uint(env, elem[0], &index))
		    return 0;
		if (!enif_get_uint(env, elem[1], &subind))
		    return 0;
		if (subind > 1) return 0;
		if (!get_dict_ent_by_index(env, index, tptr, &tptr, ptr, &ptr))
		    return 0;
		else {
		    sht_dict_ent_t* ep = (sht_dict_ent_t*) tptr;
		    tptr = (share_type_t*) ep->spec;
		    if (subind == 1) {
			tptr += ep->t_offset;
			ptr += ep->e_offset;
		    }
		}
		break;
	    default:
		return 0;
	    }
	}
	else
	    return 0;
	path = tl;
    }
    if (!enif_is_empty_list(env, path))
	return 0;
    *type_ret = tptr;
    *ptr_ret = ptr;
    return 1;
}


static ERL_NIF_TERM share_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    switch(argc) {
    case 0: {
	nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
	unsigned int oval, tval;

	__atomic_load(&ctx->num_objects, &oval, __ATOMIC_SEQ_CST);
	__atomic_load(&ctx->num_types, &tval, __ATOMIC_SEQ_CST);
	
	return enif_make_tuple2(env, 
				enif_make_uint(env, oval),
				enif_make_uint(env, tval));
    }
    case 1: {
	void* obj;
	unsigned char* ptr;
	ERL_NIF_TERM bin;
	uint64_t ui;
	
	if (enif_get_resource(env, argv[0], type_r, &obj)) {
	    share_type_t* tptr = (share_type_t*) obj;
	    size_t n = sht_sizeof_spec(tptr);
	    size_t size = sht_sizeof(tptr);
	    ptr = enif_make_new_binary(env, n*sizeof(share_type_t), &bin);
	    memcpy(ptr, tptr, n*sizeof(share_type_t));
	    return enif_make_tuple2(env,
				    enif_make_ulong(env, size),
				    bin);
	}
	else if (enif_get_resource(env, argv[0], object_r, &obj)) {
	    share_object_t* optr = (share_object_t*) obj;
	    size_t n = sht_sizeof(optr->type);
	    ERL_NIF_TERM typeref;
	    ptr = enif_make_new_binary(env, n, &bin);
	    memcpy(ptr, optr->data, n);
	    typeref = enif_make_resource(env, optr->type);
	    return enif_make_tuple2(env, typeref, bin);
	}
	else if (enif_get_uint64(env, argv[0], &ui)) { // convert atom 
	    ERL_NIF_TERM ua = (ERL_NIF_TERM) ui;
	    if (enif_is_atom(env, ua))
		return ua;
	    else
		return enif_make_badarg(env);    
	}
	else
	    return enif_make_badarg(env);
    }
    default:
	return enif_make_badarg(env);
    }
}

static void init_object(share_object_t* obj, share_type_t* type)
{
    // init needed fields from type    
    obj->type = type;
}

static share_object_t* new_object(ErlNifEnv* env, share_type_t* type,
				  size_t data_size, uint8_t* data)
{
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    size_t size = sht_sizeof(type);
    share_object_t* obj;

    if ((obj = enif_alloc_resource(object_r,sizeof(share_object_t))) == NULL)
	return NULL;
    memset(obj, 0, sizeof(share_object_t));
    if ((obj->data = enif_alloc(size)) == NULL) {
	enif_release_resource(obj);
	return NULL;
    }
    if ((data_size == 0) || (data == NULL))
	memset(obj->data, 0, size);
    else {
	if (data_size > size)
	    data_size = size;
	memcpy(obj->data, data, data_size);
    }
    init_object(obj, type);
    (void) __atomic_fetch_add(&ctx->num_objects, 1, __ATOMIC_SEQ_CST);
    enif_keep_resource(type);
    return obj;
}

static ERL_NIF_TERM share_new_type(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);    
    ERL_NIF_TERM term;
    share_type_t* tptr;
    dyn_build_t dt;
    share_type_t fixed[FIXED_SIZE];

    dt.alloc = 0;
    dt.size = dt.size0 = FIXED_SIZE;
    dt.base = dt.base0 = fixed;
    dt.cur  = 0;

    if (!build_type(env, argv[0], false, &dt)) {
	dyn_build_clean(&dt);
	return enif_make_badarg(env);
    }

    if ((tptr = enif_alloc_resource(type_r,
				    dt.cur * sizeof(share_type_t))) == NULL) {
	dyn_build_clean(&dt);
	return enif_make_badarg(env);
    }
    (void) __atomic_fetch_add(&ctx->num_types, 1, __ATOMIC_SEQ_CST);    
    memcpy(tptr, dt.base, dt.cur * sizeof(share_type_t));
    term = enif_make_resource(env, tptr);
    enif_release_resource(tptr);
    dyn_build_clean(&dt);    
    return term;
}

// New share object
// or New from binary
//
static ERL_NIF_TERM share_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM term;
    share_object_t* obj;
    share_type_t* tptr;
    ErlNifBinary bin;
    
    if (!enif_get_resource(env, argv[0], type_r, (void**)&tptr))
	return enif_make_badarg(env);
    memset(&bin, 0, sizeof(bin));
    if (argc >= 2) {
	if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
	    return enif_make_badarg(env);
    }
    if ((obj = new_object(env, tptr, bin.size, bin.data)) == NULL)
	return enif_make_badarg(env);
    term = enif_make_resource(env, obj);
    enif_release_resource(obj);
    return term;
}

static int set_value(ErlNifEnv* env, share_type_t* tptr, uint8_t* ptr,
		     ERL_NIF_TERM value)
{

    switch(*tptr) {
    case SHT_ARRAY: {
	sht_array_t* sp = (sht_array_t*) tptr;
	share_type_t* elem_type = sht_array_elem_type(sp);
	ERL_NIF_TERM list=value, hd, tl;
	int i = 0;
	
	while(enif_get_list_cell(env, list, &hd, &tl)) {
	    if (!set_value(env, elem_type,
			   ptr + i*sp->stride, hd))
		return 0;
	    i++;
	    list = tl;
	}
	if (!enif_is_empty_list(env, list))
	    return 0;
	return 1;
    }
    case SHT_STRUCT: {
	sht_struct_t* sp = (sht_struct_t*) tptr;
	ERL_NIF_TERM list=value, hd, tl;
	int i = 0;
	
	while(enif_get_list_cell(env, list, &hd, &tl)) {
	    sht_field_t* fp = &sp->spec[i];	    
	    if (!set_value(env, fp->spec + fp->t_offset,
			   ptr + fp->e_offset, hd))
		return 0;
	    i++;
	    list = tl;
	}
	if (!enif_is_empty_list(env, list))
	    return 0;
	return 1;	
    }
    case SHT_UINT8:
    case SHT_UINT16:
    case SHT_UINT32:
    case SHT_UINT64:	
    case SHT_UINT128:
    case SHT_INT8:
    case SHT_INT16:
    case SHT_INT32:
    case SHT_INT64:
    case SHT_INT128:
    case SHT_FLOAT16:
    case SHT_FLOAT32:
    case SHT_FLOAT64:
    case SHT_FLOAT128:
    case SHT_COMPLEX64:
    case SHT_COMPLEX128: {
	double dval;
	uint64_t uval;
	int64_t ival;
	int arity, sign;
	const ERL_NIF_TERM* digits;
	ERL_NIF_TERM rt, it;

	if (enif_get_double(env, value, &dval)) {
	    if (!share_set_float(tptr, dval, ptr))
		return 0;
	}
	else if (enif_get_int64(env, value, &ival)) {
	    if (!share_set_int64(tptr, ival, ptr))
		return 0;
	}
	else if (enif_get_uint64(env, value, &uval)) {
	    if (!share_set_uint64(tptr, uval, ptr))
		return 0;
	}
	else if (enif_get_big(env, value, &arity, &sign, &digits)) {
	    if (!share_set_big(tptr, arity, sign, digits, ptr))
		return 0;
	}
	else if (enif_get_list_cell(env, value, &rt, &it)) {  // complex
	    double r, i;
	    if (!get_number(env, rt, &r))
		return 0;
	    if (!get_number(env, it, &i))
		return 0;
	    if (!share_set_complex(tptr, r, i, ptr))
		return 0;
	}
	else
	    return 0;
	break;
    }
    default:
	return 0;
    }
    return 1;
}

static ERL_NIF_TERM share_setelement(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    share_object_t* obj;
    share_type_t* tptr;
    uint8_t* ptr;
    
    if (!enif_get_resource(env, argv[0], object_r, (void**) &obj))
	return enif_make_badarg(env);

    tptr = obj->type;
    ptr = obj->data;
    DEBUGF("setelement ptr=%x\r\n", ptr);
    
    if (!get_path(env, argv[1], PATH_FLAG_RESIZE, tptr, &tptr, ptr, &ptr))
	return enif_make_badarg(env);

    DEBUGF("setelement path %T points to type: %s, ptr=%x\r\n",
	   argv[1], sht_type_name(tptr), ptr);

    if (!set_value(env, tptr, ptr, argv[2]))
	return enif_make_badarg(env);
    return ATOM(ok);    
}

// FIXME handle return of full objects not only primitive elements
static ERL_NIF_TERM share_element(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    share_object_t* obj;
    share_type_t* tptr;
    uint8_t* ptr;

    if (!enif_get_resource(env, argv[0], object_r, (void**) &obj))
	return enif_make_badarg(env);
    
    ptr = obj->data;
    tptr = obj->type;
    DEBUGF("element ptr=%x\r\n", ptr);
    
    if (!get_path(env, argv[1], PATH_FLAG_NONE, tptr, &tptr, ptr, &ptr))
	return enif_make_badarg(env);
    DEBUGF("element path %T points to type: %s, ptr=%x\r\n",
	   argv[1], sht_type_name(tptr), ptr);    
    return make_value(env, tptr, ptr);
}


static ERL_NIF_TERM share_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    share_object_t* obj;
    share_type_t* tptr;
    uint8_t* ptr;

    if (!enif_get_resource(env, argv[0], object_r, (void**) &obj))
	return enif_make_badarg(env);
    tptr = obj->type;
    ptr = obj->data;

    if (argc == 2) {
	if (!get_path(env, argv[1], PATH_FLAG_NONE, obj->type, &tptr, obj->data, &ptr))
	    return enif_make_badarg(env);
    }
    if (!sht_is_array(tptr))
	return enif_make_badarg(env);
    else {
	sht_array_t* sp = (sht_array_t*) tptr;
	size_t elem_size = sht_sizeof(sht_array_elem_type(sp));
	if (sp->size == 0) {
	    share_array_t** app = (share_array_t**) ptr;
	    share_array_t* ap = *app;
	    if (ap == NULL)
		return enif_make_ulong(env, 0);
	    else
		return enif_make_ulong(env, ap->size / elem_size);
	}
	else {
	    if (sp->s == 1)
		return enif_make_ulong(env, sp->size);
	    else {
		ERL_NIF_TERM list = enif_make_list(env, 0);
		sht_array_t* shape = (sht_array_t*) sp;
		int i;
		for (i = sp->s-1; i >= 0; i--) {
		    ERL_NIF_TERM hd = enif_make_ulong(env, shape[i].size);
		    list = enif_make_list_cell(env, hd, list);
		}
		return list;
	    }
	}
    }
}

// Resize objects (arrays) only top level or dynamic!
static ERL_NIF_TERM share_resize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    share_object_t* obj;
    share_type_t* tptr;
    uint8_t* ptr;
    size_t new_size;
    
    if (!enif_get_resource(env, argv[0], object_r, (void**) &obj))
	return enif_make_badarg(env);
    if (!get_path(env, argv[1], PATH_FLAG_NONE, obj->type,
		  &tptr, obj->data, &ptr))
	return enif_make_badarg(env);
    if (!enif_get_ulong(env, argv[2], &new_size))
	return enif_make_badarg(env);
    
    if (!sht_is_array(tptr))
	return enif_make_badarg(env);
    else {
	sht_array_t* sp = (sht_array_t*) tptr;
	size_t elem_size = sht_sizeof(sht_array_elem_type(sp));
	if (sp->size > 0) {
	    return enif_make_badarg(env);
	}
	else {
	    share_array_t** app = (share_array_t**) ptr;
	    share_array_t* ap = share_array_resize(*app, elem_size*new_size);
	    *app = ap;
	}
	return ATOM(ok);
    }
}

// calculate memory size of data type in bytes
// sizeof(Object) = sht_sizeof(Object->ype)
// sizeof(Type) = sht_sizeof(type)
// sizeof(Term) = size_of_type(Term)
static ERL_NIF_TERM share_sizeof(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* obj;
    
    if (enif_get_resource(env, argv[0], type_r, &obj)) {
	share_type_t* tptr = (share_type_t*) obj;
	size_t size = sht_sizeof(tptr);
	if (size == 0)
	    return enif_make_badarg(env);
	if (argc == 2) {
	    // path not supported for types
	    return enif_make_badarg(env);
	}
	return enif_make_ulong(env, size);
    }
    else if (enif_get_resource(env, argv[0], object_r, &obj)) {
	share_object_t* optr = (share_object_t*) obj;
	share_type_t* tptr = optr->type;
	size_t size;
	
	if (argc == 2) {
	    uint8_t* ptr = optr->data;
	    if (!get_path(env, argv[1], PATH_FLAG_NONE,
			  tptr, &tptr, ptr, &ptr))
		return enif_make_badarg(env);
	}
	size = sht_sizeof(tptr);
	if (size == 0)
	    return enif_make_badarg(env);
	return enif_make_ulong(env, size);	
    }
    else {
	dyn_build_t dt;
	share_type_t fixed[FIXED_SIZE];
	size_t size;
	
	if (argc == 2) {
	    // path not supported for types specs
	    return enif_make_badarg(env);
	}

	dt.alloc = 0;
	dt.size = dt.size0 = FIXED_SIZE;
	dt.base = dt.base0 = fixed;
	dt.cur  = 0;

	if (!build_type(env, argv[0], false, &dt)) {
	    dyn_build_clean(&dt);
	    return enif_make_badarg(env);
	}
	if ((size = sht_sizeof((share_type_t*) dt.base)) == 0) {
	    dyn_build_clean(&dt);
	    return enif_make_badarg(env);
	}
	dyn_build_clean(&dt);
	return enif_make_ulong(env, size);
    }
}

static ERL_NIF_TERM share_offsetof(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* obj;
    
    if (enif_get_resource(env, argv[0], object_r, &obj)) {
	share_object_t* optr = (share_object_t*) obj;
	share_type_t* tptr = optr->type;
	uint8_t* ptr = optr->data;
	size_t offset;

	if (!get_path(env, argv[1], PATH_FLAG_NONE,
		      tptr, &tptr, ptr, &ptr))
	    return enif_make_badarg(env);
	offset = ptr - optr->data;
	return enif_make_ulong(env, offset);
    }
    else if (enif_get_resource(env, argv[0], type_r, &obj)) {
	share_type_t* tptr = (share_type_t*) obj;
	uint8_t* ptr = NULL;
	
	if (!get_path(env, argv[1], PATH_FLAG_OFFSET,
		      tptr, &tptr, ptr, &ptr))
	    return enif_make_badarg(env);
	return enif_make_ulong(env, (size_t) ptr); 
    }    
    else {
	share_type_t* tptr;
	uint8_t* ptr;
	dyn_build_t dt;
	share_type_t fixed[FIXED_SIZE];
	
	dt.alloc = 0;
	dt.size = dt.size0 = FIXED_SIZE;
	dt.base = dt.base0 = fixed;
	dt.cur  = 0;

	if (!build_type(env, argv[0], false, &dt)) {
	    dyn_build_clean(&dt);
	    return enif_make_badarg(env);
	}
	tptr = (share_type_t*) dt.base;
	ptr = NULL;
    
	if (!get_path(env, argv[1], PATH_FLAG_OFFSET,
		      tptr, &tptr, ptr, &ptr)) {
	    dyn_build_clean(&dt);
	    return enif_make_badarg(env);
	}
	return enif_make_ulong(env, (size_t) ptr); 	
    }    
}

// return the term type of object or type-object
static ERL_NIF_TERM share_typeof(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* obj;
    
    if (enif_get_resource(env, argv[0], type_r, &obj)) {
	share_type_t* tptr = (share_type_t*) obj;
	if (argc == 2) {
	    uint8_t* ptr = NULL;
	    if (!get_path(env, argv[1], PATH_FLAG_OFFSET,
			  tptr, &tptr, ptr, &ptr))
		return enif_make_badarg(env);
	}
	return sht_typeof(env, tptr);
    }
    else if (enif_get_resource(env, argv[0], object_r, &obj)) {
	share_object_t* optr = (share_object_t*) obj;
	share_type_t* tptr = optr->type;
	if (argc == 2) {
	    uint8_t* ptr = optr->data;
	    if (!get_path(env, argv[1], PATH_FLAG_NONE,
			  tptr, &tptr, ptr, &ptr))
		return enif_make_badarg(env);
	}
	return sht_typeof(env, tptr);
    }
    else {
	dyn_build_t dt;
	share_type_t fixed[FIXED_SIZE];
	share_type_t* tptr;
	ERL_NIF_TERM term;
	
	dt.alloc = 0;
	dt.size = dt.size0 = FIXED_SIZE;
	dt.base = dt.base0 = fixed;
	dt.cur  = 0;

	if (!build_type(env, argv[0], false, &dt)) {
	    dyn_build_clean(&dt);
	    return enif_make_badarg(env);
	}
	tptr = (share_type_t*) dt.base;
	if (argc == 2) {
	    uint8_t* ptr = NULL;
	    if (!get_path(env, argv[1], PATH_FLAG_OFFSET,
			  tptr, &tptr, ptr, &ptr)) {
		dyn_build_clean(&dt);
		return enif_make_badarg(env);
	    }
	}
	term = sht_typeof(env, tptr);
	dyn_build_clean(&dt);
	return term;
    }
}

static ERL_NIF_TERM share_alignment(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* obj;
    size_t align;

    if (enif_get_resource(env, argv[0], object_r, &obj)) {
	share_object_t* optr = (share_object_t*) obj;
	share_type_t* tptr = optr->type;
	if (argc == 2) {
	    uint8_t* ptr = optr->data;
	    if (!get_path(env, argv[1], PATH_FLAG_NONE,
			  tptr, &tptr, ptr, &ptr))
		return enif_make_badarg(env);
	}
	return sht_typeof(env, tptr);
    }
    else if (enif_get_resource(env, argv[0], type_r, &obj)) {
	share_type_t* tptr = (share_type_t*) obj;
	if (argc == 2) {
	    uint8_t* ptr = NULL;
	    if (!get_path(env, argv[1], PATH_FLAG_OFFSET,
			  tptr, &tptr, ptr, &ptr))
		return enif_make_badarg(env);
	}
	if ((align = sht_align(tptr)) == 0)
	    return enif_make_badarg(env);
	return enif_make_ulong(env, align);
    }
    else {
	dyn_build_t dt;
	share_type_t fixed[FIXED_SIZE];
	share_type_t* tptr;
	dt.alloc = 0;
	dt.size = dt.size0 = FIXED_SIZE;
	dt.base = dt.base0 = fixed;
	dt.cur  = 0;

	if (!build_type(env, argv[0], false, &dt)) {
	    dyn_build_clean(&dt);
	    return enif_make_badarg(env);
	}
	tptr = (share_type_t*) dt.base;
	if (argc == 2) {
	    uint8_t* ptr = NULL;
	    if (!get_path(env, argv[1], PATH_FLAG_OFFSET,
			  tptr, &tptr, ptr, &ptr)) {
		dyn_build_clean(&dt);
		return enif_make_badarg(env);
	    }
	}
	if ((align = sht_align(tptr)) == 0) {
	    dyn_build_clean(&dt);
	    return enif_make_badarg(env);
	}
	dyn_build_clean(&dt);
	return enif_make_ulong(env, align);
    }
}

// create all tracing NIFs
#ifdef NIF_TRACE

#undef NIF

static void trace_print_arg_list(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    enif_fprintf(stdout, "(");
    if (argc > 0) {
	int i;
	if (enif_is_ref(env, argv[0])) {
	    // FIXME print object type if available
	    enif_fprintf(stdout, "%T", argv[0]);
	}
	else
	    enif_fprintf(stdout, "%T", argv[0]);
	for (i = 1; i < argc; i++)
	    enif_fprintf(stdout, ",%T", argv[i]);
    }
    enif_fprintf(stdout, ")");
}

#define NIF(name, arity, func)					\
static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]) \
{ \
    ERL_NIF_TERM result;					\
    enif_fprintf(stdout, "ENTER share:%s", (name));		\
    trace_print_arg_list(env, argc, argv);			\
    enif_fprintf(stdout, "\r\n");				\
    result = func(env, argc, argv);				\
    enif_fprintf(stdout, "  RESULT=%T\r\n", (result));		\
    enif_fprintf(stdout, "LEAVE %s\r\n", (name));		\
    return result;						\
}

NIF_LIST

#endif


// Declare all nif functions
#undef NIF
#ifdef NIF_TRACE
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]); \
    static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#else
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#endif

NIF_LIST

#undef NIF
#ifdef NIF_TRACE
#define NIF(name,arity,func) NIF_FUNC(name, arity, trace##_##func##_##arity),
#else
#define NIF(name,arity,func) NIF_FUNC(name, arity, func),
#endif

static ErlNifFunc nif_funcs[] =
{
    NIF_LIST
};

static int load_atoms(ErlNifEnv* env)
{
    LOAD_ATOM(ok);
    LOAD_ATOM(error);
    LOAD_ATOM(true);
    LOAD_ATOM(false);
    
    // api names
    LOAD_ATOM(object);
    LOAD_ATOM(type);
    // function names
    LOAD_ATOM(element);
    LOAD_ATOM(setelement);

    // unsigned types
    LOAD_ATOM(uint);
    LOAD_ATOM(uchar);
    LOAD_ATOM(ushort);
    LOAD_ATOM(ulong);
    LOAD_ATOM(uintptr_t);
    LOAD_ATOM(size_t);
    LOAD_ATOM(uint8_t);
    LOAD_ATOM(uint16_t);
    LOAD_ATOM(uint32_t);
    LOAD_ATOM(uint64_t);
    LOAD_ATOM(uint128_t);
    LOAD_ATOM(uint8);
    LOAD_ATOM(uint16);
    LOAD_ATOM(uint32);
    LOAD_ATOM(uint64);
    LOAD_ATOM(uint128);    
    // signed types
    LOAD_ATOM(int);
    LOAD_ATOM(char);
    LOAD_ATOM(short);
    LOAD_ATOM(long);
    LOAD_ATOM(intptr_t);
    LOAD_ATOM(ssize_t);    
    LOAD_ATOM(int8_t);
    LOAD_ATOM(int16_t);
    LOAD_ATOM(int32_t);
    LOAD_ATOM(int64_t);
    LOAD_ATOM(int128_t);
    LOAD_ATOM(int8);
    LOAD_ATOM(int16);
    LOAD_ATOM(int32);
    LOAD_ATOM(int64);
    LOAD_ATOM(int128);    
    // float types
    LOAD_ATOM(float);
    LOAD_ATOM(double);
    LOAD_ATOM(float16_t);
    LOAD_ATOM(float32_t);
    LOAD_ATOM(float64_t);
    LOAD_ATOM(float128_t);
    LOAD_ATOM(complex);    
    LOAD_ATOM(complex64_t);
    LOAD_ATOM(complex128_t);
    LOAD_ATOM(atm);
    
    // structured types
    LOAD_ATOM(array);
    LOAD_ATOM(struct);
    LOAD_ATOM(dict_ent);

    // array options
    LOAD_ATOM(rowmajor);
    LOAD_ATOM(size);
    LOAD_ATOM(stride);
    LOAD_ATOM(alignment);
    LOAD_ATOM(offset);
    
    return 0;
}




void clean_type(share_type_t* type)
{
    // scan type spec and release type reference pointers
}

// FIXME: release recursive typerefs when implemented
static void type_dtor(ErlNifEnv *env, share_type_t* type)
{
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    unsigned int val;
    
    __atomic_fetch_sub(&ctx->num_types, 1, __ATOMIC_SEQ_CST);
    __atomic_load(&ctx->num_types, &val, __ATOMIC_SEQ_CST);
    clean_type(type);
    DEBUGF("dtor: num_types=%lu", val);
}

void clean_object(share_object_t* obj)
{
    share_type_t* tptr = obj->type;
    uint8_t* ptr = obj->data;

    if (sht_is_array(tptr)) {
	sht_array_t* sp = (sht_array_t*) tptr;
	if (sp->size == 0) {
	    share_array_t** app = (share_array_t**) ptr;
	    share_array_t* ap = *app;
	    if (ap != NULL) {
		// FIXME: release dynamic arrays
	    }
	}
    }
}

static void object_dtor(ErlNifEnv *env, share_object_t* obj)
{
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    unsigned int val;
    share_type_t* type;
    
    __atomic_fetch_sub(&ctx->num_objects, 1, __ATOMIC_SEQ_CST);
    __atomic_load(&ctx->num_objects, &val, __ATOMIC_SEQ_CST);    
    DEBUGF("dtor: num_objects=%lu", val);
    type = obj->type;
    clean_object(obj);
    enif_free(obj->data);
    enif_release_resource(type);
}

static void type_dyncall(ErlNifEnv* caller_env, void* obj, void* call_data)
{
    sht_call_t* ct = (sht_call_t*) call_data;
    DEBUGF("share_nif: dyncall%s", "");
    ct->callback(caller_env, obj, ct->arg);
}

static void object_dyncall(ErlNifEnv* caller_env, void* obj, void* call_data)
{
    sht_call_t* ct = (sht_call_t*) call_data;
    DEBUGF("share_nif: dyncall%s", "");
    ct->callback(caller_env, obj, ct->arg);
}

static int share_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    nif_ctx_t* ctx;
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit t_init;
    ErlNifResourceTypeInit o_init;    
    
    DEBUGF("load%s", "");

    t_init.dtor = (ErlNifResourceDtor*) type_dtor;
    t_init.stop = (ErlNifResourceStop*) NULL;
    t_init.down = (ErlNifResourceDown*) NULL;
    t_init.members = 4;
    t_init.dyncall = (ErlNifResourceDynCall*) type_dyncall;

    o_init.dtor = (ErlNifResourceDtor*) object_dtor;
    o_init.stop = (ErlNifResourceStop*) NULL;
    o_init.down = (ErlNifResourceDown*) NULL;
    o_init.members = 4;
    o_init.dyncall = (ErlNifResourceDynCall*) object_dyncall;
    
    if ((type_r =
	 enif_init_resource_type(env,
				 "type",
				 &t_init,
				 ERL_NIF_RT_CREATE,
				 &tried)) == NULL) {
        return -1;
    }

    if ((object_r =
	 enif_init_resource_type(env,
				 "object",
				 &o_init,
				 ERL_NIF_RT_CREATE,
				 &tried)) == NULL) {
        return -1;
    }

    if ((ctx = (nif_ctx_t*) enif_alloc(sizeof(nif_ctx_t))) == NULL)
	return -1;
    memset(ctx, 0, sizeof(nif_ctx_t));
    if (load_atoms(env) < 0)
	return -1;
    *priv_data = ctx;
    return 0;
}

static int share_upgrade(ErlNifEnv* env, void** priv_data,
			void** old_priv_data,
			ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit t_init;
    ErlNifResourceTypeInit o_init;    
    // nif_ctx_t* ctx = (nif_ctx_t*) *old_priv_data;
    
    DEBUGF("upgrade%s", "");

    t_init.dtor = (ErlNifResourceDtor*) type_dtor;
    t_init.stop = (ErlNifResourceStop*) NULL;
    t_init.down = (ErlNifResourceDown*) NULL;
    t_init.members = 4;
    t_init.dyncall = (ErlNifResourceDynCall*) type_dyncall;

    o_init.dtor = (ErlNifResourceDtor*) object_dtor;
    o_init.stop = (ErlNifResourceStop*) NULL;
    o_init.down = (ErlNifResourceDown*) NULL;
    o_init.members = 4;
    o_init.dyncall = (ErlNifResourceDynCall*) object_dyncall;
    
    if ((type_r =
	 enif_open_resource_type_x(env,
				   "type",
				   &t_init,
				   ERL_NIF_RT_CREATE |
				   ERL_NIF_RT_TAKEOVER,
				   &tried)) == NULL) {
	return -1;
    }

    if ((object_r =
	 enif_open_resource_type_x(env,
				   "object",
				   &o_init,
				   ERL_NIF_RT_CREATE |
				   ERL_NIF_RT_TAKEOVER,
				   &tried)) == NULL) {
	return -1;
    }    
    if (load_atoms(env) < 0)
	return -1;
    *priv_data = *old_priv_data;
    return 0;
}

static void share_unload(ErlNifEnv* env, void* priv_data)
{
    nif_ctx_t* ctx = (nif_ctx_t*) priv_data;    
    UNUSED(env);
    UNUSED(priv_data);
    DEBUGF("unload%s", "");
    enif_free(ctx);
}

ERL_NIF_INIT(share,nif_funcs,share_load,NULL,share_upgrade,share_unload)
