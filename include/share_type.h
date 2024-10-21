// General data type encoding (audio video vector simd etc)

#ifndef __SAMPLE_H__
#define __SAMPLE_H__

#include "erl_nif.h"
#include <stdint.h>
#include <complex.h>

#ifdef __SIZEOF_INT128__
// this will probably break as soon as stdint is updated!
typedef __uint128_t uint128_t;
typedef __int128_t int128_t;
#endif

typedef enum
{
    true = 1,
    false = 0
} bool_t;

// base types
#define SHT_NONE       0x0000  // 0000 0000
#define SHT_UNSIGNED   0x0000  // 0000 xxxx
#define SHT_SIGNED     0x0010  // 0001 xxxx
#define SHT_INTEGER    0x0020  // 0010 xxxx
#define SHT_FLT        0x0040  // 0100 xxxx
#define SHT_CMPL       0x0080  // 1000 xxxx    
#define SHT_SIZE_8     0x0003  // tttt 0003  // 2^3
#define SHT_SIZE_16    0x0004  // tttt 0004  // 2^4
#define SHT_SIZE_32    0x0005  // tttt 0005  // 2^5
#define SHT_SIZE_64    0x0006  // tttt 0006  // 2^6
#define SHT_SIZE_128   0x0007  // tttt 0007  // 2^7
#define SHT_UINT8      SHT_UNSIGNED+SHT_INTEGER+SHT_SIZE_8
#define SHT_UINT16     SHT_UNSIGNED+SHT_INTEGER+SHT_SIZE_16
#define SHT_UINT32     SHT_UNSIGNED+SHT_INTEGER+SHT_SIZE_32
#define SHT_UINT64     SHT_UNSIGNED+SHT_INTEGER+SHT_SIZE_64
#define SHT_UINT128    SHT_UNSIGNED+SHT_INTEGER+SHT_SIZE_128
#define SHT_INT8       SHT_SIGNED+SHT_INTEGER+SHT_SIZE_8
#define SHT_INT16      SHT_SIGNED+SHT_INTEGER+SHT_SIZE_16
#define SHT_INT32      SHT_SIGNED+SHT_INTEGER+SHT_SIZE_32
#define SHT_INT64      SHT_SIGNED+SHT_INTEGER+SHT_SIZE_64
#define SHT_INT128     SHT_SIGNED+SHT_INTEGER+SHT_SIZE_128
#define SHT_FLOAT16    SHT_SIGNED+SHT_FLT+SHT_SIZE_16
#define SHT_FLOAT32    SHT_SIGNED+SHT_FLT+SHT_SIZE_32
#define SHT_FLOAT64    SHT_SIGNED+SHT_FLT+SHT_SIZE_64
#define SHT_FLOAT128   SHT_SIGNED+SHT_FLT+SHT_SIZE_128
// all both float-complex and integer-complex?
#define SHT_COMPLEX64  SHT_SIGNED+SHT_CMPL+SHT_SIZE_64
#define SHT_COMPLEX128 SHT_SIGNED+SHT_CMPL+SHT_SIZE_128
#define SHT_ATM        0x0100   // ERL_NIF_TERM (atom)
#define SHT_ARRAY      0x1000   // share_array_t
#define SHT_STRUCT     0x2000   // share_struct_t

#define SHT_CHAR SHT_INT8
#define SHT_UCHAR SHT_UINT8

#if __SIZEOF_SHORT__ == 2
#define SHT_SHORT SHT_INT16
#define SHT_USHORT SHT_UINT16
#else
#error "Unsupported size of SHT_SHORT"
#endif

#if __SIZEOF_INT__ == 4
#define SHT_INT SHT_INT32
#define SHT_UINT SHT_UINT32
#elif __SIZEOF_INT__ == 8
#define SHT_INT SHT_INT64
#define SHT_UINT SHT_UINT64
#else
#error "Unsupported size of SHT_INT"
#endif

#if __SIZEOF_LONG__ == 4
#define SHT_LONG SHT_INT32
#define SHT_ULONG SHT_UINT32
#elif __SIZEOF_LONG__ == 8
#define SHT_LONG SHT_INT64
#define SHT_ULONG SHT_UINT64
#else
#error "Unsupported size of SHT_LONG"
#endif

#if __SIZEOF_SIZE_T__ == 4
#define SHT_SSIZE SHT_INT32
#define SHT_SIZE  SHT_UINT32
#elif __SIZEOF_LONG__ == 8
#define SHT_SSIZE SHT_INT64
#define SHT_SIZE SHT_UINT64
#else
#error "Unsupported size of SHT_SIZE"
#endif

#if __SIZEOF_POINTER__ == 4
#define SHT_INTPTR  SHT_INT32
#define SHT_UINTPTR SHT_UINT32
#elif __SIZEOF_LONG__ == 8
#define SHT_INTPTR  SHT_INT64
#define SHT_UINTPTR SHT_UINT64
#else
#error "Unsupported size of SHT_UINTPTR"
#endif

#define SHT_FLOAT SHT_FLOAT32
#define SHT_DOUBLE SHT_FLOAT64

typedef uintptr_t share_type_t;
typedef uintptr_t share_size_t;
typedef intptr_t share_ssize_t;
typedef uintptr_t share_atom_t;

typedef struct
{
    share_type_t type;      // SHT_ARRAY
    share_size_t s;         // number of dimension (s > 0)
    share_type_t t_size;    // size (in words) of element spec
    share_type_t e_size;    // size (in bytes) of array (0=ptr)
    share_type_t rowmajor;  // size (in words) of element spec
    share_size_t size;
    share_ssize_t stride;
    share_type_t alignment; // alignment of elements
    share_type_t spec[];    // s-1 sht_srray_t + element type
    // sht_array_t[s-1]             // s-1 "sub-arrays" follow here
    // share_type_t base_type[];    // element type data spec+t_offset
} sht_array_t;

#define sht_array_elem_type(sp) ((share_type_t*) ((sp)->spec))
#define sht_array_base_type(sp) ((share_type_t*) ((sht_array_t*)(sp))[sp->s-1].spec)


typedef struct
{
    share_atom_t name;      // atom field name
    share_size_t t_offset;  // offset (in words) element type
    share_size_t e_offset;  // offset (in bytes) element data
    share_type_t spec[];    // spec + t_offset is location of type
} sht_field_t;

typedef struct
{
    share_type_t type;       // SHT_STRUCT
    share_size_t n;          // declared number of fields    
    share_type_t t_size;     // size (in words) of all field type specs
    share_type_t e_size;     // size (in bytes) of struct type
    share_type_t alignment;  // (max) alignment of elements
    sht_field_t  spec[0];    // n fields
} sht_struct_t;

// resource object (object_r)
typedef struct
{
    share_type_t* type;   // resource of object (type)
    uint8_t* data;        // pointer to data (enif_alloc)
} share_object_t;

// dynamic arrays use this structure, declared as {array, 0, Type}
typedef struct
{
    size_t size;          // current size
    size_t alloc;         // allocated size
    uint8_t data[];
} share_array_t;

#define sht_bit_size(t)     (1 << ((t) & 0xf))
#define sht_byte_size(t)    (1 << (((t) & 0xf)-3))
#define nsht_byte_size(t,n) ((n) << (((t) & 0xf)-3))
#define sht_is_integer(t)   (((t) & SHT_INTEGER) == SHT_INTEGER)
#define sht_is_signed(t)    (((t) & SHT_SIGNED) == SHT_SIGNED)
#define sht_is_float(t)     (((t) & 0xf0) == SHT_FLT)
#define sht_is_complex(t)   (((t) & 0xf0) == SHT_CMPL)
#define sht_is_number(t)    (((t) & 0xff00) == 0x0000)
#define sht_is_atom(t)      ((t) == SHT_ATM)
#define sht_is_scalar(t)    (sht_is_number((t)) || sht_is_atom((t)))

#define sht_is_array(tp)   (*((share_type_t*) (tp)) == SHT_ARRAY)
#define sht_is_struct(tp)  (*((share_type_t*) (tp)) == SHT_STRUCT)

typedef struct
{
    void (*callback)(ErlNifEnv*, void* obj, void* arg);
    void* arg;
} sht_call_t;

// SETTERS
extern int share_set_big(share_type_t* type, int n, int sign,
			 const ERL_NIF_TERM* ds, void* ptr);
extern int share_set_int64(share_type_t* type, int64_t value, void* ptr);
extern int share_set_float(share_type_t* type, double value, void* ptr);
extern int share_set_uint64(share_type_t* type, uint64_t value, void* ptr);
extern int share_set_complex(share_type_t* type, double r, double i, void* ptr);
extern int share_set_dict_key(share_type_t* type, ERL_NIF_TERM key, void* ptr);
// GETTERS
extern int share_get_uint64(share_type_t* type, void* ptr, uint64_t* value_ptr);

#endif
