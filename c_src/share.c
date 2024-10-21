//
//

#include "../include/share_type.h"

static void inline set_complex64(float r, float i, void* ptr)
{
    *((float complex*) ptr) = CMPLXF(r, i);
}

static void inline set_complex128(double r, double i, void* ptr)
{
    *((double complex*) ptr) = CMPLX(r, i);
}

int share_set_int64(share_type_t* type, int64_t value, void* ptr)
{
    switch(*type) {
    case SHT_UINT8:  *((uint8_t*)ptr) = value; break;
    case SHT_UINT16: *((uint16_t*)ptr) = value; break;
    case SHT_UINT32: *((uint32_t*)ptr) = value; break;
    case SHT_UINT64: *((uint64_t*)ptr) = value; break;
    case SHT_UINT128: *((uint128_t*)ptr) = value; break;	
    case SHT_INT8:  *((int8_t*)ptr) = value; break;
    case SHT_INT16: *((int16_t*)ptr) = value; break;
    case SHT_INT32: *((int32_t*)ptr) = value; break;
    case SHT_INT64: *((int64_t*)ptr) = value; break;
    case SHT_INT128: *((int128_t*)ptr) = value; break;		
    case SHT_FLOAT32: *((float*)ptr) = value; break;
    case SHT_FLOAT64: *((double*)ptr) = value; break;
    case SHT_FLOAT128: *((long double*) ptr) = value; break;
    case SHT_COMPLEX64: set_complex64((float)value, 0.0, ptr); break;
    case SHT_COMPLEX128: set_complex128((double)value, 0.0, ptr); break;
    default: return 0;
    }
    return 1;
}

int share_set_uint64(share_type_t* type, uint64_t value, void* ptr)
{
    switch(*type) {
    case SHT_UINT8:  *((uint8_t*)ptr) = value; break;
    case SHT_UINT16: *((uint16_t*)ptr) = value; break;
    case SHT_UINT32: *((uint32_t*)ptr) = value; break;
    case SHT_UINT64: *((uint64_t*)ptr) = value; break;
    case SHT_UINT128: *((uint128_t*)ptr) = value; break;	
    case SHT_INT8:  *((int8_t*)ptr) = value; break;
    case SHT_INT16: *((int16_t*)ptr) = value; break;
    case SHT_INT32: *((int32_t*)ptr) = value; break;
    case SHT_INT64: *((int64_t*)ptr) = value; break;
    case SHT_INT128: *((int128_t*)ptr) = value; break;		
    case SHT_FLOAT32: *((float*)ptr) = value; break;
    case SHT_FLOAT64: *((double*)ptr) = value; break;
    case SHT_FLOAT128: *((long double*) ptr) = value; break;
    case SHT_COMPLEX64: set_complex64((float)value, 0.0, ptr); break;
    case SHT_COMPLEX128: set_complex128((double)value, 0.0, ptr); break;
    default: return 0;
    }
    return 1;
}

int share_set_int128(share_type_t* type, int128_t value, void* ptr)
{
    switch(*type) {
    case SHT_UINT8:  *((uint8_t*)ptr) = value; break;
    case SHT_UINT16: *((uint16_t*)ptr) = value; break;
    case SHT_UINT32: *((uint32_t*)ptr) = value; break;
    case SHT_UINT64: *((uint64_t*)ptr) = value; break;
    case SHT_UINT128: *((uint128_t*)ptr) = value; break;	
    case SHT_INT8:  *((int8_t*)ptr) = value; break;
    case SHT_INT16: *((int16_t*)ptr) = value; break;
    case SHT_INT32: *((int32_t*)ptr) = value; break;
    case SHT_INT64: *((int64_t*)ptr) = value; break;
    case SHT_INT128: *((int128_t*)ptr) = value; break;		
    case SHT_FLOAT32: *((float*)ptr) = value; break;
    case SHT_FLOAT64: *((double*)ptr) = value; break;
    case SHT_FLOAT128: *((long double*) ptr) = value; break;
    case SHT_COMPLEX64:  set_complex64((float)value, 0.0, ptr); break;
    case SHT_COMPLEX128: set_complex128((double)value, 0.0, ptr); break;
    default: return 0;
    }
    return 1;
}

int share_set_uint128(share_type_t* type, uint128_t value, void* ptr)
{
    switch(*type) {
    case SHT_UINT8:  *((uint8_t*)ptr) = value; break;
    case SHT_UINT16: *((uint16_t*)ptr) = value; break;
    case SHT_UINT32: *((uint32_t*)ptr) = value; break;
    case SHT_UINT64: *((uint64_t*)ptr) = value; break;
    case SHT_UINT128: *((uint128_t*)ptr) = value; break;
    case SHT_INT8:  *((int8_t*)ptr) = value; break;
    case SHT_INT16: *((int16_t*)ptr) = value; break;
    case SHT_INT32: *((int32_t*)ptr) = value; break;
    case SHT_INT64: *((int64_t*)ptr) = value; break;
    case SHT_INT128: *((int128_t*)ptr) = value; break;		
    case SHT_FLOAT32: *((float*)ptr) = value; break;
    case SHT_FLOAT64: *((double*)ptr) = value; break;
    case SHT_FLOAT128: *((long double*) ptr) = value; break;
    case SHT_COMPLEX64: set_complex64((float)value, 0.0, ptr); break;
    case SHT_COMPLEX128: set_complex128((double)value, 0.0, ptr); break;
    default: return 0;
    }
    return 1;
}

int share_set_big(share_type_t* type, int n, int sign,
		  const ERL_NIF_TERM* ds, void* ptr)
{
    if (sizeof(ERL_NIF_TERM)*n <= 8) { // 64bit
	uint64_t absval = 0;
#if SIZEOF_VOID_P == 8
	absval = ds[0];
#else
	int i;
	for (i = n-1; i >= 0; i--)
	    absval = (absval << sizeof(ERL_NIF_TERM)*8) | ds[i];
#endif
	if (sign) {
	    absval = ~(absval-1);
	    return share_set_int64(type,(int64_t)absval, ptr);
	}
	else {
	    return share_set_uint64(type, absval, ptr);
	}
    }
    else {
	uint128_t absval = 0;
	int i;
	
	for (i = n-1; i >= 0; i--)
	    absval = (absval << sizeof(ERL_NIF_TERM)*8) | ds[i];
	if (sign)
	    absval = ~(absval-1);
	switch(*type) {
	case SHT_UINT8:   *((uint8_t*)ptr) = absval; break;
	case SHT_UINT16:  *((uint16_t*)ptr) = absval; break;
	case SHT_UINT32:  *((uint32_t*)ptr) = absval; break;
	case SHT_UINT64:  *((uint64_t*)ptr) = absval; break;
	case SHT_UINT128: *((uint128_t*)ptr) = absval; break;	
	case SHT_INT8:    *((uint8_t*)ptr) = absval; break;
	case SHT_INT16:   *((uint16_t*)ptr) = absval; break;
	case SHT_INT32:   *((uint32_t*)ptr) = absval; break;
	case SHT_INT64:   *((uint64_t*)ptr) = absval; break;
	case SHT_INT128:  *((uint128_t*)ptr) = absval; break;		
	case SHT_FLOAT32: *((float*)ptr) = (int128_t)absval; break;
	case SHT_FLOAT64: *((double*)ptr) = (int128_t)absval; break;
	case SHT_FLOAT128: *((long double*) ptr) = (int128_t)absval; break;
	case SHT_COMPLEX64:
	    set_complex64((float)((int128_t)absval), 0.0, ptr); break;
	    break;
	case SHT_COMPLEX128:
	    set_complex128((double)((int128_t)absval), 0.0, ptr); break;
	default: return 0;
	}
    }
    return 1;    
}

int share_set_float(share_type_t* type, double value, void* ptr)
{
    switch(*type) {
    case SHT_FLOAT32: *((float*)ptr) = value; break;
    case SHT_FLOAT64: *((double*)ptr) = value; break;
    case SHT_FLOAT128: *((long double*) ptr) = value; break;
    case SHT_COMPLEX64: set_complex64((float)value, 0.0, ptr); break;
    case SHT_COMPLEX128: set_complex128(value, 0.0, ptr); break;
    default: return share_set_int64(type, (int64_t) value, ptr);
    }
    return 1;
}

int share_set_complex(share_type_t* type, double r, double i, void* ptr)
{
    switch(*type) {
    case SHT_COMPLEX64: set_complex64(r, i, ptr); break;
    case SHT_COMPLEX128: set_complex128(r, i, ptr); break;
    default: return share_set_float(type, r, ptr);
    }
    return 1;    
}


int share_get_uint64(share_type_t* type, void* ptr, uint64_t* value_ptr)
{
    switch(*type) {
    case SHT_UINT8:  *value_ptr = *((uint8_t*)ptr); return 1;
    case SHT_UINT16: *value_ptr = *((uint16_t*)ptr); return 1;
    case SHT_UINT32: *value_ptr = *((uint32_t*)ptr); return 1;
    case SHT_UINT64: *value_ptr = *((uint64_t*)ptr); return 1;
    case SHT_UINT128: *value_ptr = *((uint128_t*)ptr); return 1;
    case SHT_INT8: {
	int8_t v = *((int8_t*)ptr);
	if (v < 0) return 0;
	*value_ptr = v;
	return 1;
    }
    case SHT_INT16: {
	int16_t v = *((int16_t*)ptr);
	if (v < 0) return 0;
	*value_ptr = v;
	return 1;
    }
    case SHT_INT32: {
	int32_t v = *((int32_t*)ptr);
	if (v < 0) return 0;
	*value_ptr = v;
	return 1;
    }
    case SHT_INT64: {
	int64_t v = *((int64_t*)ptr);
	if (v < 0) return 0;
	*value_ptr = v;
	return 1;	
    }
    case SHT_INT128: {
	int128_t v = *((int128_t*)ptr);
	if (v < 0) return 0;
	*value_ptr = v;
	return 1;	
    }	
    case SHT_FLOAT32: {
	float f = *((float*) ptr);
	if (f < 0) return 0;
	*value_ptr = f;
	return 1;
    }
    case SHT_FLOAT64: {
	double f = *((double*) ptr);
	if (f < 0) return 0;
	*value_ptr = f;
	return 1;
    }
    case SHT_FLOAT128: {
	long double f = *((long double*) ptr);
	if (f < 0) return 0;
	*value_ptr = f;
	return 1;
    }
    case SHT_COMPLEX64: {
	complex x = *((complex*) ptr);
	if (creal(x) < 0) return 0;
	*value_ptr = creal(x);
	return 1;
    }
    case SHT_COMPLEX128: {
	double complex x = *((double complex*) ptr);
	if (creal(x) < 0) return 0;
	*value_ptr = creal(x);
	return 1;
    }	
    default: return 0;
    }
    return 1;
}
