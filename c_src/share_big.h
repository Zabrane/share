/* bignum support */

#ifndef __SHARE_BIG__
#define __SHARE_BIG__

#define TAG_PRIMARY_HEADER	0x0
#define _TAG_PRIMARY_MASK	0x3
#define _TAG_PRIMARY_SIZE	2
#define TAG_PRIMARY_BOXED	0x2
#define POS_BIG_SUBTAG          (0x2 << _TAG_PRIMARY_SIZE)
#define NEG_BIG_SUBTAG          (0x3 << _TAG_PRIMARY_SIZE)
#define _BIG_SIGN_BIT		(0x1 << _TAG_PRIMARY_SIZE)
#define _TAG_HEADER_POS_BIG	(TAG_PRIMARY_HEADER|POS_BIG_SUBTAG)
#define _TAG_HEADER_NEG_BIG	(TAG_PRIMARY_HEADER|NEG_BIG_SUBTAG)
#define _TAG_HEADER_MASK	0x3F
#define _HEADER_ARITY_OFFS	6
#define _is_bignum_header(x)	(((x) & (_TAG_HEADER_MASK-_BIG_SIGN_BIT)) == _TAG_HEADER_POS_BIG)
#define EXPAND_POINTER(AnEterm) ((ERL_NIF_TERM) (AnEterm))
#define boxed_val(x) ((ERL_NIF_TERM*) EXPAND_POINTER(((x) - TAG_PRIMARY_BOXED)))
#define is_boxed(x)	(((x) & _TAG_PRIMARY_MASK) == TAG_PRIMARY_BOXED)
#define is_big(x)	(is_boxed((x)) && _is_bignum_header(*boxed_val((x))))
#define _header_arity(x)	((x) >> _HEADER_ARITY_OFFS)
#define ptr_val(x)   ((ERL_NIF_TERM*)((x) & ~((unsigned long) 0x3)))

// debugging
static inline void print_big(char* msg,  ERL_NIF_TERM* dp, size_t n)
{
    int i;

    printf("%s[%d] ", msg, (int)n);
    for (i = 0; i < (int)n; i++) {
	printf(":%0*lx", (int)(2*sizeof(ERL_NIF_TERM)), dp[i]);
    }
    printf("\r\n");    
}


int enif_is_big(ErlNifEnv* env, ERL_NIF_TERM big_term)
{
    return is_big(big_term);
}

// make unsigned bignum from digits
static inline ERL_NIF_TERM enif_make_ubig(ErlNifEnv* env,
					  ERL_NIF_TERM* ds, size_t n)
{
    ERL_NIF_TERM dp[n];
    ERL_NIF_TERM* dptr;
    int i;
#if __BYTE_ORDER == __LITTLE_ENDIAN
    for (i = 0; i < n; i++)
	dp[i] = ds[i];
#else
    for (i = 0; i < n; i++)
	dp[i] = ds[n-1-i];    
#endif
    // trim off zero digits
    i = n-1;
    while((i>0) && !dp[i])
	i--;
    n = i+1;
    // check for "smallnum"
    if (n*sizeof(ERL_NIF_TERM) <= 8) {
	if (n == 1)
	    return enif_make_uint64(env, dp[0]);
#if SIZEOF_VOID_P == 4
	else if (n == 2) // 32 bit digits
	    return enif_make_uint64(env, dp[0]+(dp[1]<<32));
#endif
	else // internal error
	    return enif_make_badarg(env);
    }
    else {
	ERL_NIF_TERM t = enif_make_tuple_from_array(env,dp,n);
	dptr = ptr_val(t);
	*dptr = (*dptr & ~_TAG_HEADER_MASK) | _TAG_HEADER_POS_BIG;
	return t;
    }
}

static inline ERL_NIF_TERM enif_make_big(ErlNifEnv* env,
					 ERL_NIF_TERM* ds, size_t n)
{
    ERL_NIF_TERM t;
    ERL_NIF_TERM dp[n];
    ERL_NIF_TERM* dptr;    
    int i;
    int sign;
#if __BYTE_ORDER == __LITTLE_ENDIAN
    for (i = 0; i < n; i++)
	dp[i] = ds[i];
#else
    for (i = 0; i < n; i++)
	dp[i] = ds[n-1-i];
#endif
    // print_big("make_big", dp, n);
    
    // check for negative number (sign bit is set)
    if ((sign = (dp[n-1] >> (sizeof(ERL_NIF_TERM)*8-1)))) {
	for (i = 0; i < n; i++) // complement
	    dp[i] = ~dp[i];
	// add 1
	i = -1;
	do {
	    i++;
	    t = dp[i];
	    dp[i]++;
	} while((i < n-1) && (dp[i] < t));
    }
    // printf("sign=%d\r\n", sign);
    // print_big("make_big1", dp, n);
    // trim zeros
    i = n-1;
    while((i>0) && !dp[i])
	i--;
    n = i+1;
    // print_big("make_big2", dp, n);
    
    // check for "smallnum"
    if (n*sizeof(ERL_NIF_TERM) <= 8) {
	if (n == 1) {
	    if (sign) // FIXME!
		return enif_make_int64(env, -((int64_t)dp[0]));
	    else
		return enif_make_int64(env, (int64_t)dp[0]);
	}
#if SIZEOF_VOID_P == 4
	else if (n == 2) { // 32 bit digits
	    uint64_t v = dp[0]+(dp[1]<<32);
	    if (sign) // FIXME!
		return enif_make_int64(env, -((int64_t)v));
	    else
		return enif_make_int64(env, (int64_t)v);
	}
#endif
	else // internal error
	    return enif_make_badarg(env);
    }
    else {
	ERL_NIF_TERM t = enif_make_tuple_from_array(env,dp,n);
	dptr = ptr_val(t);
	*dptr = (*dptr & ~_TAG_HEADER_MASK) |
	    (sign ? _TAG_HEADER_NEG_BIG : _TAG_HEADER_POS_BIG);	
	return t;
    }    
}

static inline ERL_NIF_TERM enif_get_big(ErlNifEnv* env,
					ERL_NIF_TERM term,
					int* arity,
					int* sign,
					const ERL_NIF_TERM** ds)
{
    ERL_NIF_TERM* ptr;
    if (!is_boxed(term))
	return 0;
    ptr = boxed_val(term);
    if (!_is_bignum_header(*ptr))
	return 0;
    *sign  = *ptr & _BIG_SIGN_BIT;
    *arity = _header_arity(*ptr);
    *ds = ptr + 1;
    return 1;
}

#endif
