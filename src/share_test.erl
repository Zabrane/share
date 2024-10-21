%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%     Test share
%%% @end
%%% Created : 14 Oct 2024 by Tony Rogvall <tony@rogvall.se>

-module(share_test).

-export([test/0]).
-export([format_ctype/1]).

-compile(export_all).


test() ->
    #{type := point} = test_point(),
    _ = test_offset_array(),
    _ = test_offset_data(),
    ok = test_uint128(),
    ok = test_int128(),
    ok = test_complex(),
    ok.
    
test_dict() ->    
    Spec = {array, 4, {dict_ent, atm, double}},
    Type = share:new_type(Spec),
    Obj = share:new(Type),

    %% write keys
    share:setelement(Obj, [{0}], {a,3.0}),
    share:setelement(Obj, [{1}], {b,2.0}),
    share:setelement(Obj, [{2}], {c,1.0}),
    share:setelement(Obj, [{3}], {d,0.0}),

%%    setelement(Obj, [{0,1}], 3.0),
%%    setelement(Obj, [{1,1}], 2.0),
%%    setelement(Obj, [{2,1}], 1.0),
%%    setelement(Obj, [{3,1}], 0.0),

    [
     share:element(Obj, [{0,0}]),
     share:element(Obj, [{0,1}]),
     share:element(Obj, [{0}])
    ].
     
     

    

test_point() ->
    Spec = {struct, [{x,float},{y,float}]},
    T = share:new_type(Spec),
    share_debug:debug_type(T, Spec),
    O = share:new(T),
    share_debug:debug_object("before ", O),
    share:setelement(O, [x], 1.0),
    share:setelement(O, [y], 2.0),
    share_debug:debug_object("after ", O),
    io:format("decode_obj2 = ~p\n", [share_debug:decode_obj(O)]),
    #{type=>point,
      x=>share:element(O, [x]),
      y=>share:element(O, [y])}.

test_offset_array() ->
    Spec = {array, 8, {struct, [{a,int8_t},        %% y 0
				{b,int16_t},       %% y 1 pad 1 read 2
				{c,int32_t},       %% y 4 pad 0 read 1
				{d,int8_t},        %% y 9 pad 7 read 8
				{e,float64_t}]}},  %% y 24
    T = share:new_type(Spec),
    O = share:new(T),
    {Size,Bin} = share:info(T),
    io:format("sizeof(~p) = ~p\n", [Spec,Size]),
    io:format("decode_type = ~p\n", [share_debug:decode_type(Bin)]),
    io:format("~s\n", [format_ctype(share_debug:decode_type(Bin))]),
    lists:foreach(
      fun(I) ->
	      share:setelement(O, [I,a], 1),
	      share:setelement(O, [I,b], 2),
	      share:setelement(O, [I,c], 3),
	      share:setelement(O, [I,d], 4),
	      share:setelement(O, [I,e], 3.14)
      end, lists:seq(0, 7)),
    io:format("decode_obj = ~p\n", [share_debug:decode_obj(O)]),
    lists:map(
      fun(I) ->    
	      #{ a=>share:element(O, [I,a]),
		 b=>share:element(O, [I,b]),
		 c=>share:element(O, [I,c]),
		 d=>share:element(O, [I,d]),
		 e=>share:element(O, [I,e])}
      end, lists:seq(0, 7)).

test_offset_data() ->
    Filename = filename:join(code:priv_dir(share), "struct.dat"),
    {ok, Bin} = file:read_file(Filename),
    Spec = {array, 8, {struct, [{a,int8_t},        %% y 0
				{b,int16_t},       %% y 1 pad 1 read 2
				{c,int32_t},       %% y 4 pad 0 read 1
				{d,int8_t},        %% y 9 pad 7 read 8
				{e,float64_t}]}},  %% y 24
    share_debug:decode_obj(Spec, Bin).

test_uint128() ->
    T = share:new_type(uint128_t),
    Obj = share:new(T),
    share:setelement(Obj, [], 1000),
    1000 = share:element(Obj, []),
    share:setelement(Obj, [], 1_000_000_000_000),
    1_000_000_000_000 = share:element(Obj, []),

    V0 =  (1 bsl 127),
    share:setelement(Obj, [], V0),
    V0 = share:element(Obj, []),

    V1 =  (1 bsl 128)-1,
    share:setelement(Obj, [], V1),
    V1 = share:element(Obj, []),
    ok.

test_int128() ->
    T = share:new_type(int128_t),
    Obj = share:new(T),
    share:setelement(Obj, [], 1000),
    1000 = share:element(Obj, []),
    share:setelement(Obj, [], -1000),
    -1000 = share:element(Obj, []),

    V0 = ((1 bsl 127)-1),
    share:setelement(Obj, [], V0),
    V0 = share:element(Obj, []),

    V1 = -(1 bsl 127),
    share:setelement(Obj, [], V1),
    V1 = share:element(Obj, []),
    ok.

test_complex() ->
    T1 = share:new_type(complex),
    Obj1 = share:new(T1),
    V = [1.0|2.0],
    share:setelement(Obj1, [], V),
    V = share:element(Obj1, []),

    T2 = share:new_type(complex),
    Obj2 = share:new(T2),
    share:setelement(Obj2, [], V),
    V = share:element(Obj2, []),
    ok.

%%
%% Test a type spec
%% array
%% struct
%% basic
%%
%% Generate a c program that generate data
%% to a file. Read it back and decode
%% check offsets etc.
%%
test_c_interaction(Type) ->
    PrivDir = code:priv_dir(share),
    TypeFmt = format_ctype(Type),
    SizeFmt = format_ctype_sizes(Type),
    Data = gen_data(Type),
    DataFmt = format_data(Type,Data),
    Code = 
	[
	 "#include <stdio.h>\n",
	 "#include <stdlib.h>\n",
	 "#include <stdint.h>\n",
	 "#include <unistd.h>\n",
	 "#include <complex.h>\n",
	 [TypeFmt," x",SizeFmt," = ", DataFmt, ";\n"],
	 "int main(int argc, char** argv)\n",
	 "{\n",
	 "\twrite(1, &x, sizeof(x));\n",
	 "\texit(0);\n",
	 "}\n"],
    CFileName = filename:join(PrivDir, "share_dat.c"),
    ExecName = filename:join(PrivDir, "share_dat"),
    DatName = filename:join(PrivDir, "share.dat"),
    ok = file:write_file(CFileName, Code),
    io:put_chars(Code),
    CompileResult = os:cmd(["gcc -o ",ExecName," ",CFileName]),
    io:put_chars(CompileResult),
    os:cmd([ExecName," > ", DatName]),
    {ok,Bin} = file:read_file(DatName),
    Type1 = share:typeof(Type),
    Decode = share_debug:decode_obj(Type1, Bin),
    #{ compile_result => CompileResult,
       gen_data => Data,
       type_result => Type1, 
       file_data => Bin,
       decode_result => Decode }.


format_data({array, SizeOpt, Type}, Data) ->
    {Sizes,Opts} = get_sizes_opt(SizeOpt),
    format_array_data(Sizes, Opts, Type, Data);
format_data({struct, Fields}, Data) ->
    ["{",format_fields_data(Fields, Data), "}"];
format_data(complex, Data) ->
    format_data(complex64_t, Data);
format_data(complex64_t, [R|I]) ->
    "CMPLXF("++io_lib_format:fwrite_g(R)++","++
	io_lib_format:fwrite_g(I)++")";
format_data(complex128_t, [R|I]) ->
    "CMPLX("++io_lib_format:fwrite_g(R)++","++
	io_lib_format:fwrite_g(I)++")";
format_data(Type, Data) when is_atom(Type) ->
    if is_integer(Data) ->
	    integer_to_list(Data);
       is_float(Data) ->
	    io_lib_format:fwrite_g(Data)
    end.

format_fields_data([{_Name,Type}], [Val]) ->
    [format_data(Type, Val)];
format_fields_data([{_Name,Type}|Fs], [Val|Vs]) ->
    [format_data(Type, Val),"," | format_fields_data(Fs, Vs)].

format_array_data([Size|Sizes],Opts,Type,Vs) ->
    case length(Vs) of
	Size -> 
	    ["{",
	     lists:join(",",
			[format_array_data(Sizes,Opts,Type,Val) ||  
			    Val <- Vs ]),
	     "}"]
    end;
format_array_data([], _Opts, Type, Val) ->
    format_data(Type, Val).


gen_data({struct, Fields}) when is_list(Fields) ->
    [gen_data(Type) || {_Name,Type} <- Fields];
gen_data({array, Opts, Type}) ->
    {Sizes, Opts1} = get_sizes_opt(Opts),
    gen_array_data(Sizes, Opts1, Type);
gen_data(Type) when is_atom(Type) ->
    case Type of
	int8_t -> random_range(-16#80,16#7f);
	int16_t -> random_range(-16#8000,16#7fff);
	int32_t -> random_range(-16#80000000,16#7fffffff);
	int64_t -> random_range(-16#8000000000000000,
				16#7fffffffffffffff);
	uint8_t -> random_range(16#00,16#ff);
	uint16_t -> random_range(16#0000,16#ffff);
	uint32_t -> random_range(16#00000000,16#ffffffff);
	uint64_t -> random_range(16#0000000000000000,
				 16#ffffffffffffffff);
	float32_t -> rand:uniform();
	float64_t -> rand:uniform();
	complex64_t -> [rand:uniform()|rand:uniform()];
	complex128_t -> [rand:uniform()|rand:uniform()];
	_ ->
	    case share:typeof(Type) of
		Type -> error({bad_type, Type});
		Type0 -> gen_data(Type0)
	    end
    end.

gen_array_data([Size], _, Type) ->
    [gen_data(Type) || _ <- lists:seq(1,Size)];
gen_array_data([Size|Sizes], Opt, Type) ->
    [gen_array_data(Sizes,Opt,Type) || _ <- lists:seq(1,Size)].


random_range(Min, Max) ->
    rand:uniform(Max-Min+1)-1+Min.

get_sizes_opt(Size) when is_integer(Size), Size > 0 ->
    {[Size], []};
get_sizes_opt(Sizes=[Size|_]) when is_integer(Size), Size > 0 ->
    {Sizes, []};
get_sizes_opt(Opts) when is_list(Opts) ->
    case lists:keytake(size, 1, Opts) of
	false -> {[], Opts};
	{value,{size,Size}, Opts1} -> 
	    case Size of 
		[Size1|_] when is_integer(Size1), Size1 > 0 -> {Size, Opts1};
		_ when is_integer(Size), Size > 0 -> {[Size], Opts1}
	    end
    end.

format_ctype(Type) ->
    case Type of
	uchar -> "unsigned char";
	ushort -> "unsigned short";
	uint -> "unsigned int";
	ulong -> "unsigned long";
	uintptr_t -> "uintptr_t";
	size_t -> "size_t";
	uint8   -> "uint8_t";
	uint16  -> "uint16_t";
	uint32  -> "uint32_t";
	uint64   -> "uint64_t";
	uint128 -> "uint128_t";
	uint8_t   -> "uint8_t";
	uint16_t  -> "uint16_t";
	uint32_t  -> "uint32_t";
	uint64_t   -> "uint64_t";
	uint128_t -> "uint128_t";
	char -> "char";
	short -> "short";
	int -> "int";
	long -> "long";
	intptr_t -> "intptr_t";
	ssize_t -> "ssize_t";
	int8    -> "int8_t";
	int16   -> "int16_t";
	int32   -> "int32_t";
	int64   -> "int64_t";
	int128  -> "int128_t";
	int8_t    -> "int8_t";
	int16_t   -> "int16_t";
	int32_t   -> "int32_t";
	int64_t   -> "int64_t";
	int128_t  -> "int128_t";
	float -> "float";
	float32_t -> "float";
	double -> "double";
	float64_t -> "double";
	complex -> "float complex";
	complex64_t -> "float complex";
	complex128_t -> "double complex";
	atm -> "ERL_NIF_TERM";
	{array,0,Te} -> format_ctype(Te);
	{array,_SizeOpt,Te} -> format_ctype(Te);
	{struct,Fs} ->
	    ["struct { ", 
	     [[format_ctype(T)," ",atom_to_list(N),";"] || 
		 {N,T} <- Fs], " }"];
	{dict_ent,K,V} ->
	    ["struct { ",
	     format_ctype(K)," key; ",
	     format_ctype(V)," value; ",
	     " }"]
    end.


format_ctype_sizes(Type) when is_atom(Type) -> "";  %% base type
format_ctype_sizes({array,SizeOpt,_BaseType}) ->
    case get_sizes_opt(SizeOpt) of
	[[0|_Size], _Opts] -> "";
	{Sizes,_Opts} ->
	    [["[",integer_to_list(Size),"]"] || Size <- Sizes]
    end;
format_ctype_sizes({struct,_Fs}) -> "";
format_ctype_sizes({dict_ent,_K,_V}) -> "".
