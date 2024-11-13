%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%     Test share
%%% @end
%%% Created : 14 Oct 2024 by Tony Rogvall <tony@rogvall.se>

-module(share_test).

-export([all/0]).
-export([format_ctype/1]).

-compile(export_all).

all() ->
    #{type := point} = test_point(),
    _ = test_offset_array(),
    _ = test_offset_data(),
    ok = test_uint128(),
    ok = test_int128(),
    ok = test_complex(),
    ok = test_c_interaction(),

    ok = test_c_interaction({array, 8,
			     {struct, [{a,int8_t},        %% y 0
				       {b,int16_t},       %% y 1 pad 1 read 2
				       {c,int32_t},       %% y 4 pad 0 read 1
				       {d,int8_t},        %% y 9 pad 7 read 8
				       {e,float64_t}]}}),  %% y 24
    Type6 = {struct, [{x,{array,2,short}},
		      {y,{array,5,char}}]},
    10 = share:sizeof(Type6),
    ok = test_c_interaction(Type6),
    ok.
    
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
    T1 = share:new_type(complex64_t),
    Obj1 = share:new(T1),
    V = [1.0|2.0],
    share:setelement(Obj1, [], V),
    V = share:element(Obj1, []),

    T2 = share:new_type(complex128_t),
    Obj2 = share:new(T2),
    share:setelement(Obj2, [], V),
    V = share:element(Obj2, []),
    ok.


test_c_bitfield() ->
    %% [4,7,7,6,2]
    %% Little endian:
    %% <<_:2,B:3,A:3, _:2,C:3,D:3, _:2,_:3,E:3>> = <<60,55,2>>.
    %% Big endian:
    %% <<A:3,B:3,_:2, C:3,D:3,_:2, E:3,_:3,_:2>> 

    test_c_interaction({struct, [{a,{uchar,3}},
				 {b,{uchar,3}},
				 {c,{uchar,3}},
				 {d,{uchar,3}},
				 {e,{uchar,3}}]}).

test_bitfield_0() -> 
    %% 15 bits backed in 2 bytes
    Type = {struct, [{x,{short,7}},
		     {y,{char,4}},
		     {z,{char,4}}]},
    io:format("Type = ~p~n", [Type]),
    T1 = share:new_type(Type),
    Type1 = share:typeof(T1),
    io:format("Type1 = ~p~n", [Type1]),

    X1 = share:new(T1),
    share:setelement(X1, [], [1,2,3]),
    [1,2,3] = share:element(X1, []),
    ok.

test_bitfield_1() -> 
    %% 15 bits backed 2 bytes
    Type = {struct, [{x,{char,4}},
		     {y,{char,4}},
		     {z,{short,7}}]},
    io:format("Type = ~p~n", [Type]),
    T = share:new_type(Type),
    Type1 = share:typeof(T),
    io:format("Type1 = ~p~n", [Type1]),

    X1 = share:new(T),
    Value1 =  [1,2,3],
    share:setelement(X1, [], Value1),
    [1,2,3] = share:element(X1, []),
    ok.

test_bitfield_2() -> 
    %% 13 bits backed 2 bytes
    Type = {struct, [{x,{char,7}},
		     {y,{char,3}},
		     {z,{short,3}}]},
    io:format("Type = ~p~n", [Type]),
    T = share:new_type(Type),
    Type1 = share:typeof(T),
    io:format("Type1 = ~p~n", [Type1]),

    X1 = share:new(T),
    Value1 =  [53,-4,1],
    share:setelement(X1, [], Value1),
    [53,-4,1] = share:element(X1, []),

    ok = test_c_interaction(Type).



test_bitfield_uint32_0() -> 
    %% 15 bits backed in short = 2 bytes
    Type = {struct, [{a,{uint32_t,13}},
		     {b,{uint32_t,13}},
		     {c,{uint32_t,13}},
		     {d,{uint32_t,13}},
		     {e,{uint32_t,13}}]},
    io:format("Type = ~p~n", [Type]),
    T = share:new_type(Type),
    Type1 = share:typeof(T),
    io:format("Type1 = ~p~n", [Type1]),
    X = share:new(T),
    share:setelement(X, [], [1,2,3,4,5]),
    [1,2,3,4,5] = share:element(X, []),
    ok.

test_bitfield_uchar_uint_0() -> 
    %% 15 bits backed in uchar and uint 8 bytes
    Type = {struct, [{a,{uchar,3}},
		     {b,{uchar,3}},
		     {c,{uint,3}},
		     {d,{uchar,3}},
		     {e,{uchar,3}}]},
    io:format("Type = ~p~n", [Type]),
    T = share:new_type(Type),
    Type1 = share:typeof(T),
    io:format("Type1 = ~p~n", [Type1]),
    X = share:new(T),
    share:setelement(X, [], [1,2,3,4,5]),
    [1,2,3,4,5] = share:element(X, []),
    ok.

test_bitfield_t1() ->
    share:new_type({struct, [{a,{ushort,3}},
			     {b,{ushort,3}},
			     {c,{ushort,3}},
			     {d,{ushort,3}},
			     {e,{ushort,3}}]}).

test_bitfield_t2() ->
    share:new_type({struct, [{a,{uchar,3}},
			     {b,{uchar,3}},
			     {c,{uchar,3}},
			     {d,{uchar,3}},
			     {e,{uchar,3}}]}).

test_bitfield_t3() ->
    share:new_type({struct, [{a,{uchar,3}},
			     {b,{uchar,3}},
			     {c,{uint,3}},
			     {d,{uchar,3}},
			     {e,{uchar,3}}]}).

test_bitfield_t4() ->
    share:new_type({struct, [{a,{uint,5}},
			     {b,{uint,5}},
			     {c,{uint,5}},
			     {d,{uint,5}}]}).

%% struct BoxProps {
%%     unsigned int  opaque       : 1;
%%     unsigned int  fill_color   : 3;
%%     unsigned int               : 4; // fill to 8 bits
%%     unsigned int  show_border  : 1;
%%     unsigned int  border_color : 3;
%%     unsigned int  border_style : 2;
%%     unsigned char              : 0; // fill to nearest byte (16 bits)
%%     unsigned char width        : 4; // Split a byte into 2 fields of 4 bits
%%     unsigned char height       : 4;
%% };

test_bitfield_pack_box_props() ->
    T = share:new_type({struct, [
				 {opaque,{uint,1}},
				 {fill_color,{uint,3}},
				 {'_',{uint,4}},
				 {show_border,{uint,1}},
				 {border_color,{uint,3}},
				 {border_style,{uint,2}},
				 {'_',{uint,0}},
				 {width,{uint,4}},
				 {height,{uint,4}}
				]}),
    4 = share:sizeof(T),
    X = share:new(T),
    share:setelement(X, [], [1,5,1,3,2,10,10]).


    
test_bitfield() -> 
    %% unsigned short a : 3, b : 3, c : 3, d : 3, e : 3;
    %% 15 bits backed in short = 2 bytes
    T1 = test_bitfield_t1(),
    
    %% unsigned char a : 3, b : 3, c : 3, d : 3, e : 3;
    %% 15 bits packed in char = 3 bytes
    %% | ab | cd | e_ |
    T2 = test_bitfield_t2(),
    
    %% unsigned char a : 3, b : 3;
    %% unsigned int c : 3;
    %% unsigned char d : 3, e : 3;
    %% 15 bits packed in int = 2 bytes?
    %% | ab | cde |
    T3 = test_bitfield_t3(),

    %% unsigned int a : 5, : 3, b : 5, : 0, c : 5, : 3, d : 5;
    %% 23 bits packed in int = 3 bytes?
    %% | a | b | c | d |
    T4 = test_bitfield_t4(),
    ok.



test_c_interaction() ->
    Types0 = [int8_t, int16_t, int32_t, int64_t,
	      uint8_t, uint16_t, uint32_t, uint64_t,
	      float32_t, float64_t, complex64_t, complex128_t],
    Types1 = [char, short, int, long, uchar, ushort, uint, ulong,
	      size_t, ssize_t, intptr_t, uintptr_t,
	      float, double,
	      complex],

    lists:foreach(fun(T) -> ok = test_c_interaction(T) end, Types0),
    lists:foreach(fun(T) -> ok = test_c_interaction(T) end, Types1),
    lists:foreach(fun(T) -> ok = test_c_interaction({array, 3, T}) end, Types0),
    lists:foreach(fun(T) -> ok = test_c_interaction({array, 8, T}) end, Types0),
    lists:foreach(fun(T) -> ok = test_c_interaction(
				   {struct,[{f1,int8_t},{f2,T}]}) end, Types0),
    lists:foreach(fun(T) -> ok = test_c_interaction(
				   {struct,[{f1,T},{f2,int8_t}]}) end, Types0),
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
    TypeRef = share:new_type(Type),
    X = share:new(TypeRef, Bin),
    Data1 = share:element(X, []),
    case equal_data(Data, Data1) of
	true -> ok;
	false ->
	    #{ compile_result => CompileResult,
	       gen_data => Data,
	       type_result => Type, 
	       file_data => Bin,
	       decode_result => Data1 }
	end.
    %% Type1 = share:typeof(Type),
    %% case share_debug:decode_obj(Type1, Bin) of
    %% 	{Data1, Rest, Yn} ->
    %% 	    case equal_data(Data, Data1) of
    %% 		true -> ok;
    %% 		false ->
    %% 		    #{ compile_result => CompileResult,
    %% 		       gen_data => Data,
    %% 		       type_result => Type1, 
    %% 		       file_data => Bin,
    %% 		       rest_data => Rest,
    %% 		       y_result => Yn,
    %% 		       decode_result => Data1 }
    %% 	    end
    %% end.

-define(EPS, 1.0e-6).

equal_data(X, X) -> true;   
equal_data([X|Xs],[Y|Ys]) ->
    case equal_data(X,Y) of
	true -> equal_data(Xs,Ys);
	false -> false
    end;
equal_data(X,Y) when is_integer(X), is_integer(Y) -> X =:= Y;
equal_data(X,Y) when is_float(X), is_float(Y) -> 
    abs(X-Y) < ?EPS;
equal_data(_,_) -> false.


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

format_data({IntType,Size}, Data) when 
      is_integer(Size) andalso (Size > 0) ->
    integer_to_list(Data);
format_data(Type, Data) when is_atom(Type) ->
    if is_integer(Data) ->
	    integer_to_list(Data);
       is_float(Data) ->
	    io_lib_format:fwrite_g(Data)
    end.

format_fields_data(Fs, Vs) ->
    lists:join(",", format_fields_data_(Fs, Vs)).

format_fields_data_([], []) -> 
    [];
format_fields_data_([{'_',{_,FieldSize}}|Fs], Vs) when FieldSize >= 0 ->
    format_fields_data_(Fs, Vs);
format_fields_data_([{_Name,Type}|Fs], [Val|Vs]) ->
    [format_data(Type, Val) | format_fields_data_(Fs, Vs)].

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
    gen_fields_data(Fields);
gen_data({array, Opts, Type}) ->
    {Sizes, Opts1} = get_sizes_opt(Opts),
    gen_array_data(Sizes, Opts1, Type);
gen_data({SignedType,Size}) when 
      is_integer(Size) andalso 
      (Size > 0) andalso
      ((SignedType =:= int8_t) orelse (SignedType =:= int16_t) orelse 
       (SignedType =:= int32_t) orelse (SignedType =:= int64_t)) ->
    random_range(-(1 bsl (Size-1)), (1 bsl (Size-1))-1);
gen_data({UnsignedType,Size}) when 
      is_integer(Size) andalso (Size > 0) andalso
      ((UnsignedType =:= uint8_t) orelse 
       (UnsignedType =:= uint16_t) orelse 
       (UnsignedType =:= uint32_t) orelse 
       (UnsignedType =:= uint64_t)) ->
    random_range(0, (1 bsl Size)-1);
gen_data(Type) ->
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

gen_fields_data([]) -> [];
gen_fields_data([{'_',_Type}|Fs]) ->
    gen_fields_data(Fs);
gen_fields_data([{Name,Type}|Fs]) ->
    [gen_data(Type) | gen_fields_data(Fs)].

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

format_ctype(TypeList) when is_list(TypeList) ->
    format_ctype_(TypeList);
format_ctype(Type) when is_atom(Type) ->
    format_ctype_def(Type);
format_ctype(Type) when is_tuple(Type) ->
    case Type of
	{FieldType,_FS} when is_integer(_FS),_FS >= 0 ->
	    format_ctype(FieldType);
	{array,0,Te} -> format_ctype(Te);
	{array,_SizeOpt,Te} -> format_ctype(Te);
	{struct,Fs} ->
	    ["struct { ", 
	     [[format_ctype(T)," ",
	       if N =:= '_' -> "";
		  true -> atom_to_list(N)
	       end,
	       format_ctype_sizes(T),";"] || 
		 {N,T} <- Fs], " }"]
    end.

format_ctype_def(Type) when is_atom(Type) ->
    case Type of
	int8_t -> "int8_t";
	int16_t -> "int16_t";
	int32_t -> "int32_t";
	int64_t -> "int64_t";
	uint8_t -> "uint8_t";
	uint16_t -> "uint16_t";
	uint32_t -> "uint32_t";
	uint64_t -> "uint64_t";
	float32_t -> "float";
	float64_t -> "double";
	complex64_t -> "float complex";
	complex128_t -> "double complex";
	char -> "char";
	short -> "short";
	int -> "int";
	long -> "long";
	uchar -> "unsigned char";
	ushort -> "unsigned short";
	uint -> "unsigned int";
	ulong -> "unsigned long";
	size_t -> "size_t";
	ssize_t -> "ssize_t";
	intptr_t -> "intptr_t";
	uintptr_t -> "uintptr_t";
	float -> "float";
	double -> "double";
	complex -> "complex";
	_ -> atom_to_list(Type)
    end.

format_ctype_([]) ->
    "";
format_ctype_([Type|Ts]) ->
    case Type of 
	volatile -> "volatile "++ format_ctype_(Ts);
	unsigned -> "unsigned " ++ format_ctype_(Ts);
	signed -> "unsigned "++format_ctype_(Ts);
	int -> "int "++format_ctype_(Ts);
	short -> "short "++format_ctype_(Ts);
	long -> "long "++format_ctype_(Ts);
	char -> "char "++format_ctype_(Ts);
	float -> "float "++format_ctype_(Ts);
	double -> "double "++format_ctype_(Ts);
	complex -> "complex "++format_ctype_(Ts);
	_ -> format_ctype_def(Type) ++ " " ++ format_ctype_(Ts)
    end.

format_ctype_sizes(Type) when is_atom(Type) -> "";  %% base type
format_ctype_sizes([Type|_]) when is_atom(Type) -> "";  %% base type
format_ctype_sizes({[Type|_],FS}) when is_atom(Type),
				       is_integer(FS), FS >= 0 ->
    [":",integer_to_list(FS)];
format_ctype_sizes({IntType,FS}) when is_atom(IntType),
				      is_integer(FS), FS >= 0 ->
    [":",integer_to_list(FS)];
format_ctype_sizes({array,SizeOpt,_BaseType}) ->
    case get_sizes_opt(SizeOpt) of
	[[0|_Size], _Opts] -> "";
	{Sizes,_Opts} ->
	    [["[",integer_to_list(Size),"]"] || Size <- Sizes]
    end;
format_ctype_sizes({struct,_Fs}) -> "".
