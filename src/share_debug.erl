%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Debug internal structures
%%%    decode types and objects using info nif
%%% @end
%%% Created : 14 Oct 2024 by Tony Rogvall <tony@rogvall.se>

-module(share_debug).


-export([decode_type/1]).
-export([decode_obj/1, decode_obj/2]).
-export([debug_type/2, debug_object/2]).


-define(SHT_UNSIGNED,   16#0000).  %% 0000 xxxx    
-define(SHT_SIGNED,     16#0010).  %% 0010 xxxx
-define(SHT_INTEGER,    16#0020).  %% 0010 xxxx
-define(SHT_FLT,        16#0040).  %% 0100 xxxx
-define(SHT_CMPL,       16#0080).  %% 1000 xxxx    
-define(SHT_SIZE_8,     16#0003).  %% tttt 0003  %% 2^3
-define(SHT_SIZE_16,    16#0004).  %% tttt 0004  %% 2^4
-define(SHT_SIZE_32,    16#0005).  %% tttt 0005  %% 2^5
-define(SHT_SIZE_64,    16#0006).  %% tttt 0006  %% 2^6
-define(SHT_SIZE_128,   16#0007).  %% tttt 0007  %% 2^7

-define(SHT_UINT8,      ?SHT_UNSIGNED+?SHT_INTEGER+?SHT_SIZE_8).
-define(SHT_UINT16,     ?SHT_UNSIGNED+?SHT_INTEGER+?SHT_SIZE_16).
-define(SHT_UINT32,     ?SHT_UNSIGNED+?SHT_INTEGER+?SHT_SIZE_32).
-define(SHT_UINT64,     ?SHT_UNSIGNED+?SHT_INTEGER+?SHT_SIZE_64).
-define(SHT_UINT128,    ?SHT_UNSIGNED+?SHT_INTEGER+?SHT_SIZE_128).
-define(SHT_INT8,       ?SHT_SIGNED+?SHT_INTEGER+?SHT_SIZE_8).
-define(SHT_INT16,      ?SHT_SIGNED+?SHT_INTEGER+?SHT_SIZE_16).
-define(SHT_INT32,      ?SHT_SIGNED+?SHT_INTEGER+?SHT_SIZE_32).
-define(SHT_INT64,      ?SHT_SIGNED+?SHT_INTEGER+?SHT_SIZE_64).
-define(SHT_INT128,     ?SHT_SIGNED+?SHT_INTEGER+?SHT_SIZE_128).
-define(SHT_FLOAT32,    ?SHT_SIGNED+?SHT_FLT+?SHT_SIZE_32).
-define(SHT_FLOAT64,    ?SHT_SIGNED+?SHT_FLT+?SHT_SIZE_64).
-define(SHT_COMPLEX64,  ?SHT_SIGNED+?SHT_CMPL+?SHT_SIZE_64).
-define(SHT_COMPLEX128, ?SHT_SIGNED+?SHT_CMPL+?SHT_SIZE_128).

-define(SHT_ATM,        16#0100).   %% ERL_NIF_TERM (atom)
-define(SHT_ARRAY,      16#1000).   %% share_array_t
-define(SHT_STRUCT,     16#2000).   %% share_struct_t
-define(SHT_DICT_ENT,   16#3000).   %% share_dictent_t

%% native wordsize in bits
wordsize() ->
    erlang:system_info(wordsize)*8.

%% debug decode type code
decode_type(T) when is_reference(T) ->
    {_Size, TBin} = share:info(T),
    decode_type(TBin);
decode_type(TBin) when is_binary(TBin) ->
    WordSize = share:wordsize(),
    Words = [X || <<X:WordSize/native>> <= TBin],
    io:format("Words = ~p\n", [Words]),
    {Type,[]} = decode_type_(Words),
    Type.

decode_type_([?SHT_ARRAY,S,_TSize,_ESize,_TOffs,Align,RowMajor|Spec]) ->
    io:format("array S=~p TSize=~p,ESize=~p,TOffs=~p,Align=~p,RowMajor=~p\n", 
	      [S,_TSize,_ESize,_TOffs,Align,RowMajor]),
    {Size, Stride, Spec1} = decode_array_shape(S, Spec),
    {ElemType,Spec2} = decode_type_(Spec1),
    {{array,[{size,Size},
	     {stride,Stride},
	     {alignment,Align},
	     {rowmajor, (RowMajor > 0)}
	    ],ElemType},Spec2};
decode_type_([?SHT_STRUCT,N,_TSize,_ESize|Spec]) ->
    io:format("struct N=~p TSize=~p, ESize=~p\n", [N,_TSize,_ESize]),
    {Fields,Spec1} = decode_fields_(N, Spec, 0, []),
    {{struct,Fields}, Spec1};
decode_type_([?SHT_DICT_ENT,_TOffs,_EOffs|Spec]) ->
    io:format("dict_ent TOffs=~p, EOffs=~p\n", [_TOffs,_EOffs]),
    {[KeyType,ValType],Spec1} = decode_ntypes_(2, Spec, []),
    {{dict_ent,KeyType, ValType},Spec1};
decode_type_([?SHT_UINT8|Spec]) -> {uint8_t,Spec};
decode_type_([?SHT_UINT16|Spec]) -> {uint16_t,Spec};
decode_type_([?SHT_UINT32|Spec]) -> {uint32_t,Spec};
decode_type_([?SHT_UINT64|Spec]) -> {uint64_t,Spec};
decode_type_([?SHT_UINT128|Spec]) -> {uint128_t,Spec};
decode_type_([?SHT_INT8|Spec]) -> {int8_t,Spec};
decode_type_([?SHT_INT16|Spec]) -> {int16_t,Spec};
decode_type_([?SHT_INT32|Spec]) -> {int32_t,Spec};
decode_type_([?SHT_INT64|Spec]) -> {int64_t,Spec};
decode_type_([?SHT_INT128|Spec]) -> {int128_t,Spec};
decode_type_([?SHT_FLOAT32|Spec]) -> {float32_t,Spec};
decode_type_([?SHT_FLOAT64|Spec]) -> {float64_t,Spec};
decode_type_([?SHT_COMPLEX64|Spec]) -> {complex64_t,Spec};
decode_type_([?SHT_COMPLEX128|Spec]) -> {complex128_t,Spec};
decode_type_([?SHT_ATM|Spec]) -> {atm,Spec}.

decode_array_shape(Dim,Spec) ->
    decode_array_shape(Dim,[],[],Spec).

decode_array_shape(0,Size,Stride,Spec) -> 
    {lists:reverse(Size),lists:reverse(Stride),Spec};
decode_array_shape(Dim,Size,Stride,[Sz,St|Spec]) ->
    decode_array_shape(Dim-1,[Sz|Size],[St|Stride],Spec).

decode_ntypes_(0, Spec, Acc) ->
    {lists:reverse(Acc), Spec};
decode_ntypes_(I, Spec, Acc) ->
    {Type,Spec1} = decode_type_(Spec),
    decode_ntypes_(I-1, Spec1, [Type|Acc]).

%% first pass unpack the fields, decode name
decode_fields_(0, Spec, Offs, Acc) ->
    decode_fields__(lists:reverse(Acc), Offs, Spec, []);
decode_fields_(I, [Atm,TOffs,_EOffs|Spec], Offs, Acc) ->
    Name = share:info(Atm),
    decode_fields_(I-1, Spec, Offs+3, [{Name,Offs+3+TOffs}|Acc]).

%% second pass decode the actual value types
decode_fields__([{Name,Offs}|Fields], Offs, Spec, Acc) ->
    {Type,Spec1} = decode_type_(Spec),
    N = length(Spec) - length(Spec1),
    decode_fields__(Fields, Offs+N, Spec1, [{Name,Type}|Acc]);
decode_fields__([], _TOffs, Spec, Acc) ->
    {lists:reverse(Acc), Spec}.


%% debug decode the object
decode_obj(ObjRef) when is_reference(ObjRef) ->
    {TypeRef,Bin} = share:info(ObjRef),
    {_Size,TBin} = share:info(TypeRef),
    Type = decode_type(TBin),
    decode_obj(Type, Bin).

decode_obj(Type, Bin) ->
    decode_obj_(Type, Bin, 0).
    %%{Obj, <<>>, _Yn} = decode_obj_(Type, Bin, 0),
    %%Obj.

%% note that the atom type names are the internal version
%% in input more type names are allowed
%% int/long/size_t etc are mapped to native corresponding internal types
decode_obj_(uint8_t, <<X:8/native,Bin/binary>>, Y) -> {X, Bin, Y+1};
decode_obj_(uint16_t, <<X:16/native,Bin/binary>>, Y) -> {X, Bin, Y+2};
decode_obj_(uint32_t, <<X:32/native,Bin/binary>>, Y) -> {X, Bin, Y+4};
decode_obj_(uint64_t, <<X:64/native,Bin/binary>>, Y) -> {X, Bin, Y+8};
decode_obj_(uint128_t, <<X:128/native,Bin/binary>>, Y) -> {X, Bin, Y+16};
decode_obj_(int8_t, <<X:8/signed-native,Bin/binary>>, Y) -> {X, Bin, Y+1};
decode_obj_(int16_t, <<X:16/signed-native,Bin/binary>>, Y) -> {X, Bin, Y+2};
decode_obj_(int32_t, <<X:32/signed-native,Bin/binary>>, Y) -> {X, Bin, Y+4};
decode_obj_(int64_t, <<X:64/signed-native,Bin/binary>>, Y) -> {X, Bin, Y+8};
decode_obj_(int128_t, <<X:128/signed-native,Bin/binary>>, Y) -> {X, Bin, Y+16};
decode_obj_(float32_t, <<X:32/float-native,Bin/binary>>, Y) -> {X, Bin, Y+4};
decode_obj_(float64_t, <<X:64/float-native,Bin/binary>>, Y) -> {X, Bin, Y+8};
decode_obj_(complex64_t, <<R:32/float-native,I:32/float-native,Bin/binary>>,Y) ->
    {[R|I], Bin, Y+4+4};
decode_obj_(complex128_t, <<R:64/float-native,I:64/float-native,Bin/binary>>,Y)->
    {[R|I], Bin, Y+8+8};
decode_obj_(atm, Bin, Y) ->
    WSz = wordsize(),
    case Bin of
	<<Atm:WSz/native,Bin1/binary>> ->
	    if Atm =:= 0 ->
		    {undefined, Bin1, Y+WSz};
	       true -> %% info(Atm) looks up the real atom!
		    {share:info(Atm), Bin1, Y+WSz}
	    end
    end;
decode_obj_({array,0,Type}, Bin, Y) ->
    WSz = wordsize(),
    case Bin of
	<<Ptr:WSz/native,Bin1/binary>> ->
	    {{ptr,Type,Ptr}, Bin1, Y+WSz}
    end;
decode_obj_({array,ArrayOpts,Type}, Bin, Y) ->
    {Sizes,_Opts} = share_test:get_sizes_opt(ArrayOpts),
    decode_array_(Sizes, Type, Bin, Y);
decode_obj_(Type={dict_ent,KeyType,ValueType}, Bin, Y1) ->
    io:format("dict_ent begin = ~p\n", [Y1]),
    {Pad1, Bin1} = align_obj(Type, Bin, Y1), %% align each entry
    io:format("dict_ent pad1 = ~p\n", [Pad1]),
    {Key, Bin2, Y2} = decode_obj_(KeyType, Bin1, Y1+Pad1),
    {Pad2, Bin3} = align_obj(ValueType, Bin2, Y2),
    io:format("dict_ent pad2 = ~p\n", [Pad2]),
    {Value, Bin4, Y3} = decode_obj_(ValueType, Bin3, Y2+Pad2),
    {{Key,Value},Bin4,Y3};
decode_obj_(Type={struct,Fields}, Bin, Y) ->
    Align = share:alignment(Type),
    {Pad, Bin1} = align_data(Align, Bin, Y),
    decode_objfields_(Fields, Bin1, Y+Pad, []).

decode_objfields_([{Name,Type}|Fields], Bin, Y, Acc) ->
    {Pad, Bin1} = align_obj(Type, Bin, Y),  %% natural alignment
    Y1 = Y+Pad,
    {Value,Bin2,Y2} = decode_obj_(Type, Bin1, Y1),
    <<ValueBin:(Y2-Y1)/binary, _/binary>> = Bin1,
    io:format("field ~p = ~p (~p)\n", [Name,Value,ValueBin]),
    decode_objfields_(Fields, Bin2, Y2, [{Name,Value}|Acc]);
decode_objfields_([], Bin, Y, Acc) ->
    {{struct,lists:reverse(Acc)}, Bin, Y}.

decode_array_([Size], Type, Bin, Y) ->
    decode_nobj_(Size, Type, Bin, Y, []);
decode_array_([Size|Sizes],Type,Bin,Y) ->
    decode_array__(Size, Sizes, Type, Bin, Y, []).

decode_array__(0, _Sizes, _Type, Bin, Y, Acc) ->
    {lists:reverse(Acc), Bin, Y};
decode_array__(I, Sizes, Type, Bin, Y, Acc) ->
    {Data, Bin1, Y1} = decode_array_(Sizes, Type, Bin, Y),
    decode_array__(I-1, Sizes, Type, Bin1, Y1, [Data|Acc]).

decode_nobj_(0, _Type, Bin, Y, Acc) ->
    {lists:reverse(Acc), Bin, Y};
decode_nobj_(I, Type, Bin, Y, Acc) ->
    {Data, Bin1, Y1} = decode_obj_(Type, Bin, Y),
    decode_nobj_(I-1, Type, Bin1, Y1, [Data|Acc]).

align_obj(Type, Bin, Y) ->
    A = share:alignment(Type),
    align_data(A, Bin, Y).

align_data(A, Bin, Y) when is_integer(A), A > 0 ->
    Pad = (A - (Y rem A)) rem A,
    <<_:Pad/binary, Bin1/binary>> = Bin,
    {Pad, Bin1}.


debug_type(T, Spec) ->
    {TSize,TBin} = share:info(T),
    SizeOf = share:sizeof(Spec),
    io:format("sizeof(~p) = ~p\n", [Spec,SizeOf]),
    io:format("type descriptor size = ~p (~p)\n", [TSize,byte_size(TBin)]),
    TypeDesc = decode_type(TBin),
    io:format("type descriptor = ~p\n", [TypeDesc]).

debug_object(Msg, Obj) ->
    {TypeRef,Bin} = share:info(Obj),
    {TSize, TBin} = share:info(TypeRef),
    TypeOf = decode_type(TBin),
    SizeOf = share:sizeof(TypeOf),
    %% TypeOf = typeof(Obj), 
    io:format("~s: sizeof(~p) = ~p\n", [Msg,TypeOf,SizeOf]),
    io:format("~s: type descriptor size = ~p (~p)\n", 
	      [Msg,TSize,byte_size(TBin)]),
    Value = decode_obj(TypeOf, Bin),
    io:format("~s: obj=~p, (bin_size=~p)\n", [Msg, Value, byte_size(Bin)]),
    ok.
