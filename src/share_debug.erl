%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Debug internal structures
%%%    decode types and objects using info nif
%%% @end
%%% Created : 14 Oct 2024 by Tony Rogvall <tony@rogvall.se>

-module(share_debug).

-export([decode_type/1]).
-export([decode_obj/1, decode_obj/2, decode_obj/3]).
-export([debug_type/2, debug_object/2]).

-define(SHT_UNSIGNED,   16#00001).
-define(SHT_SIGNED,     16#00003).
-define(SHT_FLT,        16#00004).
-define(SHT_CMPLX,      16#00005).
-define(SHT_ATM,        16#00008).
-define(SHT_ARRAY,      16#00009).   %% share_array_t
-define(SHT_STRUCT,     16#0000A).   %% share_struct_t
-define(SHT_UNION,      16#0000B).   %% share_union_t

-define(SHT_SIZE_MASK,  16#00FF0).
-define(SHT_FIELD_MASK, 16#FF000).
-define(SHT_SIZE_8,     16#00080).
-define(SHT_SIZE_16,    16#00100).
-define(SHT_SIZE_32,    16#00200).
-define(SHT_SIZE_64,    16#00400).
-define(SHT_SIZE_128,   16#00800).

-define(SHT_UINT8,      (?SHT_UNSIGNED+?SHT_SIZE_8)).
-define(SHT_UINT16,     (?SHT_UNSIGNED+?SHT_SIZE_16)).
-define(SHT_UINT32,     (?SHT_UNSIGNED+?SHT_SIZE_32)).
-define(SHT_UINT64,     (?SHT_UNSIGNED+?SHT_SIZE_64)).
-define(SHT_UINT128,    (?SHT_UNSIGNED+?SHT_SIZE_128)).
-define(SHT_INT8,       (?SHT_SIGNED+?SHT_SIZE_8)).
-define(SHT_INT16,      (?SHT_SIGNED+?SHT_SIZE_16)).
-define(SHT_INT32,      (?SHT_SIGNED+?SHT_SIZE_32)).
-define(SHT_INT64,      (?SHT_SIGNED+?SHT_SIZE_64)).
-define(SHT_INT128,     (?SHT_SIGNED+?SHT_SIZE_128)).
-define(SHT_FLOAT32,    (?SHT_FLT+?SHT_SIZE_32)).
-define(SHT_FLOAT64,    (?SHT_FLT+?SHT_SIZE_64)).
-define(SHT_COMPLEX64,  (?SHT_CMPLX+?SHT_SIZE_64)).
-define(SHT_COMPLEX128, (?SHT_CMPLX+?SHT_SIZE_128)).

%% native wordsize in bits
wordsize() ->
    erlang:system_info(wordsize)*8.

%% debug decode type code
decode_type(T) when is_reference(T) ->
    {_Size, TBin} = share:info(T),
    decode_type(TBin);
decode_type(TBin) when is_binary(TBin) ->
    WordSize = wordsize(),
    Words = [X || <<X:WordSize/native>> <= TBin],
    io:format("Words = ~p\n", [Words]),
    {Type,[]} = decode_type_(Words),
    Type.

decode_type_([?SHT_ARRAY,S,_TSize,_ESize,RowMajor,
	      Size,Stride,Align|Spec]) ->
    io:format("array S=~p TSize=~p,E=~p,RowMajor=~p"
	      ",Size=~p,Stride=~p,Align=~p\n",
	      [S,_TSize,_ESize,RowMajor,Size,Stride,Align]),
    {Size1, Stride1, Align1, Spec1} = 
	decode_array_shape(S-1,[Size],[Stride],[Align],Spec),
    {ElemType,Spec2} = decode_type_(Spec1),
    {{array,[{size,Size1},
	     {stride,Stride1},
	     {alignment,Align1},
	     {rowmajor, (RowMajor > 0)}
	    ],ElemType},Spec2};
decode_type_([?SHT_STRUCT,N,_TSize,_ESize,_Align|Spec]) ->
    io:format("struct N=~p T=~p, E=~p, A=~p\n", [N,_TSize,_ESize,_Align]),
    {Fields,Spec1} = decode_fields_(N, Spec, 0, []),
    {{struct,Fields}, Spec1};
decode_type_([?SHT_UNION,N,_TSize,_ESize,_Align|Spec]) ->
    io:format("union N=~p T=~p, E=~p, A=~p\n", [N,_TSize,_ESize,_Align]),
    {Fields,Spec1} = decode_fields_(N, Spec, 0, []),
    {{union,Fields}, Spec1};
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

%% decode_array_shape(Dim,Spec) ->
%%    decode_array_shape(Dim,[],[],Spec).

decode_array_shape(0,Size,Stride,Align,Spec) -> 
    {lists:reverse(Size),lists:reverse(Stride),
     lists:reverse(Align), Spec};
decode_array_shape(Dim,Size,Stride,Align,
		   [?SHT_ARRAY,_,_TSize,_ESize,_RowMajor,
		    Sz,St,Al|Spec]) ->
    decode_array_shape(Dim-1,[Sz|Size],[St|Stride],[Al|Align],Spec).

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
    E = erlang:system_info(endian),
    decode_obj_(Type, E, Bin, 0).

decode_obj(Type,Bin) ->
    E = erlang:system_info(endian),
    decode_obj_(share:typeof(Type),E,Bin,0).

decode_obj(Type,E,Bin) ->
    Type1 = share:typeof(Type),
    decode_obj_(Type1,E,Bin,0).

decode_obj_(Type, E, Bin, Y) when is_atom(Type) ->
    case {Type,E,Bin} of 
	%% BIG ENDIAN
	{uint8_t,big,<<_:Y/unit:8,X:8/big,_/binary>>} ->
	    {X, Y+1};
	{uint16_t, big, <<_:Y/unit:8,X:16/big,_/binary>>} -> 
	    {X, Y+2};
	{uint32_t, big, <<_:Y/unit:8,X:32/big,_/binary>>} ->
	    {X, Y+4};
	{uint64_t, big, <<_:Y/unit:8,X:64/big,_/binary>>} -> 
	    {X, Y+8};
	{uint128_t, big, <<_:Y/unit:8,X:128/big,_/binary>>} ->
	    {X, Y+16};
	{int8_t, big, <<_:Y/unit:8,X:8/signed-big,_/binary>>} ->
	    {X, Y+1};
	{int16_t, big, <<_:Y/unit:8,X:16/signed-big,_/binary>>} -> 
	    {X, Y+2};
	{int32_t, big, <<_:Y/unit:8,X:32/signed-big,_/binary>>} -> 
	    {X, Y+4};
	{int64_t, big, <<_:Y/unit:8,X:64/signed-big,_/binary>>} -> 
	    {X, Y+8};
	{int128_t, big, <<_:Y/unit:8,X:128/signed-big,_/binary>>} -> 
	    {X, Y+16};
	{float32_t, big, <<_:Y/unit:8,X:32/float-big,_/binary>>} -> 
	    {X, Y+4};
	{float64_t, big, <<_:Y/unit:8,X:64/float-big,_/binary>>} ->
	    {X, Y+8};
	{complex64_t, big, <<_:Y/unit:8,R:32/float-big,I:32/float-big,_/binary>>} ->
	    {[R|I], Y+4+4};
	{complex128_t, big, <<_:Y/unit:8,R:64/float-big,I:64/float-big,_/binary>>}->
	    {[R|I], Y+8+8};
	%% LITTLE ENDIAN	
	{uint8_t,little,<<_:Y/unit:8,X:8/little,_/binary>>} ->
	    {X, Y+1};
	{uint16_t, little, <<_:Y/unit:8,X:16/little,_/binary>>} -> 
	    {X, Y+2};
	{uint32_t, little, <<_:Y/unit:8,X:32/little,_/binary>>} ->
	    {X, Y+4};
	{uint64_t, little, <<_:Y/unit:8,X:64/little,_/binary>>} -> 
	    {X, Y+8};
	{uint128_t, little, <<_:Y/unit:8,X:128/little,_/binary>>} ->
	    {X, Y+16};
	{int8_t, little, <<_:Y/unit:8,X:8/signed-little,_/binary>>} -> 
	    {X, Y+1};
	{int16_t, little,<<_:Y/unit:8,X:16/signed-little,_/binary>>} ->
	    {X, Y+2};
	{int32_t, little, <<_:Y/unit:8,X:32/signed-little,_/binary>>} ->
	    {X, Y+4};
	{int64_t, little, <<_:Y/unit:8,X:64/signed-little,_/binary>>} -> 
	    {X, Y+8};
	{int128_t, little, <<_:Y/unit:8,X:128/signed-little,_/binary>>} ->
	    {X, Y+16};
	{float32_t, little, <<_:Y/unit:8,X:32/float-little,_/binary>>} -> 
	    {X, Y+4};
	{float64_t, little, <<_:Y/unit:8,X:64/float-little,_/binary>>} -> 
	    {X, Y+8};
	{complex64_t, little, <<_:Y/unit:8,R:32/float-little,I:32/float-little,_/binary>>} ->
	    {[R|I], Y+4+4};
	{complex128_t, little, <<_:Y/unit:8,R:64/float-little,I:64/float-little,_/binary>>}->
	    {[R|I], Y+8+8};

	{atm, _E, _} ->
	    WSz = wordsize(),
	    case Bin of
		<<_:Y/unit:8,Atm:WSz/native,_/binary>> -> %% must be native!
		    if Atm =:= 0 ->
			    {undefined, Y+WSz};
		       true -> %% info(Atm) looks up the real atom!
			    {share:info(Atm), Y+WSz}
		    end
	    end
    end;
decode_obj_({array,0,Type},_E,Bin,Y) ->
    WSz = wordsize(),
    case Bin of
	<<_:Y/unit:8,Ptr:WSz/native,_/binary>> ->
	    {{ptr,Type,Ptr}, Y+WSz}
    end;
decode_obj_({array,ArrayOpts,Type},E,Bin,Y) ->
    {Sizes,_Opts} = share_test:get_sizes_opt(ArrayOpts),
    decode_array_(Sizes,Type,E,Bin,Y);
decode_obj_(Type={struct,Fields},E,Bin,Y) ->
    Align = share:alignment(Type), %% this will recalculate the alignment
    {Pad,Bin1} = align_data(Align, Bin, Y),
    {Fs,Z} = decode_struct_fields(Fields, E, Bin1, 0, 0, 0, []),
    {Fs, Y+Pad+((Z+7) bsr 3)}.

decode_field(Type,E,Bin,Z) ->
    <<_:Z, Bin1/binary>> = Bin,  %% get field start
    {X, Y1} = decode_obj_(Type,E,Bin1,0),
    {X, Z+Y1*8}.

decode_bitfield(Type, BaseSize, FieldSize, E, Bin, Z) when
      (Z band (BaseSize-1))+FieldSize > BaseSize ->
    Pad = BaseSize - (Z band (BaseSize-1)),
    decode_bitfield(Type, BaseSize, FieldSize, E, Bin, Z+Pad);
decode_bitfield(Type, BaseSize, FieldSize, E, Bin, Z) ->
    Z0 = Z band (BaseSize - 1),  %% bit offset
    BaseOffs = Z band (bnot (BaseSize - 1)),
    Offs = if E =:= little -> BaseOffs + (BaseSize - Z0 - FieldSize);
	      true -> BaseOffs + Z0
	   end,
    io:format("Z=~w,Z0=~w,BaseOffs=~w,Offs=~w,BS=~w,FS=~w\n",
	      [Z,Z0,BaseOffs,Offs,BaseSize,FieldSize]),

    case is_signed(Type) of
	false ->
	    <<_:Offs, X:FieldSize/unsigned, _/bitstring>> = Bin,
	    io:format("unsigned ~p:~w\n", [X,FieldSize]),
	    {X, Z+FieldSize};
	true ->
	    <<_:Offs, X:FieldSize/signed, _/bitstring>> = Bin,
	    io:format("signed ~p:~w\n", [X,FieldSize]),
	    {X, Z+FieldSize}
    end.

decode_struct_fields([{_Name,{Type,FieldSize}}|Fields], E, Bin, Z, 
		      Remain, PackSize, Acc) 
  when is_integer(FieldSize), FieldSize >= 0 ->
    if FieldSize =:= 0 ->
	    BaseSize = share:bitsizeof(Type),
	    Pad = BaseSize - (Z band (BaseSize-1)),
	    decode_struct_fields(Fields, E, Bin, Z+Pad, 0, 0, Acc);
       FieldSize =< Remain -> %% straddle fiel
	    io:format("straddle ~p:~p\n", [Type,FieldSize]),
	    {Value,Z1} = decode_bitfield(Type,PackSize,FieldSize,E,Bin,Z),
	    decode_struct_fields(Fields,E,Bin,Z1,Remain-FieldSize, 
				 PackSize,[Value|Acc]);
       true ->
	    BaseSize = share:bitsizeof(Type),
	    Pad = pad(BaseSize, Z),
	    {Value,Z1} = decode_bitfield(Type,BaseSize,FieldSize,E,Bin,Z+Pad),
	    decode_struct_fields(Fields, E, Bin, Z1,
				 BaseSize-FieldSize, BaseSize,
				 [Value|Acc])
    end;
decode_struct_fields([{_Name,Type}|Fields],E,Bin,Z,Remain,_PackSize,Acc) ->
    Pad = bitpad(Type, Z+Remain),
    Z1 = Z+Remain+Pad,
    {Value,Z2} = decode_field(Type, E, Bin, Z1),
    decode_struct_fields(Fields, E, Bin, Z2, 0, 0, [Value|Acc]);
decode_struct_fields([],_E,_Bin,Z,Remain,_PackSize,Acc) ->
    {lists:reverse(Acc), Z+Remain}.

decode_array_([Size],Type,E,Bin,Y) ->
    decode_nobj_(Size,Type,E,Bin,Y,[]);
decode_array_([Size|Sizes],Type,E,Bin,Y) ->
    decode_array__(Size,Sizes,Type,E,Bin,Y,[]).

decode_array__(0, _Sizes, _Type, _E, _Bin, Y, Acc) ->
    {lists:reverse(Acc), Y};
decode_array__(I, Sizes, Type, E, Bin, Y, Acc) ->
    {Data, Y1} = decode_array_(Sizes, Type, E, Bin, Y),
    decode_array__(I-1, Sizes, Type, E, Bin, Y1, [Data|Acc]).

decode_nobj_(0,_Type,_E,_Bin,Y,Acc) ->
    {lists:reverse(Acc),Y};
decode_nobj_(I,Type,E,Bin,Y,Acc) ->
    {Data,Y1} = decode_obj_(Type,E,Bin,Y),
    decode_nobj_(I-1,Type,E,Bin,Y1,[Data|Acc]).

align_obj(Type, Bin, Y) ->
    A = share:alignment(Type),
    align_data(A, Bin, Y).

align_data(A, Bin, Y) when is_integer(A), A > 0 ->
    Pad = (A - (Y rem A)) rem A,
    <<_:Pad/binary, Bin1/binary>> = Bin,
    {Pad, Bin1}.

bitpad(Type, Z) ->
    A = share:bitalignment(Type),
    pad(A, Z).

pad(A, Z) ->
    (A - (Z rem A)) rem A.

bitalign_obj(Type, Bin, Z) ->
    A = share:bitalignment(Type),
    align_data(A, Bin, Z).

bitalign_data(A, Bin, Z) when is_integer(A), A > 0 ->
    Pad = (A - (Z rem A)) rem A,
    <<_:Pad, Bin1/binary>> = Bin,
    {Pad, Bin1}.

is_signed(uint8_t) -> false;
is_signed(uint16_t) -> false;
is_signed(uint32_t) -> false;
is_signed(uint64_t) -> false;
is_signed(uint128_t) -> false;
is_signed(int8_t) -> true;
is_signed(int16_t) -> true;
is_signed(int32_t) -> true;
is_signed(int64_t) -> true;
is_signed(int128_t) -> true;
is_signed(_) -> false.


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
