%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    structured shared memory nif
%%% @end
%%% Created : 14 Nov 2022 by Tony Rogvall <tony@rogvall.se>

-module(share).

-export([init/0]).
-export([info/0, info/1, new/1, new/2, new_type/1, element/2, setelement/3]).
-export([size/1, size/2, resize/2, resize/3]).
-export([typeof/1, typeof/2, sizeof/1, sizeof/2, offsetof/2]).
-export([alignment/1, alignment/2]).
-export([bitsizeof/1, bitsizeof/2]).
-export([bitalignment/1, bitalignment/2]).
-export([bitoffsetof/2]).

-nifs([info/0, info/1, new/1, new/2, new_type/1, element/2, setelement/3]).
-nifs([size/1, size/2, resize/3]).
-nifs([typeof/1,typeof/2,  sizeof/1, sizeof/2, offsetof/2]).
-nifs([alignment/1, alignment/2]).
-nifs([bitsizeof/1, bitsizeof/2]).
-nifs([bitalignment/1, bitalignment/2]).
-nifs([bitoffsetof/2]).

-export([i/1]).

-on_load(init/0).

-compile({no_auto_import,[setelement/3]}).
-compile({no_auto_import,[element/2]}).

-type unsigned() :: non_neg_integer().
-type objref()   :: reference().
-type typeref()  :: reference().

-type uint()   :: uchar|ushort|uint|ulong|uintptr_t|size_t|
		  uint8_t|uint16_t|uint32_t|uint64_t|uint128_t|
		  uint8|uint16|uint32|uint64|uint128.
-type sint()   :: char|short|int|long|intptr_t|ssize_t|
		  int8_t|int16_t|int32_t|int64_t|int128_t|
		  int8|int16|int32|int64|int128.
-type flt()    :: float|double|float32_t|float64_t.
-type cmplx()  :: complex|complex64_t|complex128_t.
-type int()    :: uint() | sint().
-type num()    :: int() | flt() | cmplx().
-type atm()    :: atom().
-type array()  :: {array,unsigned(),typespec()}.
-type bin()    :: {array,unsigned(),uint8}.
-type str()    :: {array,unsigned(),int8}.
-type struct() :: {struct,[{FieldName::atm(),FieldType::ftypespec()}]}.
-type typespec()  :: num()|atm()|array()|struct()|str()|bin()|typeref().
-type ftypespec() :: typespec()|{unsigned,unsigned()}|{signed,unsigned()}.

-type path() :: [unsigned()|atom()|{unsigned()}|{unsigned(),unsigned()}].
-type type() :: typeref()|objref()|typespec().


-define(nif_stub,nif_stub_error(?LINE)).

init() ->
    ok = erlang:load_nif(filename:join(code:priv_dir(share),share_nif),none).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-spec info() -> {NumObjects::integer(), NumTypes::integer()}.
info() -> ?nif_stub.

%% debugging
-spec info(typeref()) -> {SizeOf::integer(), RawTypeData::binary()};
	  (objref()) -> {typeref(), RawObjData::binary()}.

info(_Ref) -> ?nif_stub.

i(Ref) ->
    case info(Ref) of
	{Size,Bin} when is_integer(Size) ->
	    share_debug:decode_type(Bin);
	{TypeRef,Bin} ->
	    {_Size,TBin} = info(TypeRef),
	    Type = share_debug:decode_type(TBin),
	    share_debug:decode_obj(Type, Bin)
    end.

-spec new_type(typespec()) -> typeref().
new_type(_TypeSpec) -> 
    ?nif_stub.

-spec new(Type :: typeref()) -> objref().
new(_Type) ->
    ?nif_stub.

-spec new(Type :: typeref(), Object::iolist()) -> objref().
new(_Type, _Object) ->
    ?nif_stub.

-spec setelement(objref(),path(),Value::term()) -> ok.
setelement(_Obj, _Path, _Value) -> 
    ?nif_stub.

-spec element(objref(),path()) -> Value::term().
element(_Obj, _Path) -> ?nif_stub.

-spec resize(Object::objref(),NewSize::unsigned()) -> objref().
resize(Object, NewSize) -> resize(Object, [], NewSize).

-spec resize(Object::objref(),Path::path(),NewSize::unsigned()) -> ok.
resize(_Object, _Path, _NewSize) -> ?nif_stub.

%% Get the array dimension(s) 
-spec size(objref()) -> unsigned().
size(_Object) ->
    ?nif_stub.

-spec size(Object::objref(),Path::path()) -> unsigned().
size(_Object, _Path) -> 
    ?nif_stub.

%% compute memory size for a shared data type in bytes
%% argument is either term representing a type, typeref or
%% objref
-spec sizeof(Object::type()) -> unsigned().
sizeof(_Object) ->
    ?nif_stub.

-spec sizeof(Object::type(), Path::path()) -> unsigned().
sizeof(_Object, _Path) ->
    ?nif_stub.

%% compute memory size for a shared data type in bits
%% argument is either term representing a type, typeref or
%% objref
-spec bitsizeof(Object::type()) -> unsigned().
bitsizeof(_Object) ->
    ?nif_stub.

-spec bitsizeof(Object::type(), Path::path()) -> unsigned().
bitsizeof(_Object, _Path) ->
    ?nif_stub.

%% compute offset to a field or an array element
-spec offsetof(Object::type(), Path::path()) -> unsigned().
offsetof(_Object, _Path) ->
    ?nif_stub.

%% compute bit offset to a field or an array element
%% NOTE that bit offset is realtive to the (gcc) packing unit
-spec bitoffsetof(Object::type(), Path::path()) -> unsigned().
bitoffsetof(_Object, _Path) ->
    ?nif_stub.

%% get the term type given typeref, objref, term (return translated)
-spec typeof(Object::type()) -> typespec().
typeof(_Object) ->
    ?nif_stub.

%% get the term type given typeref or objref
-spec typeof(Object::type(), Path::path()) -> typespec().
typeof(_Object, _Path) ->
    ?nif_stub.

-spec alignment(Object::type()) -> unsigned().
alignment(_Object) ->
    ?nif_stub.

-spec alignment(Object::type(), Path::path()) -> unsigned().
alignment(_Object, _Path) ->
    ?nif_stub.

-spec bitalignment(Object::type()) -> unsigned().
bitalignment(_Object) ->
    ?nif_stub.

-spec bitalignment(Object::type(), Path::path()) -> unsigned().
bitalignment(_Object, _Path) ->
    ?nif_stub.
