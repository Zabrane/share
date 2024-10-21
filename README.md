# Shared memory for Erlang/Nif

share aim to handle memory segments that can be passed
to erlang and nifs. Peeking and poking but also be able
to access various parts of data.

This could be useful when interfacing audio/video/graphis/data
lirbaries where it could be effient to send memory buffers
around insted of marshalling data between nifs(libraris)

There are two concepts types and data
The data describe how the data maybe interpreted, and
then the data it self.

To use share from libraries enif_dynamic_resource_call must be used.
rt_name = 'share' and resource = 'object' for objects
and
rt_name = 'share* and resource = 'type' for type objects

include "share_type.h" to access data structures needed


shared data can be as simple as an array of basic data types
or a more complex structure that can be overlayed on to the
data. In this case the share type can be used to have a simple
and common access to the data fields from Erlang (via share.erl) if needed

## primitive data type

To create a shared data type call:

   Type = share:new_type(<type>)

Then Type may be used to create objects of that type:

     X = share:new(Type)

And thats it. Kind of.

The primitive built in types are:

    int8_t | int16_t | int32_t | int64_t | int128_t |
    uint8_t | uint16_t | uint32_t | uint64_t | uint128_t |
    float32_t | float64_t | complex64_t | complex128_t

Some aliases that are mapped to the above type as:

     char | short | int | long | intptr_t | ssize_t |
     uchar | ushort | uint | ulong | uintptr_t | size_t
     
And some more:

     int8 | int16 | int32 | int64 | int128 |
     uint8 | uint16 | uint32 | uint64| uint128 |
     float | double | complex

## Examples
And now lets construct some combined data types

Structure like point / vector / colors etc

    {struct, [{FieldName::atom(),FieldType::type()}]}

And Arrays for vector over primitive types or structs

    {array, Size:size(), ElementType::type()}

Where size may be unsigned() | [unsigned()]. For more control then
[{size, size()} | {stride, stride()} | {alignment, align()} ..] is used
instead of only size.

Example

    Point2D = share:new_type({struct, [{x, float}, {y, float}]}).

and

    Point2DVector = share:new_type({array, 10, {struct, [{x, float}, {y, float}]}}).

## General way of accessing data

   Value = share:element(Object, Path).

And setting values

    share:setelement(Object, Path, Value).

Path is a general form of index to access the primitive data fields

     P = share:new(Point2D).
     share:setelement(P, [x], 12.0).
     share:setelement(P, [y], 1.0).

or
     V = share:new(Point2DVector).
     share:setelement(V, [3, x], 123.0).

# Multi dimensional arrays

  {array, 10, {struct, [{x, float}, {y, float}]}} =

  {array, [{size,10}], {struct, [{x, float}, {y, float}]}} =
  {array, [{size,10}, {stride,4}], {struct, [{x,float}, {y,float}]}} =
  {array, [{size,[10]},{stride,[4]}], {struct, [{x,float}, {y,float}]}} =
  {array, [{size,[10]},{stride,[4]},{offset,0}],{struct,[{x,float},{y,float}]}

  {array, [{size,[5,5]},{stride,[16,4]}], {struct,[{x,float},{y,float}]}}

The options are

    {size,  unsigned() | [unsigned()] }
    {stride, unsigned() | [unsigned()] }
    {offset, unsigned()}
    {align, unsigned()}

Example, a matrix with simd aligned memory


	{array, [{size,[32,32]}, {stride,[128,8]}, {align, 128}],
		{struct,[{x,float},{y,float}]}}
