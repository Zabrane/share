#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@
CC=gcc
CFLAGS+= -Wall -O3
LDFLAGS+=-shared

OBJS = 
LIBS = 

all: ../priv/share_nif.so

../priv/share_nif.so: share_nif.o share.o
	$(CC) $(LDFLAGS) share.o share_nif.o -o ../priv/share_nif.so

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d
