#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@
CC=gcc
CFLAGS+= -Wall -O3
LDFLAGS+=-shared

OBJS = share_nif.o share.o
LIBS = 

all: ../priv/share_nif.so

debug: CFLAGS += -Ddebug -DDEBUG
debug: all

../priv/share_nif.so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o ../priv/share_nif.so

clean:
	rm -f $(OBJS) *.core .*.d

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d
