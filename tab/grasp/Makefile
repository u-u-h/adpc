VPATH = ../general ../.. ../optimal
CC = gcc
LEX = flex
CPPFLAGS = -I../general -I../.. -I../optimal
CFLAGS = -Wall -O3
UNAME := $(shell uname)

ifeq ($(UNAME),Linux)
CFLAGS += -g
else
ifeq ($(UNAME),SunOS)
CFLAGS += -gdwarf-2
endif
endif

# Target-depending Settings
ifeq ($(MAKECMDGOALS),windows)
CFLAGS += -D WINDOWS
endif

all: tg

tg_obj = dll.o graph.o index_list.o bag.o array_int.o poly_type.o \
         ../../tabulate.o \
         poly_runtime.o adpc_deserialize.o random.o \
         tab_grasp.o tg_main.o set.o asm_runtime.o input.o time_util.o \
         asm_factory.o

tg: $(tg_obj)
	$(PRE) $(CC) $(CFLAGS) -o $@ $^ -lfl $(LDFLAGS)

windows: $(tg_obj)
	$(PRE) $(CC) $(CFLAGS) -o tg $^ -lfl $(LDFLAGS)

clean:
	rm -f *.o tg tg.exe
