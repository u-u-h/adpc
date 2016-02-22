VPATH = ../..
CC = gcc
LEX = flex
CFLAGS = -Wall -I. -I../..
UNAME := $(shell uname)

ifeq ($(UNAME),Linux)
CFLAGS += -g
else
ifeq ($(UNAME),SunOS)
CFLAGS += -gdwarf-2
endif
endif

all: rt

set_sources = set.c test_set.c

set: $(set_sources:.c=.o)
	$(CC) -o $@ $^

poly_sources = test_poly.c poly_type.c dll.c poly_scan.c

poly: $(poly_sources:.c=.o)
	$(CC) -o $@ $^ -lfl

index: dll.o index_list.o test_index.o
	$(CC) -o $@ $^

graph: dll.o graph.o test_graph.o
	$(CC) -o $@ $^

rt_obj = dll.o graph.o index_list.o bag.o poly_type.o tabulate.o poly_runtime.o adpc_deserialize.o test_runtime.o

rt: $(rt_obj)
	$(PRE) $(CC) -o $@ $^ -lfl

clean:
	rm -f *.o set set.exe poly poly.exe index index.exe rt rt.exe graph graph.exe
