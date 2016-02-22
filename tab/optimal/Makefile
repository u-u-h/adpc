all: taboptimal

# Define search-path
VPATH = ../..  ../general ../grasp
PATH_C = -I. -I../.. -I../general -I../grasp
PATH_LHS = -i../..

# C-object-files for the stand-alone-program taboptimal
OBJECTS_TABOPTIMAL = ../../tabulate.o taboptimal.o asm_runtime.o functions.o       \
                     output.o input.o poly_runtime.o poly_type.o dll.o set.o \
                     graph.o bag.o index_list.o taboptimal_fkt.o

# C-object-files for the tool update_poly_runtime
OBJECTS_UPR = update_poly_runtime.o tabulate.o asm_runtime.o poly_runtime.o  \
              poly_type.o dll.o set.o graph.o bag.o dll.o functions.o        \
              output.o input.o index_list.o

# Flags and tools
LDFLAGS = -lm
CFLAGS = -O3 -g -DNDEBUG -Wall $(PATH_C)
CC = gcc

# Target-depending Settings
ifeq ($(MAKECMDGOALS),windows)
CFLAGS += -D WINDOWS
endif

# Targets

taboptimal: $(OBJECTS_TABOPTIMAL)
	$(CC) -o taboptimal $(LDFLAGS) $^

windows: $(OBJECTS_TABOPTIMAL)
	$(CC) -o taboptimal $(LDFLAGS) $^

update_poly_runtime: $(OBJECTS_UPR)
	$(CC) -o update_poly_runtime $(LDFLAGS) $^

clean:
	rm -f *.o taboptimal taboptimal.exe
	rm -f update_poly_runtime update_poly_runtime.exe
