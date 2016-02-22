all : lib/sysext/sysext.so

test_sysext.o: CPPFLAGS = -I./lib/sysext
test_sysext: lib/sysext/sysext.o test_sysext.o
	$(CC) $^ -o $@


PIC = -fPIC

PY_SYSEXT = lib/sysext/py_sysext.c lib/sysext/sysext.c
PY_SYSEXT_OBJ = $(PY_SYSEXT:.c=.o)

# standard python - or debian extension?
PY_DEF := $(shell pyversions  -d)
PY_DEF := $(if $(PY_DEF),$(PY_DEF)-config, python-config)
PY_INC := $(shell $(PY_DEF) --includes)

LD = ld
# XXX right switch under LINUX?!?
LD_ALLOW_UNDEF = -G

$(PY_SYSEXT_OBJ): CFLAGS = -O -Wall -Wno-parentheses $(PIC)
$(PY_SYSEXT_OBJ): CPPFLAGS = $(PY_INC)

lib/sysext/sysext.so: $(PY_SYSEXT_OBJ)

%.so: %.o
	$(LD) $(LD_ALLOW_UNDEF) $^ $(LDFLAGS) -o $@

clean:
	rm -f $(PY_SYSEXT_OBJ) lib/sysext/sysext.so
