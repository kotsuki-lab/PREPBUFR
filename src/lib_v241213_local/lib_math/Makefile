TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(LIBDIR_CONST) -I$(LIBDIR_BASE) -I$(LIBDIR_LOG) -I$(LIBDIR_UTIL)
%.mod: %.f90 %.o
	@:

TARGET = lib_math.a

OBJ_F = \
  lib_math_exception.o\
  lib_math_operation.o\
  lib_math_stats.o\
  lib_math_unit.o\
  lib_math_gcd.o\
  lib_math_array.o\
  lib_math_linalg_operator.o\
  lib_math_linalg_util.o\
  lib_math_sphere.o\
  lib_math_interp.o\
  lib_math.o

OBJ_ALL = $(OBJ_F)

MOD_ALL = $(OBJ_F:.o=.mod)

.PHONY: all
all: $(TARGET)

$(TARGET): $(OBJ_ALL)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod $(TARGET)
