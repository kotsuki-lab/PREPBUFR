TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(LIBDIR_CONST) -I$(LIBDIR_BASE) -I$(LIBDIR_LOG)
%.mod: %.f90 %.o
	@:

TARGET = lib_util.a

OBJ_F = \
  lib_util_str_param.o\
  lib_util_char.o\
  lib_util_str.o\
  lib_util_swap.o\
  lib_util_unit.o\
  lib_util_phys.o\
  lib_util.o

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
