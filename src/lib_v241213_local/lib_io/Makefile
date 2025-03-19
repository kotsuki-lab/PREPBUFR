TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90 .F90 .c
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(LIBDIR_CONST) -I$(LIBDIR_BASE) -I$(LIBDIR_LOG) -I$(LIBDIR_UTIL) -I$(LIBDIR_MATH) -I$(LIBDIR_ARRAY)
%.o: %.F90
	$(FC) $(FCFLAGS) $(FOPT_USE_C) $(FOPT_TERMINAL_WIDTH) -c $< -I$(LIBDIR_CONST) -I$(LIBDIR_BASE) -I$(LIBDIR_LOG) -I$(LIBDIR_UTIL) -I$(LIBDIR_MATH) -I$(LIBDIR_ARRAY)
%.o: %.c
	$(CC) $(CCFLAGS) -c $< -I$(LIBDIR_BASE)
%.mod: %.f90 %.o
	@:

TARGET = lib_io.a

OBJ_F = \
  lib_io_system.o\
  lib_io_base.o\
  lib_io_arg_base.o\
  lib_io_arg_parser.o\
  lib_io_file.o\
  lib_io_binary_common.o\
  lib_io_binary_direct.o\
  lib_io_binary_stream.o\
  lib_io.o

OBJ_C = \
  func_io_system_tput.o

ifeq ($(FOPT_USE_C), -DUSE_C)
  OBJ_ALL = $(OBJ_F) $(OBJ_C)
else
  OBJ_ALL = $(OBJ_F)
endif

MOD_ALL = $(OBJ_F:.o=.mod)

.PHONY: all
all:
	$(MAKE) $(OBJ_ALL)
	$(MAKE) $(TARGET)

$(TARGET): $(OBJ_ALL)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod $(TARGET)
