TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude
include $(MKINCLUDE)

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $<
%.mod: %.f90 %.o
	@:

TARGET = lib_const.a

OBJ_F = \
  lib_const.o

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
