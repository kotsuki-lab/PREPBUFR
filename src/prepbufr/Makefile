TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

OBJS = \
def_const_pb.o\
def_type_pb.o\
mod_pb.o

TARGET = main

DIR_SPEEDY = $(TOPSRCDIR)/speedy

all:
	$(MAKE) $(OBJS)

clean:
	rm -f *.o *.mod $(TARGET)

.SUFFIXES:
.SUFFIXES: .o .f90 .mod
.f90.o:
	$(FC) $(FCFLAGS) -c $< -I$(MODDIR) -I$(DIR_SPEEDY)

%.mod: %.f90
	make $(patsubst %.f90,%.o,%<)
