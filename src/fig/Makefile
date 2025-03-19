TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

OBJS = \
mod_main.o\
main.o

TARGET = main

DIR_COMMON = $(TOPSRCDIR)/common
DIR_SPEEDY = $(TOPSRCDIR)/speedy
DIR_PREPBUFR = $(TOPSRCDIR)/prepbufr
DIR_THINNING = $(TOPSRCDIR)/thinning

all:
	$(MAKE) $(TARGET)

$(TARGET): $(OBJS) $(DIR_SPEEDY)/*.o $(DIR_PREPBUFR)/*.o $(DIR_COMMON)/*.o $(DIR_THINNING)/def_*.o $(DIR_THINNING)/mod_*.o $(LIB) $(NCEPLIB)
	$(FC) $(FCFLAGS) $(FOPT_OMP) -o $@ $^

clean:
	rm -f *.o *.mod $(TARGET)

.SUFFIXES:
.SUFFIXES: .o .f90 .mod
.f90.o:
	$(FC) $(FCFLAGS) $(FOPT_OMP) -c $< -I$(MODDIR) -I$(DIR_SPEEDY) -I$(DIR_PREPBUFR) -I$(DIR_THINNING) -I$(DIR_COMMON)

%.mod: %.f90
	make $(patsubst %.f90,%.o,%<)
