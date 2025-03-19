TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

OBJS = \
mod_sphere.o

all:
	$(MAKE) $(OBJS)

clean:
	rm -f *.o *.mod $(TARGET)

.SUFFIXES:
.SUFFIXES: .o .f90 .mod
.f90.o:
	$(FC) $(FCFLAGS) $(FOPT_OMP) -c $< -I$(MODDIR)

%.mod: %.f90
	make $(patsubst %.f90,%.o,%<)
