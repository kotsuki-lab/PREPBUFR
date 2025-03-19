TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

OBJS = \
SFMT.o\
common.o\
common_XXXXXX.o\
common_obs_XXXXXX.o

all:
	$(MAKE) $(OBJS)

clean:
	rm -f *.o *.mod *__genmod.f90

.SUFFIXES:
.SUFFIXES: .o .f90 .mod
.f90.o:
	$(FC) $(FCFLAGS) -c $< -I$(MODDIR)

%.mod: %.f90
	make $(patsubst %.f90,%.o,%<)
