TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

OBJS = \
def_const.o\
def_type.o\
mod_base.o\
mod_share.o\
mod_stats.o\
mod_common.o\
mod_set.o\
mod__all.o\
mod__wgt.o\
mod__dnst.o\
mod_make_obs.o\
mod_run.o\
main.o

TARGET = main

DIR_COMMON = $(TOPSRCDIR)/common
DIR_SPEEDY = $(TOPSRCDIR)/speedy
DIR_PREPBUFR = $(TOPSRCDIR)/prepbufr

all:
	$(MAKE) $(TARGET)
	$(INSTALL) $(TARGET) ../../.

$(TARGET): $(OBJS) $(DIR_SPEEDY)/*.o $(DIR_PREPBUFR)/*.o $(DIR_COMMON)/*.o $(LIB) $(NCEPLIB)
	$(FC) $(FCFLAGS) $(FOPT_OMP) -o $@ $^

clean:
	rm -f *.o *.mod $(TARGET)

.SUFFIXES:
.SUFFIXES: .o .f90 .mod
.f90.o:
	$(FC) $(FCFLAGS) $(FOPT_OMP) -c $< -I$(MODDIR) -I$(DIR_SPEEDY) -I$(DIR_PREPBUFR) -I$(DIR_COMMON)

%.mod: %.f90
	make $(patsubst %.f90,%.o,%<)
