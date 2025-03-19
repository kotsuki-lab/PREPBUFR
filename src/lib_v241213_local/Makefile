TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

TARGET = $(LIB)

OBJ =\
$(LIBDIR_CONST)/*.a\
$(LIBDIR_BASE)/*.a\
$(LIBDIR_TIME)/*.a\
$(LIBDIR_LOG)/*.a\
$(LIBDIR_UTIL)/*.a\
$(LIBDIR_ARRAY)/*.a\
$(LIBDIR_MATH)/*.a\
$(LIBDIR_STATS)/*.a\
$(LIBDIR_IO)/*.a

.PHONY: all
all: 
	$(MAKE) -C $(LIBDIR_CONST)
	$(MAKE) -C $(LIBDIR_BASE)
	$(MAKE) -C $(LIBDIR_TIME)
	$(MAKE) -C $(LIBDIR_LOG)
	$(MAKE) -C $(LIBDIR_UTIL)
	$(MAKE) -C $(LIBDIR_ARRAY)
	$(MAKE) -C $(LIBDIR_MATH)
	$(MAKE) -C $(LIBDIR_STATS)
	$(MAKE) -C $(LIBDIR_IO)

	$(RM) $(RMFLAGS) $(MODDIR)/*.mod && $(MKDIR) $(MKDIRFLAGS) $(MODDIR)
	$(INSTALL) $(LIBDIR_CONST)/*.mod  $(MODDIR)/.
	$(INSTALL) $(LIBDIR_BASE)/*.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_TIME)/*.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_LOG)/*.mod    $(MODDIR)/.
	$(INSTALL) $(LIBDIR_UTIL)/*.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_ARRAY)/*.mod  $(MODDIR)/.
	$(INSTALL) $(LIBDIR_MATH)/*.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_STATS)/*.mod  $(MODDIR)/.
	$(INSTALL) $(LIBDIR_IO)/*.mod     $(MODDIR)/.

	$(MAKE) $(MKFLAGS) $(TARGET)

.PHONY: install
install:
	$(RM) $(RMFLAGS) $(MODDIR)/*.mod
	$(INSTALL) $(LIBDIR_CONST)/*.mod  $(MODDIR)/.
	$(INSTALL) $(LIBDIR_BASE)/*.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_TIME)/*.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_LOG)/*.mod    $(MODDIR)/.
	$(INSTALL) $(LIBDIR_UTIL)/*.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_ARRAY)/*.mod  $(MODDIR)/.
	$(INSTALL) $(LIBDIR_MATH)/*.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_STATS)/*.mod  $(MODDIR)/.
	$(INSTALL) $(LIBDIR_IO)/*.mod     $(MODDIR)/.

	$(MAKE) $(MKFLAGS) $(TARGET)


$(TARGET): $(OBJ)
	$(RM) $(RMFLAGS) $@
	$(AR) cqT $@ $^
	echo "create $@\naddlib $@\nsave\nend" | ar -M

.PHONY: clean
clean:
	$(MAKE) -C $(LIBDIR_CONST)  clean
	$(MAKE) -C $(LIBDIR_BASE)   clean
	$(MAKE) -C $(LIBDIR_TIME)   clean
	$(MAKE) -C $(LIBDIR_LOG)    clean
	$(MAKE) -C $(LIBDIR_UTIL)   clean
	$(MAKE) -C $(LIBDIR_ARRAY)  clean
	$(MAKE) -C $(LIBDIR_MATH)   clean
	$(MAKE) -C $(LIBDIR_STATS)  clean
	$(MAKE) -C $(LIBDIR_IO)     clean
	$(RM) $(RMFLAGS) $(MODDIR)/*.mod $(TARGET)
