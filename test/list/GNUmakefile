.SUFFIXES:
.SUFFIXES: .fpp .f90 .o
#.PRECIOUS: %.f90

include ../../make.arch

FYCOLLECTIONS_BLDDIR = ../_fycollections
FYCOLLECTIONS_SRCDIR = ../../lib
FYCOLLECTIONS_LIB = $(FYCOLLECTIONS_BLDDIR)/libfycollections.a

FYTEST_BLDDIR = ../_fytest
FYTEST_SRCDIR = ../../external/fytest/lib
FYTEST_LIB = $(FYTEST_BLDDIR)/libfytest.a

LIBS = -L$(FYCOLLECTIONS_BLDDIR) -lfycollections -L$(FYTEST_BLDDIR) -lfytest

TARGETS = test_intlist test_int1list test_stringlist

ALL: $(TARGETS)


#
# Include automatic internal and manual external dependencies
# 
include Makefile.dep
include Makefile.extdep


#
# Build targets
#
test_intlist: $(test_intlist.o)
	$(LN) -o $@ $^ $(LIBS)

test_int1list: $(test_int1list.o)
	$(LN) -o $@ $^ $(LIBS)

test_stringlist: $(test_stringlist.o)
	$(LN) -o $@ $^ $(LIBS)


#
# Build rules
#

%.o: %.f90
	$(FXX) $(FXXOPT) -I$(FYCOLLECTIONS_BLDDIR) -I$(FYTEST_BLDDIR) -c $< -o $@

%.f90: %.fpp
	$(FYPP) $(FYPPOPT) -I../../include -I../../external/fytest/include $< $@


#
# Clean up targets
#
.PHONY: clean myclean extclean distclean mydistclean extdistclean

clean: myclean extclean

myclean:
	rm -f *.mod *.o *.f90

extclean:
	@[ ! -d $(FYCOLLECTIONS_BLDDIR) ] \
            || $(MAKE) -C $(FYCOLLECTIONS_BLDDIR) SRCDIR=$(FYCOLLECTIONS_SRCDIR) \
	    -f $(FYCOLLECTIONS_SRCDIR)/Makefile.build clean
	@[ ! -d $(FYTEST_BLDDIR) ] \
            || $(MAKE) -C $(FYTEST_BLDDIR) SRCDIR=$(FYTEST_SRCDIR) \
	    -f $(FYTEST_SRCDIR)/Makefile.build clean

distclean: mydistclean extdistclean

mydistclean: myclean
	rm -f $(TARGETS)

extdistclean:
	rm -rf $(FYCOLLECTIONS_BLDDIR)
	rm -rf $(FYTEST_BLDDIR)


#
# External build targets
#
#
$(FYCOLLECTIONS_LIB):
	mkdir -p $(FYCOLLECTIONS_BLDDIR)
	$(MAKE) -C $(FYCOLLECTIONS_BLDDIR) SRCDIR=$(FYCOLLECTIONS_SRCDIR) \
            FXX="$(FXX)" FXXOPT="$(FXXOPT)" \
            FYPP="$(FYPP)" FYPPOPT="$(FYPPOPT)" \
	    -f $(FYCOLLECTIONS_SRCDIR)/Makefile.build

$(FYTEST_LIB):
	mkdir -p $(FYTEST_BLDDIR)
	$(MAKE) -C $(FYTEST_BLDDIR) SRCDIR=$(FYTEST_SRCDIR) \
            FXX="$(FXX)" FXXOPT="$(FXXOPT)" \
            FYPP="$(FYPP)" FYPPOPT="$(FYPPOPT)" \
	    -f $(FYTEST_SRCDIR)/Makefile.build
