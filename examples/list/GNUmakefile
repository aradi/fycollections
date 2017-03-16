.SUFFIXES:
.SUFFIXES: .fpp .f90 .o
#.PRECIOUS: %.f90

include ../../make.arch

FYCOLLECTIONS_BLDDIR = ../_fycollections
FYCOLLECTIONS_SRCDIR = ../../lib
FYCOLLECTIONS_LIB = $(FYCOLLECTIONS_BLDDIR)/libfycollections.a

LIBS = -L$(FYCOLLECTIONS_BLDDIR) -lfycollections

TARGETS = intlist_example int1list_example int1ptrlist_example stringlist_example

ALL: $(TARGETS)


#
# Dependencies
# 
include Makefile.dep
include Makefile.extdep

#
# Build targets
#
intlist_example: $(intlist_example.o)
	$(LN) -o $@ $^ $(LIBS)

int1list_example: $(int1list_example.o)
	$(LN) -o $@ $^ $(LIBS)

int1ptrlist_example: $(int1ptrlist_example.o)
	$(LN) -o $@ $^ $(LIBS)

stringlist_example: $(stringlist_example.o)
	$(LN) -o $@ $^ $(LIBS)


#
# Build rules
#

%.o: %.f90
	$(FXX) $(FXXOPT) -I$(FYCOLLECTIONS_BLDDIR) -c $< -o $@

%.f90: %.fpp
	$(FYPP) $(FYPPOPT) -I../../include $< $@


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

distclean: mydistclean extdistclean

mydistclean: myclean
	rm -f $(TARGETS)

extdistclean:
	rm -rf $(FYCOLLECTIONS_BLDDIR)


#
# External build targets
#
# Note: Fycollections is built in a separate directory to ensure a clean build
#
$(FYCOLLECTIONS_LIB):
	mkdir -p $(FYCOLLECTIONS_BLDDIR)
	$(MAKE) -C $(FYCOLLECTIONS_BLDDIR) SRCDIR=$(FYCOLLECTIONS_SRCDIR) \
            FXX="$(FXX)" FXXOPT="$(FXXOPT)" \
            FYPP="$(FYPP)" FYPPOPT="$(FYPPOPT)" \
	    -f $(FYCOLLECTIONS_SRCDIR)/Makefile.build

