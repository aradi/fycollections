include ../make.arch

# Subdirectory where the library should be built
BLDDIR = _build

# Source directrory with respect of BLDDIR
SRCDIR = ..

.PHONY: _build
_build:
	mkdir -p $(BLDDIR)
	$(MAKE) -C $(BLDDIR) SRCDIR=.. \
            FXX="$(FXX)" FXXOPT="$(FXXOPT)" \
            FYPP="$(FYPP)" FYPPOPT="$(FYPPOPT)" \
	    -f $(SRCDIR)/Makefile.build

.PHONY: clean distclean

clean:
	@[ ! -d $(BLDDIR) ] || $(MAKE) -C $(BLDDIR) SRCDIR=$(SRCDIR) \
	    -f $(SRCDIR)/Makefile.build clean

distclean:
	rm -rf $(BLDDIR)
