include ../Makefile.template

BACKENDS = threads vmthreads process

COMMON = yor.cmi yor.cmo

INTERFACE = ithread.cmi stm.cmi

.PHONY: all install clean

all: depend common interface suball
install: commoninstall interfaceinstall subinstall

common: $(COMMON)
commoninstall: common
	$(MKDIR) $(INSTALLLIBDIR)
	$(CP) $(COMMON) $(INSTALLLIBDIR)

interface: $(INTERFACE)
interfaceinstall: interface
	$(MKDIR) $(INSTALLLIBDIR)
	$(CP) $(INTERFACE) $(INSTALLLIBDIR)

sub%:
	@for i in $(BACKENDS); do $(MAKE) -C $$i $*; done

clean: ocamlclean subclean

include .depend

