include ../Makefile.template

BACKENDS = threads vmthreads process

COMMON = yor.cmi yor.cmo libext.cmo 

INTERFACE = cothread.cmi stm.cmi mutex.mli mutex.cmi condition.mli condition.cmi

%.mli: $(OCAMLSTDLIBPATH)/%.mli
	@if [ ! -L $@ ]; then ln -s $< .; fi

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
	$(RM) $(INTERFACE)

include .depend

