include ../../Makefile.template

INCLUDES = -I ..

COMMON = coordinator.cmo
MODULES = cothread mutex
MODULESBYT = $(MODULES:%=%.cmo)
INTERFACE = $(MODULES:%=%.mli) $(MODULES:%=%.cmi)

%.mli: ../%.mli
	@if [ ! -L $@ ]; then ln -s $< .; fi
%.cmi: ../%.cmi
	@if [ ! -L $@ ]; then ln -s $< .; fi


all: $(COMMON) $(INTERFACE) $(MODULESBYT)

install:
	$(MKDIR) $(INSTALLLIBDIR)/process
	$(CP) ithread.cmo ithread.cmx $(INSTALLLIBDIR)/process
	$(LN) $(INSTALLLIBDIR)/ithread.cmi $(INSTALLLIBDIR)/process/ithread.cmi

clean: ocamlclean
	$(RM) $(INTERFACE)




