include ../Makefile.template

BACKENDS = threads vmthreads

.PHONY: all clean *install sub*

all: intf suball
install: intfinstall subinstall

clean: ocamlclean subclean

intf: stm.cmi

intfinstall: 
	$(MKDIR) $(INSTALLLIBDIR)
	$(CP) stm.cmi $(INSTALLLIBDIR)

sub%:
	@for i in $(BACKENDS); do $(MAKE) -C $$i $*; done


