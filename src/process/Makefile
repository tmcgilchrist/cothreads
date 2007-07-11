include ../../Makefile.template

ithread.cmo ithread.cmx: ithread.cmi ithread.mli

ithread.cmi: ../ithread.cmi
	@if [ ! -L "ithread.cmi" ]; then ln -s ../ithread.cmi .; fi

ithread.mli: ../ithread.mli
	@if [ ! -L "ithread.mli" ]; then ln -s ../ithread.mli .; fi

all: ithread.cmo ithread.cmx

install:
	$(MKDIR) $(INSTALLLIBDIR)/process
	$(CP) ithread.cmo ithread.cmx $(INSTALLLIBDIR)/process
	$(LN) $(INSTALLLIBDIR)/ithread.cmi $(INSTALLLIBDIR)/process/ithread.cmi

clean: ocamlclean
	$(RM) ithread.mli




