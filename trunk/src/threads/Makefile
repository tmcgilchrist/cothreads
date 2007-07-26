include ../../Makefile.template

INCLUDES=-I +threads

stm.cmo stm.cmx: stm.cmi stm.mli

stm.cmi: ../stm.cmi
	@if [ ! -L "stm.cmi" ]; then ln -s ../stm.cmi .; fi

stm.mli: ../stm.mli
	@if [ ! -L "stm.mli" ]; then ln -s ../stm.mli .; fi

all: stm.cmo stm.cmx

install:
	$(MKDIR) $(INSTALLLIBDIR)/threads
	$(CP) stm.cmo stm.cmx stm.o $(INSTALLLIBDIR)/threads
	$(LN) $(INSTALLLIBDIR)/stm.cmi $(INSTALLLIBDIR)/threads/stm.cmi

clean: ocamlclean
	$(RM) stm.mli




