include ../../Makefile.template

INCLUDES=-I +vmthreads

stm.cmo: stm.cmi stm.mli stm.ml

stm.cmi: ../stm.cmi
	@if [ ! -L "stm.cmi" ]; then ln -s ../stm.cmi .; fi

stm.mli: ../stm.mli
	@if [ ! -L "stm.mli" ]; then ln -s ../stm.mli .; fi

stm.ml: ../threads/stm.ml
	@if [ ! -L "stm.ml" ]; then ln -s ../threads/stm.ml .; fi


all: stm.cmo

install:
	$(MKDIR) $(INSTALLLIBDIR)/vmthreads
	$(CP) stm.cmo $(INSTALLLIBDIR)/vmthreads
	$(LN) $(INSTALLLIBDIR)/stm.cmi $(INSTALLLIBDIR)/vmthreads/stm.cmi

clean: ocamlclean
	$(RM) stm.mli stm.ml




