include ../Makefile.template

all: test phil santa ith_demo

INCLUDES=-thread

test: test.cmo
	$(OCAMLC) -o test $(INCLUDES) unix.cma threads.cma stm.cmo test.cmo

phil: phil.cmo
	$(OCAMLC) -o phil $(INCLUDES) unix.cma threads.cma stm.cmo phil.cmo

santa: santa.cmo
	$(OCAMLC) -o santa $(INCLUDES) unix.cma threads.cma stm.cmo santa.cmo

ith_demo: ith_demo.cmo
	$(OCAMLC) -o ith_demo -I +process unix.cma ithread.cmo ith_demo.cmo

clean: ocamlclean
	$(RM) test phil santa ith_demo