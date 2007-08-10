include ../Makefile.template

%.vmth: %.ml
	$(OCAMLC) -I +vmthreads -o $@ cothreads.cma $<

%.nath: %.ml
	$(OCAMLC) -I +threads -o $@ unix.cma cothreads.cma $<

%.nath.opt: %.ml
	$(OCAMLOPT) -I +threads -o $@ unix.cmxa cothreads.cmxa $<

%.proc: %.ml
	$(OCAMLC) -I +process -o $@ unix.cma cothreads.cma $<

%.proc.opt: %.ml
	$(OCAMLOPT) -I +process -o $@ unix.cmxa cothreads.cmxa $<

%.netw: %.ml
	$(OCAMLC) -I +networker -o $@ unix.cma cothreads.cma $<

%.netw: %.ml
	$(OCAMLOPT) -I +networker -o $@ unix.cmxa cothreads.cmxa $<

# Since prod_consum make use of some aux libraries we developed during the implementation of 
# the process backend. We handle it specially. We should reimplement it with events. This test
# also shows how to compile a source once, but link with different back-ends. (we call it
# "Object-level independence" and "linking-time semantics", comapred with "Source-level 
#  independence" and "compile-time semantics".

AUXMOD = libext coordinator
AUXMODINTFCOM = $(AUXMOD:%=%.cmi)
AUXMODBYT = $(AUXMOD:%=%.cmo)
AUXMODNAT = $(AUXMOD:%=%.cmx)
AUXMODNATA = $(AUXMOD:%=%.o)
AUXFILES = $(AUXMODINTFCOM) $(AUXMODBYT) $(AUXMODNAT) $(AUXMODNATA)

$(AUXFILES):
	@if [ ! -e $@ -a -e ../src/$@ ]; then ln -s ../src/$@ .; fi
	@if [ ! -e $@ -a -e ../src/process/$@ ]; then ln -s ../src/process/$@ .; fi

prod_consum.cmo: $(AUXFILES) prod_consum.ml
	$(OCAMLC) -c prod_consum.ml
prod_consum.cmx: $(AUXFILES) prod_consum.ml
	$(OCAMLOPT) -c prod_consum.ml
prod_consum.nath: prod_consum.cmo
	$(OCAMLC) -I +threads -o $@ unix.cma  $(AUXMODBYT) cothreads.cma $<
prod_consum.proc: prod_consum.cmo
	$(OCAMLC) -I +process  -o $@ unix.cma  cothreads.cma $<
prod_consum.nath.opt: prod_consum.cmx
	$(OCAMLOPT) -I +threads -o $@ unix.cmxa $(AUXMODNAT) cothreads.cmxa $<
prod_consum.proc.opt: prod_consum.cmx
	$(OCAMLOPT) -I +process -o $@ unix.cmxa cothreads.cmxa $<

THRTEST = coth evt lock 
STMONLY = cmt phil santa test  #require STM implementation, not yet available for process
ALLTEST = $(THRTEST) $(STMONLY)
EXTRATEST = prod_consum

all:$(ALLTEST:%=%.vmth) $(ALLTEST:%=%.nath) $(ALLTEST:%=%.nath.opt) $(THRTEST:%=%.proc) $(THRTEST:%=%.proc.opt) $(EXTRATEST:%=%.nath) $(EXTRATEST:%=%.nath.opt)	$(EXTRATEST:%=%.proc) $(EXTRATEST:%=%.proc.opt)

clean: ocamlclean
	$(RM) *.vmth *.nath *.proc *.netw *.opt

