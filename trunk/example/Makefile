include ../Makefile.template


%.vmth: %.cmo
	$(OCAMLC) -I +vmthreads -o $@ $(if $(findstring ray, $@), ray.cmo) cothreads.cma $^

%.nath: %.cmo
	$(OCAMLC) -I +threads -o $@ $(if $(findstring ray,$@), ray.cmo) unix.cma cothreads.cma $<

%.nath.opt: %.cmx
	$(OCAMLOPT) -I +threads -o $@ $(if $(findstring ray,$@), ray.cmx) unix.cmxa cothreads.cmxa $<

%.proc: %.cmo
	$(OCAMLC) -I +process -o $@ $(if $(findstring ray,$@), ray.cmo) unix.cma cothreads.cma $<

%.proc.opt: %.cmx
	$(OCAMLOPT) -I +process -o $@ $(if $(findstring ray,$@), ray.cmx) unix.cmxa cothreads.cmxa $<

%.netw: %.cmo
	$(OCAMLC) -I +networker -o $@ $(if $(findstring ray,$@), ray.cmo) unix.cma cothreads.cma $<

%.netw.opt: %.cmx
	$(OCAMLOPT) -I +networker -o $@ $(if $(findstring ray,$@), ray.cmx) unix.cmxa cothreads.cmxa $<



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

ALLTEST = coth evt lock ray_col ray_nocol test mvar merge mcast phil santa
EXTRATEST = prod_consum

all: $(ALLTEST:%=%.vmth) $(ALLTEST:%=%.nath) $(ALLTEST:%=%.nath.opt) $(ALLTEST:%=%.proc) $(ALLTEST:%=%.proc.opt) $(EXTRATEST:%=%.nath) $(EXTRATEST:%=%.nath.opt)	$(EXTRATEST:%=%.proc) $(EXTRATEST:%=%.proc.opt)

clean: ocamlclean
	$(RM) *.vmth *.nath *.proc *.netw *.opt *.pgm

