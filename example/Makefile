include ../Makefile.template

# If coThreads is built but not installed, pass LOCAL variable to make e.g "make all LOCAL=../src"
INCLUDES=$(if $(LOCAL),-I $(LOCAL))
PATHROOT=$(if $(LOCAL),$(LOCAL)/,+)

IFRAY = $(if $(findstring ray,$@), $(if $(findstring opt,$@),ray.cmx,ray.cmo))
VMPARAM = -I $(PATHROOT)vmthreads $(IFRAY) cothreads.cma 
NATHPARAM = -I $(PATHROOT)threads $(IFRAY) unix.cma cothreads.cma
NATHOPTPARAM = $(NATHPARAM:%.cma=%.cmxa)
PROCPARAM = -I $(PATHROOT)process $(IFRAY) unix.cma cothreads.cma
PROCOPTPARAM = $(PROCPARAM:%.cma=%.cmxa)

%.vmth: %.cmo
	$(OCAMLC) -o $@ $(VMPARAM) $< 
%.nath: %.cmo
	$(OCAMLC) -o $@ $(NATHPARAM) $<
%.nath.opt: %.cmx
	$(OCAMLOPT) -o $@ $(NATHOPTPARAM) $<
%.proc: %.cmo
	$(OCAMLC) -o $@ $(PROCPARAM)  $<
%.proc.opt: %.cmx
	$(OCAMLOPT) -o $@ $(PROCOPTPARAM)  $<

ALLTEST = coth evt lock ray_col ray_nocol test mvar merge mcast phil santa
all: $(ALLTEST:%=%.vmth) $(ALLTEST:%=%.nath) $(ALLTEST:%=%.nath.opt) $(ALLTEST:%=%.proc) $(ALLTEST:%=%.proc.opt) 

clean: ocamlclean
	$(RM) *.vmth *.nath *.proc *.netw *.opt *.pgm

# TODO:
# NETWPARAM = -I +networker $(IFRAY) unix.cma cothreads.cma
# NETWOPTPARAM = $(NETWOPTPARAM:%.cma=%.cmxa)
# %.netw: %.cmo
# 	$(OCAMLC) -o $@ $(NETWPARAM) $<
# %.netw.opt: %.cmx
# 	$(OCAMLOPT) -o $@ $(NETWOPTPARAM) $<
