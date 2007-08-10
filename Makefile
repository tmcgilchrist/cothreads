include ./Makefile.template

all:
	$(MAKE) -C $(SOURCEDIR) $@

install:
	$(MAKE) -C $(SOURCEDIR) $@

uninstall:
	$(MAKE) -C $(SOURCEDIR) $@

doc:
	$(MAKE) -C $(DOCDIR) $@

clean: ocamlclean
	$(MAKE) -C $(SOURCEDIR) $@
	$(MAKE) -C $(DOCDIR) $@
	$(MAKE) -C $(EXAMPLEDIR) $@




