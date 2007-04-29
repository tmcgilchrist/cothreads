include ./Makefile.template

all:
	$(MAKE) -C $(SOURCEDIR) $@

install:
	$(MAKE) -C $(SOURCEDIR) $@

uninstall:
	$(RM) $(INSTALLLIBDIR)

doc:
	$(MAKE) -C $(DOCDIR) $@

clean: ocamlclean
	$(MAKE) -C $(SOURCEDIR) $@
	$(MAKE) -C $(DOCDIR) $@
	$(MAKE) -C $(EXAMPLEDIR) $@




