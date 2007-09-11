include ../Makefile.template

BACKENDS = threads vmthreads process

AUXMOD = libext libextunix
AUXMODBYT = $(AUXMOD:%=%.cmo)
AUXMODNAT = $(AUXMOD:%=%.cmx)

COMMONMOD = mutex condition event
COMMONMODINTFSRC = $(COMMONMOD:%=%.mli)
COMMONMODINTFCOM = $(COMMONMOD:%=%.cmi)

EXTRAMOD = cothread stm
EXTRAMODINTFSRC = $(EXTRAMOD:%=%.mli)
EXTRAMODINTFCOM = $(EXTRAMOD:%=%.cmi)

INSTALLFILES = $(COMMONMODINTFCOM) $(EXTRAMODINTFSRC) $(EXTRAMODINTFCOM) 


$(COMMONMODINTFSRC): %: $(OCAMLSTDLIBPATH)/%
	@if [ ! -L $@ ]; then ln -s $< .; fi

.PHONY: all install clean uninstall sub%

all: $(AUXMODBYT) $(AUXMODNAT) $(COMMONMODINTFCOM) $(EXTRAMODINTFCOM) suball

install: all subinstall
	$(MKDIR) $(INSTALLLIBDIR)
	$(CP) $(INSTALLFILES) $(INSTALLLIBDIR)

uninstall: subuninstall
	for i in $(INSTALLFILES); do $(RM) $(INSTALLLIBDIR)/$$i; done

clean: ocamlclean subclean
	find . -type l -exec $(RM) '{}' \;

sub%:
	@for i in $(BACKENDS); do $(MAKE) -C $$i $*; done

