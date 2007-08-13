include ../../Makefile.template

INCLUDES = -I ..

BACKEND = process

AUXMOD = libext yor
AUXMODBYT = $(AUXMOD:%=%.cmo)
AUXMODNAT = $(AUXMOD:%=%.cmx)
LOCALMOD = coordinator
COMMONMOD = mutex condition event cothread
COMMONMODNAT = $(COMMONMOD:%=%.cmx)
COMMONMODINTFSRC = $(COMMONMOD:%=%.mli)
COMMONMODINTFCOM = $(COMMONMOD:%=%.cmi)
COMMONMODINTF = $(COMMONMODINTFSRC) $(COMMONMODINTFCOM)
EXTRAMOD = thread
EXTRAMODNAT =$(EXTRAMOD:%=%.cmx)
EXTRAMODINTFCOM = $(EXTRAMOD:%=%.cmi)
ALLMOD = $(LOCALMOD) $(COMMONMOD) $(EXTRAMOD)
ALLMODBYT = $(ALLMOD:%=%.cmo)
ALLMODNAT = $(ALLMOD:%=%.cmx)

LIB = threads cothreads
LIBBYT = $(LIB:%=%.cma)
LIBNAT = $(LIB:%=%.cmxa)
LIBNATA = $(LIB:%=%.a)

INSTALLDIR = $(INSTALLLIBDIR)/$(BACKEND)
INSTALLFILES = $(COMMONMODINTFCOM) $(EXTRAMODINTFCOM) $(COMMONMODNAT) $(EXTRAMODNAT) $(LIBBYT) $(LIBNAT) $(LIBNATA)

$(LIBBYT): %: $(ALLMODBYT)
	$(OCAMLC) -a -o $@ $(AUXMODBYT) $(ALLMODBYT)
$(LIBNAT): %: $(ALLMODNAT)
	$(OCAMLOPT) -a -o $@ $(AUXMODNAT) $(ALLMODNAT)

$(COMMONMODINTF): %: ../%
	@if [ ! -e $@ -a -e $< ]; then ln -s $< .; fi

.PHONY: all install clean uninstall

all: $(COMMONMODINTF) $(LIBBYT) $(LIBNAT)

install: all
	$(MKDIR) $(INSTALLDIR)
	$(CP) $(INSTALLFILES) $(INSTALLDIR)

clean: ocamlclean

uninstall:
	$(RM) $(INSTALLDIR)



