include ../../Makefile.template

INCLUDES = -I ..

BACKEND = process

AUXMOD = libext yor
AUXMODBYT = $(AUXMOD:%=%.cmo)
AUXMODNAT = $(AUXMOD:%=%.cmx)
LOCALMOD = coordinator
COMMONMOD = cothread mutex condition event
COMMONMODNAT = $(COMMONMOD:%=%.cmx)
COMMONMODINTFSRC = $(COMMONMOD:%=%.mli)
COMMONMODINTFCOM = $(COMMONMOD:%=%.cmi)
COMMONMODINTF = $(COMMONMODINTFSRC) $(COMMONMODINTFCOM)
EXTRAMOD = thread
EXTRAMODINTFCOM = $(EXTRAMOD:%=%.cmi)
ALLMOD = $(LOCALMOD) $(COMMONMOD) $(EXTRAMOD)
ALLMODBYT = $(ALLMOD:%=%.cmo)
ALLMODNAT = $(ALLMOD:%=%.cmx)

LIB = cothreads
LIBBYT = $(LIB:%=%.cma)
LIBNAT = $(LIB:%=%.cmxa)
LIBNATA = $(LIB:%=%.a)

INSTALLDIR = $(INSTALLLIBDIR)/$(BACKEND)
INSTALLFILES = $(COMMONMODINTFCOM) $(EXTRAMODINTFCOM) $(LIBBYT) $(COMMONMODNAT) $(LIBNAT) $(LIBNATA)

$(LIBBYT): $(ALLMODBYT)
	$(OCAMLC) -a -o $(LIBBYT) $(AUXMODBYT) $(ALLMODBYT)
$(LIBNAT): $(ALLMODNAT)
	$(OCAMLOPT) -a -o $(LIBNAT) $(AUXMODNAT) $(ALLMODNAT)

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



