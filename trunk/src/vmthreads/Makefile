include ../../Makefile.template

INCLUDES=-I +vmthreads -I ..

BACKEND = vmthreads

AUXMOD = libext
AUXMODBYT = $(AUXMOD:%=%.cmo)

COMMONMOD = cothread stm
COMMONMODSRC = $(COMMONMOD:%=%.ml)
COMMONMODBYT = $(COMMONMOD:%=%.cmo)
COMMONMODINTFSRC = $(COMMONMOD:%=%.mli)
COMMONMODINTFCOM = $(COMMONMOD:%=%.cmi)
COMMONMODINTF = $(COMMONMODINTFSRC) $(COMMONMODINTFCOM)

LIB = cothreads
LIBBYT = $(LIB:%=%.cma)

INSTALLDIR = $(INSTALLLIBDIR)/$(BACKEND)
INSTALLFILES = $(COMMONMODINTFCOM) $(LIBBYT)


$(LIBBYT): $(COMMONMODBYT)
	$(OCAMLC) -a -o $@ threads.cma $(AUXMODBYT) $+

$(COMMONMODINTF):%:../%
	@if [ ! -e $@ -a -e ../$@ ]; then ln -s $< .; fi

$(COMMONMODSRC):%:../threads/%
	@if [ ! -e $@ -a -e $< ]; then ln -s $< .; fi

.PHONY: all install clean uninstall

all: $(COMMONMODSRC) $(COMMONMODINTF) $(LIBBYT)

install: all
	$(MKDIR) $(INSTALLDIR)
	$(CP) $(INSTALLFILES) $(INSTALLDIR)

clean: ocamlclean

uninstall:
	for i in $(INSTALLFILES); do $(RM) $(INSTALLDIR)/$$i; done
