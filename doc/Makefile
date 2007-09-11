TOPDIR = ..
include $(TOPDIR)/Makefile.template

all: doc

doc:
	$(MKDIR) html
	$(MKDIR) man
	$(OCAMLDOC) -html -d html -colorize-code $(TOPDIR)/$(SOURCEDIR)/*.mli $(TOPDIR)/$(SOURCEDIR)/process/thread.mli
	$(OCAMLDOC) -man -d man -man-mini $(TOPDIR)/$(SOURCEDIR)/*.mli $(TOPDIR)/$(SOURCEDIR)/process/thread.mli

clean: 
	$(RM) html man *~ .depend