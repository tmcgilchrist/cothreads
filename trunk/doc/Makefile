TOPDIR = ..
include $(TOPDIR)/Makefile.template

all: doc

doc:
	$(MKDIR) html
	$(MKDIR) man
	$(OCAMLDOC) -html -d html -colorize-code $(TOPDIR)/$(SOURCEDIR)/*.mli
	$(OCAMLDOC) -man -d man -man-mini $(TOPDIR)/$(SOURCEDIR)/*.mli

clean: 
	$(RM) html man *~