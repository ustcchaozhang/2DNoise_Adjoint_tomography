# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2015 by Thomas Forbriger (BFO Schiltach) 
# 
# Top level Makefile for Seitosh
# 
# REVISIONS and CHANGES 
#    28/04/2015   V1.0   Thomas Forbriger
# 
# ============================================================================
#
# options to be set for src/compile.sh
COMPILEOPT=--pause

all:

# collect files to be maintained
EDITFILES=$(filter-out flist src contrib %.bak,$(wildcard *) \
                             $(shell find src -maxdepth 1 -type f) \
                             $(shell find contrib -type f))

flist: Makefile $(EDITFILES)
	echo $(filter-out src/% contrib/%,$^) | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(filter src/%,$^) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(filter contrib/%,$^) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist
	src/compile.sh $(COMPILEOPT) clean
	
# ======================================================================
# check mandatory environment variable settings
# ---------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCSCRIPTDIR)

# ======================================================================
# compile and install
#
#  install
.PHONY: install
.PHONY: doc
.PHONY: all
doc all: %: ; src/compile.sh $(COMPILEOPT) $@
install: %: 
	mkdir -pv $(LOCSCRIPTDIR)
	/bin/cp -vpd src/compile.sh $(LOCSCRIPTDIR)
	/bin/cp -vpd contrib/utilities/checkcopying $(LOCSCRIPTDIR)
	/bin/cp -vpd contrib/utilities/checkcopyright $(LOCSCRIPTDIR)
	/bin/cp -vpd contrib/utilities/checkgplphrase $(LOCSCRIPTDIR)
	/bin/cp -vpd contrib/utilities/checklibs.sh $(LOCSCRIPTDIR)
	/bin/cp -vpd contrib/utilities/checklinkedlibs $(LOCSCRIPTDIR)
	/bin/cp -vpd contrib/utilities/depcheck.sh $(LOCSCRIPTDIR)
	/bin/cp -vpd contrib/utilities/gplphrase $(LOCSCRIPTDIR)
	/bin/cp -vpd contrib/utilities/new $(LOCSCRIPTDIR)
	/bin/cp -vpd contrib/utilities/systemtype.sh $(LOCSCRIPTDIR)
	src/compile.sh $(COMPILEOPT) $@

# ----- END OF Makefile ----- 
