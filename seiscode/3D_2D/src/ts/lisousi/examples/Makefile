# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2015 by Thomas Forbriger (BFO Schiltach) 
# 
# examples for lisousi
# 
# REVISIONS and CHANGES 
#    12/05/2015   V1.0   Thomas Forbriger
# 
# ============================================================================
#
# plots used by Forbriger et al. (2014, doi:10.1093/gji/ggu199):
PUBPLOTS=bietZ bietR bietCZ bietCR fullspace bietmod \
         bietZpcol bietRpcol fullspacecol bietmodcol

.PHONY: all
all: $(addsuffix .pdf,$(PUBPLOTS))

# Makefiles containing rules
MAKEFILES=$(filter-out %.bak,$(wildcard Makefile*))
# parameter files for refmet
# --------------------------
#  main control file for given cases
REFMETMAIN=$(wildcard *.main)
#  subsurface structure
REFMETMOD=$(wildcard *.mod)
#  source definition
REFMETSRC=$(wildcard *.src)
#  receiver definition
REFMETRCV=$(wildcard *.rcv)
#  traveltime picks
PICKS=$(wildcard *.pck)

flist: README $(MAKEFILES) $(REFMETMAIN) $(REFMETMOD) $(REFMETSRC) \
  $(REFMETRCV) $(PICKS)
	echo README | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(MAKEFILES) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(REFMETMAIN) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(REFMETMOD) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(REFMETSRC) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(REFMETRCV) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(PICKS) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.xxx

# ======================================================================
# report
#
# report control files for synthetic seismograms:
.PHONY: reportccontrol
reportcontrol: $(REFMETMAIN) $(REFMETMOD) $(REFMETSRC) $(REFMETRCV)
	head -n 1 $^

# ======================================================================
# read sub-Makefiles
#
include Makefile.model
include Makefile.synthetics
include Makefile.filter
include Makefile.FourierBessel
include Makefile.SingleTraceTransformation
include Makefile.residuals
include Makefile.plots
include Makefile.tests
#
# ======================================================================
# conversion patterns
#----------------------------------------------------------------------
# format conversion
%.asc: %.bin; any2any --verb --over --otype ascii --itype bin $@ $<

#----------------------------------------------------------------------

%.pdf: %.ps; ps2pdf $<
%.psp: %.ps; gv $<; /bin/rm -fv $<
%.pse: %.ps; evince  $<; /bin/rm -fv $<
%.pde: %.pdf; evince -w $<; /bin/rm -fv $<
%.pdp: %.pdf; acroread $<; /bin/rm -fv $<

# ----- END OF Makefile ----- 
