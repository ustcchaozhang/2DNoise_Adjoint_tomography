#!/bin/bash
# this is <stuploxx.sh>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
# 
# standard stuploxx plots
# 
# REVISIONS and CHANGES 
#    23/04/2010   V1.0   Thomas Forbriger
# 
# ============================================================================
#

# prepare list of files
PANEL=1
FILES=
COMMONOPTIONS='sf:1.05 "n:%S %I %C filename: %F"'
while test z$1 != z
do
  FILES="$FILES $1 p:$PANEL $COMMONOPTIONS"
  PANEL=$(expr $PANEL + 1 )
  shift
done
echo $FILES
eval stuploxx --device=plot.ps/ps \
  --tstitle=0.1 --title="" \
  -labr -labe -labc -labh=0.08 \
  $FILES
gv plot.ps
# ----- END OF stuploxx.sh ----- 
