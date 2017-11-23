#!/bin/gawk
# this is <extractmod.awk>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
# 
# extract polynomial model from gremlin log file
# 
# REVISIONS and CHANGES 
#    13/03/2006   V1.0   Thomas Forbriger
# 
# ============================================================================
#
BEGIN {hot=0 ; basename="modout"; fico=0 }
/^that's it now/ { 
  print "found model";
  ilico=0; 
  hot=1;
  fico=fico+1;
  filename=sprintf("%s.%5.5d", basename, fico);
  print "filename is " filename;
  printf("next model no. %d\n", fico) >filename;
  next 
}
/number of sections/ {
  if (hot) {
    ilico=0 ;
    nsec=substr($0, 1, 5);
  }
}
{
  if (hot) {
    print >> filename;
  }
  ilico=ilico+1;
  if (ilico > (nsec*7+2)) { hot=0 }
}


# ----- END OF extractmod.awk ----- 
