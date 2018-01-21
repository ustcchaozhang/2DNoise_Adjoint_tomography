c this is <sousou_workspace.inc>
c------------------------------------------------------------------------------
c
c 17/11/98 by Thomas Forbriger (IfG Stuttgart)
c
c here we hold signal spectra etc.
c
c REVISIONS and CHANGES
c    17/11/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
      complex spectra(maxsamples)
      complex slowspec(maxsamples)
      real slowamp(maxslow+2),slowvalue(maxslow),velo(maxslow+2)
      real graymap(maxslow, maxspec), plotmap(maxplot,maxplot)
      real df, slowestim, timeshift
      integer nspecsamp, slowestimidx
      integer firsttrap,secondtrap,thirdtrap
      real firsttrapval, secondtrapval,thirdtrapval,midoff
      common /slowness/slowamp,slowvalue,velo
      common /thegray/ graymap, plotmap
      common /sarr/ spectra,slowspec
      common /themore/ df,nspecsamp,slowestim, slowestimidx, timeshift,
     &  firsttrap,secondtrap,firsttrapval,secondtrapval,thirdtrap,
     &  thirdtrapval,midoff
c
c ----- END OF sousou_workspace.inc -----
