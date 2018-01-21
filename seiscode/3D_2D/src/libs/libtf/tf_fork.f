c this is <tf_fork.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c Copyright (c) 1999 by Gerhard Mueller (IMG Frankfurt)
c
c do a fast fourier transform
c
c This code was originally published by Gerhard Müller
c in his lecture notes on digital signal processing:
c Gerhard Mueller, 1999. Digitale Signalverarbeitung. Skriptum zur
c gleichnamigen Vorlesung. Institut für Meteorologie und Geophysik,
c Universität Frankfurt.
c
c The original algorithm appears to be due to Claerbout, J.F., 
c "Fundamentals of Geophysical Data Processing with Applications 
c to Petroleum Prospecting", McGraw-Hill, 1976.
c
c ----
c This program is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c 
c You should have received a copy of the GNU General Public License
c along with this program; if not, write to the Free Software
c Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
c ----
c
c REVISIONS and CHANGES
c    24/06/97   V1.0   Thomas Forbriger
c
c==============================================================================
cS
c
c this fast fourier transform algorithm is derived from
c g.muellers reflectivity program psexpl
c
c pay ATTENTION: this routine doesn't check for 
c                LX being an integer power of two
c
c                but LX has to be an INTEGER POWER OF TWO!
c 
c howto
c
c   CX might be a complex time series or a complex spectrum
c   LX is the number of samples in CX
c   SIGNI defines the sign of the exponetial term:
c         SIGNI=1.   calculate time series from spectrum
c         SIGNI=-1.  calculate spectral coefficients from time series
c
c   what the routine actually does is:
c
c   CXl = 1/sqrt(LX) * sum_(k=1)^(LX) CXk * exp(i*2*PI*SIGNI*(k-1)*(l-1)/LX)
c
c      with l=1,...LX
c           k=1,...LX
c 
c      and i being the imaginary unit
c
        SUBROUTINE tf_fork(LX,CX,SIGNI)
        COMPLEX CX(LX),CARG,CW,CTEMP
        real signi
c 
cE
c        PI=3.14159265
        real pi,sc
        parameter(pi=3.1415926535897931159979)
        integer j,i,m,istep,l,lx
c	  PI=4.*atan(1.)
        J=1
        SC=1./FLOAT(LX)
        SC=SQRT(SC)
        DO 5  I=1,LX
        IF(I.GT.J) GOTO 2
        CTEMP=CX(J)*SC
        CX(J)=CX(I)*SC
        CX(I)=CTEMP
2       M=LX/2
3       IF(J.LE.M) GOTO 5
        J=J-M
        M=M/2
        IF(M.GE.1) GOTO3
5       J=J+M
        L=1
6       ISTEP=2*L
        DO 8  M=1,L
        CARG=CMPLX(0.,1.)*(PI*SIGNI*FLOAT(M-1))/FLOAT(L)
        CW=CEXP(CARG)
        DO 8 I=M,LX,ISTEP
        CTEMP=CW*CX(I+L)
        CX(I+L)=CX(I)-CTEMP
8       CX(I)=CX(I)+CTEMP
        L=ISTEP
        IF(L.LT.LX) GOTO 6
        RETURN
        END
c
c ----- END OF tf_fork.f -----
