c this is <dat_dcpc.f>
c------------------------------------------------------------------------------
cS
c
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c modify greens matrix according to datamode
c matrix may be copied from array to array (specified by index)
c from and to may be equal
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    20/08/98   V1.1   new formula for dynamic range reduction
c    07/04/00   V1.2   * introduced polynomial trend removal
c                      * introduced new datamodes 5,6,7,8
c                      * normalize standard logstretch function
c                      * introduced fmaxmap prefit mode
c    09/04/00   V1.3   * now use conord1 and conord2
c                      * reorganized mode settings 
c                      * added polynomial fit to real part
c
c modes now are:
c
c datamode  action
c 1         take complex values as they are
c 2         take complex values and reduce amplitude dynamic
c 3         take real amplitudes from complex values
c 4         take real amplitudes from complex values and reduce dynamic range
c
c if datamode is greater then 4 the datamode number is assumed to be a sum of
c the following flag values plus 4:
c
c 1   take absolute value
c 2   increase contrast according to setting of conord1, conord2 and conthresh
c 4   remove polynomial trend from real part of synthetics up to order polyord
c 8   remove polynomial trend rather from absolute value
c 16  remove polynomial trend also from read data
c
c======================================================================
c 
      subroutine dat_dcpc(from, to)
c
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c
      integer from, to
c 
cE
      integer i,j
      double precision a, logfac,b
c
      double precision nh, sum, legendre, msum, maxamp, x
      integer n, takefrom
c
      logical mpoly
      logical mpolyabs
      logical mpolyread
      logical mcontrast
      logical mabsolute
      integer submode
c
      double precision f
      f(a,x)=log10(1.d0+((10.d0**a)-1.d0)*x)/dble(a)
c
c
      if (verb_subaction) print *,'ENTER dat_dcpc(',from,',',to,')'
c
      if (verb_subaction) print *,
     &  'NOTICE (dat_dcpc): modify green data from ',from, ' to ',to,
     &  ' using datamode ',datamode
c
c find out submode flags
      if (datamode.le.4) then
c 
        if (verb_debug) print *,
     &    'DEBUG (dat_dcpc): data_maxamp=',data_maxamp
        logfac=((10.d0**logstretch)-1.d0)/data_maxamp
        if (verb_debug) print *,
     &    'DEBUG (dat_dcpc): logstretch=',logstretch,' logfac=',logfac
c   
        if (datamode.eq.1) then
          do i=rng_smin,rng_smax
            do j=rng_fmin,rng_fmax
c          do i=1,data_nslo
c            do j=1,data_nfre
              green(i,j,to)=green(i,j,from)
            enddo
          enddo
        elseif (datamode.eq.2) then
          if (verb_substrategy) print *,
     &       'NOTICE (dat_dcpc): use complex log-stretch ',logstretch
          do i=rng_smin,rng_smax
            do j=rng_fmin,rng_fmax
c          do i=1,data_nslo
c            do j=1,data_nfre
              a=abs(green(i,j,from))
c              green(i,j,to)=log10(logstretch*a+1.)*(green(i,j,from)/a)
              green(i,j,to)=log10(1.+logfac*a)*(green(i,j,from)/a)/logstretch
            enddo
          enddo
        elseif ((datamode.eq.3).or.(datamode.gt.4)) then
          do i=rng_smin,rng_smax
            do j=rng_fmin,rng_fmax
c          do i=1,data_nslo
c            do j=1,data_nfre
              green(i,j,to)=abs(green(i,j,from))
            enddo
          enddo
        elseif (datamode.eq.4) then
          if (verb_substrategy) print *,
     &      'NOTICE (dat_dcpc): use real log-stretch ',logstretch
          do i=rng_smin,rng_smax
            do j=rng_fmin,rng_fmax
c          do i=1,data_nslo
c            do j=1,data_nfre
c              green(i,j,to)=log10(1.+logstretch*abs(green(i,j,from)))
              green(i,j,to)=log10(1.+logfac*abs(green(i,j,from)))/logstretch
            enddo
          enddo
        else
          stop 'ERROR (dat_dcpc): unknown data mode'
        endif
      else
        takefrom=from
        submode=datamode-4
        mpoly=.false.
        mpolyabs=.false.
        mpolyread=.false.
        mcontrast=.false.
        mabsolute=.false.
c        print *,'submode ',submode,mod(submode,2)
        if (mod(submode,2).ne.0) mabsolute=.true.
        submode=int(submode/2)
c        print *,'submode ',submode,mod(submode,2),mabsolute
        if (mod(submode,2).ne.0) mcontrast=.true.
        submode=int(submode/2)
c        print *,'submode ',submode,mod(submode,2),mcontrast
        if (mod(submode,2).ne.0) mpoly=.true.
        submode=int(submode/2)
c        print *,'submode ',submode,mod(submode,2),mpoly
        if (mod(submode,2).ne.0) mpolyabs=.true.
        submode=int(submode/2)
c        print *,'submode ',submode,mod(submode,2),mpolyabs
        if (mod(submode,2).ne.0) mpolyread=.true.
        submode=int(submode/2)
c        print *,'submode ',submode,mod(submode,2),mpolyread
c
c here we remove a polynomial trend for marine data
        if (mpoly) then
          if ((mpolyread).or.(from.eq.di_mcalc).or.(from.eq.di_mref)) then
            if (mpolyabs) then
c              print *,'poly abs'
              if (verb_substrategy) 
c              if (.true.) 
     &          print *,'NOTICE (dat_dcpc): remove absolute polynomial trend ',
     &          takefrom,' -> ',to
              nh=dble(rng_smax-rng_smin+1)/2.d0
              do n=0,max(0,min(6,polyord))
                msum=0.d0
                do j=rng_fmin,rng_fmax
                  sum=0.d0
                  do i=rng_smin,rng_smax
                    sum=sum+legendre(n,(dble(i-rng_smin)/nh-1.d0))*
     &                abs(green(i,j,takefrom))
                  enddo
                  sum=sum/nh
                  do i=rng_smin,rng_smax
                    green(i,j,to)=abs(green(i,j,takefrom))-
     &                          sum*legendre(n,(dble(i-rng_smin)/nh-1.d0))
                  enddo
                  msum=msum+sum
                enddo
                takefrom=to
                msum=msum/dble(data_nfre)
                if (verb_substrategy) 
c              if (.true.) 
     &            print *,'NOTICE (dat_dcpc): polynomial order: ',n,
     &                    ' mean coefficient: ',msum
              enddo
            else
c              print *,'poly real'
c              if (.true.) 
              if (verb_substrategy) 
     &          print *,'NOTICE (dat_dcpc): remove real polynomial trend ',
     &          takefrom,' -> ',to
              nh=dble(rng_smax-rng_smin+1)/2.d0
              do n=0,max(0,min(6,polyord))
                msum=0.d0
                do j=rng_fmin,rng_fmax
                  sum=0.d0
                  do i=rng_smin,rng_smax
                    a=real(green(i,j,takefrom))
                    sum=sum+legendre(n,(dble(i-rng_smin)/nh-1.d0))*a
                  enddo
                  sum=sum/nh
                  do i=rng_smin,rng_smax
                    a=real(green(i,j,takefrom))
                    x=a
                    b=imag(green(i,j,takefrom))
                    a=a-sum*legendre(n,(dble(i-rng_smin)/nh-1.d0))
                    green(i,j,to)=dcmplx(a,b)
c                    print *,i,j,x,a,b,green(i,j,to)
                  enddo
                  msum=msum+sum
                enddo
                takefrom=to
                msum=msum/dble(data_nfre)
c              if (.true.) 
                if (verb_substrategy) 
     &            print *,'NOTICE (dat_dcpc): polynomial order: ',n,
     &                    ' mean coefficient: ',msum
              enddo
            endif
c          else
c            print *,'NOPOLY',mpoly,mpolyabs,mpolyread
          endif
        endif
c 
c increase contrast
        if (mcontrast) then
          if (verb_substrategy) 
c              if (.true.) 
     &      print *,'NOTICE (dat_dcpc): increase contrast',
     &              ' with threshold (order: ',conord1,',',conord2,
     &              ' threshold: ',conthresh,')'
          do j=rng_fmin,rng_fmax
            maxamp=1.d-70
            do i=rng_smin,rng_smax
              a=abs(green(i,j,takefrom))
              maxamp=max(maxamp,a)
            enddo
            do i=rng_smin,rng_smax
              a=abs(green(i,j,takefrom))
              x=a/maxamp
              if (x.gt.conthresh) then
                x=(x-conthresh)/(1.d0-conthresh)
                x=f(conord2,x)*(1.d0-conthresh)+conthresh
              else
                x=-1.d0*(x-conthresh)/conthresh
                x=conthresh-f(conord1,x)*conthresh
              endif
              green(i,j,to)=maxamp*x*green(i,j,takefrom)/a
            enddo
          enddo
          takefrom=to
        endif
c take absolute value
        if (mabsolute) then
          if (verb_substrategy) 
c              if (.true.) 
     &      print *,'NOTICE (dat_dcpc): take absolute amplitude'
          do j=rng_fmin,rng_fmax
            do i=rng_smin,rng_smax
              green(i,j,to)=abs(green(i,j,takefrom))
            enddo
          enddo
          takefrom=to
        endif
      endif
c     
      if (verb_subaction) print *,'LEAVE dat_dcpc'
c
      return
      end

c----------------------------------------------------------------------
c legendre polynome

      double precision function legendre(n,x)
      integer n, nord
      double precision x
      double precision lnorm
      lnorm(nord)=sqrt((2.d0*dble(nord)+1)/2.d0)
      if (n.eq.0) then
        legendre=lnorm(0)
      elseif (n.eq.1) then
        legendre=lnorm(1)*x
      elseif (n.eq.2) then
        legendre=lnorm(2)*0.5d0*(3.d0*(x**2)-1.d0)
      elseif (n.eq.3) then
        legendre=lnorm(3)*0.5d0*(5.d0*(x**3)-3.d0*x)
      elseif (n.eq.4) then
        legendre=lnorm(4)*0.125d0*(35.d0*(x**4)-30.d0*(x**2)+3.d0)
      elseif (n.eq.5) then
        legendre=lnorm(5)*0.125d0*(63.d0*(x**5)-70.d0*(x**3)+15.d0*x)
      elseif (n.eq.6) then
        legendre=lnorm(6)*
     &           0.0625d0*(231.d0*(x**6)-315.d0*(x**4)+105.d0*(x**2)-5.d0)
      else
        stop 'illegal polynomial order!'
      endif
      return
      end
c
c
c ----- END OF dat_dcpc.f -----
