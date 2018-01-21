/*! \file affpgplot.cc
 * \brief code to be used together with aff containers (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 29/04/2005
 * 
 * code to be used together with aff containers (implementation)
 * 
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version. 
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 * ----
 * 
 * REVISIONS and CHANGES 
 *  - 29/04/2005   V1.0   Thomas Forbriger
 *  - 19/06/2006   V1.1   provide series function
 *  - 18/01/2011   V1.2   Trange member functions were renamed
 * 
 * ============================================================================
 */
#define TF_AFFPGPLOT_CC_VERSION \
  "TF_AFFPGPLOT_CC   V1.1"
#define TF_AFFPGPLOT_CC_CVSID \
  "$Id$"

#include<iostream>
#include<pgplotxx/affpgplot.h>
#include<aff/fortranshape.h>
#include<aff/lib/collector.h>

namespace pgplot {

  //! here we provide support for aff containers
  namespace pgaff {
       
    //! function to plot gray image
    pgplot::basic_device& gray(pgplot::basic_device& dev,
                               const Tarray& a,
                               const pgplot::Trange& range,
                               const Ttransform& tr)
      {
        aff::FortranArray<Tarray> fa(a, true);
        float* pa=fa.castedpointer<float>();
        int f1=fa.first(0);
        int f2=fa.first(1);
        int l1=fa.last(0);
        int l2=fa.last(1);
        int d1=fa.dimlast(0);
        int d2=fa.dimlast(1);
        dev.gray(pa, d1, d2, f1, l1, f2, l2, 
                 range.min, range.max,
                 tr.pointer());
        return(dev);
      }

    /*----------------------------------------------------------------------*/
       
    //! function to plot gray image
    pgplot::basic_device& gray(pgplot::basic_device& dev,
                               const Tarray& a,
                               const pgplot::Trange& range,
                               const bool& xisfirst)
      {
        Trect win;
        dev.qwin(win);
        Ttransform tr=create_transform(a, win, xisfirst);
        gray(dev,a,range,tr);
        return(dev);
      }

    /*----------------------------------------------------------------------*/
       
    //! function to plot gray image
    pgplot::basic_device& gray(pgplot::basic_device& dev,
                               const Tarray& a,
                               const bool& xisfirst)
      {
        Trect win;
        dev.qwin(win);
        Trange vrange=array_value_range(a);
        gray(dev,a,vrange,xisfirst);
        return(dev);
      }

    /*----------------------------------------------------------------------*/

    Ttransform create_transform(const Tarray::Tcoc& a,
                                const pgplot::Trect& rect,
                                const bool& xisfirst)
    {
      Ttransform retval;
      Tarray::Tshape shape(a.shape());
      shape.setfirst(Tarray::Tshape::TIndexVec(1));
      retval=0.;
      int ix,iy,ibx,iby,nx,ny,fx,fy;
      ibx=0;
      iby=3; 
      if (xisfirst) 
      {
        ix=1;
        iy=5;
        nx=shape.size(0);
        ny=shape.size(1);
        fx=shape.first(0);
        fy=shape.first(1);
      }
      else
      {
        ix=2; 
        iy=4; 
        nx=shape.size(1);
        ny=shape.size(0);
        fx=shape.first(1);
        fy=shape.first(0);
      }
      Trange xrange=rect.x;
      Trange yrange=rect.y;
      retval[ix]=xrange.fullrange()/float(nx);
      retval[iy]=yrange.fullrange()/float(ny);
      retval[ibx]=xrange.min-retval[ix]*(float(fx)-0.5);
      retval[iby]=yrange.min-retval[iy]*(float(fy)-0.5);
      return(retval);
    }

    /*----------------------------------------------------------------------*/

    namespace util
    {

      template<class C>
        class Extractrange {
          typedef typename C::Tcoc Tcont;
          typedef typename C::Tvalue Tvalue;
          public:
            typedef Trange Tretval;
            //! initialize member data
            Extractrange(const Tcont& c): Mrange(c(c.first()),c(c.first())) { }
            //! collect another value
            void operator() (const Tvalue& v)
            { Mrange.extend(Trange(v,v)); }
            //! return result of operation
            Tretval result() const { return(Mrange); }
          private:
            Trange Mrange;
        }; // class Extractrange

    } // namespace util

    Trange array_value_range(const Tarray::Tcoc& a)
    {
      return(aff::func::util::collect<Tarray::Tcoc, util::Extractrange>(a));
    } // array_value_range

    Trange series_value_range(const Tseries::Tcoc& a)
    {
      return(aff::func::util::collect<Tseries::Tcoc, util::Extractrange>(a));
    } // array_value_range

  } // namespace pgaff

} // namespace pgplot
/* ----- END OF affpgplot.cc ----- */
