/*! \file affpgplot.h
 * \brief pgplot functions supporting aff containers (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/04/2005
 * 
 * pgplot functions supporting aff containers (prototypes)
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
 *  - 28/04/2005   V1.0   Thomas Forbriger
 *  - 19/06/2006   V1.1   
 *                        - provide series function
 *                        - added line drawing functionality
 *  - 15/03/2011   V1.2
 *                        - implemented function pt
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_AFFPGPLOT_H_VERSION

#define TF_AFFPGPLOT_H_VERSION \
  "TF_AFFPGPLOT_H   V1.2"

#include<pgplotxx/pgplotxx.h>
#include<aff/array.h>
#include<aff/series.h>
#include<aff/iterator.h>

namespace pgplot {

/*! \brief Interface provided through affpgplot.h
 *
 * \defgroup affpgplot_h Interface provided through affpgplot.h
 */
/*@{*/

  //! here we provide support for aff containers
  namespace pgaff {

    //! transform matrix for gray shade plot function
    typedef aff::SimpleRigidArray<float, 6> Ttransform;

    //! array type to be passed to gray function
    typedef aff::Array<float> Tarray;
    typedef aff::Series<float> Tseries;

    //! function to plot gray image
    pgplot::basic_device& gray(pgplot::basic_device& dev,
                               const Tarray& a,
                               const pgplot::Trange& range,
                               const Ttransform& tr);
    
    //! function to plot gray image
    pgplot::basic_device& gray(pgplot::basic_device& dev,
                               const Tarray& a,
                               const pgplot::Trange& range,
                               const bool& xisfirst=true);
    
    //! function to plot gray image
    pgplot::basic_device& gray(pgplot::basic_device& dev,
                               const Tarray& a,
                               const bool& xisfirst=true);

    //! create standard transform array
    Ttransform create_transform(const Tarray::Tcoc& a,
                                const pgplot::Trect& rect,
                                const bool& xisfirst=true);

    //! return range of values
    Trange array_value_range(const Tarray::Tcoc& a);
    Trange series_value_range(const Tseries::Tcoc& a);

    /*----------------------------------------------------------------------*/

    /*! \brief draw an open polygon from a dense series
     *
     * This implements \c cpgline functionality.
     *
     * \param dev device to be plotted on
     * \param x series of x-coordinates
     * \param y corresponding series of y-coordinates
     *
     * \param SX series class for x
     * \param SY series class for y
     * \param T pgplot device class
     *
     * \return returns a reference to the device context
     */
    template<class T, class SX, class SY>
    T& line(T& dev, const SX& x, const SY& y)
    {
      typename aff::Browser<SX> ix=x;
      typename aff::Browser<SY> iy=y;
      float fx=float(*ix);
      float fy=float(*iy);
      dev.move(fx, fy);
      while ((ix.valid()) && (iy.valid()))
      { fx=float(*ix); fy=float(*iy); dev.draw(fx, fy); ++ix; ++iy; }
      return(dev);
    }

    /*----------------------------------------------------------------------*/

    /*! \brief error bar directions
     *
     */
    enum Eerrdir {
      //! +x (x to x+e)
      Fdirright=1,
      //! +y (y to y+e)
      Fdirtop=2,
      //! -x (x to x-e)
      Fdirleft=3,
      //! -y (y to y-e)
      Fdirbottom=4,
      //! +/-x (x-e to x+e)
      Fdirhor=5,
      //! +/-y (y-e to y+e)
      Fdirver=6
    };

    /*----------------------------------------------------------------------*/

    /*! \brief draw error bars from series
     *
     * This implements \c cpgerrb functionality.
     *
     * \param dev device to be plotted on
     * \param x series of x-coordinates
     * \param y corresponding series of y-coordinates
     * \param e corresponding series of error bars
     * \param t length of terminals to be drawn
     *
     * \param SX series class for x
     * \param SY series class for y
     * \param SE series class for e
     * \param T pgplot device class
     *
     * \return returns a reference to the device context
     */
    template<class T, class SX, class SY, class SE>
    T& errb(T& dev, const SX& x, const SY& y, const SE& e, 
                   const Eerrdir& dir=Fdirver, const float& t=1.)
    {
      typename aff::Browser<SX> ix=x;
      typename aff::Browser<SY> iy=y;
      typename aff::Browser<SE> ie=e;
      float fx=float(*ix);
      float fy=float(*iy);
      float fe=float(*ie);
      int idir=int(dir);
      while ((ix.valid()) && (iy.valid()) && (ie.valid()))
      { fx=float(*ix); fy=float(*iy); fe=float(*ie);
        dev.err1(idir, fx, fy, fe, t); ++ix; ++iy; ++ie; }
      return(dev);
    }

    /*----------------------------------------------------------------------*/

    /*! \brief draw symbols from series
     *
     * This implements \c cpgpt functionality.
     *
     * \param dev device to be plotted on
     * \param x series of x-coordinates
     * \param y corresponding series of y-coordinates
     * \param s type of symbol
     *
     * \param SX series class for x
     * \param SY series class for y
     * \param T pgplot device class
     *
     * \return returns a reference to the device context
     */
    template<class T, class SX, class SY>
    T& pt(T& dev, const SX& x, const SY& y, const int& s)
    {
      typename aff::Browser<SX> ix=x;
      typename aff::Browser<SY> iy=y;
      float fx=float(*ix);
      float fy=float(*iy);
      while ((ix.valid()) && (iy.valid()))
      { fx=float(*ix); fy=float(*iy);
        dev.pt1(fx, fy, s); ++ix; ++iy; }
      return(dev);
    } // T& pt(T& dev, const SX& x, const SY& y, const int& s)

  } // namespace pgaff

/*@}*/

} // namespace pgplot

#endif // TF_AFFPGPLOT_H_VERSION (includeguard)

/* ----- END OF affpgplot.h ----- */
