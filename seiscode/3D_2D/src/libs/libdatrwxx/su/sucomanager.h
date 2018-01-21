/*! \file sucomanager.h
 * \brief manage coordinate scaling (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/12/2010
 * 
 * manage coordinate scaling (prototypes)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 * 
 * REVISIONS and CHANGES 
 *  - 06/12/2010   V1.0   Thomas Forbriger
 *  - 22/01/2012   V1.1   
 *                        - handle control parameters
 *                        - Coordinates and ScalCoo are prepared to receive
 *                          control parameters
 *                        - add conversion functions
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_SUCOMANAGER_H_VERSION

#define DATRW_SUCOMANAGER_H_VERSION \
  "DATRW_SUCOMANAGER_H   V1.1"

#include<datrwxx/suheaderstruct.h>
#include<datrwxx/suformat.h>

namespace datrw {

  namespace su {

    /*======================================================================*/
    // conversion functions

    /*! \brief convert a decimal power to a SeismicUn*x scale value
     * \ingroup group_su
     *
     * \param p exponent for the power of ten
     * \return corresponding SeismicUn*x scale value like scalco or scalel
     */
    short powertoscale(const int& p);

    /*----------------------------------------------------------------------*/

    /*! \brief convert a SeismicUn*x scale value to a decimal power
     * \ingroup group_su
     *
     * \param s SeismicUn*x scale value like scalco or scalel
     * \param strict if true: abort if scale does not match original
     *               definition
     * \return corresponding exponent for the power of ten
     */
    int scaletopower(short s, const bool& strict=true);

    /*----------------------------------------------------------------------*/

    /*! \brief fix a SeismicUn*x scale value 
     * \ingroup group_su
     *
     * some code out there produces SU data with scale values understood
     * as exponents of ten; SEGY defines the scale value to be taken as
     * numerator or denominator of the scaling factor with values -10000,
     * -1000, -100, -10, 1, 10, 100, 1000, 10000 only; here we allow for
     * the non-standard setting indicated by the absolute value of scale
     * being larger than 1 and smaller than 10
     *
     * \param s scale value to be fixed (input and output parameter)
     * \param strict if true: abort if scale does not match original definition
     */
    void fixscalevalue(short& s, const bool& strict=true);

    /*----------------------------------------------------------------------*/

    /*! \brief convert scale value to a factor to be applied
     * \ingroup group_su
     *
     * \param s SeismicUn*x scale value like scalco or scalel
     * \param strict if true: abort if scale does not match original
     *               definition
     * \return factor to be multiplied with actual coordinate value
     */
    double scalefactor(short s, const bool& strict=true);

    /*======================================================================*/

    /*! \brief scaled coordinate.
     * \ingroup group_su
     *
     * This struct holds one coordinate together with a scale value.
     * It provides functions to support appropriate scaling and to explore
     * dynamic range.
     *
     * This class is used insed datrw::su::Coordinates to scale coordinates
     * coherently, since SU provides only scaling factors for all horizontal
     * coordinates on one hand and all vertical cooridnates on the other hand
     * together.
     * The functions inside this class are primarily required upon writing SU
     * data.
     *
     * \sa Coordinates
     */
    struct ScalCoo {
      //! constructor
      ScalCoo(const datrw::su::options::SpatialSampling& ctrl,
              const bool& debug=false)
        : scale(ctrl.scalco), coo(0), Mcontrol(ctrl), Mdebug(debug) 
      { 
        fixscalevalue(this->scale, Mcontrol.bestrict);
      }

      /*! \brief lower limit of values
       *
       * Coordinate values smaller than this will be regarded as zero.
       * They cannot be represented, since the scaling limits the exponential
       * factor to the range 1.e-4 to 1.e+4. It is unsafe to handle these
       * values with the standard scaling algorithm, since this requires to
       * take the logarithm of the coordinate value, which will approach
       * infinity for coordinates approaching zero.
       */
      static double effectivezero;

      /*! \brief maximum number of significant digits to be used
       *
       * Floating point number representation and conversion easily leads to
       * cases where 0.01 becomes 0.00999999977648 which is not intended.
       * In such cases we will round to the nearest value.
       */
      static int maxnsigdigits;

      //! set from header fields
      void set(const short& s, const int& c);
      //! set from coordinate value in meters
      void set(const double& v);
      //! return decimal power of scaling factor
      int power() const;
      //! scale to given scaling factor as defined by decimal power
      void scaletopower(const int& p);
      //! smallest possible power larger or equal desired
      int smallestpower(const short& desiredscale 
                        = datrw::su::subformat::def::scalco) const;
      //! return coordinate value
      double value() const;
      //! adjust scale to optimal representation of significant digits
      void adjustscale();

      //! scale like scalco
      short scale;
      //! coordinate
      int coo;
      //! control parameters
      datrw::su::options::SpatialSampling Mcontrol;
      //! debug mode
      bool Mdebug;
    }; // struct ScalCoo

    /*----------------------------------------------------------------------*/

    /*! \brief full set of coordinates.
     * \ingroup group_su
     *
     * This struct holds a full set of coordinates for a SEG-Y trace header.
     * It provides functions to read the values from a given trace header and
     * to set values in a trace header. Further it provides a function to
     * chose equal scaling values for horizontal coordinates on one hand and
     * vertical coordinates on the other hand.
     * This class combines six members of type datrw::su::ScalCoo in order to
     * scale coordinates coherently.
     * It is used in the set functions of datrw::su::SUheader not as a member
     * data but as a conversion and scaling tool.
     *
     * \sa ScalCoo, SUheader.set()
     */
    struct Coordinates {
      //! constructor
      Coordinates(const datrw::su::options::SpatialSampling& ctrl,
                  const bool& debug=false)
        : sx(ctrl, debug), sy(ctrl, debug), gx(ctrl, debug), gy(ctrl, debug),
        sdepth(ctrl, debug), gelev(ctrl, debug), 
        Mcontrol(ctrl), Mdebug(debug)
      { }
      //! read values from SU header
      void getvaluesfrom(const TraceHeaderStruct& h);
      //! set values in SU header
      void setvaluesin(TraceHeaderStruct& h);
      //! equalize scaling
      void equalizescaling();

      //! source x coordinate
      ScalCoo sx;
      //! source y coordinate
      ScalCoo sy;
      //! receiver x coordinate
      ScalCoo gx;
      //! receiver y coordinate
      ScalCoo gy;
      //! source z coordinate
      ScalCoo sdepth;
      //! source y coordinate
      ScalCoo gelev;
      //! control parameters
      datrw::su::options::SpatialSampling Mcontrol;
      //! debug mode
      bool Mdebug;
    }; // struct Coordinates

  } // namespace su

} // namespace datrw

#endif // DATRW_SUCOMANAGER_H_VERSION (includeguard)

/* ----- END OF sucomanager.h ----- */
