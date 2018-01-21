/*! \file suformat.cc
 * \brief format specific declarations for SeismicUnix (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 03/12/2010
 * 
 * format specific declarations for SeismicUnix (implementation)
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
 * REVISIONS and CHANGES 
 *  - 03/12/2010   V1.0   Thomas Forbriger
 *  - 21/01/2012   V1.1   provide option classes
 *  - 24/01/2012   V1.2   provide functions to evaluate output modifiers
 * 
 * ============================================================================
 */
#define DATRW_SUFORMAT_CC_VERSION \
  "DATRW_SUFORMAT_CC   V1.2"

#include <datrwxx/debug.h>
#include <datrwxx/su.h>
#include <datrwxx/suformat.h>
#include <datrwxx/formatmodifier.h>
#include <datrwxx/sucomanager.h>

namespace datrw {

  //@{
  /*! \brief Format properties
   * \ingroup group_su
   */
  const bool su::isbinary=true;
  const char* const su::streamID="su";
  //@}

  namespace su {

    namespace subformat {

      namespace key {
          
        const char* const forceseismic="seismic";
        const char* const forceultrasonic="ultrasonic";
        const char* const coodigits="digits";
        const char* const scalco="scalco";
        const char* const strict="strict";

      } // namespace key

      /*----------------------------------------------------------------------*/

      namespace def {

        const unsigned int coodigits=4;
        const short scalco=-100;
        const double thresholdzero=1.e-12;
        const double thresholdcmp=1.e-4;

      } // namespace def

    } // namespace subformat

    /*======================================================================*/

    bool options::TemporalSampling::isconsistent() const
    {
      return (!(this->forceultrasonic && this->forceseismic));
    } // bool options::TemporalSampling::isconsistent() const

    /*----------------------------------------------------------------------*/

    /*! \brief evaluate input stream format modifiers
     * \ingroup group_su
     */
    options::SUHeaderControl inputmodifiers(const std::string& modifier,
                                            const bool& debug)
    {
      options::SUHeaderControl retval;
      datrw::Subformat subformat(modifier);
      retval.spatialsampling.bestrict
        =subformat.isset(datrw::su::subformat::key::strict);
      DATRW_assert_modifiers_are_recognized(subformat, 
                                            "su::inputmodifiers()");
      DATRW_debug(debug,
                  "su::inputmodifiers()",
                  DATRW_value(retval.temporalsampling.forceultrasonic)
                  << "\n" <<
                  DATRW_value(retval.temporalsampling.forceseismic)
                  << "\n" <<
                  DATRW_value(retval.spatialsampling.bestrict)
                  << "\n" <<
                  DATRW_value(retval.spatialsampling.scalco)
                  << "\n" <<
                  DATRW_value(retval.spatialsampling.coodigits));
      return(retval);
    } // options::SUHeaderControl inputmodifiers(const std::string& mf)

    /*----------------------------------------------------------------------*/

    /*! \brief evaluate output stream format modifiers
     * \ingroup group_su
     */
    options::SUHeaderControl outputmodifiers(const std::string& modifier,
                                             const bool& debug)
    {
      options::SUHeaderControl retval;
      datrw::Subformat subformat(modifier);
      retval.temporalsampling.forceultrasonic
        =subformat.isset(datrw::su::subformat::key::forceultrasonic);
      retval.temporalsampling.forceseismic
        =subformat.isset(datrw::su::subformat::key::forceseismic);
      if (subformat.isset(datrw::su::subformat::key::coodigits))
      {
        subformat(datrw::su::subformat::key::coodigits) 
          >> retval.spatialsampling.coodigits;
      }
      if (subformat.isset(datrw::su::subformat::key::scalco))
      {
        subformat(datrw::su::subformat::key::scalco) 
          >> retval.spatialsampling.scalco;
      }
      datrw::su::fixscalevalue(retval.spatialsampling.scalco);
      DATRW_assert(retval.temporalsampling.isconsistent(),
                   "su::outputmodifiers(): "
                   "used format modifiers are in conflict");
      DATRW_assert_modifiers_are_recognized(subformat, 
                                            "su::outputmodifiers()");
      DATRW_debug(debug,
                  "su::outputmodifiers()",
                  DATRW_value(retval.temporalsampling.forceultrasonic)
                  << "\n" <<
                  DATRW_value(retval.temporalsampling.forceseismic)
                  << "\n" <<
                  DATRW_value(retval.spatialsampling.bestrict)
                  << "\n" <<
                  DATRW_value(retval.spatialsampling.scalco)
                  << "\n" <<
                  DATRW_value(retval.spatialsampling.coodigits));
      return(retval);
    } // options::SUHeaderControl outputmodifiers(const std::string& mf)

    /*----------------------------------------------------------------------*/


  } // namespace su

} // namespace datrw

/* ----- END OF suformat.cc ----- */
