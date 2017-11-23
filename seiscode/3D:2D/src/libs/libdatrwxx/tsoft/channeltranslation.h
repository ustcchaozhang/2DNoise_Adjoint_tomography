/*! \file channeltranslation.h
 * \brief translate TSOFT channel name (they are too long for SFF headers) (prototypes)
 * \ingroup group_tsoft
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 11/11/2009
 * 
 * translate TSOFT channel name (they are too long for SFF headers) (prototypes)
 * 
 * Copyright (c) 2009 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 11/11/2009   V1.0   Thomas Forbriger
 *  - 10/01/2013   V1.1   add sensor data conversion codes for SG056
 *                        extract full channel description on request
 * 
 * ============================================================================
 */

#include<iostream>
#include<string>

// include guard
#ifndef DATRW_CHANNELTRANSLATION_H_VERSION

#define DATRW_CHANNELTRANSLATION_H_VERSION \
  "DATRW_CHANNELTRANSLATION_H   V1.1"

namespace datrw {

  namespace tsoft {

    /*! \brief DDAS3 sensor data conversion codes
     * \ingroup group_tsoft
     * Sensor data conversion codes as defined in GWR document
     * DDAS3_man_technical_ch5_UIPCsoftware_2010-07-16_BFO-Excerpt
     */
    enum Econversion {
      SIDIODE, //!< 
      SIDIODE1, //!< 
      SIDIODE2, //!< 
      SIDIODE3, //!< Convert voltage across Standard Silicone Diode to Kelvins
      SIDIODE4, //!< Convert voltage across Non-Magnetic Silicone Diode to Kelvins
      MASFLO1,  //!< 
      GEPLHe19, //!< Convert voltage across 19” AMI LHe sensor to % Helium
      GEPLHe23, //!< Convert voltage across 23” AMI LHe sensor to % Helium
      POWERV,   //!< Convert Voltage to % Power where
      PT100_K,  //!< 
      PT100V_K, //!< Convert Voltage across Pt100 RTD to Kelvins
      PT100,    //!< 
      PT100V,    //!< 
      PSI_1_1,  //!< Convert Current from Dewar Pressure gauge to PSI
      H2OFLO1,  //!< 
      TDV,      //!< Convert Voltage across Honeywell TD5 temperature sensor to degrees Celsius
      TD,       //!< 
      PSI_500,  //!< Convert Current from Gas Regulator and Helium Compressor Pressure gauge to PSI
      PSI_2500, //!< Convert Current from Gas Regulator and Helium Compressor Pressure gauge to PSI
      PSI_3000, //!< Convert Current from Gas Regulator and Helium Compressor Pressure gauge to PSI
      PSI_3447, //!< 
      KPA_3447, //!< Convert Current from Gas Regulator and Helium Compressor Pressure gauge to KPa
      KPA_20684,//!< Convert Current from Gas Regulator and Helium Compressor Pressure gauge to KPa
      KPA_17236,//!< Convert Current from Gas Regulator and Helium Compressor Pressure gauge to KPa
      MPA_17236,//!< Convert Current from Gas Regulator and Helium Compressor Pressure gauge to MPa
      MPA_3447, //!< Convert Current from Gas Regulator and Helium Compressor Pressure gauge to MPa
      MPA_20684,//!< Convert Current from Gas Regulator and Helium Compressor Pressure gauge to MPa
      FAN16,    //!< Convert Voltage to % Fan speed
      C20P4,    //!< Conversion factor 20.4 for Dewar Heater current
      CNSP,     //!< not specified
      CNFD      //!< not found in channel translation table
    }; // enum Econversion

    /*----------------------------------------------------------------------*/

    /*! \brief struct to hold a pair of channel names
     * \ingroup group_tsoft
     */
    struct Channel {
      //! \brief channel description used in TSOFT
      const char* TSOFTname;
      //! \brief description to be used in SFF for this channel
      const char* SFFname;
      //! \brief conversion code for auxilliary channels
      const Econversion ConversionCode;
    }; // struct Channel

    /*----------------------------------------------------------------------*/

    /*! \brief hold TSOFT channel description
     * \ingroup group_tsoft
     */
    struct TSOFTchannelid
    {
      std::string location, instrument, datatype;
    }; // struct TSOFTchannelid

    /*----------------------------------------------------------------------*/

    /*! \brief hold SFF channel description
     * \ingroup group_tsoft
     */
    struct SFFchannelid
    {
      std::string station, channel, instrument, auxid;
    }; // struct SFFchannelid

    /*----------------------------------------------------------------------*/

    /*! \brief hold channel description
     * \ingroup group_tsoft
     */
    struct ChannelDescription
    {
      TSOFTchannelid tid;
      SFFchannelid sid;
      Econversion cc;
    }; // struct ChannelDescription

    /*----------------------------------------------------------------------*/

    /*! \brief global variable: channel name translation table
     * \ingroup group_tsoft
     */
    extern const Channel translationtable[];

    /*======================================================================*/
    // functions
      
    /*! \brief split TSOFT channel description
     * \ingroup group_tsoft
     */
    TSOFTchannelid tchannelid(const Channel& ci);

    /*----------------------------------------------------------------------*/
      
    /*! \brief split SFF channel description
     * \ingroup group_tsoft
     */
    SFFchannelid schannelid(const Channel& ci);

    /*----------------------------------------------------------------------*/

    /*! \brief report translation table
     * \ingroup group_tsoft
     */
    void reporttranslation(std::ostream& os);

    /*----------------------------------------------------------------------*/

    /*! \brief report comments from 1s files
     * \ingroup group_tsoft
     */
    void reportdatacomments(std::ostream& os);

    /*----------------------------------------------------------------------*/

    /*! \brief check uniqueness of translation table
     * \ingroup group_tsoft
     */
    bool translationisunique(const bool& verbose=true);

    /*----------------------------------------------------------------------*/

    /*! \brief return SFF channel ID for given TSOFT channel ID
     * \ingroup group_tsoft
     */
    SFFchannelid translate(const TSOFTchannelid& ci);

    /*----------------------------------------------------------------------*/

    /*! \brief return channel data for given SFF channel ID
     * \ingroup group_tsoft
     */
    ChannelDescription channel(const SFFchannelid& ci);

  } // namespace tsoft

} // namespace datrw

#endif // DATRW_CHANNELTRANSLATION_H_VERSION (includeguard)

/* ----- END OF channeltranslation.h ----- */
