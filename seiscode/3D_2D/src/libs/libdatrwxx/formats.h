/*! \file formats.h
 * \brief common description of formats (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 26/11/2010
 * 
 * common description of formats (prototypes)
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
 *  - 26/11/2010   V1.0   Thomas Forbriger
 *  - 02/09/2011   V1.1   support seife format
 *  - 03/11/2011   V1.2   added data formats ASCII, binary, and ThiesDL1
 *  - 05/12/2011   V1.3   provide format specfic online help 
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_FORMATS_H_VERSION

#define DATRW_FORMATS_H_VERSION \
  "DATRW_FORMATS_H   V1.3"

#include <iostream>
#include <string>

namespace datrw {

  /*! ID flags for supported file formats
   *
   * These flags are used for input as well as for output streams.
   * Not all of them will be supported by input and output streams as well.
   */
  enum Eformat {
    Fsff,     //<! SFF data
    Fpdas,    //<! PDAS data
    Fhpmo,    //<! BFO HP-MO data (Grossmann format)
    Fmseed,   //<! MiniSEED data
    Fbonjer,  //<! Format specified by K. Bonjer
    Fsac,     //<! SAC binary data
    Fgse,     //<! raw GSE data
    Ftsoft,   //<! TSOFT data
    Ftfascii, //<! ASCII format of T. Forbrigers any2ascii
    Fsu,      //<! SeismicUn*x format
    Fseife,   //<! seife format
    Fascii,   //<! raw ASCII format
    Fbinary,  //<! raw binary format
    Fthiesdl1 //<! Thies DL1 pluviometer data format
  }; // enum Eformat

  /*----------------------------------------------------------------------*/

  /*! ID flags to indicate variable type used for storing samples
   *
   * \todo
   * move this to datatypes.h
   *
   * \deprecated
   * The use of Edatatype flags is deprecated and will be replaced by
   * a more verbose class or struct which can be extended in the future
   *
   * \sa \ref page_properties
   */
  enum Edatatype {
    Fint,    //<! File format uses integer data
    Ffloat,  //<! File format uses float data
    Fdouble, //<! File format uses double data
    Fall     //<! File format is able to store any data type
  }; // enum Edatatype

  /*----------------------------------------------------------------------*/

  //! true, if underlying file format is binary
  bool isbinary(const Eformat& format);

  /*! \brief convert identifier from and to string representation
   * @{
   */
  Eformat anyID(std::string identifier);
  std::string anyID(const Eformat& id);
  /**@}*/

  // print out information about supported data types
  void supported_data_types(std::ostream& os=std::cout);

  // print out information about supported data types for input
  void supported_input_data_types(std::ostream& os=std::cout);

  // print out information about supported data types for output
  void supported_output_data_types(std::ostream& os=std::cout);

  // print out information about data conversion
  void online_help(std::ostream& os=std::cout);

  // print out information about data conversion for on specific format
  void online_help(const Eformat& format,
                   std::ostream& os=std::cout,
                   const bool& modifierhelp=false);

  // print out information about data conversion for on specific format
  void online_help(const std::string& format,
                   std::ostream& os=std::cout,
                   const bool& modifierhelp=false);

} // namespace datrw

#endif // DATRW_FORMATS_H_VERSION (includeguard)

/* ----- END OF formats.h ----- */
