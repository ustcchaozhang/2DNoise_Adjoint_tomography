/*! \file datatypes.h
 * \brief handle data types and data type conversion (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/12/2010
 * 
 * handle data types and data type conversion (prototypes)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
 *
 * This pair of source files is intended to support any information about
 * actual data types handled internally and in files. Are data stored and
 * passed as int or double? Is data file format able to maintain double
 * precision or integer accuracy?
 *
 * Part of this (Edatatype) still is located in formats.h and should be moved
 * to this file in the future.
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
 *  - 17/12/2010   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_DATATYPES_H_VERSION

#define DATRW_DATATYPES_H_VERSION \
  "DATRW_DATATYPES_H   V1.0"

#include<iostream>

namespace datrw {

  namespace datatypes {

    /*! global flag, indicated whether type conversion should be reported.
     */
    extern bool verbose_type_conversion;

    //! indicate unkown type
    extern const char* unknown_type_id;

    /*======================================================================*/

    //! actually print report
    void print_conversion_report(std::ostream& os, 
                                 const std::string& from,
                                 const std::string& to);

    template<typename C>
      inline std::string type_id() { return(unknown_type_id); }

    template<> inline std::string type_id<float>() { return("float"); }
    template<> inline std::string type_id<double>() { return("double"); }
    template<> inline std::string type_id<int>() { return("int"); }

  } // namespace datatypes

  /*----------------------------------------------------------------------*/

  //! switch on type conversion verbosity
  inline void report_type_conversion()
  {
    ::datrw::datatypes::verbose_type_conversion=true;
  } // void report_type_conversion()

  /*----------------------------------------------------------------------*/

  //! switch off type conversion verbosity
  inline void dont_report_type_conversion()
  {
    ::datrw::datatypes::verbose_type_conversion=false;
  } // void dont_report_type_conversion()

  /*----------------------------------------------------------------------*/

  /*! call this function template to report a type conversion
   *
   * \param from  type of source
   * \param to    type of destination
   * \param os    C++ output stream to write to
   */
  template<typename from, typename to>
    inline void report_conversion(std::ostream& os)
    {
      std::string typeid1=::datrw::datatypes::type_id<from>();
      std::string typeid2=::datrw::datatypes::type_id<to>();
      ::datrw::datatypes::print_conversion_report(os, typeid1, typeid2);
    } // void report_conversion(std::ostream& os)

  /*----------------------------------------------------------------------*/

  /*! call this function to check whether two types equal
   *
   * \param t1  first type
   * \param t2  second type
   * \return true if t1 equals t2
   */
  template<typename t1, typename t2>
    inline bool types_are_equal()
    {
      bool retval;
      std::string typeid1=::datrw::datatypes::type_id<t1>();
      std::string typeid2=::datrw::datatypes::type_id<t2>();
      retval=(typeid1==typeid2);
      return(retval);
    } // bool types_are_equal()

  /*======================================================================*/

} // namespace datrw

#endif // DATRW_DATATYPES_H_VERSION (includeguard)

/* ----- END OF datatypes.h ----- */
