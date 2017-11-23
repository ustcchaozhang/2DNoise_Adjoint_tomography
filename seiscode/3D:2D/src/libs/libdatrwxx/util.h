/*! \file util.h
 * \brief utilities used by more than one type of data reader (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 22/12/2004
 * 
 * utilities used by more than one type of data reader (prototypes)
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 22/12/2004   V1.0   Thomas Forbriger
 *  - 29/07/2011   V1.1   added clipstring
 *  - 02/09/2011   V1.2   remove DATRW_debug from here, this macro is
 *                        presented in debug.h already
 *  - 06/09/2011   V1.3   moved string manipulation functions to this file:
 *                        commatospace and trimws
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_UTIL_H_VERSION

#define DATRW_UTIL_H_VERSION \
  "DATRW_UTIL_H   V1.3"

#include<iostream>
#include<string>
#include<datrwxx/error.h>
#include<datrwxx/datatypes.h>
#include<aff/iterator.h>

namespace datrw {

/*! \defgroup group_util Internal utilities: Miscellaneous utility functions
 */

  /*! \brief Some components used by several I/O format modules
   * \ingroup group_util
   */
  namespace util {

    /*----------------------------------------------------------------------*/
    /*! \brief function template to convert a whole series
     * \ingroup group_util
     *
     * \param Cin series container type of the input series
     *            like datrw::Tdseries
     * \param Cout series container type of the output series
     *            like datrw::Tfseries
     * \param data actual input series container
     * \return series container with converted data
     */
    template<class Cin, class Cout>
    Cout convert(const typename Cin::Tcoc& data)
    {
      int nsamples=data.size();
      Cout retval(nsamples);
      typedef typename Cin::Tvalue Tinvalue;
      typedef typename Cout::Tvalue Toutvalue;
      aff::Iterator<Cout> IO(retval);
      aff::Browser<Cin> BI(data);
      ::datrw::report_conversion<Tinvalue, Toutvalue>(std::cout);
      while (IO.valid() && BI.valid()) 
      { 
        Tinvalue sample=*BI;
        *IO=static_cast<Toutvalue>(sample); 
        ++IO;
        ++BI;
      }
      return(retval);
    } // template function convert

    /*----------------------------------------------------------------------*/

    /*! \brief two-argument conversion function which allows template function
     * specialization
     * \ingroup group_util
     *
     * \param Cin series container type of the input series
     *            like datrw::Tdseries
     * \param Cout series container type of the output series
     *            like datrw::Tfseries
     * \param data actual input series container
     * \param outdata container with converted data
     */
    template<class Cin, class Cout>
    void convert(const Cin& data,
                 Cout& outdata)
    {
      outdata=convert<Cin, Cout>(data);
    } // template function convert

    /*----------------------------------------------------------------------*/

    /*! \brief specialization of function in case where no conversion is
     * required
     * \ingroup group_util
     *
     * \param C series container type of the input and output series
     *            like datrw::Tdseries
     * \param data actual input series container
     * \param outdata container with copied reference to data
     */
    template<class C>
    void convert(const C& data,
                 C& outdata)
    {
      outdata=data;
    } // template function convert

    /*----------------------------------------------------------------------*/

    /*! \brief read the trace data
     * \ingroup group_util
        \param is the input stream
        \param nsamples the number of samples
        \param streamname name of the stream for Exceptions
        \return the trace
    */
    template<class C>
    C readasciidouble(std::istream& is, const int nsamples,
      const std::string& streamname) 
    {
      std::string errorstr = "ERROR ("+streamname+
        "::?series): number of samples is not positive!";
      DATRW_assert(nsamples>0, errorstr.c_str());
      C series(nsamples);
      double inval;
      errorstr = "ERROR ("+streamname+"::?series): bad stream!";
      ::datrw::report_conversion<double, typename C::Tvalue>(std::cout);
      for (int i = 0; i < nsamples; ++i) 
      {
        DATRW_assert(is.good(), errorstr.c_str());
        is >> inval;
        series(i) = static_cast<typename C::Tvalue>(inval);
      }
      return series;
    } // template function readascii

    /*======================================================================*/

    /*! \brief return number of significant digits
     * \ingroup group_util
     */
    int nsignificantdigits(double v, const bool& debug=false);

    /*! \brief return number of trailing digits (after decimal point)
     * \ingroup group_util
     */
    int ntrailingdigits(double v, const bool& debug=false);

    /*======================================================================*/

    /*! \brief strip substring
     * \ingroup group_util
     *
     * Strips off first substring up to given delimiter.
     * The string is passed as a reference and will be modified (i.e. the
     * stripped substring as well as the delimiter will be erased).
     *
     * \param s input string
     * \param delim delimiter to look for
     * \return first part of string up to delimiter
     */
    std::string clipstring(std::string& s, const std::string& delim=":");

    /*----------------------------------------------------------------------*/

    /*! replace comma by whitespace
     * \ingroup group_util
     *
     * \param s input string
     * \return input string with all commas replaced by whitespace
     */
    std::string commatospace(std::string s);

    /*----------------------------------------------------------------------*/

    /*! \brief remove leading and trailing whitespace
     * \ingroup group_util
     *
     * \param s any string
     * \return value a input string with any leading and trailing whitespace
     *         removed
     */
    std::string trimws(std::string s);

  } // namespace util

} // namespace datrw

#endif // DATRW_UTIL_H_VERSION (includeguard)

/* ----- END OF util.h ----- */
