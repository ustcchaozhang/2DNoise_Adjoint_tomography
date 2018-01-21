/*! \file seifeio.h
 * \brief seife input/output (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 14/02/2011
 * 
 * seife input/output (prototypes)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 14/02/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_SEIFEIO_H_VERSION

#define DATRW_SEIFEIO_H_VERSION \
  "DATRW_SEIFEIO_H   V1.0   "

#include<string>
#include<iostream>
#include<sffxx.h>
#include<libtime++.h>
#include<aff/series.h>

namespace datrw {

  namespace seife {

    /*! \brief Standard parameters used for seife output
     * \defgroup group_seife_standard_parameters Standard parameters used for seife output
     * \ingroup group_seife
     * @{
     */
    extern const char* const seife_standard_format;
    extern const unsigned int seife_standard_precision;
    extern const unsigned int seife_standard_width;
    extern const unsigned int seife_standard_columns;
    /**@}*/

    /*----------------------------------------------------------------------*/

    /*! \brief seife format header parameter line
     * \ingroup group_seife
     * \sa \ref sec_seife_formatdefinition
     */
    class ParameterLine {
      public:
        //! default constructor
        ParameterLine():
          Mdebug(false),
          Mnsamples(0),
          Mformat(seife_standard_format),
          Mdt(1.),
          Mtime(0,0)
        { }
        //! create header from string
        ParameterLine(const std::string& line, const bool& debug=false):
          Mdebug(debug)
        { this->set(line); }
        //! set values from line
        void set(const std::string& line);
        //! create line for output from values
        std::string line() const;

        /*! \name query functions
         */
        ///@{
        //! return number of samples
        unsigned int nsamples() const { return Mnsamples; }
        //! return Fortran data format
        std::string format() const { return Mformat; }
        //! return sumpling interval
        double dt() const { return Mdt; }
        //! return time of first sample as offset from midnight
        libtime::TRelativeTime time() const { return Mtime; }
        ///@}

        /*! \name set functions
         */
        ///@{
        //! return number of samples
        void nsamples(const unsigned int& n) { Mnsamples=n; }
        //! return Fortran data format
        void format(const std::string& f) { Mformat=f; }
        //! return sumpling interval
        void dt(const double& d) { Mdt=d; }
        //! return time of first sample as offset from midnight
        void time(const libtime::TRelativeTime& t) { Mtime=t; }
        ///@}
      private:
        //! request debug output
        bool Mdebug;
        //! number of samples
        unsigned int Mnsamples;
        //! Fortran data format
        std::string Mformat;
        //! sampling interval
        double Mdt;
        //! time of first sample
        libtime::TRelativeTime Mtime;
    }; // class ParameterLine

    /*----------------------------------------------------------------------*/

    /*! \brief class to hold complete seife header
     * \ingroup group_seife
     * \sa \ref sec_seife_formatdefinition
     */
    class Header {
      public:
        //! read header from input
        Header(std::istream& is) { this->read(is); }
        //! default constructor
        Header() { }
        //! create header with empty comments
        Header(const ParameterLine& pl):
          Mparameters(pl)
        {
          Mfree.lines.clear();
        }
        //! read header from input
        void read(std::istream& is);
        ///! write header to stream
        void write(std::ostream& os) const;
        //! set comments
        void set(const ::sff::FREE& free) { Mfree=free; }
        //! set parameters
        void set(const ParameterLine& parameters) { Mparameters=parameters; }
        //! return comments
        ::sff::FREE comments() const { return Mfree; }
        //! return parameters
        ParameterLine parameters() const { return Mparameters; }
      private:
        //! numerical header parameters
        ParameterLine Mparameters;
        //! comments
        ::sff::FREE Mfree;
    }; // class Header

    /*----------------------------------------------------------------------*/

    //! \brief standard sample type
    typedef double Tsample;

    //! \brief standard time series type
    typedef aff::Series<Tsample> Tseries;

    /*! \brief write samples to file
     * \param os output stream to write to
     * \param s series to be written
     */
    void write_series(std::ostream& os, const Tseries::Tcoc& s);

  } // namespace seife

} // namespace datrw

#endif // DATRW_SEIFEIO_H_VERSION (includeguard)

/* ----- END OF seifeio.h ----- */
