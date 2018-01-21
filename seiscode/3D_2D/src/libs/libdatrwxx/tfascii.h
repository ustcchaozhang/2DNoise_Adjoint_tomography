/*! \file tfascii.h
 * \brief read Thomas Forbrigers ASCII data (prototypes) 
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Daniel Armbruster
 * \date 05/10/2010
 * 
 * Purpose: read Thomas Forbrigers ASCII data (prototypes) 
 *
 * ----
 * This file is part of libdatrwxx.
 *
 * libdatrwxx is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * libdatrwxx is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with libdatrwxx.  If not, see <http://www.gnu.org/licenses/>.
 * ----
 * 
 * Copyright (c) 2010 by Daniel Armbruster
 * 
 * REVISIONS and CHANGES 
 *  - 05/10/2010   V0.1  Daniel Armbruster
 *  - 23/11/2010   V1.1  Thomas Forbriger: introduced static member data
 * 
 * ============================================================================
 */
 
#ifndef DATRW_TFASCII_H_VERSION

#define DATRW_TFASCII_H_VERSION \
  "DATRW_TFASCII_H   V1.1"

#include <datrwxx/datread.h>
#include <datrwxx/readtfascii.h>

namespace datrw {

  namespace tfascii {

    extern const bool isbinary;
    extern const char* const streamID;

  } // namespace tfascii 

  /*----------------------------------------------------------------------*/

  /*! \brief input stream to read seismic data provided by T. Forbriger's
   *   any2ascii
   *
   * \ingroup group_tfascii
   *
   * The concept is based on SFF data contents and we will make use of SFF
   * structures.
   */
  class itfasciistream: public idatstream {
    public:
      typedef idatstream Tbase;
      //! constructor
      /*!
          \param is the input stream
          \param verbose for verbose output
      */
      itfasciistream(std::istream& is, const bool& verbose = false);
      //! destructor
      virtual ~itfasciistream();
      //! to read the data as double
      /*!
          \return the timeseries (double)
      */
      virtual Tdseries dseries();
      //! to read the data as float
      /*!
          \return the timeseries (float)
      */
      virtual Tfseries fseries();
      //! to read the data as integer
      /*!
          \return the timeseries (integer)
      */
      virtual Tiseries iseries();
      //! reads only the header of the data
      virtual void skipseries() { set_traceheader(); }
      //! print some info about data conversion
      /*!
          \param os the output stream
      */
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    private:
      //! for verbose output
      const bool Mverbose;
      //! set the file header data 
      void set_fileheader();
      //! set the trace header data 
      void set_traceheader();
      //! member function
      /*!
          \return if verbose output is decided
      */
      bool get_verbose() const;
  }; // class itfasciistream

} // namespace datrw

#endif // DATRW_TFASCII_H_VERSION (includeguard)

/* ----- END OF tfascii.h ----- */
