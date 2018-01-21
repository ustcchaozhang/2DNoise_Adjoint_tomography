/*! \file readtfascii.h
 * \brief read data obtained in ASCII (any2ascii) from T. Forbriger 
 * \brief (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Daniel Armbruster
 * \date 05/10/2010
 * 
 * Purpose: read data obtained in ASCII (any2ascii) from T. Forbriger 
 *          (prototypes)
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
 * 05/10/2010  V0.1  Daniel Armbruster
 * 
 * ============================================================================
 */
 
// include guard
#ifndef DATRW_READTFASCII_H_VERSION

#define DATRW_READTFASCII_H_VERSION \
  "DATRW_READTFASCII_H   V0.1   "

#include<iostream>
#include<string>
#include<sffxx.h>

namespace datrw {

  /*! \brief all functions, classes, etc. to read data format of T.Forbriger's
   *  brief any2ascii
   *
   * \defgroup group_tfascii Reading module for: output of any2ascii
   *
   */

  /*!  \brief all functions, classes, etc. to read data format of T.Forbriger's
   *  any2ascii
   *
   * \ingroup group_tfascii
   */
  namespace tfascii {
    /*! \brief contains a few helper functions to extract data while reading 
     *  \brief a file
     */
    namespace helper {
      /*--------------------------------------------------------------------*/
      //! split given string
      /*!
          This function will split a line after the following scheme:\n
          number of samples:        1000 m
          will be converted into:\n
          1000 

          \param line reference to the line which will be splitted
          \param unit if a value has a unit or not

      */
      void extractvalue(std::string& line, bool unit = false);
      //! splits a given string after the colon
      /*!
          This helper function implements the same functionality as 
          TfasciiContainer::extractvalue. The only difference is that now
          a string will be splitted after colon.

          \param line string which will be splitted after its colon
      */
      void extract(std::string& line);

    } // namespace helper

    /*----------------------------------------------------------------------*/
    //! name of the stream
    extern const char* const streamname;

    /*----------------------------------------------------------------------*/
    //! holds the fileheader data
    struct FileHeader {
      bool hasfilefree, hassrce;
      ::sff::FREE filefree;
      ::sff::SRCE srce;
    };
    
    /*----------------------------------------------------------------------*/
    //! holds the traceheader data
    struct TraceHeader {
      bool hastracefree, hasinfo, haswid2;
      ::sff::WID2 wid2;
      ::sff::FREE tracefree;
      ::sff::INFO info;
    };

    /*----------------------------------------------------------------------*/
    //! read the complete fileheader
    /*!
        \param is the input stream
        \param verbose for verbose output or not
        \return the FileHeader of the data

    */
    FileHeader readfileheader(std::istream& is, const bool& verbose = false);

    /*----------------------------------------------------------------------*/
    //! read the complete traceheader
    /*!
        \param is the input stream
        \param verbose for verbose output or not
        \return the TraceHeader of the data

    */
    TraceHeader readtraceheader(std::istream& is, const bool& verbose = false);
   
    /*----------------------------------------------------------------------*/
    //! function to print online help
    void help(std::ostream& os=std::cout);

  } // namespace tfascii

} // namespace datrw


#endif // DATRW_READTFASCII_H_VERSION (includeguard)


/* ----- END OF readtfascii.h ----- */
