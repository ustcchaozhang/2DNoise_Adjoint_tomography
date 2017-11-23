/*! \file dataline.h
 * \brief class to represent one data line (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 25/11/2008
 * 
 * class to represent one data line (prototypes)
 * 
 * Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 25/11/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef DL_DATALINE_H_VERSION

#define DL_DATALINE_H_VERSION \
  "DL_DATALINE_H   V1.0   "
#define DL_DATALINE_H_CVSID \
  "$Id$"

#include<string>
#include<libtime++.h>
#include"error.h"

namespace dl1 {

/*! \defgroup group_dataline DataLine: Class to operate on one line of data
 */
/** @{ */

  //! \brief Exception indicating an inconsistent data line.
  class ExceptionInconsistentLine:
    public dl1::Exception {
      public:
        //! Create with message, failed assertion, and code position
        ExceptionInconsistentLine(const char* message, 
                  const char* file,
                  const int& line,
                  const char* condition,
                  std::string dataline):
          Exception(message, file, line, condition), Mdataline(dataline) { }
        virtual ~ExceptionInconsistentLine() { }
        virtual void report() const;
      private:
        void my_report() const;
        std::string Mdataline;
    }; // class ExceptionInconsistentLine

  /*======================================================================*/

  /*! \brief Store one line of data as read from DL1.
   */
  class DataLine {
    public:
      //! swallow data line
      DataLine(const std::string& line);
      //! return data line
      std::string line() const { return(Mline); }
      //! return date and time
      libtime::TAbsoluteTime time() const { return(Mtime); }
      //! return value of precipitation 
      double precipitation() const { return(Mvalue); }
      //! return count value
      unsigned int counts() const { return(Mcounts); }

      //! gain value in mm/count
      static const double gain;
    private:
      /*! check consistency of data line
       * and fill data value fields
       */
      void evalline();

      //! literal data line as received from DL1
      std::string Mline;
      //! date and time of this sample
      libtime::TAbsoluteTime Mtime;
      //! value of sample in mm precipitation 
      double Mvalue;
      //! value of sample in counts
      unsigned int Mcounts;
  }; // class DataLine

/** @} */

} // namespace dl1

#endif // DL_DATALINE_H_VERSION (includeguard)

/* ----- END OF dataline.h ----- */
