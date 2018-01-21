/*! \file thiesdl1line.h
 * \brief handle a ThiesDL1 data line (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/09/2011
 * 
 * handle a ThiesDL1 data line (prototypes)
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
 * REVISIONS and CHANGES 
 *  - 13/09/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_THIESDL1LINE_H_VERSION

#define DATRW_THIESDL1LINE_H_VERSION \
  "DATRW_THIESDL1LINE_H   V1.0   "

#include <libtime++.h>
#include <datrwxx/error.h>

namespace datrw {

  namespace thiesdl1 {

    //! \brief Exception indicating an inconsistent data line.
    class ExceptionInconsistentLine:
      public Exception {
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

  } // namespace thiesdl1

} // namespace datrw

#endif // DATRW_THIESDL1LINE_H_VERSION (includeguard)

/* ----- END OF thiesdl1line.h ----- */
