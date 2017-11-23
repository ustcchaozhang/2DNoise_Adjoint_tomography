/*! \file thiesdl1file.h
 * \brief handle a ThiesDL1 data file (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/09/2011
 * 
 * handle a ThiesDL1 data file (prototypes)
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
#ifndef DATRW_THIESDL1FILE_H_VERSION

#define DATRW_THIESDL1FILE_H_VERSION \
  "DATRW_THIESDL1FILE_H   V1.0   "

#include <libtime++.h>
#include <sffxx.h>
#include <datrwxx/error.h>
#include <datrwxx/thiesdl1line.h>
#include <datrwxx/types.h>

namespace datrw {

  namespace thiesdl1 {

    //! SEED channel identifier for precipitation 
    extern const char* const precipitationID;

    //! expected sampling interval of DL1
    extern const libtime::TRelativeTime dl1samplinginterval;

    //! \brief Exception indicating an inconsistent data line.
    class ExceptionRecordWindow:
      public datrw::Exception {
        public:
          //! Create with message, failed assertion, and code position
          ExceptionRecordWindow(const char* message, 
                                const char* file,
                                const int& line,
                                const char* condition,
                                libtime::TAbsoluteTime earliest,
                                libtime::TAbsoluteTime latest,
                                std::string dataline):
            Exception(message, file, line, condition), 
            Mearliest(earliest),
            Mlatest(latest),
            Mdataline(dataline) { }
          virtual ~ExceptionRecordWindow() { }
          virtual void report() const;
        private:
          void my_report() const;
          libtime::TAbsoluteTime Mearliest;
          libtime::TAbsoluteTime Mlatest;
          std::string Mdataline;
      }; // class ExceptionInconsistentLine

    /*======================================================================*/

    /*! \brief Store the header of a data file
     *
     */
    struct FileHeader {
      public:
        //! clear entries
        void clear();
        //! A collection of header lines with hash sign stripped off
        ::sff::FREE lines;
        //! Expected initial data line (as announced in header lines)
        std::string expectedinitialdataline;
        //! Expected final data line (as announced in header lines)
        std::string expectedfinaldataline;
        //! Initial data line
        std::string initialdataline;
        //! earliest date
        libtime::TAbsoluteTime earliestdate;
        //! latest date
        libtime::TAbsoluteTime latestdate;
        //! creation date
        libtime::TAbsoluteTime creationdate;
        //! true if header data was read successfully
        bool readsuccessfully;
        //! return WID2 header
        ::sff::WID2 wid2line() const;
        //! number of samples
        unsigned int nsamples() const;
    }; // struct FileHeader

    /*----------------------------------------------------------------------*/

    /*! \brief Read and parse a file header
     */
    FileHeader readheader(std::istream & is);

    /*======================================================================*/
  
    /*! \brief Store a full data file.
     *
     * The container is filled by the read function.
     */
    class File {
      public:
        /*! The file container is just created and initial values (indicating
         * emptyness) are set.
         * Use the read function to actually fill the container.
         */
        File() { this->clear(); } 
        //! actually read file
        void readwithheader(std::istream& is);
        //! actually read file
        void read(std::istream& is, const FileHeader& header);
        //! clear container
        void clear();
        /*! check whether container was properly filled
         *
         * \param throwerrors if true, throws an error upon the first
         *   condition not properly set
         * \return true if instance contains meaningful and consistent data
         */
        bool isproperlyfilled(const bool& throwerrors=false) const;
        //! return FREE lines read from file header
        ::sff::FREE filefree() const { return Mheader.lines; };
        //! return FREE lines produced while reading the data
        ::sff::FREE tracefree() const { return Mtracefree; };
        //! return data block of values
        Tdseries dseries() const;
        //! return data block of values
        Tfseries fseries() const;
        //! return data block of counts
        Tiseries iseries() const;
        //! number of samples expected in this data set
        int nsamples() const;
        //! return WID2 header
        ::sff::WID2 wid2line() const;
        //! return file header
        FileHeader header() const { return Mheader; };

        //! true if unexpected data time was found
        bool foundunexpecteddatatime() const
        { return(Mfoundunexpecteddatatime); }

        //! set tolerance for redundant samples
        void tolerateredundant(const bool flag=true) 
        { Mbetolerantagainstredundant=flag; }
        //! set tolerance for wrong time
        void toleratewrongtime(const bool flag=true) 
        { Mbetolerantagainstwrongtime=flag; }
      private:
        //! drop a data line
        void put(const DataLine& line);

        //! is this container empty and ready for reading?
        bool Mreadyforreading;
        //! true if data was read successfully
        bool Mreadsuccessfully;
        //! file header
        FileHeader Mheader;
        //! number of samples
        unsigned int Mnsamples;
        //! found unexpected data time
        bool Mfoundunexpecteddatatime;
        //! prepare are series of counts
        Tiseries Miseries;
        //! an array to keep track of samples
        aff::Series<bool> Mfilled;
        //! comment header lines
        ::sff::FREE Mtracefree;

        //! mode: do not abort upon redundant samples
        bool Mbetolerantagainstredundant;
        //! mode: do not abort upon wrong sample time
        bool Mbetolerantagainstwrongtime;
    }; // class File

  } // namespace thiesdl1

} // namespace datrw

#endif // DATRW_THIESDL1FILE_H_VERSION (includeguard)

/* ----- END OF thiesdl1file.h ----- */
