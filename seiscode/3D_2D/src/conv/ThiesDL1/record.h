/*! \file record.h
 * \brief class to represent a data record (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 24/03/2014
 * 
 * class to represent a data record (prototypes)
 * 
 * Copyright (c) 2008, 2014 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 25/11/2008   V1.0   Thomas Forbriger (thof)
 *  - 22/12/2008   V1.1   tolerance modes log weird lines and set flags
 *  - 20/03/2014 thof:    
 *                        modifications applied to class dl1::Record and
 *                        member function dl1::Record::put
 *                        - discard Mbetolerantagainstredundant
 *                        - accept duplicate time stamps as part of normal
 *                          operation (see \ref page_thiesdl1_duplicates)
 *                        - add sample values for duplicate time stamps
 *  - 24/03/2014 thof:
 *                        - avoid tsioxx modules
 * 
 * ============================================================================
 */

// include guard
#ifndef DL_RECORD_H_VERSION

#define DL_RECORD_H_VERSION \
  "DL_RECORD_H   2014-03-24"
#define DL_RECORD_H_CVSID \
  "$Id$"

#include<list>
#include<libtime++.h>
#include<sffxx.h>
#include<datrwxx/types.h>
#include"dataline.h"
#include"error.h"

namespace dl1 {

/*! \defgroup group_record Record: Class to operate on a complete data record
 */
/** @{ */

  typedef datrw::Tiseries Tiseries;
  typedef datrw::Tdseries Tdseries;

  //! SEED channel identifier for precipitation 
  extern const char* const precipitationID;

  //! \brief Exception indicating an inconsistent data line.
  class ExceptionRecordWindow:
    public dl1::Exception {
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

  //! \brief Store a full data record.
  class Record {
    public:
      typedef std::list<DataLine> Tdatalist;
      typedef std::list<std::string> Tlinelist;
      /*! prepare a data record for a give time span
       *
       * \param earliest data and time for earliest data to be stored
       * \param latest data and time for latest data to be stored
       * \param violationexception throw exception if sample does not fit in
       *          expected time window
       */
      Record(const libtime::TAbsoluteTime& earliest,
             const libtime::TAbsoluteTime& latest);
      //! append one data line
      void put(const DataLine& line);
      //! set initial line
      void setinitial(const std::string& line);
      //! set final line
      void setfinal(const std::string& line);
      //! return lines
      Tlinelist lines() const;
      //! return header lines
      Tlinelist header() const;
      //! return data block of values
      Tdseries::Tcoc dseries() const;
      //! return data block of counts
      Tiseries::Tcoc iseries() const;
      //! number of samples expected in this data set
      int nsamples() const;
      //! return WID2 header
      sff::WID2 wid2line() const;
      //! true if unexpected data time was found
      bool foundunexpecteddatatime() const
      { return(Mfoundunexpecteddatatime); }
      //! return log messages for most recent request
      Tlinelist logmessages() const { return(Mlogmessages); }

      //! set tolerance mode
      static void betolerantagainstwrongtime(const bool& flag=true)
      { Mbetolerantagainstwrongtime=flag; }
    private:
      //! check whether data is completely available
      void checkinitialandfinal() const;
      //! earliest time
      libtime::TAbsoluteTime Mearliest;
      //! latest time
      libtime::TAbsoluteTime Mlatest;
      //! time of creation
      libtime::TAbsoluteTime Mcreation;
      //! place to store my data
      Tdatalist Mdatalist;
      //! initial line from DL1
      std::string Minitial;
      //! final line from DL1
      std::string Mfinal;
      //! found unexpected data time
      bool Mfoundunexpecteddatatime;
      //! list of messages created by most recent data request
      Tlinelist Mlogmessages;
      //! prepare are series of counts
      Tiseries Miseries;
      //! an array to keep track of samples
      aff::Series<bool> Mfilled;
      //! number of samples
      int Mnsamples;

      //! mode: do not abort upon wrong sample time
      static bool Mbetolerantagainstwrongtime;
  }; // class Record

/** @} */

} // namespace dl1

#endif // DL_RECORD_H_VERSION (includeguard)

/* ----- END OF record.h ----- */
