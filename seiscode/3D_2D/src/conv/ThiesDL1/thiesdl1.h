/*! \file thiesdl1.h
 * \brief class that provides access to Thies DL1 via serial port (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 25/11/2008
 * 
 * class that provides access to Thies DL1 via serial port (prototypes)
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
#ifndef DL_THIESDL1_H_VERSION

#define DL_THIESDL1_H_VERSION \
  "DL_THIESDL1_H   V1.0   "
#define DL_THIESDL1_H_CVSID \
  "$Id$"

#include<iostream>
#include<serialxx/serialport.h>
#include<libtime++.h>
#include"error.h"

namespace dl1 {

/*! \defgroup group_DL1 DL1: Class for low-level access to Thies DL1 logger
 */
/** @{ */

  //! expected sampling interval of DL1
  extern const libtime::TRelativeTime dl1samplinginterval;

  //! \brief Exception to be thrown just to indicate end of input.
  class TimeOut {
    public:
      TimeOut(const int& timeout): Mtimeout(timeout) { }
      int timeout() const { return(Mtimeout); }
    private:
      int Mtimeout;
  }; // class TimeOut

  /*! \brief Access DL1 data logger via serial port.
   */
  class DL1 {
    public:
      //! open device to DL1 logger
      DL1(const std::string& device, const bool& debug=false);
      //! activate DL1
      void activate() const;
      /*! read one line of data
       * throw TimeOut if time spent is larger than timeout seconds
       * and timeout > 0
       */
      std::string getline(const int& timeout=0);
      //! dump response to stream
      void dumpresponse(std::ostream& os);
      //! send command string
      void sendcommand(const std::string& command,
                       const bool& activate=true) const;

      //! command HH
      void sendHH() const { this->sendcommand("HH"); }
      //! command PD
      void sendPD() const { this->sendcommand("PD"); }
      //! command SS
      void sendSS() const { this->sendcommand("SS"); }
      //! command LL
      void sendLL() const { this->sendcommand("LL"); }
      //! command MM
      void sendMM() const { this->sendcommand("MM"); }
      //! command ZZ
      void sendZZ() const { this->sendcommand("ZZ"); }
      //! command XX
      void sendXX() const { this->sendcommand("XX"); }
      //! command DD
      void sendDD() const { this->sendcommand("DD"); }
      //! command ts
      void sendts(const libtime::TAbsoluteTime& time) const;
      //! command ds
      void sendds(const libtime::TAbsoluteTime& time) const;
      //! command DT
      void sendDT(const int& day) const;
      //! command DM
      void sendDM(const int& month) const;
      //! command DJ
      void sendDJ(const int& year) const;
      //! command ZH
      void sendZH(const int& hour) const;
      //! command ZM
      void sendZM(const int& minute) const;

      //! set time and date
      void settime(const libtime::TAbsoluteTime& time,
                   std::ostream& os=std::cout);
      //! set current time and date (from PCs system time)
      void setcurrenttime(std::ostream& os=std::cout);

      //! sequence terminating DL1 data lines
      static const char* const EOL;
      //! sequence terminating dump lines
      static const char* const dumpEOL;
      //! character to initiate command
      static const char* const STX;
      //! character that terminates a command
      static const char* const ETX;
    private:
      //! initialize communication
      void init();
      //! convert numerical value to literal string
      static std::string numtostring(const int& i);
      //! convert numerical value to char
      static std::string numtochar(const int& i);
      //! my serial port that is attached to the DL1
      serialport::SerialPort Mport;
      //! debug mode
      bool Mdebug;
      //! maintein input buffer for reading from DL1
      std::string Minput;
  }; // class DL1

/** @} */

} // namespace dl1

#endif // DL_THIESDL1_H_VERSION (includeguard)

/* ----- END OF thiesdl1.h ----- */
