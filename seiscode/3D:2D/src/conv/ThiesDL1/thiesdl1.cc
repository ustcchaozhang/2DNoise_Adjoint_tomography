/*! \file thiesdl1.cc
 * \brief class that provides access to Thies DL1 via serial port (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 25/11/2008
 * 
 * class that provides access to Thies DL1 via serial port (implementation)
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
 * REVISIONS and CHANGES 
 *  - 25/11/2008   V1.0   Thomas Forbriger
 *  - 12/01/2009   V1.1   changed EOL-sequence
 *  - 13/01/2009   V1.2   
 *                        - returned to original EOL sequence
 *                        - uses raw mode port now
 *                        - set default port modes first
 * 
 * ============================================================================
 */
#define DL_THIESDL1_CC_VERSION \
  "DL_THIESDL1_CC   V1.0   "
#define DL_THIESDL1_CC_CVSID \
  "$Id$"

#include "thiesdl1.h"
#include "error.h"
#include "logger.h"
#include<iostream>
#include<sstream>

using std::cout;
using std::endl;

namespace dl1 {

  const libtime::TRelativeTime dl1samplinginterval(0,0,1);

  const char* const DL1::EOL="\r\n";
  //const char* const DL1::EOL="\n";
  const char* const DL1::dumpEOL="\n";
  const char* const DL1::STX="\x02";
  const char* const DL1::ETX="\x03";

  /*----------------------------------------------------------------------*/

  DL1::DL1(const std::string& device, const bool& debug):
    Mport(device), Mdebug(debug)
  { 
    if (Mdebug) { cout << "DL1 (DEBUG): opened port " << device << endl; }
    this->init();
  }

  /*----------------------------------------------------------------------*/

  /*
   * Do not use setdefaults!
   * The ttydefaults in particular set different control characters.
   *
   * The default settings do not work. We require the following settings:
   *
   * speed 9600 baud; rows 0; columns 0; line = 0;
   * intr = ^C; quit = ^\; erase = ^?; kill = ^U; eof = ^D; eol = <undef>;
   * eol2 = <undef>; swtch = <undef>; start = ^Q; stop = ^S; 
   * susp = ^Z; rprnt = ^R;
   * werase = ^W; lnext = ^V; flush = ^O; min = 1; time = 0;
   * parenb -parodd cs7 hupcl -cstopb cread clocal -crtscts
   * -ignbrk -brkint -ignpar -parmrk -inpck -istrip -inlcr -igncr -icrnl ixon
   * ixoff
   * -iuclc ixany -imaxbel -iutf8
   * -opost -olcuc -ocrnl onlcr -onocr -onlret -ofill -ofdel nl0 cr0 tab0 bs0
   * vt0 ff0
   * -isig -icanon -iexten -echo echoe echok -echonl -noflsh -xcase -tostop
   * -echoprt
   * echoctl echoke
   *
   * They can be obtained by
   * stty -F /dev/ttyS0 1c00:4:dad:a30:3:1c:7f:15:4:0:1:0:11:13:1a:0:12:f:17:16:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0
   *
   */

  void DL1::init() 
  {
    if (Mdebug) { cout << "DL1 (DEBUG): set port to raw mode" << endl; }
    Mport.makeraw();
    if (Mdebug) { cout << "DL1 (DEBUG): set mode 7E1" << endl; }
    Mport.mode7E1();
    if (Mdebug) { cout << "DL1 (DEBUG): set port speed to 9600 baud" << endl; }
    Mport.baud9600();
    if (Mdebug) { cout << "DL1 (DEBUG): enable xon/xoff flow control" << endl; }
    Mport.flowxon();
    if (Mdebug) { cout << "DL1 (DEBUG): clear input buffer" << endl; }
    this->Minput.clear();
    this->activate();
  }

  /*----------------------------------------------------------------------*/

  void DL1::activate() const
  {
    if (Mdebug) { cout << "DL1 (DEBUG): activate data logger" << endl; }
    Mport.write("0");
    ::sleep(1);
  }

  /*----------------------------------------------------------------------*/

  void DL1::sendcommand(const std::string& command,
                        const bool& activate) const
  {
    if (activate) this->activate();
    if (Mdebug) { cout << "DL1 (DEBUG): send command " << command << endl; }
    std::string commandsequence;
    commandsequence.append(this->STX);
    commandsequence.append(command);
    commandsequence.append(this->ETX);
    commandsequence.append(this->EOL);
    Mport.write(commandsequence);
  }

  /*----------------------------------------------------------------------*/
  
  std::string DL1::getline(const int& timeout) 
  {
    std::string retval;
    libtime::TAbsoluteTime now=libtime::utc();
    const int nbuf=255;
    const std::string eolstring(DL1::EOL);
    char buffer[nbuf]; 
    bool hot=true;
    bool timeisout=false;
    Mport.sendxon();
    // enter loop until
    // 1. EOL is found
    // 2. or timeout
    while (hot)
    {
      // read data from port
      int nbytes=Mport.read(buffer, nbuf);
      if (Mdebug && (nbytes>0))
      {
        std::cout << "DL1 (DEBUG): " 
          << "read " << nbytes << " characters" << std::endl;
      }
      // append to input buffer
      if (nbytes>0) { this->Minput.append(buffer, nbytes); }
      // look for EOL
      std::string::size_type eolpos=this->Minput.find(DL1::EOL);
      // extract line if EOL was found
      if (eolpos!=std::string::npos) 
      { 
        retval=this->Minput.substr(0, eolpos);
        this->Minput.erase(0, eolpos+eolstring.length());
        hot=false; 
      } 
      // check for timeout otherwise
      else if (timeout>0)
      {
        libtime::TRelativeTime passed=libtime::utc()-now;
        int seconds=int(libtime::time2double(passed));
        if (seconds>timeout) { timeisout=true; hot=false; }
      }
    }
    Mport.sendxoff();
    if (timeisout) { throw TimeOut(timeout); }
    return(retval);
  } // std::string DL1::getline(const int& timeout)

  /*----------------------------------------------------------------------*/

  //! command DT
  void DL1::sendDT(const int& day) const
  {
    DL1_assert((day<32)&&(day>0),"value out of range");
    this->sendcommand("DT"+DL1::numtostring(day), false);
  }

  /*----------------------------------------------------------------------*/

  //! command DM
  void DL1::sendDM(const int& month) const
  {
    DL1_assert((month<13)&&(month>0),"value out of range");
    this->sendcommand("DM"+DL1::numtostring(month), false);
  }

  /*----------------------------------------------------------------------*/

  //! command DJ
  void DL1::sendDJ(const int& y) const
  {
    int year=y;
    if (year>99) { year=y-100*int(y/100); }
    DL1_assert((year<100)&&(year>=0),"value out of range");
    this->sendcommand("DJ"+DL1::numtostring(year), false);
  }

  /*----------------------------------------------------------------------*/

  //! command ZH
  void DL1::sendZH(const int& hour) const
  {
    DL1_assert((hour<24)&&(hour>=0),"value out of range");
    this->sendcommand("ZH"+DL1::numtostring(hour), false);
  }

  /*----------------------------------------------------------------------*/

  //! command ZM
  void DL1::sendZM(const int& minute) const
  {
    DL1_assert((minute<60)&&(minute>=0),"value out of range");
    this->sendcommand("ZM"+DL1::numtostring(minute), false);
  }

  /*----------------------------------------------------------------------*/

  void DL1::settime(const libtime::TAbsoluteTime& time,
                    std::ostream& os) 
  {
    this->sendZM(time.minute());
    this->dumpresponse(os);
    this->sendZH(time.hour());
    this->dumpresponse(os);
    this->sendDT(time.day());
    this->dumpresponse(os);
    this->sendDM(time.month());
    this->dumpresponse(os);
    this->sendDJ(time.year());
    this->dumpresponse(os);
  }

  /*----------------------------------------------------------------------*/

  void DL1::setcurrenttime(std::ostream& os) 
  {
    this->activate();
    // wait until beginning of new minute
    if (Mdebug) { cout << "DL1 (DEBUG): wait until new minute" << endl; }
    libtime::TAbsoluteTime now=libtime::utc();
    os << "waiting for full minute ";
    while (now.second()!=0)
    {
      os << ".";
      os.flush();
      this->activate();
      now=libtime::utc();
    }
    this->settime(libtime::utc(), os);
  }

  /*----------------------------------------------------------------------*/

  void DL1::dumpresponse(std::ostream& os) 
  {
    try {
      while (true) { os << this->getline(2) << DL1::dumpEOL; }
    } catch (dl1::TimeOut) {
    }
  }

  /*----------------------------------------------------------------------*/

  void DL1::sendts(const libtime::TAbsoluteTime& time) const
  {
    std::string command="ts";
    command.append(this->numtochar(time.day()));
    command.append(this->numtochar(time.month()));
    command.append(this->numtochar((time.year()-100*int(time.year()/100))));
    this->sendcommand(command);
  }

  /*----------------------------------------------------------------------*/

  void DL1::sendds(const libtime::TAbsoluteTime& time) const
  {
    std::string command="ds";
    command.append(this->numtochar(time.day()));
    command.append(this->numtochar(time.month()));
    command.append(this->numtochar((time.year()-100*int(time.year()/100))));
    command.append(this->numtochar(time.hour()));
    command.append(this->numtochar(time.minute()));
    this->sendcommand(command);
  }

  /*----------------------------------------------------------------------*/

  std::string DL1::numtochar(const int& i)
  {
    char c[2];
    c[0]=char(i)+char(28);
    c[1]=0;
    return(std::string(c));
  }

  /*----------------------------------------------------------------------*/

  std::string DL1::numtostring(const int& i)
  {
    std::ostringstream oss;
    oss << i;
    return(oss.str());
  }

} // namespace dl1

/* ----- END OF thiesdl1.cc ----- */
