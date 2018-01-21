/*! \file serialport.cc
 * \brief code to access a serial port (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 24/11/2008
 * 
 * code to access a serial port (implementation)
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
 *  - 24/11/2008   V1.0   Thomas Forbriger
 *  - 13/01/2009   V1.1   new function: makeraw
 * 
 * ============================================================================
 */
#define TF_SERIALPORT_CC_VERSION \
  "TF_SERIALPORT_CC   V1.1"
#define TF_SERIALPORT_CC_CVSID \
  "$Id$"

#include <serialxx/serialport.h>
#include <serialxx/error.h>
#include <unistd.h>
#include <iostream>

namespace serialport {

  //! open serial port
  SerialPort::SerialPort(const std::string& devname,
                         const int& flags)
  {
    Mfd=open(devname.c_str(), flags);
    SERIALPORT_assert(Mfd>0, "ERROR on opening port");
    Mdebug=false;
    ::tcgetattr(Mfd, &Mdevoptions);
    Musexon=false;
  }

  /*----------------------------------------------------------------------*/

  //! close serial port
  SerialPort::~SerialPort()
  {
    // restore options
    ::tcsetattr(Mfd, TCSANOW, &Mdevoptions);
    SERIALPORT_assert(close(Mfd)!=-1, "ERROR on closing port");
  }

  /*----------------------------------------------------------------------*/

  //! write to port
  void SerialPort::write(const std::string& text) const
  {
    int n,len=text.size();
    n=::write(Mfd, text.c_str(), len);
    SERIALPORT_assert(n==len, "ERROR on writing to port");
  }

  /*----------------------------------------------------------------------*/

  //! read from port
  std::string SerialPort::read(const std::string& delim) const
  {
    std::string retval;
    const int nbuf=255;
    char buffer[nbuf];
    bool hot=true;
    this->sendxon();
    while (hot)
    {
      int nbytes=::read(Mfd, buffer, nbuf);
      if (Mdebug) 
      { 
        std::cout << "SerialPort::read: " << std::endl
          << "read " << nbytes << " characters" << std::endl;
      }
      if (nbytes>0) { retval.append(buffer, nbytes); }
      if (retval.find(delim)!=std::string::npos) { hot=false; }
    }
    this->sendxoff();
    return(retval);
  }

  /*----------------------------------------------------------------------*/

  //! low-level read from port
  int SerialPort::read(char* buffer, const int& bufsize) const
  { return(::read(Mfd, buffer, bufsize)); }

  /*----------------------------------------------------------------------*/

  void SerialPort::sendxon() const
  { if (Musexon) { ::tcflow(Mfd, TCION); } }

  /*----------------------------------------------------------------------*/

  void SerialPort::sendxoff() const
  { if (Musexon) { ::tcflow(Mfd, TCIOFF); } }

  /*----------------------------------------------------------------------*/

  void SerialPort::clocal() const
  {
    struct termios options;
    ::tcgetattr(Mfd, &options);
    options.c_cflag |= CLOCAL;
    ::tcsetattr(Mfd, TCSANOW, &options);
  } // void SerialPort::clocal() const

  /*----------------------------------------------------------------------*/

  void SerialPort::mode7E1() const
  {
    struct termios options;
    ::tcgetattr(Mfd, &options);
    options.c_cflag |= PARENB;
    options.c_cflag &= ~PARODD;
    options.c_cflag &= ~CSTOPB;
    options.c_cflag &= ~CSIZE;
    options.c_cflag |= CS7;
    ::tcsetattr(Mfd, TCSANOW, &options);
  } // void SerialPort::mode7E1() const

  /*----------------------------------------------------------------------*/

  void SerialPort::flowxon() 
  {
    struct termios options;
    ::tcgetattr(Mfd, &options);
    options.c_iflag |= (IXON | IXOFF | IXANY);
    ::tcsetattr(Mfd, TCSANOW, &options);
    Musexon=true;
    ::tcflow(Mfd, TCIOFF); 
  } // void SerialPort::flowxon() const

  /*----------------------------------------------------------------------*/

  void SerialPort::baud9600() const
  {
    struct termios options;
    ::tcgetattr(Mfd, &options);
    ::cfsetispeed(&options, B9600);
    ::cfsetospeed(&options, B9600);
    ::tcsetattr(Mfd, TCSANOW, &options);
  } // void SerialPort::baud9600() const

  /*----------------------------------------------------------------------*/

  void SerialPort::makeraw() const
  {
    struct termios options;
    ::tcgetattr(Mfd, &options);
    ::cfmakeraw(&options);
    ::tcsetattr(Mfd, TCSANOW, &options);
  } // void SerialPort::makeraw() const

} // namespace serialport

/* ----- END OF serialport.cc ----- */
