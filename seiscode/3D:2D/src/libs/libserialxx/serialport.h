/*! \file serialport.h
 * \brief code to access a serial port (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 24/11/2008
 * 
 * code to access a serial port (prototypes)
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
 *  - 13/01/2009   V1.1   new functions: makeraw, setdefaults
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_SERIALPORT_H_VERSION

#include <fcntl.h>
#include <termios.h>
#include <unistd.h>
#include <string>

#define TF_SERIALPORT_H_VERSION \
  "TF_SERIALPORT_H   V1.1"
#define TF_SERIALPORT_H_CVSID \
  "$Id$"

namespace serialport {

  /*! This class provides access to a serial port
   */
  class SerialPort {
    public:
      //! open port by name of device file
      SerialPort(const std::string& devname, 
                 const int& flags=(O_RDWR | O_NOCTTY | O_NDELAY));
      //! close port
      ~SerialPort();
      //! write string to port
      void write(const std::string& text) const;
      //! read from port until delimiter
      std::string read(const std::string& delim="\n") const;
      //! low-level read 
      int read(char* buffer, const int& bufsize) const;
      //! set debug mode
      void debug(const bool& flag) { Mdebug=flag; }

      //! send xon
      void sendxon() const;
      //! send xoff
      void sendxoff() const;

      //! set xon/xoff slow control
      void flowxon();
      //! set 7 data bits, even parity, 1 stop bit
      void mode7E1() const;
      //! select 9600 baud
      void baud9600() const;
      //! issue cfmakeraw 
      void makeraw() const;
      //! set default values from ttydefaults.h
      void setdefaults() const;
      //! set clocal
      void clocal() const;
    private:
      //! file descriptor to this port
      int Mfd;
      //! debug flag
      bool Mdebug;
      //! remember device status
      struct termios Mdevoptions;
      //! use xon/xoff flow control
      bool Musexon;
  }; // class SerialPort

} // namespace serialport

#endif // TF_SERIALPORT_H_VERSION (includeguard)

/* ----- END OF serialport.h ----- */
