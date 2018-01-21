/*! \file setdefaults.cc
 * \brief set tty defaults (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 13/01/2009
 * 
 * set tty defaults (implementation)
 * 
 * Copyright (c) 2009 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 13/01/2009   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_SETDEFAULTS_CC_VERSION \
  "TF_SETDEFAULTS_CC   V1.0   "
#define TF_SETDEFAULTS_CC_CVSID \
  "$Id$"

#include <serialxx/serialport.h>

#define TTYDEFCHARS
#include<sys/ttydefaults.h>

namespace serialport {

  void SerialPort::setdefaults() const
  {
    struct termios options;
    ::tcgetattr(Mfd, &options);
    options.c_iflag=TTYDEF_IFLAG;
    options.c_oflag=TTYDEF_OFLAG;
    options.c_cflag=TTYDEF_CFLAG;
    options.c_lflag=TTYDEF_LFLAG;
    ::cfsetispeed(&options, TTYDEF_SPEED);
    ::cfsetospeed(&options, TTYDEF_SPEED);
    for (int i=0; i<NCCS; ++i)
    { options.c_cc[i]=::ttydefchars[i]; }
    ::tcsetattr(Mfd, TCSANOW, &options);
  } // void SerialPort::setdefaults() const

} // namespace serialport

/* ----- END OF setdefaults.cc ----- */
