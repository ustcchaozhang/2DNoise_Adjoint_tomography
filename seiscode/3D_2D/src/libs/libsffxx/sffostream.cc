/*! \file sffostream.cc
 * \brief SFF output stream (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 10/02/2004
 * 
 * SFF output stream (implementation)
 *
 * ----
 * libsffxx is free software; you can redistribute it and/or modify
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 10/02/2004   V1.0   Thomas Forbriger
 *  - 21/04/2004   V1.1   some improvements - provide debug functionality
 * 
 * ============================================================================
 */
#define TF_SFFOSTREAM_CC_VERSION \
  "TF_SFFOSTREAM_CC   V1.1   "

#include <sffostream.h>

namespace sff {

namespace helper {

using std::cout;
using std::endl;

  ostream_manager::ostream_manager(std::ostream& os, const bool& debug): 
    Mos(os), 
    Mlast(false), Mfileheaderwritten(false),
    Mfilefreedefined(false), Mtraceinprogress(false),
    Mnormmode(NM_maxdyn), Mdebug(debug) { }

  void ostream_manager::setfileheader(const FileHeader& fh)
  {
    Mfileheader=fh;
    Mfilefreedefined=true;
    if (Mdebug) 
    { cout << "DEBUG (ostream_manager::setfileheader) finished" << endl; }
  }

  void ostream_manager::settraceheader(const TraceHeader& th)
  {
    Mtraceheader=th;
    Mtraceinprogress=true;
    if (Mdebug) 
    { cout << "DEBUG (ostream_manager::settraceheader) finished" << endl; }
  }

  void ostream_manager::setinfo(const INFO& info)
  {
    Mtraceheader.setinfo(info);
    Mtraceinprogress=true;
    if (Mdebug) { cout << "DEBUG (ostream_manager::setinfo) finished" << endl; }
  }

  void ostream_manager::setwid2(const WID2& wid2)
  {
    Mtraceheader.setwid2(wid2);
    Mtraceinprogress=true;
    if (Mdebug) { cout << "DEBUG (ostream_manager::setwid2) finished" << endl; }
  }

  void ostream_manager::setsrce(const SRCE& srce)
  {
    Mfileheader.setsrce(srce);
    if (Mdebug) { cout << "DEBUG (ostream_manager::setsrce) finished" << endl; }
  }

  void ostream_manager::setfree(const FREE& free)
  {
    if (Mfilefreedefined || Mtraceinprogress)
    {
      Mtraceheader.setfree(free);
      if (Mdebug) { cout << "DEBUG (ostream_manager::setfree)" 
        << " trace FREE" << endl; }
    }
    else
    {
      Mfileheader.setfree(free);
      Mfilefreedefined=true;
      if (Mdebug) { cout << "DEBUG (ostream_manager::setfree)" 
        << " file FREE" << endl; }
    }
  }

  void ostream_manager::setnormmode(const Enormmode& normmode)
  {
    Mnormmode=normmode;
    if (Mdebug) 
    { cout << "DEBUG (ostream_manager::setnormmode) finished" << endl; }
  }

  void ostream_manager::nextislast()
  {
    Mlast=true;
    if (Mdebug) 
    { cout << "DEBUG (ostream_manager::nextislast) finished" << endl; }
  }

  void ostream_manager::clearlast()
  {
    Mlast=false;
    if (Mdebug) 
    { cout << "DEBUG (ostream_manager::clearlast) finished" << endl; }
  }

  TraceHeader ostream_manager::traceheader() const
  {
    TraceHeader retval(Mtraceheader);
    retval.setlast(Mlast);
    if (Mdebug) 
    { cout << "DEBUG (ostream_manager::traceheader) finished" << endl; }
    return(retval);
  }

  const FileHeader& ostream_manager::fileheader() const
  {
    return(Mfileheader);
  }

  const Enormmode& ostream_manager::normmode() const
  {
    return(Mnormmode);
  }

  void ostream_manager::flushheader()
  {
    if (Mdebug) { cout << "DEBUG (ostream_manager::flushheader):" << endl; }
    if (!Mfileheaderwritten)
    {
      Mos << Mfileheader;
      Mfileheaderwritten=true;
      if (Mdebug) { cout << " flushed" << endl; }
    }
    else
    {
      if (Mdebug) { cout << " no waiting header to be flushed" << endl; }
    }
  }

} // namespace helper
} // namespace sff

/* ----- END OF sffostream.cc ----- */
