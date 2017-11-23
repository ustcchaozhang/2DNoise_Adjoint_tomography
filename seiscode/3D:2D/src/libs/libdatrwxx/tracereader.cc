/*! \file tracereader.cc
 * \brief provide more efficient reading of sequential traces (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/07/2008
 * 
 * provide more efficient reading of sequential traces (implementation)
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
 *  - 08/07/2008   V1.0   Thomas Forbriger
 *  - 25/11/2010   V1.1   
 *                       - use correct open mode upon ifstream open
 *  - 07/09/2011   V1.2   support format modifier strings
 *  - 29/11/2011   V1.3   introduced assertopen; query functions must not be
 *                        called with no file being open
 * 
 * ============================================================================
 */
#define DATRW_TRACEREADER_CC_VERSION \
  "DATRW_TRACEREADER_CC   V1.3"

#include <datrwxx/tracereader.h>
#include <datrwxx/error.h>

namespace datrw {

  sequentialtracereader::~sequentialtracereader()
  {
    if (this->Mopen)
    {
      delete MPis;
      delete MPias;
    }
  } // sequentialtracereader::~sequentialtracereader()

  /*----------------------------------------------------------------------*/

  void sequentialtracereader::assertopen() const
  {
    DATRW_assert(this->Mopen,
                 "ERROR (sequentialtracereader): "
                 "a query function was called with no file being open");
  } // void sequentialtracereader::assertopen() const

  /*----------------------------------------------------------------------*/

  bool sequentialtracereader::select(const std::string& filename,
                                     const int& itrace,
                                     const std::string& format)
  {
    if (Mdebug)
    {
      std::cerr << "DEBUG: sequentialtracereader: "
        << "entered select with parameters:" << std::endl
        << "  filename: " << filename << std::endl
        << "  itrace: " << itrace << std::endl
        << "  format: " << format << std::endl;
      if (Mopen)
      {
        std::cerr << "  current filename: " << Mfilename << std::endl
          << "  current trace: " << Mindex << std::endl;
      }
    }
    bool doopen=false;
    // check: must a new file be opened?
    if (!Mopen) 
    {
      doopen=true;
      if (Mdebug)
      {
        std::cerr << "DEBUG: sequentialtracereader: "
          << "open file, because no file is open" << std::endl;
      }
    }
    else
    {
      if (filename != Mfilename) 
      { 
        doopen=true; 
        if (Mdebug)
        {
          std::cerr << "DEBUG: sequentialtracereader: "
            << "open file, because file name differs" << std::endl;
        }
      }
      else if (itrace < Mindex) 
      { 
        doopen=true; 
        if (Mdebug)
        {
          std::cerr << "DEBUG: sequentialtracereader: "
            << "open file, because trace index (" << itrace << ") "
            << "is less than current index (" << Mindex << ")" << std::endl;
        }
      }
    }
    // if yes: open new file
    if (doopen)
    {
      if (this->Mopen)
      { 
        if (Mdebug)
        {
          std::cerr << "DEBUG: sequentialtracereader: "
            << "close previous file" << std::endl;
        }
        delete MPis;
        delete MPias;
      } 
      Mindex=1;
      Mfilename=filename;
      MPis=new std::ifstream(Mfilename.c_str(),
                             datrw::ianystream::openmode(format));
      MPias=new datrw::ianystream(*MPis, format, Mdebug);
      Mopen=true;
      if (Mdebug)
      {
        std::cerr << "DEBUG: sequentialtracereader: "
          << "file " << Mfilename << " is opened" << std::endl;
      }
    }
    // skip to selected trace
    int nskip=itrace-Mindex;
    DATRW_assert(nskip>=0, "sequentialtracereader::select: "
                   "cannot skip backwards!");
    if (Mdebug)
    {
      std::cerr << "DEBUG: sequentialtracereader: "
        << nskip << " skips: " << std::endl;
    }
    for (int iskip=0; iskip<nskip; ++iskip)
    {
      DATRW_assert(MPias->good(), "sequentialtracereader::select: "
                     "input is not good!");
      MPias->skipseries();
      ++Mindex;
    }
    return(MPias->good());
  } // sequentialtracereader::select

  /*----------------------------------------------------------------------*/

  bool sequentialtracereader::select(const std::string& filename,
                                     const int& itrace,
                                     const Eformat& format)
  {
    return(this->select(filename, itrace, anyID(format)));
  } // sequentialtracereader::select

} // namespace datrw

/* ----- END OF tracereader.cc ----- */
