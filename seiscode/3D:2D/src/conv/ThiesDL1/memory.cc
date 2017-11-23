/*! \file memory.cc
 * \brief maintain a local memory files (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 27/11/2008
 * 
 * maintain a local memory files (implementation)
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
 *  - 27/11/2008   V1.0   Thomas Forbriger (thof)
 *  - 24/03/2014 thof:    create directories for memory path
 * 
 * ============================================================================
 */
#define DL_MEMORY_CC_VERSION \
  "DL_MEMORY_CC   V2014-03-24"
#define DL_MEMORY_CC_CVSID \
  "$Id$"

#include<iostream>
#include<fstream>
#include<tfxx/fs.h>
#include<tfxx/filestatus.h>
#include "error.h"
#include "memory.h"
#include "logger.h"

namespace dl1 {

  Memory::Memory(const std::string& filename, const bool& reread):
    Mfilename(filename), Mreread(reread)
  { 
    // create base directory if not present
    std::string dirname=tfxx::fs::dirname(filename);
    if (!tfxx::file::writable(dirname.c_str()))
    { 
      try {
        tfxx::fs::mkdirp(dirname);
      }
      catch (tfxx::error::Exception& e) {
        Logger(log_err) << "cannot create directory " << dirname;
        DL1_abort("cannot create directory for memory file");
      }
    }

    // try reading memory file 
    try {
      read(); 
    }
    catch (...)
    {
      DL1_abort("fatal error when trying to read read memory file");
    }
  } // Memory::Memory(const std::string& filename, const bool& reread=false)

  /*----------------------------------------------------------------------*/

  void Memory::read()
  {
    Mmap.clear();
    std::ifstream is(Mfilename.c_str()); 
    while (is.good())
    {
      std::string key, value;
      is >> key;
      if (is.good())
      {
        getline(is, value);
        Mmap[key]=value;
        // std::cout << "key: " << key << " value: " << value << std::endl;
      }
    }
  } // void Memory::read()

  /*----------------------------------------------------------------------*/

  void Memory::write() const
  {
    std::ofstream os(Mfilename.c_str()); 
    DL1_assert(os.good(), "cannot write to memory file");
    Tmemorymap::const_iterator I=Mmap.begin();
    while (I!=Mmap.end())
    {
      os << I->first << " " << I->second << std::endl;
      ++I;
    }
  } // void Memory::write()

  /*----------------------------------------------------------------------*/

  std::string Memory::get(const std::string& key)
  {
    if (Mreread) { this->read(); }
    std::string retval("");
    if (Mmap.count(key)!=0) { retval=Mmap[key]; }
    return(retval);
  } // std::string Memory::get(const std::string& key)
  /*----------------------------------------------------------------------*/

  void Memory::set(const std::string& key, const std::string& value)
  {
    Mmap[key]=value; 
    this->write();
  } // void Memory::set(const std::string& key, const std::string& value)

  /*----------------------------------------------------------------------*/

  void Memory::setdefault(const std::string& key, const std::string& value)
  {
    if (!this->available(key))
    { 
      Mmap[key]=value; 
      this->write();
    }
  } // void Memory::setdefault(const std::string& key, const std::string& value)

  /*----------------------------------------------------------------------*/

  bool Memory::available(const std::string& key) const
  {
    return((Mmap.count(key)!=0));
  } // bool Memory::available(const std::string& key) const

}  // namespace dl1

/* ----- END OF memory.cc ----- */
