/*! \file memory.h
 * \brief maintain a local memory files (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 27/11/2008
 * 
 * maintain a local memory files (prototypes)
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
 *  - 27/11/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef DL_MEMORY_H_VERSION

#define DL_MEMORY_H_VERSION \
  "DL_MEMORY_H   V1.0   "
#define DL_MEMORY_H_CVSID \
  "$Id$"

#include<string>
#include<map>
#include"error.h"

namespace dl1 {

/*! \defgroup group_memory Memory: Class to maintain a non-volatile memory
 */
/** @{ */

  /*! \brief Maintains the program's persistent memory.
   */
  class Memory {
    public:
      typedef std::map<std::string, std::string> Tmemorymap;
      Memory(const std::string& filename,
             const bool& reread=false);
      /*! set default value
       * this function set the value only if there was none previously set
       */
      void setdefault(const std::string& key, const std::string& value);
      //! set value
      void set(const std::string& key, const std::string& value);
      //! get value
      std::string get(const std::string& key);
      //! get value
      std::string operator()(const std::string& key)
      { return(this->get(key)); }
      //! true if value is available
      bool available(const std::string& key) const;
    private:
      //! name of memory file
      std::string Mfilename;
      //! re-read file upon each get
      bool Mreread;
      //! my map
      Tmemorymap Mmap;

      //! read file
      void read();
      //! write file
      void write() const;
  }; // class Memory

/** @} */

}  // namespace dl1

#endif // DL_MEMORY_H_VERSION (includeguard)

/* ----- END OF memory.h ----- */
