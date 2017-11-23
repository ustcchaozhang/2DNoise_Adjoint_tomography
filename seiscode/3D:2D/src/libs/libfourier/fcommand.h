/*! \file fcommand.h
 * \brief evaluate filter commands (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/01/2003
 * 
 * evaluate filter commands (prototypes)
 * 
 * Copyright (c) 2003 by Thomas Forbriger (IMG Frankfurt) 
 *
 * ----
 * libfourier is free software; you can redistribute it and/or modify
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
 *  - 05/01/2003   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_FCOMMAND_H_VERSION

#define TF_FCOMMAND_H_VERSION \
  "TF_FCOMMAND_H   V1.0   "

#include<iostream>
#include<fourier/filters.h>

namespace fourier {

  class FilterCommands: public Filter {
    public:
      typedef Filter Tbase;
      typedef Tbase::Tcvalue Tcvalue;

      FilterCommands(): Tbase() { }
      FilterCommands(const char* filename, const bool& verbose=false):
        Tbase() { read(filename, verbose); }
      FilterCommands(std::istream& is, const bool& verbose=false): 
        Tbase() { read(is, verbose); }

      static void help(std::ostream& os, const bool& verbose=false);
      void read(std::istream& is, const bool& verbose=false);
      void read(const char* filename, const bool& verbose=false);
      bool command(const char* command, const bool& verbose=false);
  }; // class FilterCommands

}  // namespace fourier

#endif // TF_FCOMMAND_H_VERSION (includeguard)

/* ----- END OF fcommand.h ----- */
