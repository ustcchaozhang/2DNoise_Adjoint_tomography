/*! \file hexdump.cc
 * \brief output hex dump of any structure (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 27/06/2016
 * 
 * output hex dump of any structure (implementation)
 * 
 * Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
 * 
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.

 * ----
 *
 * REVISIONS and CHANGES 
 *  - 27/06/2016   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_HEXDUMP_CC_VERSION \
  "TF_HEXDUMP_CC   V1.0"

#include <tfxx/hexdump.h>

namespace tfxx {

  namespace util {

    std::ostream& hexdump(const void* pp, const unsigned int& size,
                          std::ostream& os,
                          const char& c, const unsigned int&n)
      {
        // obtain pointer with desired type
        const unsigned char* p=reinterpret_cast<const unsigned char*>(pp);
        // character counter in object
        unsigned short int ip=0;
        // character counter in line
        unsigned short int ic=0;
        // character array for printable characters
        char *pc=new char[n];

        // report size of object
        os << "size of object: " << size << " bytes" << std::endl;

        // memorize output stream flags
        std::ostream::fmtflags flags=os.flags();

        // cycle through all bytes in p
        while (ip<size)
        {
          // start line with hex display of byte offset of first character
          os << "0x";
          os << std::hex << std::setfill('0') << std::setw(4) 
             << std::nouppercase << ip;

          // cycle through all characters in line
          while ((ip < size) && (ic<n))
          {
            // extract numerical representation of current character
            unsigned char ch=*p;
            unsigned short int byte(ch);
            // print hex value of current byte value
            os << " ";
            os << std::hex << std::setfill('0') << std::setw(2) 
               << std::nouppercase << byte;
            os.flags(flags);
            // store printable representation of character
            pc[ic]=c;
            if (std::isprint(ch)) { pc[ic]=ch; }
            // step to next byte in object
            ++p;
            ++ip;
            ++ic;
          } // while ((ip < size) && (ic<n))

          // restore state flags of output stream
          os.flags(flags);
          // forward line columns to position of printable characters
          for (unsigned int i=ic; i<n; i++) { os << "   "; }
          // output printable representation of bytes
          os << " ";
          for (unsigned int i=0; i<ic; i++) { os << pc[i]; }
          // end line
          os << std::endl;
          ic=0;
        } // while (ip<size)

        // delete temporary container of printable characters
        delete[] pc;
        return os;
      } // std::ostream& hexdump(const char* p, const unsigned int& size,
        //                       std::ostream& os=std::cout,
        //                       const char& c='.', const unsigned int&n=16)

  } // namespace util

} // namespace tfxx

/* ----- END OF hexdump.cc ----- */
