/*! \file fortranio.cc
 * \brief read and write FORTRAN file data
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/01/2002
 * 
 * read and write FORTRAN file data (implementation)
 * 
 * Copyright '(c)' 2002 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 13/01/2002   V1.0   Thomas Forbriger
 *  - 29/01/2002   V1.1   do not pass non-references to temporaries
 *  - 07/02/2002   V1.2   reflect changes in TIterator and TBrowser
 *  - 08/02/2002   V1.3   read next chunk only when data is requested
 *  - 14/11/2002   V1.4   copied from libclass
 *  - 15/11/2002   V1.5   do no buffering anymore
 *  - 19/11/2002   V1.6
 *                        - extract functions are replaced by function template
 *                        - use ioswap functionality
 *  - 28/11/2002   V1.7   
 *                        - flush buffer only if filled
 *                        - support magic numbers
 *                        - needs a cleaning destructor
 * 
 * ============================================================================
 */
#define TF_FORTRANIO_CC_VERSION \
  "TF_FORTRANIO_CC   V1.7   "

#include <iostream>
#include <string>
#include <tfxx/fortranio.h>
#include <tfxx/error.h>

using std::cerr;
using std::endl;

namespace tfxx {
namespace fortranio {

/*======================================================================*/
//
// FortranBinInput
// ===============

  //! return next character from buffer
  void FortranBinInput::read_block_size()
  {
    tfxx::ioswap::IOUnion<int> buf;
    // in case we are already in the file, we have to read and check the
    // byte-count at the end of the block
    if (Mnbytes != 0)
    {
      TFXX_assert((Mnremain==0),
        "FotranBinInput: internal error - remain count is not zero!");
      Mistream.read(buf.bytes, sizeof(int));
      int endcount=buf.value;
      if (Mswap) { endcount=tfxx::ioswap::swap(buf.value); }
      TFXX_assert((Mnbytes == endcount),
        "FortranBinInput: bount count at end of block does not match!");
    }
    // read byte-count at beginning of next block
    Mistream.read(buf.bytes, sizeof(int));
    Mnremain=buf.value;
    if (Mswap) { Mnremain=tfxx::ioswap::swap(buf.value); }
    Mnbytes=Mnremain;
    TFXX_assert((Mistream.good()),
      "FortranBinInput: stream is not good after reading block size");
  }

/*----------------------------------------------------------------------*/

  //! finish open block
  void FortranBinInput::finish_block()
  {
    // do we have to read any more from current block?
    while (Mnremain>0) { this->extract_next_char(); }
    // had this block finite size? Then read end count and compare
    if (Mnbytes>0) {
      tfxx::ioswap::IOUnion<int> buf;
      Mistream.read(buf.bytes, sizeof(int));
      int endcount=buf.value;
      if (Mswap) { endcount=tfxx::ioswap::swap(buf.value); }
      TFXX_assert((Mnbytes == endcount),
        "FortranBinInput (finish_block): "
        "bount count at end of block does not match!");
    }
    // we do not open a new block - indicate by zero blocksize
    Mnbytes=0;
  }

/*----------------------------------------------------------------------*/

  //! return next character from buffer
  char FortranBinInput::extract_next_char()
  {
    char result;
    TFXX_assert((Mistream.good()),
      "FortranBinInput: stream is not good upon reading next char");
    if (Mnremain < 1) { read_block_size(); }
    Mistream.read(&result, 1);
    Mnremain--;
    return(result);
  }

/*----------------------------------------------------------------------*/

  //! extract a set of characters
  void FortranBinInput::extract_chars(char* buf, const int& n)
  {
    for (int i=0; ((i<n) && more()); i++)
    { buf[i]=extract_next_char(); }
  }

/*----------------------------------------------------------------------*/

  //! look out for expected magic number and adjust swapping flag
  bool FortranBinInput::match_magic(const char* cmagic)
  {
    // skip rest of previous block
    this->finish_block();
    tfxx::ioswap::Emagic_type result
      =tfxx::ioswap::file_magic_test(Mistream, cmagic, true);
    if (result==tfxx::ioswap::magic_nomatch) return(false);
    if (result==tfxx::ioswap::magic_swap) { Mswap=true; }
    else { Mswap=false; }
    return(true);
  }

/*======================================================================*/
//
// FortranBinOutput
// ================

  //! finally write the data (flush buffer)
  void FortranBinOutput::end_block()
  {
    std::string buffer=Mbuffer.str();
    tfxx::ioswap::IOUnion<int> count;
    count.value=buffer.size();
    if (count.value>0)
    {
      Mostream.write(count.bytes, sizeof(int));
      Mostream.write(buffer.c_str(), count.value);
      Mostream.write(count.bytes, sizeof(int));
      Mbuffer.str(std::string());
    }
  }

/*----------------------------------------------------------------------*/

  //! put a set of characters to the buffer
  void FortranBinOutput::put_chars(const char* buf, const int& n)
  { Mbuffer.write(buf, n); }

/*----------------------------------------------------------------------*/

  //! put one character to the buffer
  void FortranBinOutput::put_char(const char& c)
  { Mbuffer.write(&c, 1); }

/*----------------------------------------------------------------------*/

  //! write magic number to file
  void FortranBinOutput::write_magic(const char* cmagic)
  { 
    this->end_block();
    this->put(tfxx::ioswap::magic(cmagic)); 
    this->end_block();
  }

} // namespace fortranio
} // namespace tfxx

/* ----- END OF fortranio.cc ----- */
