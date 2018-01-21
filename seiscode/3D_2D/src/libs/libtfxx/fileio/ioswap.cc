/*! \file ioswap.cc
 * \brief some functions supporting swapping after I/O (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 18/11/2002
 * 
 * some functions supporting swapping after I/O (implementation)
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 18/11/2002   V1.0   Thomas Forbriger
 *  - 19/11/2002   V1.1   should have reached stbility
 *  - 24/06/2003   V1.2   don't specify default parameters twice
 *  - 19/07/2005   V1.3 
 *                        - use bytesex.h now
 *                        - removed struct IOTsize; this is obsolete, since
 *                          I learned that the sizeof function is a
 *                          compile-time literal
 *                        - removed check_assumed_size() and CHECK_MACRO()
 *                          for the same reason
 * 
 * ============================================================================
 */
#define TF_IOSWAP_CC_VERSION \
  "TF_IOSWAP_CC   V1.3"

#include <tfxx/bytesex.h>
#include <tfxx/error.h>
#include <iostream>

using std::cout;
using std::endl;

namespace tfxx {

namespace ioswap {
   
//----------------------------------------------------------------------
//! function to create the magic number
int magic(const char* const cmagic)
{
  union {
    unsigned int uival;
    int ival;
  } out;
  union {
    char chr[sizeof(int)];
    unsigned char uchr[sizeof(int)];
  } in;
  const int& intsize=sizeof(int);
  for (int i=0; i<intsize; i++)
  { in.chr[i]=cmagic[i]; }
  out.uival=0;
  for (int i=0; i<intsize; i++)
  { out.uival=out.uival*256+in.uchr[i]; }
  return(out.ival);
} // magic()

/*----------------------------------------------------------------------*/
//! check for my CPU model
Ecpu_type cpu()
{
  Ecpu_type result=cpu_unknown;
  IOUnion<int> u1,u2;
  TFXX_assert((sizeof(int) == 4),
              "The integer memory size on this CPU differs from the"
              "required value of 4");
  const int& intsize=sizeof(int);
  char test_seq[]="ABCD";
  // prepare sequence and reverse sequence
  for (int i=0; i<intsize; i++)
  {
    u1.bytes[i]=test_seq[i];
    u2.bytes[i]=test_seq[intsize-i-1];
  }
  // request magic number for sequence
  int magnum=magic(u1.bytes);
  // test against byte representation of sequence and reverse sequence
  if (magnum == u1.value)
  {
    result=cpu_Motorola;
  }
  else if (magnum == u2.value)
  {
    result=cpu_Intel;
  }
  return(result);
} // cpu()

/*----------------------------------------------------------------------*/

//! check magic number in file
Emagic_type file_magic_test(std::istream& is, const char* const cmagic,
                            const bool& fortranmode)
{
  Emagic_type result=magic_nomatch;
  IOUnion<int> req_magic, in_magic;
  // create requested magic number
  req_magic.value=magic(cmagic);
  // skip Fortran block size
  if (fortranmode) is.read(in_magic.bytes, sizeof(int));
  // read magic number and compare
  is.read(in_magic.bytes, sizeof(int));
  if (in_magic.value == req_magic.value)
  { result=magic_match; }
  else if (in_magic.value == swap(req_magic.value))
  { result=magic_swap; }
  // skip Fortran block size
  if (fortranmode) is.read(in_magic.bytes, sizeof(int));
  return(result);
} // file_magic_test()

/*----------------------------------------------------------------------*/

//! write magic number to file
void file_magic_write(std::ostream& os, const char* const cmagic,
                      const bool& fortranmode)
{
  IOUnion<int> ifour, imagic;
  ifour.value=sizeof(int);
  imagic.value=magic(cmagic);
  if (fortranmode) os.write(ifour.bytes, sizeof(int));
  os.write(imagic.bytes, sizeof(int));
  if (fortranmode) os.write(ifour.bytes, sizeof(int));
} // file_magic_write()

} // namespace ioswap

} // namespace tfxx

/* ----- END OF ioswap.cc ----- */
