/*! \file gsexx_cm6.cc
 * \brief definition of CM6 subformat functions (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger, Stefan Stange, A. Greve, and others
 * \date 29/03/2002
 * 
 * definition of CM6 subformat functions (implementation)
 * 
 * Copyright (c) 1998 by Stefan Stange (LED Freiburg) 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 *
 * Parts of this code are derived from code written by Stefan Stange.
 * \sa GSE2::waveform::CM6::encode
 * \sa GSE2::waveform::CM6::decode
 * 
 * libgsexx is free software; you can redistribute it and/or modify
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
 * 
 * REVISIONS and CHANGES 
 *  - 29/03/2002   V1.0   Thomas Forbriger
 *  - 17/12/2003   V1.1   filled decode function
 * 
 * ============================================================================
 */
#define TF_GSEXX_CM6_CC_VERSION \
  "TF_GSEXX_CM6_CC   V1.1   "
#define TF_GSEXX_CM6_CC_CVSID \
  "$Id$"

#include <gsexx.h>
#include "gsexx_TDAT2.h"
#include <cmath>

namespace GSE2 {
namespace waveform {

/*! \namespace GSE2::waveform::CM6
 *
 * This namespace contains functions that do the encoding and decoding of 
 * %CM6 subformat data. They are not used directly by the public. They are
 * only used through the TDAT2readCM6 and TDAT2writeCM6 classes.
 *
 * \sa GSE2::waveform::TDAT2readCM6
 * \sa GSE2::waveform::TDAT2writeCM6
 */
namespace CM6 {

//! %CM6 subformat encoding function.
/*!
 * The function encodes the integer value in %CM6 subformat. It returns the
 * character sequence representing the numerical value. The second differences
 * must have already been supplied to the data stream. The value is already a
 * second difference value.
 *
 * \author Thomas Forbriger, Stefan Stange and others 
 *         (see comments in the source code)
 *
 * \param value integer value to be encoded in %CM6 subformat
 * \return character string with encoded integer value
 *
 * \sa GSE2::waveform::TDAT2writeCM6
 */
std::string encode(const intT& invalue)
{
  // string variable to store return value
  std::string retval;
 
// The original version of the core of this function was coded by Stefan
// Stange. The code (compress_6b) can be found in gse_functions.c in his
// library. Here is the original comment by Stefan Stange:
/*********************************************************************
  Function: compress_6b
    This routine computes the 6Byte encoding of integer data according
    GSE2.0 based on cmprs6.f in CODECO by Urs Kradolfer. Again, here we
    can cope with consecutive chunks of a data series.
    Input is the data series (integer) and the # of samples. The character
    representation of the data is successively stored to the dynamic
    character buffer written by Andreas Greve.
    Attention: Clipping is at 2**27 - 1 although it looks like 2**28 -1
    in the FORTRAN-Code.

    St. Stange, 28.4.1998
*********************************************************************/

  // copy of value to be manipulated
  intT value=invalue;
  // this defines the value to character mapping
  char achar[] = 
       " +-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  
  // some powers of 2
  //                 2**5 2**10 2**15  2**20    2**25     2**27      
  long expo_2[] = { 0, 32, 1024, 32768, 1048576, 33554432, 134217728 };

  // some powers of 2 minus 1
  //            -1 +      2**5  2**10  2**15   2**20     2**25     
  long expo_2m1_o[] = { 01, 037, 01777, 077777, 03777777, 0177777777 };

  // internal values
  int nflag;
  int mflag = 32;
  long jc;
  int case_expo;

  nflag = 1;
  // convert sign
  if (value < 0 )
  { nflag += 16; value = -value; }

  // clip at 2**27 -1 
  // value = (value >= expo_2[6]) ? expo_2[6] - 1 : value; 
  // the original code clipped at 2**27 -1
  // we consider a number that large to be illegal
  if (value >= expo_2[6]) 
  {
    std::cerr << "ERROR (CM6::encode): "
      << "sample value exceeds largest value which can be handled\n"
          << "Error is triggered by absolute value being ";
    std::cerr.setf(std::ios_base::hex,std::ios_base::basefield);
    std::cerr << "0x0" << value << ">=" << "0x0" << expo_2[6] << std::endl;
    std::cerr << "The input value passed to the encoder is "
      << "0x0" << invalue << std::endl;
    throw Terror("ERROR (CM6::encode): illegal value");
  }

  // compute the exponent base 2
  std::frexp (double(value), &case_expo); 
  // and reduce by integer division
  case_expo = case_expo/5;	

  // check value
  if (case_expo > 5 || case_expo < 0) 
  {
    std::cerr << "ERROR (CM6::encode): exponent is " 
                                       << case_expo << std::endl;
    std::cerr << "ERROR (CM6::encode): sample value is is " 
                                       << invalue << std::endl;
    throw Terror("ERROR (CM6::encode): illegal exponent");
  }

  for ( ; case_expo > 0; case_expo--)
  {				
    // create one character per turn
    jc = value/expo_2[case_expo] + nflag + mflag;
    /*if (jc > 64 || jc < 1) return jc;*/
    retval+=achar[jc];
    value = value & expo_2m1_o[case_expo];
    nflag = 1;
  }

  // one character to go
  jc = value + nflag;
  retval+=achar[jc];

  return(retval);
}

/*----------------------------------------------------------------------*/

//! %CM6 subformat decoding function.
/*!
 * The function decodes an integer value from %CM6 subformat. It takes an
 * input stream to read from. This is necessary since the function has to skip
 * line-ends on its own. If you want to read from a character string, use
 * string streams. The numerical value returned is just the next decoded
 * integer value from the stream. It is a second difference value. You still
 * have to remove second differences afterwards.
 *
 * \author Thomas Forbriger, Stefan Stange and others 
 *         (see comments in the source code)
 *
 * \param is input stream to read from
 * \return next integer value decoded from stream.
 *
 * \sa GSE2::waveform::TDAT2readCM6
 */
intT decode(std::istream& is)
{
// The original version of the core of this function was coded by Stefan
// Stange. The code (decomp_6b) can be found in gse_functions.c in his
// library. Here is the original comment by Stefan Stange:

/*********************************************************************
* Function: decomp_6b
*   This routine evolves the data series from the 6Byte encoding according
*   GSE2.0 based on dcomp6.f in CODECO by Urs Kradolfer. 
*   Input is the character representation (meaning the file pointer to it),
*   the number of samples to be expected and the pointer to the data.
*   Output is the data series in LONG (has to be allocated elsewhere!). 
*   The GSE file must be opened and positioned to or before the "DAT2" line.
*   Returns actual # of samples or -1 as error code.
*   Calls no other routines.
*   St. Stange, 1.10.1998  , verified for PC-byte-sex 11.4.2001
*********************************************************************/
  static int ichar[]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,2,3,4,5,6,7,
             8,9,10,11,0,0,0,0,0,0,0,12,13,14,15,16,17,18,19,20,21,22,
             23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,0,0,0,0,0,0,
             38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,
             57,58,59,60,61,62,63,0,0,0,0,0,0},/*1 more than in FORTRAN*/
             isign=020, ioflow=040, mask1=017, mask2=037, m127=0177;

  int k, inn, jsign=0, joflow=0;
  char inchar;    // character just read from stream
  intT retval;    // decoded samples

  // read next
  is.get(inchar);
  while (inchar == '\n') { is.get(inchar); }
  if (isspace(inchar))
    throw Terror("ERROR (CM6::decode): illegal whitespace");
  if (is.eof())
    throw Terror("ERROR (CM6::decode): illegal end of file");
  				
  /* get ascii code of input character, strip off any higher bits
  (don't know whether it does what it says) and get number representation */
  
  k = int(int(inchar) & m127); 
  inn = ichar[k];
  
  jsign = (inn & isign);            /* get sign bit */
  joflow = (inn & ioflow);          /* get continuation bit if any */
  retval = (long)(inn & mask1);      /* remove dispensable bits and store */

  while (joflow != 0)               /* loop over other bytes in sample */
  {
    retval <<= 5;                    /* multiply with 32 for next byte */
    
    // read next
    is.get(inchar);
    while (inchar == '\n') { is.get(inchar); }
    if (isspace(inchar))
      throw Terror("ERROR (CM6::decode): illegal whitespace");
    if (is.eof())
      throw Terror("ERROR (CM6::decode): illegal end of file");

    /* now the same procedure as above */
    k = int(int(inchar) & m127);
    inn = ichar[k];
    joflow = (inn & ioflow); 	            /* get continuation bit if any */
    retval = retval + intT(inn & mask2);  /* remove bits and store */

  } /* finish up sample if there is no further continuation bit */

  if (jsign != 0) retval = -retval;	      /* evaluate sign bit */

  return(retval);
} // decode
  
} // namespace CM6

} // namespace waveform
} // namespace GSE2

/* ----- END OF gsexx_cm6.cc ----- */
