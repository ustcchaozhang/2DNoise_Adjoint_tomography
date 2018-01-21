/*! \file gsexx_TDAT2.cc
 * \brief definition of TDAT2 functions (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 29/03/2002
 * 
 * definition of TDAT2 functions (implementation)
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 06/12/2011   V1.1   introduced some code for debugging second
 *                        differences
 * 
 * ============================================================================
 */
#define TF_GSEXX_TDAT2_CC_VERSION \
  "TF_GSEXX_TDAT2_CC   V1.1"
#define TF_GSEXX_TDAT2_CC_CVSID \
  "$Id$"

#include <gsexx.h>
#include "gsexx_TDAT2.h"

namespace GSE2 {
namespace waveform {

/*
 * documentation for class TDAT2sum
 * --------------------------------
 * all functions are inline
 */

/*! \class TDAT2sum
 *
 * This class handles the checksum and sample count for DAT2 input as well as
 * for DAT2 output. It should only be used by the TDAT2 classes by
 * inheritance.
 *
 * \sa GSE2::waveform::TDAT2read
 * \sa GSE2::waveform::TDAT2write
 */

//! GSE line identifier
const char* const TDAT2sum::GSEID="DAT2";

/*----------------------------------------------------------------------*/

/*! \fn TDAT2sum::TDAT2sum(const intT& msamp)
 *
 * \param msamp Number of samples to be written or to be read. In any case
 *              this is known in advance after reading or writing the WID2
 *              line.
 */

/*! \fn void TDAT2sum::add(const intT& value)
 *
 * Update the checksum by sample value and increment sample count.
 *
 * \param value next sample value (not difference value)
 */

/*======================================================================*/

/*
 * documentation for class TDAT2read
 * ---------------------------------
 * all functions are inline
 */

/*! \class TDAT2read
 *
 * This is an abstract base class, which defines a standard interface for
 * reading %GSE2 %waveform data in any subformat. A derived class for
 * each supported subformat must be defined to make this concept useable.
 *
 * \sa GSE2::waveform::TDAT2readCM6
 * \sa GSE2::waveform::TDAT2readCM8
 * \sa GSE2::waveform::TDAT2readINT
 * \sa GSE2::waveform::TDAT2write
 */

/*! \fn intT TDAT2read::get()
 *
 * This is a wrapper function. It gets a sample value from the stream through
 * the implementation class specific get function get_from_stream. This value
 * is then passed to the TDAT2sum class to update the checksum.
 *
 * \return next sample value (not difference value) from stream
 */

/*! \fn TDAT2read::TDAT2read(istream& is, const intT& msamp)
 *
 * Standard constructor for the reading classes. It takes an input stream to
 * read a waveform data stream from. If you want to read from a string, use
 * string streams.
 *
 * \param is input stream to read from
 * \param msamp number of samples to be read (may be checked with the ::hot()
 *              function)
 */

// conversion wrapper function
intT TDAT2read::operator()(std::istream& is)
{
  intT retval;
  // check for DAT2 line
  if (this->TDAT2sum::nread() == 0) 
  { 
    std::string lineID;
    is >> lineID;
    if (!GSEIDmatch<TDAT2sum>(lineID)) throw
      Terror("ERROR (TDAT2::read): missing DAT2 line!");
    char c=' ';
    while (c != '\n') { is.get(c); }
  }

  if (!this->TDAT2sum::hot()) throw 
    Terror("ERROR (TDAT2::read): requesting more samples than specified!");
  retval=convert(is);
  this->TDAT2sum::add(retval);

  // check checksum
  if (!TDAT2sum::hot()) {
    TCHK2 checksum;
    checksum.read(is);
    if (this->TDAT2sum::checksum().value()!=checksum.value())
    {
      if (!Terror::silent)
      {
        std::cerr << "checksum read: " << checksum.value()
          << " calculated: " << this->TDAT2sum::checksum().value()
          << std::endl;
      }
      throw
      Terror("ERROR (TDAT2::read): conflicting checksum in CHK2 line!");
    }
  }
  return(retval);
}

/*======================================================================*/

/*
 * documentation for class TDAT2write
 * ----------------------------------
 * all functions are inline
 */

/*! \class TDAT2write
 *
 * This is an abstract base class, which defines a standard interface for
 * writing %GSE2 %waveform data in any subformat. A derived class for
 * each supported subformat must be defined to make this concept useable.
 *
 * \sa GSE2::waveform::TDAT2writeCM6
 * \sa GSE2::waveform::TDAT2writeCM8
 * \sa GSE2::waveform::TDAT2writeINT
 * \sa GSE2::waveform::TDAT2read
 */

/*! \fn void TDAT2write::put(const intT& value)
 *
 * This is a wrapper function. It takes a sample value. updates the checksum
 * and passes the value to the implementation class specific put function.
 * This is done through put_to_stream.
 *
 * \param value next sample value (not difference value) to be written
 */

/*! \fn TDAT2write::TDAT2write(ostream& os, const intT& msamp, 
 *                             const intT& linelength)
 *
 * Standard constructor for the writing classes. It takes an output stream to
 * write a waveform data stream to. If you want to write to a string, use
 * string streams.
 *
 * \param os output stream to write to
 * \param msamp number of samples to be written (may be checked with the
 *              ::hot() function)
 * \param linelength line length for the encoded waveform data in the file
 */

// conversion wrapper function
std::string TDAT2write::operator()(const intT& value)
{
  std::string retval;
  if (this->TDAT2sum::nread() == 0) 
  { 
    retval+=TDAT2sum::GSEID;
    retval+=" \n";
  }
  if (!this->TDAT2sum::hot()) throw 
    Terror("ERROR (TDAT2write): writing more samples than specified!");
  this->TDAT2sum::add(value);
  retval+=convert(value);
  if (!TDAT2sum::hot()) {
    retval+='\n';
    retval+=this->TDAT2sum::checksum().write();
  }
  return(retval);
}

/*======================================================================*/

/*
 * documentation and function code for class TDAT2readCM6
 * ------------------------------------------------------
 */

/*! \class TDAT2readCM6
 *
 * This is a class to read %GSE2 %waveform data in %CM6 subformat.
 *
 * \sa GSE2::waveform::TDAT2read
 *
 * \if internal
 * \par Internal details
 * This class uses the decoding function CM6::decode.
 * \sa GSE2::waveform::CM6
 * \endif
 */

/*! \fn TDAT2readCM6::TDAT2readCM6(istream& is, const intT& msamp)
 *
 * Constructor to create a specific implementation class object for reading
 * %CM6 encoded data. 
 *
 * \param is input stream to read from
 * \param msamp number of samples to be read (may be checked with the ::hot()
 *              function)
 */

/*----------------------------------------------------------------------*/

/*
 * documentation and function code for class TDAT2writeCM6
 * -------------------------------------------------------
 */

/*! 
 * Implementation class specific get function. The function reads a character
 * sequence from the input stream, takes it converted integer value (second
 * difference value), removes the second differences, and returns it.
 *
 * \return integer value (not difference value) read from stream.
 */
intT TDAT2readCM6::convert(std::istream& is)
{
  // it's simple :-)
  return(Mremovediff(CM6::decode(is)));
}

/*======================================================================*/

/*! \class TDAT2writeCM6
 *
 * This is a class to write %GSE2 %waveform data in %CM6 subformat.
 *
 * \sa GSE2::waveform::TDAT2write
 *
 * \if internal
 * \par Internal details
 * This class uses the encoding function CM6::encode.
 * \sa GSE2::waveform::CM6
 * \endif
 */

/*! \fn TDAT2writeCM6::TDAT2writeCM6(ostream& os, const intT& msamp, 
 *                                   const intT& linelength)
 *
 * Constructor to create a specific implementation class object for writing
 * %CM6 encoded data. 
 *
 * \param os output stream to write to
 * \param msamp number of samples to be written (may be checked with the
 *              ::hot() function)
 * \param linelength line length for the encoded waveform data in the file
 */

/*----------------------------------------------------------------------*/

/*! 
 * Implementation class specific put function. The function takes second
 * differences, encodes the resulting value and writes the characters to the
 * output stream.
 *
 * \param value integer value (not difference value) to write.
 */
std::string TDAT2writeCM6::convert(const intT& value)
{
  std::string charcode;
  try {
    charcode=CM6::encode(Mapplydiff(value));
  }
  catch (Terror e)
  {
    std::cerr.setf(std::ios_base::dec,std::ios_base::basefield);
    std::cerr << "TDAT2writeCM6::convert caught exception" << std::endl;
    Mapplydiff.report_status(std::cerr);
    std::cerr << "current value: " << value << std::endl;
    throw e;
  }
  std::string retval;
  // iterate through all characters
  for (std::string::const_iterator c=charcode.begin(); c!=charcode.end(); c++)
  {
    // next line if full
    if (Mlinelength <= Mcpos) 
    {
      retval+='\n';
      Mcpos=0;
    }
    retval+=*c;
    Mcpos++;
  }
  return(retval);
}

} // namespace waveform
} // names(all sophistipace GSE2

/* ----- END OF gsexx_TDAT2.cc ----- */
