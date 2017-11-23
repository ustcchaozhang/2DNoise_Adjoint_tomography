/*! \file fortranio.h
 * \brief read and write FORTRAN file data
 * 
 * \ingroup fortranio_h
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/01/2002
 * 
 * read and write FORTRAN file data (prototypes)
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
 *  - 07/02/2002   V1.1   reflect changes in TIterator and TBrowser
 *  - 14/11/2002   V1.2   copied from libclass
 *  - 15/11/2002   V1.3   no arrays for buffering
 *  - 19/11/2002   V1.4   remove all access functions
 *                        rather use I/O operator
 *  - 28/11/2002   V1.5   
 *                        - support reading and writing of magic numbers
 *                        - input needs a cleaning destructor
 *  - 19/07/2005   V1.6   use bytesex.h now
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_FORTRANIO_H_VERSION

#define TF_FORTRANIO_H_VERSION \
  "TF_   V1.5   "

#include<tfxx/bytesex.h>
#include<fstream>
#include<sstream>

namespace tfxx {

/*! \defgroup group_fortranio FORTRAN I/O functions
 * \brief FORTRAN file data input and output functions
 *
 * \since November 2002
 *
 * The functions in this group serve mainly to read FORTRAN binary data files
 * into C++ variables or arrays. There may be also writing functions and
 * functions for FORTRAN formatted ASCII files added.
 * This module makes extensive use of the \ref group_ioswap
 *
 * I/O is most easily accomplished by using the overloaded versions of the I/O
 * \ref fortranio_opin "input" and \ref fortranio_opout "output"
 * operators. For I/O of complex types specialized versions are presented in
 * complexio.h. These operators are placed in the global namespace.
 *
 * \anchor anchor_fortranio_firsttest
 * \par Test of I/O-routines
 * The I/O routines where testet on 20/11/2002. The test code may be found in
 * tests/fortraniotest.cc, which is documented in \ref example_fortraniotest,
 * and in \ref anchor_fortranio_f77code "tests/fortranF77.f", which is the
 * Fortran 77 part of the test module. The Fortran part was compiled and run
 * on two systems:
 *
 *   -# \c AIX \c geo31 \c 3 \c 4 \c 002030455700 (Motorola CPU)
 *   -# \c Linux \c geo19 \c 2.4.4-64GB-SMP \c #1 \c SMP \c i686 \c unknown
 *      (Intel CPU)
 *
 * The C++ part of the test was only compiled on the Linux machine. The test
 * was successfully passed for magic numbers, I/O and byteswapping I/O
 * (from/to Fortran77/C++ and Motorola/Intel) performed with the following C++
 * types:
 *
 *   -# int
 *   -# long int
 *   -# long long int
 *   -# float
 *   -# double
 *   -# complex<float>
 *   -# complex<double>
 *
 * \sa example_fortraniotest
 * \sa tfxx::fortranio::FortranBinOutput& operator<<(tfxx::fortranio::FortranBinOutput&, T&)
 * \sa tfxx::fortranio::FortranBinInput& operator>>(tfxx::fortranio::FortranBinInput&, const T&)
 */

/*! \brief Interface provided through fortranio.h
 * \defgroup fortranio_h Interface provided through fortranio.h
 * \ingroup group_fortranio, group_ioswap
 */

/*! \brief contains all FORTRAN file data input output functions
 * \ingroup group_fortranio, fortranio_h
 * \sa example_fortraniotest
 * \sa group_fortranio
 */
namespace fortranio {

/*! \brief read FORTRAN binary data
 * \ingroup group_fortranio, fortranio_h
 *
 * This class reads FORTRAN binary data. FORTRAN binary data is always written
 * in chunks of several bytes, where each chunk is preceded and followed by a
 * 4-byte integer byte-count. This class takes care of this structure of
 * byte-counts an returns only the real data (omitting the counts).
 *
 * No buffering is done, because we rely on stream-buffering.
 *
 * By creating the object the reading process is initialized.
 * For convenient access I/O operators are provided. 
 *
 * No checking for block alignment is performed. This means, the class does
 * not complain in case the first three bytes of a double are at the end of
 * one block and the next five bytes are at the beginning of the next block.
 * The class just ensures that block byte-counts are skipped.
 *
 * \sa example_fortraniotest
 * \sa tfxx::fortranio::FortranBinInput
 * \sa tfxx::fortranio::FortranBinInput& operator>>(tfxx::fortranio::FortranBinInput&, const T&)
 * \
 */
class FortranBinInput {
  public:
    //! start reading from input stream \c is
    FortranBinInput(std::istream& is, const bool& swap=false):
      Mistream(is), Mswap(swap), Mnbytes(0), Mnremain(0) { }
    //! needs a cleaning destructor
    ~FortranBinInput() { this->finish_block(); }
    //! are there more data expected?
    bool more() const 
    { return((Mnbytes>0) || Mistream.good()); }
    /*! extract a value (perform swapping if requested)
     *
     * Pass by reference argument because overloading by return type is not
     * possible.
     *
     * \param value value of type \c T read from stream
     */
    template<typename T> void get(T& value);
    //! expect magic number and adjust swapping flag
    bool match_magic(const char* cmagic);

  private:
    //! return next element from input stream
    char extract_next_char();
    //! extract next set of characters
    void extract_chars(char* buf, const int& n);
    //! read next block size
    void read_block_size();
    //! finish open block
    void finish_block();

    //! The input stream to read from
    std::istream& Mistream;
    //! Has byte swapping to be performed
    bool Mswap;
    //! Number of bytes expected in current block
    long int Mnbytes;
    //! Number of bytes still to read from current block
    long int Mnremain;
};

/*! \brief write FORTRAN binary data
 * \ingroup group_fortranio, fortranio_h
 *
 * This class writes FORTRAN binary data. FORTRAN binary data is always written
 * in chunks of several bytes, where each chunk is preceded and followed by a
 * 4-byte integer byte-count. This class takes care of this structure of
 * byte-counts.
 *
 * For convenient use we must buffer the output data. The user just pushes
 * data to the stream and calls function end_block() after each data block. We
 * will then write this block with a preceding and following byte-count. 
 * This implies the overhead string-streams introduce to this problem. However
 * file I/O is not the part where we emphasize runtime efficiency.
 *
 * By creating the object the writing process is initialized.
 * Only basic put-functions (for char type) are provided. Use I/O operators
 * for convenient access.
 *
 * \sa example_fortraniotest
 * \sa tfxx::fortranio::FortranBinInput
 * \sa tfxx::fortranio::FortranBinOutput& operator<<(tfxx::fortranio::FortranBinOutput&, T&)
 */
class FortranBinOutput {
  public:
    //! start writing to output stream \c os
    FortranBinOutput(std::ostream& os):
      Mostream(os), Mbuffer(std::string()) { }
    //! we need a flushing destructor
    ~FortranBinOutput() { this->end_block(); }
    //! put a value to the output
    template<typename T>
      void put(const T& value);
    //! finish data block
    void end_block();
    //! write magic number (to separate block)
    void write_magic(const char* cmagic);

  private:
    //! put the next char to the output
    void put_char(const char& c);
    //! put some chars to the output
    void put_chars(const char* buf, const int& n);

    //! The output stream to write to
    std::ostream& Mostream;
    //! The buffering is done by a stringstream
    std::ostringstream Mbuffer;
};

/*! \example tests/fortranF77.f
 * \anchor anchor_fortranio_f77code
 *
 * You will find and example for the module fortranio in
 * tests/fortraniotest.cc and tests/fortranF77.f.
 *
 * \sa tfxx::fortranio::FortranBinInput
 * \sa tfxx::fortranio::FortranBinOutput
 * \sa group_fortranio
 * \sa example_fortraniotest
 */

/*! \example tests/fortraniotest.cc
 *
 * You will find and example for the module fortranio in
 * tests/fortraniotest.cc and tests/fortranF77.f.
 *
 * \sa tfxx::fortranio::FortranBinInput
 * \sa tfxx::fortranio::FortranBinOutput
 * \sa example_fortraniotest
 * \sa group_fortranio
 */

/*----------------------------------------------------------------------*/
// definition of member function templates
template<typename T> 
  void FortranBinOutput::put(const T& value)
{
  tfxx::ioswap::IOUnion<T> buf;
  buf.value=value;
  put_chars(buf.bytes, sizeof(T));
}

template<typename T> 
  void FortranBinInput::get(T& value)
{
  tfxx::ioswap::IOUnion<T> buf;
  extract_chars(buf.bytes, sizeof(T));
  value=buf.value;
  if (Mswap) { value=tfxx::ioswap::swap(buf.value); }
}

/*----------------------------------------------------------------------*/

/*! \brief Output operator template for class FortranBinOutput
 * \ingroup group_fortranio, fortranio_h
 *
 * The compiler will consider namespace tfxx::fortranio due to the involved
 * FortranBinOutput object.
 * There is 
 * \ref anchor_blitzfortranio_output "another output operator template"
 * to write raw Blitz++ arrays.
 *
 * \anchor fortranio_opout
 */
template<typename T>
  tfxx::fortranio::FortranBinOutput& 
    operator << (tfxx::fortranio::FortranBinOutput& fo, const T& value)
{
  fo.put(value);
  return(fo);
}

/*! \brief Input operator template for class FortranBinInput
 * \ingroup group_fortranio, fortranio_h
 *
 * The compiler will consider namespace tfxx::fortranio due to the involved
 * FortranBinInput object.
 * There is 
 * \ref anchor_blitzfortranio_input "another input operator template"
 * to read raw Blitz++ arrays.
 *
 * \anchor fortranio_opin
 */
template<typename T>
  tfxx::fortranio::FortranBinInput& 
    operator >> (tfxx::fortranio::FortranBinInput& fi, T& value)
{
  fi.get(value);
  return(fi);
}

} // namespace fortranio

} // namespace tfxx

#endif // TF_FORTRANIO_H_VERSION (includeguard)

/* ----- END OF fortranio.h ----- */
