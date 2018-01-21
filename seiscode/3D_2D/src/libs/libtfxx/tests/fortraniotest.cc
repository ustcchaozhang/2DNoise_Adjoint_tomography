/*! \file fortraniotest.cc
 * \brief C++ part of the Fortran I/O test routines
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 15/11/2002
 * 
 * C++ part of the Fortran I/O test routines
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
 *  - 15/11/2002   V1.0   Thomas Forbriger
 *  - 20/11/2002   V1.1   passed tests on AIX and Intel Linux
 *  - 19/07/2005   V1.2 
 *                        - use bytesex.h now
 *                        - removed struct IOTsize; this is obsolete, since
 *                          I learned that the sizeof function is a
 *                          compile-time literal
 *                        - removed check_assumed_size() and CHECK_MACRO()
 *                          for the same reason
 * 
 * ============================================================================
 */
#define FORTRANIOTEST_VERSION \
  "FORTRANIOTEST   V1.2   C++ part of the Fortran I/O test routines"

#include <iostream>
#include <cassert>
#include <tfxx/commandline.h>
#include <tfxx/fortranio.h>
#include <tfxx/complexio.h>

using std::cout;
using std::cerr;
using std::endl;

/*! \defgroup example_fortraniotest TEST: Fortran I/O and byte swapping.
 *
 * @{
 */
/*----------------------------------------------------------------------*/
/*! \brief Function template to test dry byte swapping.
 * \ingroup example_fortranio
 *
 * \param T generic type to be used during test
 *
 * This function tages no argument and returns nothing. All output will be
 * written to stdout.
 *
 * \sa group_ioswap
 */
template<class T>
void test_swap()
{
  tfxx::ioswap::IOUnion<T> u1, u2; 
  T v1,v2;
  const int nbytes=sizeof(T);
  char charpool[]="ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  char charresult[26];
  assert(nbytes<26);
  for (int i=0; i<nbytes; i++)
  { 
    u1.bytes[i]=charpool[i];
    charresult[i]=charpool[i];
  }
  charresult[nbytes]='\0';
  cout << "in: ";
  cout.width(10);
  cout << charresult;
  // take the long way (using these types explicitely)
  v1=u1.value;
  v2=tfxx::ioswap::swap(v1);
  u2.value=v2;
  for (int i=0; i<nbytes; i++)
  { 
    charresult[i]=u2.bytes[i];
  }
  charresult[nbytes]='\0';
  cout << " out: ";
  cout.width(12);
  cout << charresult << endl;
}

/*! \brief Macro function to preform easy to use swap check.
 * \ingroup example_fortranio
 *
 * \param T type to be used
 */
#define SWAPIT( T ) \
  cout.width(40); \
  cout << "  swap test with \'" #T "\': "; \
  test_swap<T>();

/*! \brief Ask for CPU type and print result
 * \ingroup example_fortranio
 */
void cpu_type()
{
  switch (tfxx::ioswap::cpu()) {
    case tfxx::ioswap::cpu_Intel:
      cout << "We are running on an Intel type CPU" << endl;
      break;
    case tfxx::ioswap::cpu_Motorola:
      cout << "We are running on a Motorola type CPU" << endl;
      break;
    case tfxx::ioswap::cpu_unknown:
      cout << "The type of this CPU is unknown" << endl;
      break;
    default:
      cerr << "ERROR: illegal return value from " <<
        "tfxx::ioswap::cpu()" << endl;
      abort();
  }
}

const char mymagic[]="ABCD";

/*! \brief write test data
 * \ingroup example_fortranio
 */
void write_data(const std::string& name, const bool& verbose=false)
{
  if (verbose) cout << "write data to \'" << name << "\'" << endl;
  std::ofstream os(name.c_str());
  tfxx::ioswap::file_magic_write(os, mymagic, true);

  tfxx::fortranio::FortranBinOutput fo(os);
  const int nval=10;
  fo << nval;
  cout << "nval: " << nval << endl;
  for (int i=0; i<nval; i++)
  { 
    int val1=(i+1)*4;
    int val2=100*val1;
    fo << val1 << val2; 
    cout << "  val1/2: " << val1 << ", " << val2 << endl;
  }
  fo.end_block();
  for (int i=0; i<nval; i++)
  { 
    double val=(i+1)*15.; 
    cout << "  val: " << val << endl;
    fo << val; 
  }
  fo.end_block();
  long long int llint=1551;
  long int lint=2662;
  std::complex<double> dcplx(4.5,5.4);
  std::complex<float> scplx(14.5,15.4);
  fo << llint << lint << dcplx << scplx;
  cout << "extra: " << llint << ", " << lint << ", " 
    << dcplx << ", " << scplx << endl;;
}

/*! \brief read test data
 * \ingroup example_fortranio
 */
void read_data(const std::string& name, const bool& verbose=false)
{
  if (verbose) cout << "read data from \'" << name << "\'" << endl;
  std::ifstream is(name.c_str());
  tfxx::ioswap::Emagic_type 
    match=tfxx::ioswap::file_magic_test(is, mymagic, true);
  if (verbose) {
    switch(match) {
      case tfxx::ioswap::magic_match:
        cout << "Bytesex matches" << endl;
        break;
      case tfxx::ioswap::magic_swap:
        cout << "Magic number found, byte data has to be swapped" << endl;
        break;
      case tfxx::ioswap::magic_nomatch:
        cout << "Magic number not found" << endl;
        break;
      default:
        cerr << "ERROR: illegal return value from " <<
          "tfxx::ioswap::file_magic_test()" << endl;
        break;
    }
  }
  if ((match==tfxx::ioswap::magic_match)
      || (match==tfxx::ioswap::magic_swap))
  {
    tfxx::fortranio::FortranBinInput fi(is, (match==tfxx::ioswap::magic_swap));
    int nval;
    fi >> nval;
    cout << "nval: " << nval << endl;
    for (int i=0; i<nval; i++)
    { 
      int val1,val2;
      fi >> val1 >> val2; 
      cout << "  val1/2: " << val1 << ", " << val2 << endl;
    }
    for (int i=0; i<nval; i++)
    { 
      double val;
      fi >> val; 
      cout << "  val: " << val << endl;
    }
    long long int llint;
    long int lint;
    std::complex<double> dcplx;
    std::complex<float> scplx;
    fi >> llint >> lint >> dcplx >> scplx;
    cout << "extra: " << llint << ", " << lint << ", " 
      << dcplx << ", " << scplx << endl;;
  }
}

/*----------------------------------------------------------------------*/
/*! \brief Main program.
 *
 * This code tests reading and writing of binary data. And it performs some
 * tests on the byte swapping facilities.
 *
 * \sa group_ioswap
 * \sa group_fortranio
 */
int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    FORTRANIOTEST_VERSION "\n"
    "usage: fortraniotest [-v] [-c] [-s] [-C]" "\n"
    "                     [-w [file]] [-r [file]]" "\n"
    "   or: fortraniotest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    " " "\n"
    "-v           be verbose" "\n"
    "-c           check byte size of data types" "\n"
    "-s           test byte swapping" "\n"
    "-C           tell us about the CPU model" "\n"
    "-w [file]    write to file" "\n"
    "-r [file]    read from file" "\n"
    FORTRANIOTEST_CVSID
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: check byte size
    {"c",arg_no,"-"},
    // 3: test byte swapping
    {"s",arg_no,"-"},
    // 4: tell about CPU
    {"C",arg_no,"-"},
    // 5: write to file
    {"w",arg_opt,"junk"},
    // 6: write to file
    {"r",arg_opt,"junk"},
    {NULL}
  };

  // no arguments? print usage...
  if (iargc<2) 
  {
    cerr << usage_text << endl;
    exit(0);
  }

  // collect options from commandline
  Commandline cmdline(iargc, argv, options);

  // help requested? print full help text...
  if (cmdline.optset(0))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    exit(0);
  }

  bool verbose=cmdline.optset(1);
  bool check_size=cmdline.optset(2);
  bool byte_swapping=cmdline.optset(3);
  bool tell_CPU=cmdline.optset(4);
  bool write_file=cmdline.optset(5);
  std::string outfile=cmdline.string_arg(5);
  bool read_file=cmdline.optset(6);
  std::string infile=cmdline.string_arg(6);

  if (verbose) { cout << FORTRANIOTEST_VERSION << endl << endl; }
  if (check_size) 
  { 
    cout << "NOTICE: check_assumed_size() no longer exists!"
      << endl
      <<    "        and the even better news: there is no need for it"
      << endl;
  }
  if (byte_swapping) 
  {
    SWAPIT(char)
    SWAPIT(int)
    SWAPIT(long int)
    SWAPIT(long long int)
    SWAPIT(float)
    SWAPIT(double)
  }
  if (tell_CPU) cpu_type();
  if (write_file) write_data(outfile, verbose);
  if (read_file) read_data(infile, verbose);
}

/*!
 * @} end of example_fortraniotest
 */
/* ----- END OF fortraniotest.cc ----- */
