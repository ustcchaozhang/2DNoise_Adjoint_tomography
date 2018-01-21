/*! \file blitztest.cc
 * \brief test all blitz support modules
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/11/2002
 * 
 * test all blitz support modules
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
 *  - 28/11/2002   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define BLITZTEST_VERSION \
  "BLITZTEST   V1.0   test all blitz support modules"

#include <iostream>
#include <blitz/array.h>
#include <tfxx/error.h>
#include <tfxx/commandline.h>
#include <tfxx/blitzutil.h>
#include <tfxx/blitzfortranio.h>

using std::cout;
using std::cerr;
using std::endl;

    
/*! \defgroup example_blitz TEST: Blitz array reading and writing
 *  
 */ 

/*! \brief test blitz binary I/O operators
 * \ingroup example_blitz
 *
 */
int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    BLITZTEST_VERSION "\n"
    "usage: blitztest [-v] [-if file] [-of file]" "\n"
    "   or: blitztest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "-v           be verbose" "\n"
    "-of file     write output to \"file\"" "\n"
    "-if file     read input from \"file\"" "\n"
    BLITZTEST_CVSID
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: output file
    {"of",arg_yes,"junk"},
    // 3: input file
    {"if",arg_yes,"junk"},
    {NULL}
  };

  // no arguments? print usage...
  if (iargc<1) 
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
  std::string outfile=cmdline.string_arg(2);
  std::string infile=cmdline.string_arg(3);

  if (verbose)
  {
    cout << BLITZTEST_VERSION << endl << endl
      << "Creating arrays:" << endl;
  }

  blitz::Array<float, 2> A(blitz::Range(3,6),blitz::Range(1,3),
                           blitz::fortranArray);
  blitz::Array<float, 2> B(blitz::Range(3,6),blitz::Range(1,3));

  {
    blitz::firstIndex i;
    blitz::secondIndex j;
    A=100+10*i+j;
    B=200+10*i+j;
  }

  cout << endl << "Fortran array: " << endl
               << "==============" << endl << A << endl;
  A.dumpStructureInformation();

  cout << endl << "C array: " << endl
               << "========" << endl << B << endl;
  B.dumpStructureInformation();

  if (verbose) cout << endl << "writing to " << outfile << "..." << endl;
  {
    std::ofstream os(outfile.c_str());
    tfxx::fortranio::FortranBinOutput fo(os);
    fo << A << B;
  }

  blitz::Array<float, 2> C,D,E(blitz::fortranArray),F(blitz::fortranArray);
  
  if (verbose) cout << endl << "reading from " << infile 
    << " into C arrays..." << endl; 
  try
  {
    std::ifstream is(infile.c_str());
    tfxx::fortranio::FortranBinInput fi(is);
    fi >> C >> D;
  }
  catch (tfxx::error::Exception)
  {
    cout.flush();
    TFXX_abort("sorry...");
  }

  if (verbose) cout << endl << "reading from " << infile 
    << " into Fortran arrays..." << endl;
  {
    std::ifstream is(infile.c_str());
    tfxx::fortranio::FortranBinInput fi(is);
    fi >> E >> F;
  }

  cout << endl << "C array from Fortran array:"  << endl
               << "===========================" << endl << C << endl;
  C.dumpStructureInformation();

  cout << endl << "C array from C array:"  << endl
               << "=====================" << endl << D << endl;
  D.dumpStructureInformation();

  cout << endl << "Fortran array from Fortran array:" << endl
               << "=================================" << endl << E << endl;
  E.dumpStructureInformation();

  cout << endl << "Fortran array from C array:" << endl
               << "===========================" << endl << F << endl;
  F.dumpStructureInformation();

  /*----------------------------------------------------------------------*/

  cout << endl << "Test shape copy function" << endl
               << "========================" << endl;

  blitz::Array<int,3> G(blitz::Range(3,4),
                        blitz::Range(3,5),
                        blitz::Range(2,4),
                        blitz::fortranArray);
  {
    blitz::firstIndex i;
    blitz::secondIndex j;
    blitz::thirdIndex k;
    G=i*100+j*10+k;
  }

  cout << endl << "Fortran array for type int:" << endl
               << "---------------------------" << endl << G << endl;
  G.dumpStructureInformation();

  blitz::Array<std::complex<double>,3> H;
  tfxx::blitzutil::setToSameDomain(H,G.domain());

  cout << endl << "Array of same domain and type complex<double>:" << endl
               << "----------------------------------------------" 
               << endl << H << endl;
  H.dumpStructureInformation();

  TFXX_assert(tfxx::blitzutil::sameDomain(G,H),
              "setToSameDomain failed!");
}

/* ----- END OF blitztest.cc ----- */
