/*! \file gsexx_tests.cc
 * \brief test cases for GSE++ module (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 25/03/2002
 * 
 * test cases for GSE++ module (implementation)
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
 *  - 25/03/2002   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_GSEXX_TESTS_CC_VERSION \
  "TF_GSEXX_TESTS_CC   V1.0   "
#define TF_GSEXX_TESTS_CC_CVSID \
  "$Id$"

#include<fstream>
#include<cmath>
#include<cstdlib>
#include<iostream>
#include<gsexx.h>

//! Macro that provides a tool for verbose coding.
/*! \ingroup tests
 * \internal
 *
 * The argument has to be a valid line of code. The macro expands in two
 * lines. The first line prints the code. The second executes the code.
 */
#define TESTCODE( ARG ) cout << #ARG << endl; ARG

namespace GSE2 {

//! Functions testing the GSE2 module.
/*! \namespace GSE2::tests
 * \internal
 * \ingroup tests
 * \defgroup tests Test functions for module GSE.
 */
namespace tests {

/*======================================================================*/
/*
 * Test first and second differences
 * ---------------------------------
 */

//! Test the template metaprograms that apply and remove differences.
/*! \ingroup tests
 * \internal
 * \sa GSE2::waveform::apply1stdiffT
 * \sa GSE2::waveform::apply2nddiffT
 * \sa GSE2::waveform::remove1stdiffT
 * \sa GSE2::waveform::remove1stdiffT
 */
void test_diff()
{
  std::cout << std::endl;
  std::cout 
    << "Test the template metaprograms that apply and remove differences."
    << std::endl;
      
  // test values
  int values[] = { 1,3,5,7,3,-2,-4,6 }; 
  // number of test values
  const int nval=8;
  // declare differencers
  waveform::apply2nddiffT diff2nd;
  waveform::apply1stdiffT diff1st;
  // declare inverse operators
  waveform::remove2nddiffT rem2nd;
  waveform::remove1stdiffT rem1st;
  for (int i=0; i<nval; i++)
  {
    // apply 1st differences
    int v1st=diff1st(values[i]); 
    // apply 2nd differences
    int v2nd=diff2nd(values[i]); 
    // remove 1st differences
    int v1strem=rem1st(v1st); 
    // remove 2nd differences
    int v2ndrem=rem2nd(v2nd); 
    // print results
    std::cout.width(3); std::cout << i          << "   " << "value=";
    std::cout.width(3); std::cout << values[i]  << "   " << "1st diff=";
    std::cout.width(3); std::cout << v1st       << "   " << "1st rem=";
    std::cout.width(3); std::cout << v1strem    << "   " << "2nd diff=";
    std::cout.width(3); std::cout << v2nd       << "   " << "2nd rem=";
    std::cout.width(3); std::cout << v2ndrem    << " ";
    std::cout << std::endl;
  }
} // function test_diff

/*======================================================================*/
/*
 * Test checksum class
 * -------------------
 */

//! Test the TCHK2 class.
/*! \ingroup tests
 * \internal
 * \sa GSE2::waveform::TCHK2
 */
void test_chk2()
{
  std::cout << std::endl;
  std::cout << "Test the checksum algorithm."
    << std::endl;
      
  // number of test values
  const int nval=30;
  // declare checksum object
  waveform::TCHK2 checksum;

  for (int j=0; j<3; j++)
  {
    waveform::intT val=1;
    for (int i=0; i<nval; i++)
    {
      checksum.add(val);
      // print results
      std::cout.width(3); std::cout << j << ",";
      std::cout.width(2); 
      std::cout << i                   << "   " << "value=";
      std::cout.width(11); 
      std::cout << val                 << "   " << "checksum=";
      std::cout.width(10); std::cout << checksum.value()    << std::endl;
      // increase sample value in powers of two
      val*=2;
    }
  }

  std::cout << checksum.write();
} // function test_chk2

/*======================================================================*/
/*
 * Test DAT2 class CM6 subformat
 * -----------------------------
 */
void test_dat2cm6()
{
  std::cout << std::endl;
  std::cout << "Test the DAT2 class with CM6 subformat."
    << std::endl;
  std::cout << "1. write waveform"
    << std::endl;

  const int msamples=2000;
  int data[msamples];
  int indata[msamples];
  for (int i=0; i<msamples; i++)
  {
    data[i]=int(1.e5*std::sin(i*2.*3.141592653*5./(msamples-35)));
    indata[i]=0;
  }

  GSE2::waveform::TWID2 wid2line;
  wid2line.Fsamps=msamples;
  wid2line.Fsamprate=1.;
  wid2line.Fstation="BFO";
  wid2line.Finstype="STS-2";
  wid2line.Fchannel="BHZ";
  std::cout << wid2line.line();

  GSE2::waveform::TDAT2writeCM6 writer(msamples);
  int i=0;
  while (writer.hot())
  {
    if (i >= msamples)
    {
      std::cerr << "ERROR: missed last sample!" << std::endl;
      abort();
    }
    std::cout << writer(data[i]);
    i++;
  }
 
  // write to file
  {
    std::ofstream os("junk.dat");
    os << wid2line.line();
    GSE2::waveform::TDAT2writeCM6 fwriter(msamples);
    int i=0;
    while (fwriter.hot())
    {
      if (i >= msamples)
      {
        std::cerr << "ERROR: missed last sample!" << std::endl;
        abort();
      }
      os << fwriter(data[i]);
      i++;
    }
  }

  {
    char c;
    std::cerr << "manipulate junk.dat - if you like to..." << std::endl;
    std::cin.get(c);
  }

  GSE2::waveform::TWID2 newwid2line;
  newwid2line.Fstation="STA";
  newwid2line.Fchannel="CHA";
  newwid2line.Fauxid="AUX";
  newwid2line.Fyear=1;
  newwid2line.Fmonth=2;
  newwid2line.Fday=3;

  // read from file
  {
    std::ifstream is("junk.dat");
    GSE2::waveform::TDAT2readCM6 freader(msamples);
    newwid2line.read(is);
    int i=0;
    while (freader.hot())
    {
      if (i >= msamples)
      {
        std::cerr << "ERROR: missed last sample!" << std::endl;
        abort();
      }
      indata[i] =freader(is);
      i++;
    }
  }
 
  // write to second file
  {
    std::ofstream os("junk2.dat");
    os << newwid2line.line();
    GSE2::waveform::TDAT2writeCM6 fwriter(msamples);
    int i=0;
    while (fwriter.hot())
    {
      if (i >= msamples)
      {
        std::cerr << "ERROR: missed last sample!" << std::endl;
        abort();
      }
      os << fwriter(indata[i]);
      i++;
    }
  }
      
} // test_dat2cm6
  
} // namespace tests
} // namespace GSE2

/*======================================================================*/

//! Main program calling all test functions.
/*! \ingroup tests
 * \internal
 */
int main()
{
  std::cout << "This is a program to test the GSE2-module components." <<
    std::endl;

  GSE2::Terror::silent=true;
  try {
    // test differences
    GSE2::tests::test_diff();
    // test checksums
    GSE2::tests::test_chk2();
    // test dat2 subformat cm6
    GSE2::tests::test_dat2cm6();
  } catch (GSE2::Terror& e) {
    std::cerr << e.message() << std::endl;
  }
}

/* ----- END OF gsexx_tests.cc ----- */
