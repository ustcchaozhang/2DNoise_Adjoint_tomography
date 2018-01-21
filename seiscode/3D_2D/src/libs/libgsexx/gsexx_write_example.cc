/*! \file gsexx_write_example.cc
 * \brief Example for writing GSE data
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 25/01/2006
 * 
 * Example for writing GSE data
 * 
 * Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 25/01/2006   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define GSEXX_WRITE_EXAMPLE_VERSION \
  "GSEXX_WRITE_EXAMPLE   V1.0   Example for writing GSE data"
#define GSEXX_WRITE_EXAMPLE_CVSID \
  "$Id$"

#include<string>
#include<fstream>
#include<iostream>
#include<gsexx.h>

using std::cout;
using std::cerr;
using std::endl;

// name of example data file
const char* examplefile="example.gse";

int main(int iargc, char* argv[])
{
  cout << GSEXX_WRITE_EXAMPLE_VERSION << endl;
  cout << GSEXX_WRITE_EXAMPLE_CVSID << endl;
  cout << endl;
  cout << "This program provides an example of basic writing to" << endl
       << "GSE data files. The program will write two traces" << endl
       << "of CM6 encoded data to a file called '" 
       << examplefile << "'." << endl;
  cout << endl;

  // open output file
  std::ofstream os(examplefile); 

  // write first time series
  {
    // number of samples to be written
    const int nsamples=2000; 

    // prepare WID2 line
    GSE2::waveform::TWID2 wid2line; 
    wid2line.Fyear=2006; 
    wid2line.Fmonth=1; 
    wid2line.Fday=25; 
    wid2line.Fhour=10; 
    wid2line.Fminute=30; 
    wid2line.Fseconds=12.456789; 
    wid2line.Fsamps=nsamples; 
    wid2line.Fsamprate=20.; 
    wid2line.Fstation="BFO"; 
    wid2line.Finstype="STS2"; 
    wid2line.Fchannel="BHZ"; 
    wid2line.Fauxid="any"; 
    wid2line.Fvang=0.; 
    wid2line.Fhang=90.; 
    wid2line.Fcalib=1.e5; 
    wid2line.Fcalper=10.; 
    wid2line.Fsubformat=GSE2::waveform::SF_CM6; 
      
    // report WID2 line
    std::cout << wid2line.line();

    // write WID2 line to data file
    os << wid2line.line();

    // create CM6 encoding data writer for appropriate number of samples
    GSE2::waveform::TDAT2writeCM6 writer(nsamples);

    // write data
    int i=0;
    while (writer.hot())
    {
      // calculate synthetic sample
      int sample=int(1.e5*std::sin(i*2.*3.141592653*5./(nsamples-35)));
      // output sample to data file
      os << writer(sample);
      i++;
    }
  }
    
  // write second time series
  {
    // number of samples to be written
    const int nsamples=2300; 

    // prepare WID2 line with the bare minimum of information required
    GSE2::waveform::TWID2 wid2line; 
    wid2line.Fsamps=nsamples; 
    wid2line.Fsamprate=1.; 
    wid2line.Fsubformat=GSE2::waveform::SF_CM6; 

    // report WID2 line
    std::cout << wid2line.line();

    // write WID2 line to data file
    os << wid2line.line();

    // create CM6 encoding data writer for appropriate number of samples
    GSE2::waveform::TDAT2writeCM6 writer(nsamples);

    // write data
    int i=0;
    while (writer.hot())
    {
      // calculate synthetic sample
      int sample=int(1.2e5*(std::sin(i*2.*3.141592653*5./(nsamples-35))
                          +std::cos(i*2.*3.141592653*5.5/(nsamples-35))));
      // output sample to data file
      os << writer(sample);
      i++;
    }
  }

  // that's it!
  cout << "data file is written..." << endl;
  
} // main

/* ----- END OF gsexx_write_example.cc ----- */
