/*! \file fcommand.cc
 * \brief process filter commands (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/01/2003
 * 
 * process filter commands (implementation)
 * 
 * Copyright (c) 2003 by Thomas Forbriger (IMG Frankfurt) 
 *
 * ----
 * libfourier is free software; you can redistribute it and/or modify
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
 * 
 * REVISIONS and CHANGES 
 *  - 05/01/2003   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_FCOMMAND_CC_VERSION \
  "TF_FCOMMAND_CC   V1.0   "

#include <fourier/fcommand.h>
#include <fstream>
#include <string>
#include <sstream>
#include <fourier/error.h>

using std::cout;
using std::endl;

namespace fourier {

  void FilterCommands::FilterCommands::help(std::ostream& os,
                                            const bool& verbose) 
  {
    os << "filter library commands:" << endl;
    os << "rem            any comment (ignored)" << endl;
    os << "#              any comment (ignored)" << endl;
    os << "dif            differentiate" << endl;
    os << "int            integrate" << endl;
    os << "lp2 in h       second order low-pass" << endl;
    os << "hp2 in h       second order high-pass" << endl;
    os << "lpb in ord     "
      << "Butterworth low-pass of order ord" << endl;
    os << "hpb in ord     "
      << "Butterworth high-pass of order ord" << endl;
    os << "fac factor     mutiply by factor" << endl;
    os << "mod normal     "
      << "switch back to normal filters (default)" << endl;
    os << "mod inverse    switch to inverse filters" << endl;
    os << "mod period     "
      << "switch to specification by period (default)" << endl;
    os << "mod frequency  switch to specification by frequency " << endl;
    os << "end            terminate file reading" << endl;
    os << endl;
    os << "\"in\" specifies the filter freqeuncy. It is either given" << endl;
    os << "as a frequency (Hz) or as a period (s) depending on the" << endl;
    os << "selected mode." << endl;
    if (verbose)
    {
      os << endl;
      os << TF_FCOMMAND_H_VERSION << endl;
      os << TF_FILTERS_H_VERSION << endl;
      os << TF_POLESNZEROES_H_VERSION << endl;
    }
  }

  /*----------------------------------------------------------------------*/

  void FilterCommands::read(const char* filename, const bool& verbose)
  {
    std::ifstream ifs(filename);
    read(ifs, verbose);
  }

  /*----------------------------------------------------------------------*/

  void FilterCommands::read(std::istream& is, const bool& verbose)
  {
    bool hot=is.good();
    std::string line;
    while(hot) 
    {
      std::getline(is, line);
      hot=command(line.c_str(), verbose) && is.good();
    }
  }

  /*----------------------------------------------------------------------*/

  bool FilterCommands::command(const char* command, const bool& verbose)
  {
    if (verbose) cout << " set filter: " << command << endl;
    bool result=true;
    std::string cmd(command);
    std::istringstream cmdline(cmd);
    std::string token;
    cmdline >> token;
    double par, h;
    int ord;
    if (token.find("rem")!=std::string::npos)
    { } else if (token.find("#")!=std::string::npos)
    { } else if (token.find("dif") != std::string::npos)
    { setdif();
    } else if (token.find("int") != std::string::npos)
    { setint();
    } else if (token.find("lp2") != std::string::npos)
    { cmdline >> par >> h; setlp2(par, h);
    } else if (token.find("lpb") != std::string::npos)
    { cmdline >> par >> ord; setlpb(par, ord);
    } else if (token.find("hp2") != std::string::npos)
    { cmdline >> par >> h; sethp2(par, h);
    } else if (token.find("hpb") != std::string::npos)
    { cmdline >> par >> ord; sethpb(par, ord);
    } else if (token.find("fac") != std::string::npos)
    { cmdline >> par; numfactor(par);
    } else if (token.find("mod") != std::string::npos)
    { 
      cmdline >> token;
      if (token.find("frequency")!=std::string::npos)
      { setfreqmod();
      } else if (token.find("period")!=std::string::npos)
      { setpermod();
      } else if (token.find("normal")!=std::string::npos)
      { setnormal();
      } else if (token.find("inverse")!=std::string::npos)
      { setinverse();
      } else
      { FOURIER_abort("ERROR (FilterCommands): illegal mode!"); }
    } else if (token.find("end") != std::string::npos)
    { result=false; 
    } else 
    { FOURIER_abort("ERROR (FilterCommands): illegal command!"); }
    return(result);
  }

}  // namespace fourier

/* ----- END OF fcommand.cc ----- */
