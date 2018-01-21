/*! \file mocox.cc
 * \brief convert gremlin1 model (modversion2)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 30/12/2002
 * 
 * convert gremlin1 model (modversion2)
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
 *  - 30/12/2002   V1.0   Thomas Forbriger
 *  - 31/12/2002   V1.1   started work on conversion to flnode model
 *  - 04/01/2003   V1.2   conversion to FLNODE works!
 * 
 * ============================================================================
 */
#define MOCOX_SHORT_VERSION \
  "MOCOX   V1.2"
#define MOCOX_VERSION \
  MOCOX_SHORT_VERSION "   convert gremlin1 model (modversion2)"
#define MOCOX_CVSID \
  "$Id$"

#include <iostream>
#include <stdio.h>
#include <cmath>
#include <fstream>
#include <gremlin1/polymodel.h>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <aff/array.h>
#include <aff/slice.h>
#include <aff/subarray.h>
#include <aff/iterator.h>
#include <aff/dump.h>
#include <rheology.h>

using std::cout;
using std::cerr;
using std::endl;

const char flnode_id[]="FLNODE";

typedef gremlin1::PolynomialModelFile::Tarray Tarray;
typedef gremlin1::PolynomialModelFile::Tiarray Tiarray;
typedef gremlin1::PolynomialModelFile::TCarray TCarray;

/*======================================================================*/
// functions
// =========

// create a complete set of nodes
void dump_nodes(const TCarray& nodes)
{
  cout << endl
    << "Converted model:" << endl;
  const int fieldwidth=12;
  char buffer[fieldwidth];

  // captions
  cout << "  ";
  cout.width(fieldwidth); cout << "depth (m)";
  cout.width(fieldwidth); cout << "v_P (km/s)";
  cout.width(fieldwidth); cout << "v_S (km/s)";
  cout.width(fieldwidth); cout << "rho (g/cm^3)";
  cout.width(fieldwidth); cout << "Q_P";
  cout.width(fieldwidth); cout << "Q_S";
  cout << endl;

  // values
  for (int i=nodes.f(0); i<=nodes.l(0); ++i)
  {
    cout << "  ";
    for (int j=nodes.f(1); j<=nodes.l(1); ++j)
    {
      std::snprintf(buffer, fieldwidth, "%10.4f", nodes(i,j));
      cout.width(fieldwidth);
      cout << buffer;
    }
    cout << endl;
  }

} // dump_nodes

/*----------------------------------------------------------------------*/

// create a complete set of nodes
Tarray convert_flnode(const TCarray& nodes, const bool& verbose)
{
  Tarray flnodes(nodes.size(0)+1,nodes.size(1));
  int nf=nodes.f(0);
  int nl=nodes.l(0);
  flnodes=0.;

  Tarray setflnodes=subarray(flnodes)(nf,nl);

  TCarray indepth =slice(nodes)()(1);
  TCarray invp    =slice(nodes)()(2);
  TCarray invs    =slice(nodes)()(3);
  TCarray inrho   =slice(nodes)()(4);
  TCarray inqp    =slice(nodes)()(5);
  TCarray inqs    =slice(nodes)()(6);

  Tarray fldepth =slice(setflnodes)()(1);
  Tarray flrho   =slice(setflnodes)()(2);
  Tarray flvp    =slice(setflnodes)()(3);
  Tarray flvs    =slice(setflnodes)()(4);
  Tarray flqk    =slice(setflnodes)()(5);
  Tarray flqm    =slice(setflnodes)()(6);

  aff::deepcopy(indepth,fldepth);
  aff::deepcopy(invp   ,flvp   );
  aff::deepcopy(invs   ,flvs   );
  aff::deepcopy(inrho  ,flrho  );

  // convert Q values
  for (int i=invp.f(0); i<=invp.l(0); ++i)
  {
    rheology::IsoFromVelocity moduli(invp(i), invs(i), inrho(i),
                                     inqp(i), inqs(i));
    flqk(i)=moduli.Qkappa();
    flqm(i)=moduli.Qmu();
  }

  // duplicate last line
  Tarray fllast=slice(flnodes)(flnodes.l(0));
  Tarray setfllast=slice(setflnodes)(setflnodes.l(0));
  aff::deepcopy(setfllast,fllast);

  return(flnodes);
} // convert_flnode

/*----------------------------------------------------------------------*/

// create a complete set of nodes
void write_flnode(const std::string& filename,
                  const TCarray& nodes,
                  const bool& verbose,
                  const std::string& sourcefile="unknown")
{
  Tarray flnodes=convert_flnode(nodes, verbose);
  std::ofstream ofs(filename.c_str());
  ofs << flnode_id << endl;
  ofs << "converted by " << MOCOX_SHORT_VERSION 
    << " from " << sourcefile << endl;
  const double reference_frequency=0.;
  const double earth_radius=6371.;
  const int damping_mode=1;     // 1 = no dispersion
  const int anisotropy_mode=0;  // 0 = isotropic model
  const int fieldwidth=12;
  char buffer[fieldwidth];
  std::snprintf(buffer, fieldwidth, "%10.4f", reference_frequency);
  ofs << buffer << " ";
  ofs << damping_mode << " ";
  ofs << anisotropy_mode << " ";
  std::snprintf(buffer, fieldwidth, "%10.4f", earth_radius);
  ofs << buffer << endl;
  ofs << flnodes.size(0) << endl;
  for (int i=flnodes.f(0); i<=flnodes.l(0); ++i)
  {
    ofs << "  ";
    for (int j=flnodes.f(1); j<=flnodes.l(1); ++j)
    {
      std::snprintf(buffer, fieldwidth, "%10.4f", flnodes(i,j));
      ofs.width(fieldwidth);
      ofs << buffer;
    }
    ofs << endl;
  }
} // write_flnode

/*----------------------------------------------------------------------*/

// create a complete set of nodes
Tarray create_nodes(const gremlin1::PolynomialModelFile& modelfile,
                    const bool& verbose)
{
  // create local alias
  int nsections=modelfile.nsections();
  TCarray means=modelfile.sectionmeans();

  /* scale factor
   * May be passed as function argument in future versions. 
   * For velocities it has the meaning of
   * 
   *    v*dl/l
   *
   * where v is some reference velocity (in km/s - the unit of gremlin1
   * files) and dl/l is the relative change of the vertical wavelength due to
   * inappropriate sampling.
   *
   * For other parameters v is a reference parameter value and dl/l is the
   * change of an integral property defined by the parameter (wavelength is 
   * velocity times period and wavelength variation is the integral over the
   * velocity residual times the period - the period cancels in dl/l). I doubt
   * that there might exist a physical meaning of dl/l for other parameters
   * than velocity.
   *
   * However this parameter is not critical. The sampling is mainly based on
   * the curvature parameter (second order polynomial coefficient) and the
   * scale parameter allows some fine-tuning if desired.
   */
  double scalefactor=0.01;

  if (verbose) cout << "  find appropriate number of nodes per section:" 
    << endl         << "    scale-factor: " << scalefactor << endl;
  
  // array to store first and last node index
  Tiarray ifirst(nsections);
  Tiarray ilast(nsections);
  // array to store number of node layers per section
  Tiarray nlay(nsections);
  nlay=1;
  int totalnodes=0;
  // find number of layers per section
  for (int i=1; i<=nsections; ++i)
  {
    if (verbose) cout << "    sec #" << i <<":";
    ifirst(i)=totalnodes+1;
    double d=modelfile.thickness(i);
    if (verbose) {
      cout << " d=";
      cout.width(8); cout.precision(4);
      cout << d << "m";
      cout << " z=";
      cout.width(8); cout.precision(4);
      cout << modelfile.bottom(i) << "m";
    }
    // all second order coefficients in this section
    TCarray c(aff::slice(means)(3)(i));
    for (int j=c.f(0); j<=c.l(0); ++j)
    {
      int n=1+static_cast<int>(d*sqrt(std::abs(c(j))/(3.*scalefactor)));
      nlay(i)= nlay(i)>n ? nlay(i) : n;
    }
    if (verbose) {
      cout << " nlay=";
      cout.width(3);
      cout << nlay(i);
    }
    
    // we need a node per layer and an extra node at each interface
    totalnodes += nlay(i)+1;
    ilast(i)=totalnodes;

    if (verbose) {
      cout << " first=";
      cout.width(3);
      cout << ifirst(i);
      cout << " last=";
      cout.width(3);
      cout << ilast(i);
    }
    if (verbose) cout << endl;
  }

  // fill result with depth and values
  // result array:
  Tarray result(totalnodes, gremlin1::PolynomialModelFile::nparameters+1);
  if (verbose) cout << "  define node depth and fill in values:" << endl;
  for (int i=1; i<=nsections; ++i)
  {
    double top=modelfile.top(i);
    double dz=modelfile.thickness(i)/nlay(i);
    for (int j=0; j<=nlay(i); ++j)
    {
      if (verbose) cout << "    sec #" << i <<":";
      // index of node
      int inode=ifirst(i)+j;
      // depth of node
      double depth=j*dz+top;
      result(inode,1)=depth;
      if (verbose) {
        cout << " node=";
        cout.width(3);
        cout << inode;
        cout << " depth=";
        cout.width(8);
        cout.precision(4);
        cout << depth << "m";
      }
      for (int k=result.f(1); k<result.l(1); ++k)
      {
        result(inode, 1+k)=modelfile.value(k,depth,i);
      }
      if (verbose) cout << endl;
    }
  }

  // adjust mean values in section
  if (verbose) cout << "  adjust mean values in sections:" << endl;
  for (int i=result.f(1); i<result.l(1); ++i)
  {
    if (verbose) {
      cout << "    parameter ";
      switch (i) {
        case 1: cout << "p-velocity"; break;
        case 2: cout << "s-velocity"; break;
        case 3: cout << "density"; break;
        case 4: cout << "Qp"; break;
        case 5: cout << "Qs"; break;
        default: cout << "ERROR!";
      }
      cout << endl;
    }
    for (int j=1; j<=nsections; ++j)
    {
      if (verbose) cout << "      sec #" << j <<":";
      // extract parameter values
      Tarray sec=aff::subarray(result)(ifirst(j),ilast(j))(i+1,i+1);
      double mean=0;
      for (int k=sec.f(0); k<=sec.l(0); ++k)
      { mean += sec(k); }
      mean *= 2.;
      mean -= sec(ifirst(j));
      mean -= sec(ilast(j));
      mean /= (2.*nlay(j));
      double polymean=means(1,j,i);
      double addvalue=polymean-mean;
      if (verbose) {
        cout << " mean=";
        cout.width(8);
        cout.precision(4);
        cout << mean;
        cout << " orig. mean=";
        cout.width(8);
        cout.precision(4);
        cout << polymean;
        cout << " adjust by:";
        cout.width(12);
        cout.precision(4);
        cout << (100.*addvalue/mean) << "%";
      }
      for (int k=sec.f(0); k<=sec.l(0); ++k)
      { sec(k) += addvalue; }
      if (verbose) cout << endl;
    }
  }

  return(result);
} // create_nodes

/*======================================================================*/
// main program
// ============

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    MOCOX_VERSION "\n"
    "usage: mocox file [-w file] [-v] [-dump] [-flnode file]" "\n"
    "                  [-nodes]" "\n"
    "   or: mocox --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "Read a gremlin1 model file and operate on it." "\n"
    "\n"
    "file         file name of model file" "\n"
    "\n"
    "-v           be verbose" "\n"
    "-dump        dump model to terminal" "\n"
    "-w file      write model to gremlin1 format \"file\"" "\n"
    "-flnode file convert to FLNODE model \"file\"" "\n"
    "-nodes       convert to nodes file and dump" "\n"
    "\n"
    MOCOX_CVSID
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: write polynomial model
    {"w",arg_yes,"-"},
    // 3: dump input model to terminal
    {"dump",arg_no,"-"},
    // 4: convert to FLNODE model
    {"flnode",arg_yes,"-"},
    // 5: dump input model to terminal
    {"nodes",arg_no,"-"},
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

  /*----------------------------------------------------------------------*/
  // extract options

  bool opt_verbose=cmdline.optset(1);
  bool opt_writeg1=cmdline.optset(2);
  std::string arg_writeg1=cmdline.string_arg(2);
  bool opt_dump=cmdline.optset(3);
  bool opt_flnode=cmdline.optset(4);
  std::string arg_flnode=cmdline.string_arg(4);
  bool opt_nodes=cmdline.optset(5);

  TFXX_assert(cmdline.extra(), "ERROR (mocox): missing file name!");
  std::string filename=cmdline.next();

  /*----------------------------------------------------------------------*/
  // read file

  if (opt_verbose) cout << "read model from \""
    << filename << "\"" << endl;
  gremlin1::PolynomialModelFile modelfile(filename.c_str());
  if (opt_dump)
  {
    cout << endl;
    cout << "Model values are:" << endl
      <<    "-----------------" << endl << endl
      << modelfile;
  }

  /*----------------------------------------------------------------------*/
  // write to file

  if (opt_writeg1)
  {
    if (opt_verbose) cout << endl << "write model to \""
      << arg_writeg1 << "\"" << endl;
    std::ofstream ofs(arg_writeg1.c_str());
    ofs << modelfile;
  }

  /*======================================================================*/
  // convert to flnode is requested

  if (opt_flnode||opt_nodes)
  {
    if (opt_verbose) cout << endl << "convert to nodes model:" << endl;
    // first find out the number of nodes we beed
    Tarray nodes=create_nodes(modelfile, opt_verbose);

    // dump nodes if requested
    if (opt_nodes)
    {
      dump_nodes(nodes);
    }

    // write flnode file if requested
    if (opt_flnode)
    {
      write_flnode(arg_flnode,nodes,opt_verbose,filename);
    }
  } // if (opt_flnode)


}

/* ----- END OF mocox.cc ----- */
