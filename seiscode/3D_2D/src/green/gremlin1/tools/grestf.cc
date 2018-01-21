/*! \file grestf.cc
 * \brief source time function from Green
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 04/01/2003
 * 
 * source time function from Green
 * 
 * Copyright (c) 2003 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 04/01/2003   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define GRESTF_VERSION \
  "GRESTF   V1.0   source time function from Green"
#define GRESTF_CVSID \
  "$Id$"

#include <iostream>
#include <fstream>
#include <cmath>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <aff/slice.h>
#include <gremlin1/greenspec.h>
#include <gremlin1/srcires.h>
#include <fourier/fcommand.h>

using std::cout;
using std::cerr;
using std::endl;

typedef gremlin1::GreenSpectrum::Tcarray Tcarray;
typedef gremlin1::GreenSpectrum::Tcvalue Tcvalue;
typedef std::complex<double> Tzvalue;

/*======================================================================*/
// functions

gremlin1::GreenSpectrum::Tcarray 
resortarray(const gremlin1::GreenSpectrum::Tcarray::Tcoc& inarray)
{
  gremlin1::GreenSpectrum::Tcarray result(inarray.size(1),
                                          inarray.size(0));
  for (int i=inarray.f(0); i<=inarray.l(0); ++i)
  {
    gremlin1::GreenSpectrum::Tcarray spec=aff::slice(result)()(i);
    spec.copyin(aff::slice(inarray)(i));
  }
  return(result);
}

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    GRESTF_VERSION "\n"
    "usage: grestf ires data [-v] [-damp v]" "\n"
    "              [-datafil f] [-filter f] [-restifi]" "\n"
    "              [-slraw f -slfil f] [-skipzero]" "\n"
    "   or: grestf --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "-v           verbose" "\n"
    "-datafil f   instrument response inherent to data;" "\n"
    "             this filter should specify known system properties\n"
    "             of geophones and data recorders such that the\n"
    "             resulting source time function actually represents\n"
    "             the force time function of the source only;\n"
    "             filter file \"f\" may contain filter commands\n"
    "             as specified below\n"
    "-filter  f   filter to be applied to result;" "\n"
    "             filter file \"f\" may contain filter commands\n"
    "             as specified below\n"
    "-restifi     do not apply datafil to synthetics" "\n"
    "             as an inverse filter (see \"concept\" below)" "\n"
    "-damp v      set damping factor to v" "\n"
    "-slraw f     write raw stf from linear regression to f" "\n"
    "-slfil f     write filtered stf from linear regression to f" "\n"
    "-skipzero    skip frequency zero" "\n"
    "\n"
    "ires         synthetic Fourier-Bessel expansion coefficients" "\n"
    "             (greda format calculated by syg)" "\n"
    "data         Fourier-Bessel expansion coefficients from recorded data" "\n"
    "             (greda format calculated by greda)" "\n"
    "\n"
    "This program calculates Fourier expansion coefficients for a source\n"
    "time function and writes them in a format used by gremlin (command\n"
    "\"resp\" ind menu \"file\" of gremlin). The Fourier coefficients are\n"
    "obtained by linear regression (i.e. least-squares optimization).\n"
    "They are calculated such that when applied to a synthetic Fourier-\n"
    "Bessel coeffients, these coefficients match a set of Fourier-Bessel-\n"
    "coefficients from recorded data in a least-squares sense. The\n"
    "optimization can be damped (i.e. a kind of water-level is applied)\n"
    "to the linear regression. Additionally several frequency domain\n"
    "filter can be applied to account for the system properties of\n"
    "geophones and recording equipment.\n"
    "\n"
    "concept:" "\n"
    "If -restifi is not set, we will apply -datafil to the synthetics" "\n"
    "\"ires\". then the force time-function is derived and finally the" "\n"
    "-filter will be applied to the force time-function" "\n"
    "If -restifi is set, force time-function will be derived first and" "\n"
    "-datafil will be applied as an inverse filter to it before" "\n"
    "-filter is applied" "\n"
    "\n"
    GRESTF_CVSID
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: instrument response inherent to data
    {"datafil",arg_yes,"-"},
    // 3: filter to be applied to result
    {"filter",arg_yes,"-"},
    // 4: use datafil for restitution
    {"restifi",arg_no,"-"},
    // 5: damping factor
    {"damp",arg_yes,"1.e-8"},
    // 6: output of raw stf from linear regresseion
    {"slraw",arg_yes,"-"},
    // 7: output of filtered stf from linear regresseion
    {"slfil",arg_yes,"-"},
    // 8: skip zero frequency
    {"skipzero",arg_no,"-"},
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
    fourier::FilterCommands::help(cerr);
    exit(0);
  }

  bool opt_verbose      =cmdline.optset(1);
  bool opt_datafil      =cmdline.optset(2);
  std::string arg_datafil=cmdline.string_arg(2);
  bool opt_filter       =cmdline.optset(3);
  std::string arg_filter=cmdline.string_arg(3);
  bool opt_restifi      =cmdline.optset(4);
  double arg_damp       =cmdline.double_arg(5);
  bool opt_slraw        =cmdline.optset(6);
  std::string arg_slraw =cmdline.string_arg(6);
  bool opt_slfil        =cmdline.optset(7);
  std::string arg_slfil =cmdline.string_arg(7);
  bool opt_skipzero     =cmdline.optset(8);

  bool act_stflinreg= opt_slfil || opt_slraw;

  const char missing_argument[]="ERROR: missing command line argument!";
  TFXX_assert(cmdline.extra(), missing_argument);
  std::string ires_file=cmdline.next();
  TFXX_assert(cmdline.extra(), missing_argument);
  std::string data_file=cmdline.next();

  /*----------------------------------------------------------------------*/
  // read synthetics

  if (opt_verbose) cout << GRESTF_VERSION << endl;

  if (opt_verbose) cout << "reading impulse response Green from "
    << ires_file << endl;
  gremlin1::GreenSpectrum iresgreen;
  {
    std::ifstream ifs(ires_file.c_str());
    ifs >> iresgreen;
  }

  if (opt_verbose)
  {
    cout << "  " << iresgreen.nf() << " frequencies, "
      << iresgreen.np() << " slowness values" << endl;
    cout << "  frequency range: "
      << iresgreen.fmin() << " to "
      << iresgreen.f(iresgreen.nf()) << " Hz" << endl;
    cout << "  slowness range: "
      << iresgreen.pmin()*1.e3 << " to "
      << iresgreen.p(iresgreen.np())*1.e3 << " s/km" << endl;
  }

  /*----------------------------------------------------------------------*/
  // read data

  if (opt_verbose) cout << "reading field data Green from "
    << data_file << endl;
  gremlin1::GreenSpectrum datagreen;
  {
    std::ifstream ifs(data_file.c_str());
    ifs >> datagreen;
  }

  if (opt_verbose)
  {
    cout << "  " << datagreen.nf() << " frequencies, "
      << datagreen.np() << " slowness values" << endl;
    cout << "  frequency range: "
      << datagreen.fmin() << " to "
      << datagreen.f(datagreen.nf()) << " Hz" << endl;
    cout << "  slowness range: "
      << datagreen.pmin() << " to "
      << datagreen.p(datagreen.np())*1.e3 << " s/km" << endl;
  }

  /*----------------------------------------------------------------------*/
  // consistency checks

  const char inconsistent_data[]="ERROR: files are not consistent!";
  TFXX_assert((iresgreen.nf()==datagreen.nf()),inconsistent_data);
  TFXX_assert((iresgreen.np()==datagreen.np()),inconsistent_data);
  TFXX_assert((std::abs(iresgreen.fmin()-datagreen.fmin())<=
               1.e-5*iresgreen.fmin()), inconsistent_data);
  TFXX_assert((std::abs(iresgreen.pmin()-datagreen.pmin())<=
               1.e-5*iresgreen.pmin()), inconsistent_data);
  TFXX_assert((std::abs(1.-iresgreen.df()/datagreen.df())<1.e-5),
              inconsistent_data);
  TFXX_assert((std::abs(1.-iresgreen.dp()/datagreen.dp())<1.e-5),
              inconsistent_data);

  // global ranges
  const double fmin=datagreen.fmin();
  const double fmax=datagreen.fmax();
  const int nf=datagreen.nf();
  const int np=datagreen.np();
  const double df=datagreen.df();

  /*----------------------------------------------------------------------*/
  // read filter files
  
  fourier::FilterCommands fil_dataresp;
  fourier::FilterCommands fil_filter;

  if (opt_datafil)
  {
    if (opt_verbose) cout << "reading filter from " << arg_datafil << endl;
    fil_dataresp.read(arg_datafil.c_str(), opt_verbose);
    fil_dataresp.setfreqmod();
  }

  if (opt_filter)
  {
    if (opt_verbose) cout << "reading filter from " << arg_filter << endl;
    fil_filter.read(arg_filter.c_str(), opt_verbose);
    fil_filter.setfreqmod();
  }

  /*----------------------------------------------------------------------*/

  // create resorted arrays (rows are frequencies then)
  //Tcarray rsrtires=resortarray(iresgreen.green());
  //Tcarray rsrtdata=resortarray(datagreen.green());

  /*----------------------------------------------------------------------*/
  // start linear regression part

  gremlin1::GremlinIres lr_iresraw(df,fmin,fmax);
  gremlin1::GremlinIres lr_iresfil(df,fmin,fmax);

  if (act_stflinreg)
  {

    /*----------------------------------------------------------------------*/
    // prepare dataset for linear regression
   
    Tcarray lr_data=datagreen.green().copyout();
    Tcarray lr_synt=iresgreen.green().copyout();

    if (!opt_restifi)
    {
      if (opt_verbose) cout << "apply data response to synthetics" << endl;
      Tcarray lr_synt=iresgreen.green();
      for (int i=lr_synt.f(0); i<=lr_synt.l(0); ++i)
      {
        for (int j=lr_synt.f(1); j<=lr_synt.l(1); ++j)
        {
          lr_synt(i,j) *= fil_dataresp.eval(iresgreen.f(j));
        }
      }
    }

    /*----------------------------------------------------------------------*/
    // calculate linear regression

    if (opt_verbose)
    {
      cout << "calculate linear regression" << endl
        << "  and apply filter" << endl;
      if (opt_restifi) cout << "  and apply inverse data response" << endl;
    }
    int iires=lr_iresraw.n1();
    for (int i=lr_synt.f(1); i<=lr_synt.l(1); ++i)
    {
      Tzvalue stfraw(0.,0.);
      Tzvalue stffil(0.,0.);
      double f=datagreen.f(i);
      if ((!opt_skipzero) || ((2.*f)>df))
      {
        Tzvalue numerator=Tzvalue(0.,0.);
        Tzvalue denominator=Tzvalue(0.,0.);

        for (int j=lr_synt.f(0); j<=lr_synt.l(0); ++j)
        {
          numerator += std::conj(lr_synt(j,i))*lr_data(j,i);
          denominator += std::norm(lr_synt(j,i));
        }
        denominator += (arg_damp*arg_damp);

        stfraw= numerator/denominator;
        stffil = stfraw * fil_filter.eval(f);
        if (opt_restifi) stffil /= fil_dataresp.eval(f);
      }

      lr_iresraw.ires()(iires)=static_cast<Tcvalue>(stfraw);
      lr_iresfil.ires()(iires)=static_cast<Tcvalue>(stffil);

      ++iires;
    }

    if (opt_slraw)
    {
      if (opt_verbose) cout << "write raw stf to " << arg_slraw << endl;
      std::ofstream os(arg_slraw.c_str());
      os << lr_iresraw;
    }

    if (opt_slfil)
    {
      if (opt_verbose) cout << "write filtered stf to " << arg_slfil << endl;
      std::ofstream os(arg_slfil.c_str());
      os << lr_iresfil;
    }

  } // if (act_stflinreg)

  /*----------------------------------------------------------------------*/

  
}

/* ----- END OF grestf.cc ----- */
