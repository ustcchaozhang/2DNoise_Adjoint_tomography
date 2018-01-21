/*! \file geophon.cc
 * \brief calculate geophone response
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 10/03/2005
 *
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
 * 
 * calculate geophone response
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
 * terms to be used:
 *
 * Ri:      input resistance of recorder
 * Rc:      coil resistance of geophone
 * R1:      resistance in series to geophone
 * Rd:      damping resistance parallel to recorder
 * Rs:      shunt resistance to geophone
 *          maybe Ri and Rd in parallel if R1=0
 *          or additionally with R1 in series
 * 
 * REVISIONS and CHANGES 
 *  - 10/03/2005   V1.0   Thomas Forbriger
 *  - 16/12/2005   V1.1   provide two new modes
 *  - 19/12/2005   V1.2   provide filter calculation
 *  - 26/07/2011   V1.3   added further comments to the online help
 *  - 27/07/2011   V1.4   provide correct impulse response for damped geophone
 *                        renamed program from geophon to geophone
 * 
 * ============================================================================
 */
#define GEOPHON_VERSION \
  "GEOPHONE   V1.4   calculate geophone response"

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <tfxx/misc.h>
#include <aff/series.h>
#include <aff/seriesoperators.h>
#include <sffostream.h>

using std::cout;
using std::cerr;
using std::endl;

struct Tparameters {
  /* basic parameters
   * hoc:   open circuit damping
   * C:     geophone constant
   * heff:  effective damping 
   * Rc:    coil resistance
   * Ri:    effective damping resistance
   *        in standard application (without extra damping) this is the input
   *        resistance of the recording system essentially
   *        in case of additional damping, this is the effective resistance
   *        shunting the geophone
   * fn:    natural frequency
   */
  double hoc, C, heff, Rc, Ri, fn;
  /* divider parameters
   * R1:    resistance in series to geophone
   * Rd:    resistance parallel to recording system
   * V:     division factor relative to standard application (only recorder
   *        shunts the geophone)
   */
  double R1, Rd, V;
  /* geophone type */
  std::string type;
}; // struct Tparameters

struct Toptions {
  bool saveresponse, verbose, R2mode, R1mode, writeresfile, Rd1mode;
  bool debug;
  std::string outfile,resfile;
  double T, dt, V, Rd, R1;
}; // struct Toptions

const double pi=3.1415926535;

typedef aff::Series<double> Tseries;

/*----------------------------------------------------------------------*/

/* for the given parameters
 * fn:      natural frequency
 * h:       effective damping
 * dt:      sampling interval
 * n:       number of samples
 *
 * this function calculates the impulse response of the system
 * and returns it as a series
 */
Tseries response(const double& fn, const double& h, const double& dt, 
                 const int& n)
{
  const double eps=1.e-4;
  Tseries result(n);
  for (int i=result.first(); i<=result.last(); i++)
  {
    double t=double(i)*dt;
    double omn=2.*pi*fn;
    if (h>(1.+eps))
    {
      double hfac=std::sqrt(h*h-1.);
      result(i)=std::exp(-omn*h*t)*
        (std::exp(-omn*t*hfac)-std::exp(omn*t*hfac))/
        (-2.*omn*hfac);
    } else if (h<(1.-eps))
    {
      double hfac=std::sqrt(1.-h*h);
      result(i)=std::sin(omn*t*hfac)*std::exp(-omn*h*t)/(omn*hfac);
    } else
    {
      result(i)=t*std::exp(-omn*t);
    }
  }
  return(result);
} // response

/*----------------------------------------------------------------------*/

/* returns effectice damping as a fraction of critical damping for
 * fn:      natural frequency
 * hoc:     open circuit damping
 * C:       geophone constant
 * Rc:      coil resistance
 * Rs:      effective shunt resistance
 */
inline double func_heff(const double& fn, const double& hoc, const double& C,
                 const double& Rc, const double& Rs)
{
  double heff;
  heff=hoc+(C/((Rs+Rc)*fn));
  return heff;
}

/*----------------------------------------------------------------------*/

/* returns shunt resistance required for
 * fn:      natural frequency
 * hoc:     open circuit damping
 * C:       geophone constant
 * Rc:      coil resistance
 * heff:    desiresd effective damping
 */
inline double func_Rs(const double& fn, const double& hoc, const double& C,
                 const double& Rc, const double& heff)
{
  double Rs;
  Rs=(C/((heff-hoc)*fn))-Rc;
  return Rs;
}

/*----------------------------------------------------------------------*/

/* returns effective shunt resistance parallel to geophone for
 * Ri:      input resistance of recording system
 * Rd:      damping resistance parallel to recorder
 * R1:      resistance in series to geophone
 */
inline double func_Rs(const double& Ri, const double& Rd,
                      const double& R1)
{
  double Rs;
  Rs=R1+1./((1./Rd)+(1./Ri));
  return Rs;
}

/*----------------------------------------------------------------------*/

/* returns external damping resistance parallel to recorder for
 * Rs:      effective shunt resistance
 * Ri:      input resistance of recording system
 */
inline double func_Rd(const double& Rs, const double& Ri)
{
  double Rd;
  Rd=1./((1./Rs)-(1./Ri));
  return Rd;
}

/*----------------------------------------------------------------------*/

/* returns voltage divider for standard application with given
 * Ri:      input resistance of recording system
 * Rc:      coil resistance
 */
inline double func_V(const double& Ri, const double& Rc)
{
  double V;
  // factor for standard application
  V=1./((Rc/Ri)+1.);
  return V;
}

/*----------------------------------------------------------------------*/

/* returns voltage divider for full external damping
 * Ri:      input resistance of recording system
 * Rc:      coil resistance
 * R1:      resistance in series to geophone
 * Rd:      resistance parallel to recorder
 */
inline double func_V(const double& Ri, const double& Rc,
                     const double& R1, const double& Rd)
{
  double V;
  // factor for standard application
  V=func_V(func_Rs(Ri, Rd, 0.), (R1+Rc));
  return V;
}

/*----------------------------------------------------------------------*/

/* returns external damping resistance parallel to recorder for
 * Ri:      input resistance of recording system
 * Rc:      coil resistance
 * V:       desired divider constant (V <= 1)
 */
inline double func_RdV(const double& Ri, const double& Rc,
                       const double& V)
{
  double Rd;
  TFXX_assert((V <= 1),"ERROR (func_RdV): division factor not less than 1!");
  TFXX_assert((V > 0),"ERROR (func_RdV): division factor not greater than 0!");
  Rd=Rc/((1./V-(Rc/Ri)-1.));
  return Rd;
}

/*----------------------------------------------------------------------*/

/* returns value for resistance in series with geophone for
 * Rc:      coil resistance
 * Ri:      input resistance of recording system
 * Rs:      desired shunt resistance
 *          which is R1 in series of (Ri and Rd in parallel)
 * V:       desired divider constant (V <= 1)
 */
inline double func_R1(const double& Ri, const double& Rc,
                      const double& Rs, const double& V)
{
  double R1;
  TFXX_assert((V <= 1),"ERROR (func_R1): division factor not less than 1!");
  TFXX_assert((V > 0),"ERROR (func_R1): division factor not greater than 0!");
  R1=Rs-V*(Rc+Rs);
  return R1;
}

/*----------------------------------------------------------------------*/

/* returns value for resistance parallel to recorder for
 * Rc:      coil resistance
 * Ri:      input resistance of recording system
 * Rs:      desired shunt resistance
 *          which is R1 in series of (Ri and Rd in parallel)
 * V:       desired divider constant (V <= 1)
 */
inline double func_R2(const double& Ri, const double& Rc,
                      const double& Rs, const double& V)
{
  double R2;
  TFXX_assert((V <= 1),"ERROR (func_R1): division factor not less than 1!");
  TFXX_assert((V > 0),"ERROR (func_R1): division factor not greater than 0!");
  R2=1./((1./(V*(Rc+Rs)))-(1./Ri));
  return R2;
}

/*----------------------------------------------------------------------*/
/* write parameters to output stream
 */
void writeparameters(std::ostream& os,
                     const std::string& prefix,
                     const Tparameters& para,
                     const double& hnd,
                     const double& heff, const double& Rs,
                     const double& R1, const double& Rd,
                     const double& Afac, const double& Afaceff)
{
  os << prefix << GEOPHON_VERSION << endl;
  os << prefix << endl;
  os << prefix << "geophone parameters:" << endl;
  os << prefix << "--------------------" << endl;
  os << prefix << "              geophone type: " << para.type
    << endl;
  os << prefix << "          natural frequency: " << para.fn << " Hz"
    << endl;
  os << prefix << "            coil resistance: " << para.Rc << " Ohm"
    << endl;
  os << prefix << "       open circuit damping: " << para.hoc
    << endl;
  os << prefix << "        geophone constant C: " << para.C << " Ohm*Hz"
    << endl;
  os << prefix << "  C=Rt*Bc*fn=(K**2)/(4*pi*m)" << endl;

  os << prefix << endl;
  os << prefix << "  recorder input resistance: " << para.Ri << " Ohm"
    << endl;

  os << prefix << endl;
  os << prefix << "reference parameters:" << endl;
  os << prefix << "---------------------" << endl;
  os << prefix << "(parameters for the case that the geophones are" << endl
     << prefix << "connected directly to the recorder, without any" << endl
     << prefix << "external resitance)" << endl;
  os << prefix << "                                            damping: " 
    << hnd << endl;
  os << prefix << "  amplitude as a fraction of open circuit amplitude: " 
    << 100.*Afac << "%" << endl;

  os << prefix << endl;
  os << prefix << "parameters with external damping resistance:" << endl;
  os << prefix << "--------------------------------------------" << endl;
  os << prefix << "    shunt resistance parallel to recorder terminals: " 
    << Rd << " Ohm" << endl;
  os << prefix << "    additional resistance in series to the geophone: " 
    << R1 << " Ohm" << endl;
  os << prefix << "                                  effective damping: " 
    << heff << endl;
  os << prefix << "  amplitude as a fraction of open circuit amplitude: " 
    << 100.*Afaceff << "%" << endl;
  os << prefix << "      amplitude as a fraction of reference fraction: " 
    << 100.*Afaceff/Afac << "%" << endl;

  os << prefix << endl;
  os << prefix 
    << "    effective shunt resistance parallel to geophone terminals: " 
    << Rs << " Ohm" << endl;
  os << prefix 
    << "       effective geophone resistance (with series resistance): " 
    << para.Rc+R1 << " Ohm" << endl;
  os << prefix 
    << "  eff. recorder resistance (with ext. resistance in parallel): " 
    << func_Rs(para.Ri, Rd, 0.) << " Ohm" << endl;

  os << prefix << endl;
  os << prefix 
    << "All damping values are given as a fraction of critical damping."
    << endl;
} // writeparameters

/*----------------------------------------------------------------------*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    GEOPHON_VERSION "\n"
    "usage: geophone [-hoc hoc] [-heff heff] [-Ri Ri] [-Rc Rc]" "\n"
    "                [-C C] [-fn fn] [-type type] [-o file]" "\n"
    "                [-T T] [-dt dt] [-Rd1 Rd,R1]" "\n"
    "   or: geophone --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "-v                 be verbose" "\n"
    "-DEBUG             debug mode" "\n"
    "\n"
    "geophone parameters:\n"
    "-hoc hoc           open circuit damping" "\n"
    "-Rc Rc             coil resistance" "\n"
    "-C C               geophone constant" "\n"
    "                   C=Rt*Bc*fn=(K**2)/(4*pi*m)" "\n"
    "-fn fn             natural frequency" "\n"
    "-type type         geophone type" "\n"
    "\n"
    "recorder parameters:\n"
    "-Ri Ri             recorder input resistance" "\n"
    "\n"
    "desired damping:\n"
    "-heff heff         desired effective damping" "\n"
    "\n"
    "output parameters:\n"
    "-o file            name of file to save impulse response" "\n"
    "                   parameters T and dt specify the sampling for\n"
    "                   the impulse response time series\n"
    "-T T               length of time series" "\n"
    "                   as a fraction of the natural period" "\n"
    "-dt dt             sampling interval" "\n"
    "                   as a fraction of the natural period" "\n"
    "\n"
    "-resfile file      Write an appropriate seife deconvolution" "\n"
    "                   filter to \"file\". The filter is designed" "\n"
    "                   to deconvolve the output of the damped geophone" "\n"
    "                   to the repsonse of the standard application (i.e." "\n"
    "                   geophone connected directly to recorder)." "\n"
    "\n"
    "alternative modes of operation:\n"
    "-R2 V              return R2 for voltage division factor V" "\n"
    "                   specified relative to the voltage obtained" "\n"
    "                   by standard operation (Ri being the only" "\n"
    "                   resistance external to the geophone)." "\n"
    "                   heff is ignored in this case." "\n" 
    "-R1 V              return R1 and R2 for voltage division factor V" "\n"
    "                   maintaining a damping of heff." "\n"
    "                   V is specified like with option -R2." "\n"
    "-Rd1 Rd,R1         The values R1 and Rd specify the additional" "\n"
    "                   resistance in series with the geophone (R1) and" "\n"
    "                   parallel to the recorder (Rd). The program" "\n"
    "                   returns the resulting response." "\n"
    "\n"
    "All damping values are given as a fraction of critical damping." "\n"
    "\n"
    "Usually you specify hoc, Ri, Rc, C, fn, and heff. Then the program" "\n"
    "will tell you which shunt resistance to use parallel to the recorder" "\n"
    "input terminals." "\n"
    "\n"
    "In case you specify V by option -R2, the program will ignore the" "\n"
    "desired effective damping heff and will tell you, which shunt" "\n"
    "resistance to use to obtain the desired voltage division factor V." "\n" 
    "\n"
    "In case you specify V by option -R1, the program will tell you," "\n"
    "which shunt and series resistance to use to obtain the desired" "\n"
    "voltage division factor V and damping heff." "\n" 
    "\n"
    "In case you specify the series resistance R1 and the shunt resistantce\n"
    "Rd in parallel to the recorder by option -Rd1, the program returns" "\n"
    "the resulting response." "\n"
    "\n"
    "Circuit diagram:\n"
    "\n"
    "    +--------+     +-----+           +--------+\n"
    "    |   +----|-----| R1  |-----+-----|---+    |\n"
    "    |   |    |     +-----+     |     |   |    |\n"
    "    | +---+  |               +---+   | +---+  |\n"
    "    | |   |  |               |   |   | |   |  |\n"
    "    | |Rc |  |               |Rd |   | |Ri |  |\n"
    "    | |   |  |               |   |   | |   |  |\n"
    "    | +---+  |               +---+   | +---+  |\n"
    "    |   |    |                 |     |   |    |\n"
    "    |   +----|-----------------+-----|---+    |\n"
    "    |        |                       |        |\n"
    "    |geophone|                       |recorder|\n"
    "    +--------+                       +--------+\n"
    "       ||||\n"
    "       ||||  usually R1=0 and Rd=infinity\n"
    "        ||\n"
    "        ||   use finite Rd for larger \n"
    "        ||   effective damping\n"
    "        ||\n"
    "        ||   use R1 to set up a voltage divider\n"
    "        \\/   and small effective damping\n"
    "\n"
    "Background:\n"
    "Usually geophones are directly connected to the recording system.\n"
    "This is equivalent to R1=0 and Rd=infinity. With Ri >> Rc the\n"
    "effective damping of the geophone is only slightly larger than\n"
    "the open circuit damping of the geophone.\n"
    "\n"
    "If a damping of the geophone larger than the open circuit damping\n"
    "is desired, a finite Rd can be connected in parallel to the input\n"
    "of the recording system. This program provides the appropriate value\n"
    "if the desired effective damping is passed in the parameter to\n"
    "option -heff. Alternatively the effective damping heff is\n"
    "provided as output, if values for R1 and Rd are passed as\n"
    "parameters to option -Rd1.\n"
    "\n"
    "In some cases near offset geophones are saturated in the recording\n"
    "system by ground motion close to the seismic source. In this case a\n"
    "small resistance Rd can be used to set up a voltage divider with\n"
    "the coil resistance Rc thus makeing the voltage to be recorded\n"
    "smaller accordingly. At the same time this will strongly damp the\n"
    "geophone itself thus preventing mechanical saturation and clipping.\n"
    "If this is desired, the parallel resistance required for a given\n"
    "amplitude factor V is provided by the program, if V is passed as\n"
    "a parameter to option -R1.\n"
    "\n"
    "If strong overdamping of the geophone has to be avoided, for\n"
    "example to maintain the same effective damping for all geophones\n"
    "in a profile, a series resistance R1 together with a parallel\n"
    "resistance Rd can be used to adjust a voltage divider with\n"
    "factor V and a desired effective damping heff at the same time.\n"
    "In this case pass V as a parameter to option -R2 and heff as a\n"
    "parameter to option -heff.\n"
    "\n"
    "If R1 and Rd are known from the configuration of the seismic\n"
    "experiment, you can use both values as parameters to option\n"
    "-Rd1 in order to obtain the effective response characteristic\n"
    "of the geophone.\n"
    "\n"
    "Geophone response are provided in three ways:\n"
    "1. Effective geophone parameters are printed to standard output.\n"
    "2. A filename passed as a parameter to option -resfile will\n"
    "   cause the program to create a filter control file with this\n"
    "   name. This filter control file can be used together with\n"
    "   stufi or tidofi in order to restore the response of a\n"
    "   geophone as used with R1=0 and Rd=infinity in terms of\n"
    "   effective damping as well as singal amplitude.\n"
    "3. A filename passed as a parameter to option -o will cause\n"
    "   the program to produce an SFF data file containing the\n"
    "   the impulse response of the geophone. The first trace contains\n"
    "   the response of the geophone as if recorded with R1=0 and\n"
    "   Rd=infinity. The second trace contains the impulse response\n"
    "   of a geophone if operated with a finite R1 and Rd as\n"
    "   specified by the command line arguments. The effective\n"
    "   geophone parameters are given in the first line of the\n"
    "   trace FREE block.\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: open circuit damping
    {"hoc",arg_yes,"0.315"},
    // 3: desired effective damping 
    {"heff",arg_yes,"0.707"},
    // 4: recorder input resistance
    {"Ri",arg_yes,"20000."},
    // 5: coil resistance
    {"Rc",arg_yes,"375."},
    // 6: geophone constant
    {"C",arg_yes,"6000."},
    // 7: natural frequency
    {"fn",arg_yes,"8."},
    // 8: geophone type
    {"type",arg_yes,"SM-4"},
    // 9: geophone type
    {"o",arg_yes,"response.sff"},
    // 10: length of time series
    {"T",arg_yes,"10."},
    // 11: sampling interval
    {"dt",arg_yes,"1.e-2"},
    // 12: series resistance
    {"R1",arg_yes,"0."},
    // 13: series resistance
    {"R2",arg_yes,"1.e12"},
    // 14: Rd and R1
    {"Rd1", arg_yes, "-"},
    // 15: deconvolution filter
    {"resfile", arg_yes, "res.fil"},
    // 16: deconvolution filter
    {"DEBUG", arg_no, "-"},
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

  /*
  // dummy operation: print option settings
  for (int iopt=0; iopt<2; iopt++)
  {
    cout << "option: '" << options[iopt].opt_string << "'" << endl;
    if (cmdline.optset(iopt)) {  cout << "  option was set"; }
    else { cout << "option was not set"; }
    cout << endl;
    cout << "  argument (string): '" << cmdline.string_arg(iopt) << "'" << endl;
    cout << "     argument (int): '" << cmdline.int_arg(iopt) << "'" << endl;
    cout << "    argument (long): '" << cmdline.long_arg(iopt) << "'" << endl;
    cout << "   argument (float): '" << cmdline.float_arg(iopt) << "'" << endl;
    cout << "  argument (double): '" << cmdline.double_arg(iopt) << "'" << endl;
    cout << "    argument (bool): '";
    if (cmdline.bool_arg(iopt))
    { cout << "true"; } else { cout << "false"; }
    cout << "'" << endl;
  }
  while (cmdline.extra()) { cout << cmdline.next() << endl; }

  // dummy operation: print rest of command line
  while (cmdline.extra()) { cout << cmdline.next() << endl; }
  */

  Tparameters para;
  para.hoc=cmdline.double_arg(2);
  para.heff=cmdline.double_arg(3);
  para.Ri=cmdline.double_arg(4);
  para.Rc=cmdline.double_arg(5);
  para.C=cmdline.double_arg(6);
  para.fn=cmdline.double_arg(7);
  para.type=cmdline.string_arg(8);

  Toptions opt;
  opt.verbose=cmdline.optset(1);
  opt.saveresponse=cmdline.optset(9);
  opt.outfile=cmdline.string_arg(9);
  opt.T=cmdline.double_arg(10);
  opt.dt=cmdline.double_arg(11);
  opt.R1mode=cmdline.optset(12);
  opt.V=cmdline.double_arg(12);
  opt.R2mode=cmdline.optset(13);
  if (!opt.R1mode) { opt.V=cmdline.double_arg(13); }
  opt.Rd1mode=cmdline.optset(14);
  if (opt.Rd1mode)
  {
    std::string args=cmdline.string_arg(14);
    args.replace(args.find(","),1," ");
    std::istringstream is(args);
    is >> opt.Rd >> opt.R1;
  }
  opt.writeresfile=cmdline.optset(15);
  opt.resfile=cmdline.string_arg(15);
  opt.debug=cmdline.optset(16);

  /*----------------------------------------------------------------------*/

/*
  double Rd=1./(1./((para.C/((para.heff-para.hoc)*para.fn))-para.Rc)
                -(1./para.Ri));
  double hnd=para.hoc+para.C/(para.fn*(para.Ri+para.Rc));
  double Rs=1./((1./Rd)+(1./para.Ri));
  double Afac=100.*para.Ri/(para.Ri+para.Rc);
  double Afacs=100.*Rs/(Rs+para.Rc);
*/

  // calculate reference parameters
  double hnd=func_heff(para.fn, para.hoc, para.C, para.Rc, para.Ri);
  double Afac=func_V(para.Ri, para.Rc);

  double Rd, R1=0., heff, Afaceff, Rs;
  // case Rd1
  if (opt.Rd1mode)
  {
    Rd=opt.Rd;
    R1=opt.R1;
    Rs=func_Rs(para.Ri, Rd, R1);
    heff=func_heff(para.fn, para.hoc, para.C, para.Rc, Rs);
    Afaceff=func_V(para.Ri, para.Rc, R1, Rd);
  }
  // case R1
  else if (opt.R1mode)
  {
    Afaceff=opt.V*Afac;
    heff=para.heff;
    Rs=func_Rs(para.fn, para.hoc, para.C, para.Rc, para.heff);
    Rd=func_R2(para.Ri, para.Rc, Rs, Afaceff);
    R1=func_R1(para.Ri, para.Rc, Rs, Afaceff);
  }
  // case R2
  else if (opt.R2mode)
  {
    R1=0.;
    Afaceff=opt.V*Afac;
    Rd=func_RdV(para.Ri, para.Rc, Afaceff);
    Rs=func_Rs(para.Ri, Rd, 0.);
    heff=func_heff(para.fn, para.hoc, para.C, para.Rc, Rs);
  }
  // default case
  else
  {
    R1=0.;
    heff=para.heff; 
    Rs=func_Rs(para.fn, para.hoc, para.C, para.Rc, para.heff);
    Rd=func_Rd(Rs, para.Ri);
    // effective input resistance
    Afaceff=func_V(para.Ri, para.Rc, R1, Rd);
  }

  /*----------------------------------------------------------------------*/
  // report
  writeparameters(cout, "", para, hnd, heff, Rs, R1, Rd, Afac, Afaceff);

  if (opt.saveresponse) {
    if (opt.verbose) {
      cout << endl;
      cout << "write impulse response to " << opt.outfile << endl;
    }

    double dt=opt.dt/para.fn;
    int n=int(opt.T/opt.dt);
    Tseries srnd=response(para.fn,hnd,dt,n);
    srnd *= Afac;
    Tseries sreff=response(para.fn,heff,dt,n);
    sreff *= Afaceff;

    sff::WID2 wid2;
    wid2.dt=dt;

    std::ostringstream oss;
    sff::FREE ndfree, efffree;
    oss.str("");
    oss << para.type << " geophone impulse response ("
      << "fn=" << para.fn << "Hz, " << "h=" << hnd << ")";
    ndfree.append(oss.str());
    oss.str("");
    oss << para.type << " geophone impulse response ("
      << "fn=" << para.fn << "Hz, " << "h=" << heff << ")";
    efffree.append(oss.str());

    TFXX_debug(opt.debug, "geophon (main):",
               "para.heff = " << heff << "\n"
               "oss.str(): " << oss.str());

    std::ofstream ofs(opt.outfile.c_str());
    sff::SFFostream<Tseries> os(ofs);
    os << srnd;
    os << wid2;
    os << ndfree;
    os << sreff;
    os << wid2;
    os << efffree;
    
  } // if (opt.saveresponse)

  if (opt.writeresfile)
  {
    if (opt.verbose) {
      cout << endl;
      cout << "write deconvolution filter to " << opt.resfile << endl;
    }

    std::ofstream os(opt.resfile.c_str());
    os << "rem  deconvolution filter calculated by" << endl;
    writeparameters(os, "rem  ", para, hnd, heff, Rs, R1, Rd, Afac, Afaceff);
    os << "fac  " << (Afac/Afaceff) << endl;
    os << "he2  " << 1./para.fn << ","
                  << heff << ","
                  << 1./para.fn << ","
                  << hnd << endl;
    os << "end  " << endl;
  } // if (opt.writeresfile)
}

/* ----- END OF geophon.cc ----- */
