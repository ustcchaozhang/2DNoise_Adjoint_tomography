/*! \file lisousi.cc
 * \brief line source simulation
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 20/07/2012
 * 
 * line source simulation
 * 
 * Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * LISOUSI is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version. 
 * 
 * lisousi is distributed in the hope that it will be useful,
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
 *  - 20/07/2012   V1.0   Thomas Forbriger
 *  - 23/08/2012   V1.1   implemented alternatives and limitation of number of
 *                        samples
 *  - 24/09/2012   V1.2   provide Fourier domain processing
 *  - 02/10/2012   V1.3   provide mixed processing 
 *                        correct amplitude factor sqrt(2)
 *  - 04/10/2012   V1.4   new feature: use numerical Fourier transform of
 *                        1/sqrt(t)
 *                        provide adjustments for 1/sqrt(t) near t=0
 *                        provide equal integration method
 *  - 11/10/2012   V1.5   reworking the online documentation and the
 *                        parameter definitions
 *  - 12/10/2012   V1.6   make application of analytic Fourier tarnsform the
 *                        default
 *  - 12/10/2012   V1.7   introduce taper delay slowness
 *  - 18/10/2012   V1.8   implement new option: continuous transition from
 *                        single velocity transformation to direct wave
 *                        transformation
 *  - 19/10/2012   V1.9   disabled tapslo default value
 *  - 20/10/2012   V1.10  properly implemented reflected wave transformation
 *  - 12/04/2013   V1.11  implement near-field transformation for constant
 *                        velocity approach
 *  - 16/04/2013   V1.11a intermediate version until near-field single
 *                        velocity transformation is implemented
 *  - 29/07/2013   V1.11b provide scaling according to spatial distance to
 *                        source
 *  - 11/10/2013   V1.12  all experimental functions are hidden from the user
 *                        program passed test: publication figures are well
 *                        reproduced by the current version
 *  - 12/05/2015   V1.12a provide updated documentation
 *  - 21/09/2015   V1.12b indicate fix by advancing version id
 *  - 26/10/2015   V1.12c provide format specific usage text
 *  - 25/02/2016   V1.12d fix online usage description
 * 
 * ============================================================================
 */
#define LISOUSI_VERSION \
  "LISOUSI   V1.12d   line source simulation"

#include "lisousi.h"
#include "wnintegration.h"
#include "functions.h"
#include "usage_text.h"
#include "help_text.h"
#include "description_text.h"
#include "experimental_text.h"
#include <cstdlib>

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text_version[]=
  {
    LISOUSI_VERSION "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: output file format
    {"xhelp",arg_opt,"all"},
    // 2: generate debug output
    {"DEBUG",arg_no,"-"},
    // 3: verbose mode
    {"v",arg_no,"-"},
    // 4: overwrite mode
    {"o",arg_no,"-"},
    // 5: input file format
    {"type",arg_yes,"sff"},
    // 6: output file format
    {"Type",arg_yes,"sff"},
    // 7: apply taper first, than do convolution
    {"taperlast",arg_no,"-"},
    // 8: use Picas taper
    {"sqrttaper",arg_no,"-"},
    // 9: limit the number of samples
    {"limitlength",arg_no,"-"},
    // 10: processing shall take place in the Fourier domain
    {"fredomain",arg_no,"-"},
    // 11: scaling velocity in the Fourier domain
    {"velocity",arg_yes,"1."},
    // 12: apply filter in the time domain
    {"tdfilter",arg_no,"-"},
    // 13: apply filter by analytic Fourier coefficients
    {"fdfilter",arg_no,"-"},
    // 14: pad with zeroes
    {"pad",arg_yes,"1"},
    // 15: shift time axis
    {"tshift",arg_yes,"1."},
    // 16: lower limit of time values
    {"tlim",arg_yes,"0."},
    // 17: fixed value of time for small values
    {"tfac",arg_yes,"0.1"},
    // 18: choose convolution weights by integration
    {"integshift",arg_yes,"0.5"},
    // 19: delay taper function
    {"tapdel",arg_yes,"0."},
    // 20: do not use integration for construction of 1/sqrt(t)
    {"nointeg",arg_no,"-"},
    // 21: upper limit slowness
    {"tapslo",arg_yes,"0."},
    // 22: transition from fredomain to standard approach
    {"transition",arg_yes,"0.,1."},
    // 23: exact single velocity transformation (instead of far-field
    // approximation)
    {"fdexplosion",arg_no,"-"},
    // 24: single force in full space
    {"fdzforce",arg_no,"-"},
    // 25: single vertical force on homogeneous halfspace (Lamb's problem)
    {"fdlamb",arg_no,"-"},
    // 26: vp/vs ratio
    {"vpvsratio",arg_yes,"1.732"},
    // 27: quality factor or P-wave and S-wave modulus
    {"quality",arg_yes,"30.,30."},
    // 28: trapezoid rule parameters for wavenumber integration
    {"ktrapezoid",arg_yes,"1000,2.,0.1"},
    // 29: use specific radial component transformation, where applicable
    {"radial",arg_no,"-"},
    // 30: single force in full space by wavenumber integration
    {"fdwizforce",arg_no,"-"},
    // 31: use spatial distance to source rather than offset along surface
    {"spatialdistance",arg_no,"-"},
    // 32: report undocumented and experimental options
    {"listexperimental",arg_no,"-"},
    {NULL}
  };

  /*----------------------------------------------------------------------*/
  /* we keep the framework for file specific parameters
   * but we do not use them
   */

  static const char formatkey[]="f";
  static const char tracekey[]="t";
  /*
  static const char applykey[]="a";
  static const char prefiltertaperkey[]="prft";
  static const char postfiltertaperkey[]="poft";
  */

  // define commandline argument modifier keys
  static const char* cmdlinekeys[] 
    ={formatkey, tracekey, 0};
  /*
    ={tracekey, formatkey, applykey, prefiltertaperkey, postfiltertaperkey, 0};
    */

  /*----------------------------------------------------------------------*/

  // no arguments? print usage...
  if (iargc<2) 
  {
    cerr << usage_text_version << endl;
    cerr << usage_text << endl;
    exit(0);
  }

  // collect options from commandline
  Commandline cmdline(iargc, argv, options);

  // help requested? print full help text...
  if (cmdline.optset(0))
  {
    cerr << usage_text_version << endl;
    cerr << usage_text << endl;
    cerr << help_text << endl;
    cerr << endl;
    cerr << description_text << endl;
    datrw::supported_data_types(cerr); 
    exit(0);
  }

  // help on file format details requested? 
  if (cmdline.optset(1))
  {
    cerr << usage_text_version << endl;
    cerr << usage_text << endl;
    cerr << endl;
    if (cmdline.string_arg(1) == "all")
    {
      datrw::online_help(cerr); 
    }
    else
    {
      datrw::online_help(cmdline.string_arg(1), cerr); 
    }
    exit(0);
  }

  // list experimental options
  if (cmdline.optset(32))
  {
    cerr << usage_text_version << endl;
    cerr << usage_text << endl;
    cerr << endl;
    cerr << experimental_text << endl;
    exit(0);
  }

  // extract commandline options
  Options opt;
  // 1 is --xhelp
  {
    std::istringstream iss;
    std::string args;
    opt.debug=cmdline.optset(2);
    opt.verbose=cmdline.optset(3);
    opt.overwrite=cmdline.optset(4);
    opt.inputformat=cmdline.string_arg(5);
    opt.outputformat=cmdline.string_arg(6);
    opt.taperfirst=!cmdline.optset(7);
    opt.sqrttaper=cmdline.optset(8);
    opt.limitlength=cmdline.optset(9);
    opt.fredomain=cmdline.optset(10);
    opt.velocity=cmdline.double_arg(11);
    opt.tdfilter=cmdline.optset(12);
    opt.fdfilter=cmdline.optset(13);
    opt.npad=cmdline.int_arg(14);
    opt.tshift=cmdline.double_arg(15);
    opt.tlim=cmdline.double_arg(16);
    opt.tfac=cmdline.double_arg(17);
    opt.integshift=cmdline.double_arg(18);
    opt.tapdel=cmdline.double_arg(19);
    opt.nointeg=cmdline.optset(20);
    opt.tapslo=cmdline.double_arg(21);
    opt.tapsloset=cmdline.optset(21);
    opt.transition=cmdline.optset(22);
    opt.fdtype=Ffdfarfield;
    if (cmdline.optset(23)) { opt.fdtype=Ffdexplosion; };
    if (cmdline.optset(24)) { opt.fdtype=Ffdzforce; };
    if (cmdline.optset(25)) { opt.fdtype=Ffdlamb; };
    opt.vpvsratio=cmdline.double_arg(26);
    args=cmdline.string_arg(27);
    args.replace(args.find(","),1," ");
    iss.str(args);
    iss >> opt.pquality >> opt.squality;
    args=cmdline.string_arg(28);
    args.replace(args.find(","),1," ");
    iss.str(args);
    iss >> opt.ipa.nsteps >> opt.ipa.edge >> opt.ipa.taper;
    opt.radial=cmdline.optset(29);
    if (cmdline.optset(30)) { opt.fdtype=Ffdwizforce; };
    opt.spatialdistance=cmdline.optset(31);
  }

  /* ---------------------------------------------------------------------- */
  /* check options for plausibility, etc */

  // notify user if experimental option is selected
  if ((opt.fdtype==Ffdexplosion) || 
      (opt.fdtype==Ffdzforce) ||
      (opt.fdtype==Ffdlamb) ||
      (opt.fdtype==Ffdwizforce) || (opt.radial)) {
    cerr << "WARNING:" << endl;
    cerr << "You selected an experimental option!" << endl;
    cerr << "This function is not yet properly implemented." << endl;
    cerr << "It is not well tested and is likely to provide" << endl;
    cerr << "false results.\n" << endl;
  }

  // check offset range for hybrid transformation
  if (opt.transition)
  {
    std::istringstream transopt(tfxx::string::patsubst(cmdline.string_arg(22),
                                                       ",", " "));
    transopt >> opt.transition1 >> opt.transition2;
    TFXX_assert((opt.transition1>=0.) && (opt.transition2>=0.),
                "transition offsets must be positive");
    TFXX_assert(opt.transition2>opt.transition1,
                "second transition offset must be larger than first");
  }

  // plausibility checks for command line parameters
  TFXX_assert(opt.tshift>=0.,
             "just positive shifting is allowed");
  TFXX_assert(opt.tlim>=0.,
             "just positive times are allowed");
  TFXX_assert(opt.tfac>=0.,
             "just positive time values are allowed");
  TFXX_assert((opt.integshift>=0.)&&(opt.integshift<1.),
             "just values in [0.,1.) are allowed for -integ parameter");
  TFXX_assert(opt.tapslo>=0.,
             "just positive slowness values are allowed");

  /* ---------------------------------------------------------------------- */

  if (opt.verbose)
  { cout << LISOUSI_VERSION << endl; }

  // extract commandline arguments
  TFXX_assert(cmdline.extra(), "missing output file");
  std::string outfile=cmdline.next();
  TFXX_assert(cmdline.extra(), "missing input file");
  tfxx::cmdline::Tparsed arguments=parse_cmdline(cmdline, cmdlinekeys);
  if ((arguments.size()>1) && opt.verbose)
  {
    cout << "NOTICE: file specific information (SRCE line and file FREE)" <<
      endl
      <<    "        will be taken from first file only!" << endl;
  }

  /*======================================================================*/
  /*
   * all prerequisites exist
   * options and parameters are extracted from the command line
   * file names are extracted from the command line
   *
   */

  /*----------------------------------------------------------------------*/
  /*
   * report: what are we going to apply to the data
   *
   * prepare a character string FREE block
   * this can be written to file as well as output to terminal
   */
  sff::FREE processingreport;

  /*----------------------------------------------------------------------*/
  // prepare file FREE block
  sff::FREE filefree;
  {
    filefree.append(LISOUSI_VERSION);
    std::string line;
    filefree.append("output file name:");
    filefree.append("  " + outfile);
    filefree.append("input file selection:");
    tfxx::cmdline::Tparsed::const_iterator file=arguments.begin();
    while (file != arguments.end())
    {
      filefree.append("  " + file->name);
      line="  ";
      tfxx::cmdline::Toptionmap::const_iterator option=file->options.begin();
      while (option != file->options.end())
      {
        line += "  " + option->first + ":" + option->second;
        ++option;
      }
      if (line.size()>2) { filefree.append(line); }
      ++file;
    }
    if (arguments.size()>1)
    {
      filefree.append("In cases where more than one input file is read,");
      filefree.append("the SRCE line is taken from the first file only (if");
      filefree.append("present there).");
    }
    filefree.append(processingreport);
  }

  if (opt.verbose) { cout << filefree << endl; }

  /*======================================================================*/
  // start actual processing
  // =======================
  //
 
  // prepare expansion coefficients for wavenumber integration methods
  Exco expansioncoefficients;

  if (opt.fredomain)
  {
    Model mod;
    mod.Vs=1.e3*opt.velocity;
    mod.Vp=1.e3*opt.velocity*opt.vpvsratio;
    mod.Qs=opt.squality;
    mod.Qp=opt.pquality;
    if (opt.fdtype==Ffdwizforce)
    {
      FSECZ ec(mod);
      expansioncoefficients=Exco(ec, opt.ipa);
    }
    else if (opt.fdtype==Ffdlamb)
    {
      if (opt.radial)
      {
        HSECR ec(mod);
        expansioncoefficients=Exco(ec, opt.ipa);
      }
      else
      {
        HSECZ ec(mod);
        expansioncoefficients=Exco(ec, opt.ipa);
      }
    }
  }

  /*----------------------------------------------------------------------*/

  // open output file
  // ----------------
  if (opt.verbose) { cout << "open output file " << outfile << endl; }
  // check if output file exists and open
  if (!opt.overwrite)
  {
    std::ifstream file(outfile.c_str(),std::ios_base::in);
    TFXX_assert((!file.good()),"ERROR: output file exists!");
  }
  std::ios_base::openmode oopenmode
    =datrw::oanystream::openmode(opt.outputformat);
  std::ofstream ofs(outfile.c_str(), oopenmode);
  datrw::oanystream os(ofs, opt.outputformat, opt.debug);

  // set flag to process header of first input file
  bool firstfile=true;
  // cycle through all input files
  // -----------------------------
  tfxx::cmdline::Tparsed::const_iterator infile=arguments.begin();
  while (infile != arguments.end())
  {
    // open input file
    if (opt.verbose) { cout << "open input file " << infile->name << endl; }
    std::string inputformat=opt.inputformat;
    if (infile->haskey(formatkey)) 
    {
      inputformat=infile->value(formatkey);
    }
    std::ios_base::openmode iopenmode
      =datrw::ianystream::openmode(inputformat);
    std::ifstream ifs(infile->name.c_str(), iopenmode);
    datrw::ianystream is(ifs, inputformat);
      
    // handle file header
    // ------------------
    sff::SRCE insrceline;
    bool srceavailable=false;
    if (firstfile)
    {
      if (is.hasfree()) 
      { 
        sff::FREE infilefree;
        is >> infilefree;
        filefree.append("block read from first input file:");
        filefree.append(infilefree);
      }
      if (os.handlesfilefree()) { os << filefree; }
      if (is.hassrce())
      {
        is >> insrceline;
        srceavailable=true;
        if (os.handlessrce()) { os << insrceline; }
      }
    }

    // cycle through traces of input file
    // ----------------------------------
    // setup trace selection
    typedef tfxx::RangeList<int> Trangelist;
    bool doselect=infile->haskey(tracekey);
    Trangelist traceranges=
      tfxx::string::rangelist<Trangelist::Tvalue>(infile->value(tracekey));

    int itrace=0;
    while (is.good())
    {
      ++itrace;
      if ((!doselect) || traceranges.contains(itrace))
      {
        TFXX_debug(opt.debug, "main", "process trace #" << itrace );
        if (opt.verbose)
        { std::cout << "process trace #" << itrace << ":"; }
        Tseries series;
        is >> series;
        sff::WID2 wid2;
        is >> wid2;
        TFXX_debug(opt.debug, "main", "  series and WID2 are read:\n" << wid2);
        sff::INFO info; 
        if (is.hasinfo()) { is >> info; }

        // apply filter only if requested
        // ------------------------------
        if ((!doselect) || traceranges.contains(itrace))
        {
  /*======================================================================*/
  /* actual processing takes place here */

          // pad in the case of frequency domain processing
          if (!opt.tdfilter)
          {
            series=padseries(series, opt);
          }

          // extract trace parameters
          TFXX_assert(is.hasinfo() && srceavailable,
                      "Source and receiver coordinates are required");

          Parameters par;
          if (opt.spatialdistance)
          {
            par.offset=::sff::sourcedistance(insrceline, info);
          }
          else
          {
            par.offset=::sff::offset(insrceline, info);
          }
          par.t0=libtime::time2double(insrceline.date-wid2.date);
          par.T=wid2.dt*series.size();
          par.dt=wid2.dt;
          par.nsamples=wid2.nsamples;

          if (opt.verbose)
          {
            cout << "  offset=" << par.offset << "m"
              << "; "
              << "  t0=" << par.t0 << "s"
              << "; "
              << "  T=" << par.T << "s"
              << "; "
              << "  dt=" << par.dt << "s"
              << endl;
          }

          TFXX_assert(fabs(par.t0) < 1.e-6,
                      "Cannot handle shift of time origin in data");

          // store space for results
          Tseries singlevelocityseries, directwaveseries;

          /* processing is prepared
           * select different approaches
           * ---------------------------
           */
          if (opt.fredomain || (opt.transition && 
                                (par.offset <= opt.transition2)))
          {
            singlevelocityseries
              =singlevelocitytransformation(series,
                                            par,
                                            expansioncoefficients,
                                            opt);
          } 

          if ((!opt.fredomain) || (opt.transition && 
                                        (par.offset >= opt.transition1)))
          {
            directwaveseries=tdtapertransformation(series,
                                                   par,
                                                   opt);
          } 

          /* transformation results are available
           * construct final output
           */
          // mixing stage
          if (opt.transition)
          {
            series=transitionmixer(singlevelocityseries,
                                   directwaveseries,
                                   par, opt);
          }
          else if (opt.fredomain)
          {
            series=singlevelocityseries;
          }
          else
          {
            series=directwaveseries;
          }

          if (opt.limitlength)
          {
            series.setlastindex(wid2.nsamples+series.f()-1);
          }

          TFXX_debug(opt.debug, "main", "  series is filtered");
  /*
   * processing has finished
   */
  /*======================================================================*/

          if (opt.verbose) { std::cout << " filtered" << std::endl; }
        // ------------------------------
        }
        else
        {
          if (opt.verbose) { std::cout << " passed unchanged" << std::endl; }
        }

        os << wid2;
        if (is.hasinfo()) { if (os.handlesinfo()) { os << info; } }
        if (is.hasfree() || true) 
        {
          sff::FREE tracefree;
          is >> tracefree;
          tracefree.append(LISOUSI_VERSION);
          tracefree.append("read from file " + infile->name);
          if ((!doselect) || traceranges.contains(itrace))
          {
            tracefree.append(processingreport);
          }
          else
          {
            tracefree.append("passed unchanged");
          }
          if (os.handlestracefree()) { os << tracefree; } 
        }
        TFXX_debug(opt.debug, "main", 
                   "trace #" << itrace << " successfully processed");
        os << series;
        TFXX_debug(opt.debug, "main", "  series and WID are written");
      }
      else
      {
        TFXX_debug(opt.debug, "main", "skip trace #" << itrace );
        if (opt.verbose)
        { std::cout << "     skip trace #" << itrace << std::endl; }
        is.skipseries();
      }
    }
    
    // go to next file
    firstfile=false;
    ++infile;
  }

}


/* ----- END OF lisousi.cc ----- */
