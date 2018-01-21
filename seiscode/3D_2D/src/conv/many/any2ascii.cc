/*! \file any2ascii.cc
 * \brief convert any input format to ASCII
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 24/09/2004
 * 
 * convert any input format to ASCII
 * 
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * This file is part of the conv/many suite.
 *
 * The conv/many suite is free software; you can redistribute it and/or modify
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
 *  - 24/09/2004   V1.0   Thomas Forbriger
 *  - 27/04/2006   V1.1   introduced option -integer
 *  - 04/05/2006   V1.2   introduced option -precision
 *  - 26/06/2007   V1.3   provide human readable header output
 *  - 01/02/2010   V1.4   now provides additional time column
 *  - 19/04/2010   V1.5   
 *                       - correction: evaluate option -hu correctly
 *  - 25/11/2010   V1.6   
 *                       - use correct open mode upon ifstream open
 *  - 21/06/2012   V1.7  print note: program has become obsolete
 * 
 * ============================================================================
 */
#define ANY2ASCII_VERSION \
  "ANY2ASCII   V1.7   convert any input format to ASCII (obsolete)"

#include <iostream>
#include <fstream>
#include <cstdio>
#include <map>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <tfxx/stringfunc.h>
#include <datrwxx/readany.h>

using std::cout;
using std::cerr;
using std::endl;

/*======================================================================*/


struct Options {
  bool verbose, alltraces, headerfiles, hrdump;
  bool integerdata, timecolumn, hfinfilename;
  int precision;
  std::string dataformat;
  std::string headerfieldselection;
}; // struct Options

struct Header {
  bool hastracefree, hasfilefree, hasinfo, hassrce;
  sff::FREE tracefree, filefree;
  sff::WID2 wid2;
  sff::INFO info;
  sff::SRCE srce;
  int itrace;
  std::string filename;
}; // struct Header

/*======================================================================*/

void writeheader(std::ostream& os, const Header& header) 
{ 
  os << ANY2ASCII_VERSION << endl;
  os << "input file: " << header.filename << endl;
  os << "input trace: " << header.itrace << endl << endl;
  os << "file header: " << endl;
  os << "============ " << endl;
  if (header.hassrce) { sff::verbose(os, header.srce); } 
  else { os << "file header contains no SRCE line" << endl; }
  if (header.hasfilefree) { sff::verbose(os, header.filefree); } 
  else { os << "file header contains no FREE block" << endl; }
  os << endl;
  os << "trace header: " << endl;
  os << "============= " << endl;
  sff::verbose(os, header.wid2);
  if (header.hasinfo) { sff::verbose(os, header.info); } 
  else { os << "trace header contains no INFO line" << endl; }
  if (header.hastracefree) { sff::verbose(os, header.tracefree); } 
  else { os << "trace header contains no FREE block" << endl; }
  os << endl;
}

void writeheaderhr(std::ostream& os, const Header& header) 
{ 
  os << ANY2ASCII_VERSION << endl;
  os << "input file: " << header.filename << endl;
  os << "input trace: " << header.itrace << endl << endl;
  os << "file header: " << endl;
  if (header.hasfilefree) { os << header.filefree << endl; } 
  if (header.hassrce) { os << header.srce.line() << endl << endl;; } 
  os << "trace header: " << endl;
  os << header.wid2.line() << endl;
  if (header.hastracefree) { os << header.tracefree << endl; } 
  if (header.hasinfo) { os << header.info.line() << endl << endl;; } 
}

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    ANY2ASCII_VERSION "\n"
    "usage: any2ascii [-v] [-hf] [-all] [-type type] [-integer] [-hu]" "\n"
    "                 [-precision p] [-tcol] [-namemod m]" "\n"
    "                 infile outbase" "\n"
    "   or: any2ascii --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "read any input data and convert samples to ASCII" "\n"
    "\n"
    "Notice: With the introduction of the format 'ascii' in" "\n"
    "        libdatrwxx, this program has become obsolete. Use" "\n"
    "        any2any instead." "\n"
    "\n"
    "infile       name of input file" "\n"
    "outbase      basename for output files" "\n"
    "\n" 
    "-v           be verbose" "\n"
    "-hf          write header information to separate file" "\n"
    "-all         extract all traces, not only first" "\n"
    "-type type   input file has data format \"type\" (default: sff)" "\n"
    "-integer     read and write integer data" "\n"
    "-precision p set output precision for floating point output" "\n"
    "-hu          dump header in human readable form" "\n"
    "-tcol        print time of sample in first column" "\n"
    "-namemod m   modify filename with header fields" "\n"
    "             modifiers \"m\" can be:" "\n"
    "             d: date of first sample" "\n"
    "             t: time of first sample" "\n"
    "             u: time of first sample with parts of seconds" "\n"
    "             s: station name" "\n"
    "             c: channel name" "\n"
    "             a: auxilliary ID" "\n"
    "             i: instrument" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"verbose",arg_no,"-"},
    // 2: header to separate file(s) 
    {"hf",arg_no,"-"},
    // 3: extract all traces
    {"all",arg_no,"-"},
    // 4: extract all traces
    {"type",arg_yes,"sff"},
    // 5: extract all traces
    {"integer",arg_no,"-"},
    // 6: extract all traces
    {"precision",arg_yes,"7"},
    // 7: human readable header dump
    {"hu",arg_no,"-"},
    // 8: print time in first column
    {"tcol",arg_no,"-"},
    // 9: print time in first column
    {"namemod",arg_yes,"-"},
    {NULL}
  };

  /* ---------------------------------------------------------------------- */

  cerr << ANY2ASCII_VERSION "\n"
    "******************************************************************\n"
    "THIS PROGRAM IS OBSOLETE\n"
    "any2ascii is no longer maintained and may vanish in the future\n"
    "consider to use any2any instead\n"
    "******************************************************************\n";

  /* ---------------------------------------------------------------------- */

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
    cerr << help_text << endl << endl;
    datrw::supported_data_types(cerr);
    datrw::online_help(cerr);
    exit(0);
  }

  Options opt;

  opt.verbose=cmdline.optset(1);
  opt.headerfiles=cmdline.optset(2);
  opt.alltraces=cmdline.optset(3);
  opt.dataformat=cmdline.string_arg(4);
  opt.integerdata=cmdline.optset(5);
  opt.precision=cmdline.int_arg(6);
  opt.hrdump=cmdline.optset(7);
  opt.timecolumn=cmdline.optset(8);
  opt.hfinfilename=cmdline.optset(9);
  opt.headerfieldselection=cmdline.string_arg(9);

  TFXX_assert(cmdline.extra(), "ERROR: missing input filename!");
  std::string infile=cmdline.next();
  TFXX_assert(cmdline.extra(), "ERROR: missing output filename!");
  std::string outfile=cmdline.next();

  std::ifstream ifs(infile.c_str(),
                    datrw::ianystream::openmode(opt.dataformat));
  TFXX_assert(ifs.good(), "ERROR: cannot open input file!");
  datrw::ianystream is(ifs, opt.dataformat);

  Header header;
  header.filename=infile;

  // remember file FREE
  header.hasfilefree=is.hasfree();
  if (header.hasfilefree) { is >> header.filefree; }
  // remember SRCE
  header.hassrce=is.hassrce();
  if (header.hassrce) { is >> header.srce; }

  // filename map to count traces
  std::map<std::string, int> tracecounter;

  bool hot=is.good();
  int itrace=0;
  while (hot)
  {
    // read data
    ++itrace;
    if (opt.verbose) 
    {
      cout << "trace #" << itrace << endl;
    }

    header.itrace=itrace;

    datrw::Tdseries timeseries;
    datrw::Tiseries itimeseries;
    if (opt.integerdata)
    { is >> itimeseries; }
    else
    { is >> timeseries; }
    is >> header.wid2;
    header.hastracefree=is.hasfree();
    if (header.hastracefree) { is >> header.tracefree; }
    header.hasinfo=is.hasinfo();
    if (header.hasinfo) { is >> header.info; }

    /*----------------------------------------------------------------------*/
    // create filename

    std::string outbase=outfile + ".";

    if (opt.hfinfilename)
    {
      for (std::string::const_iterator I=opt.headerfieldselection.begin();
           I != opt.headerfieldselection.end(); ++I)
      {
        libtime::TAbsoluteTime date=header.wid2.date;
        std::ostringstream oss;
        if (*I == 'd')
        {
          oss.width(4);
          oss.fill('0');
          oss << date.year();
          oss.width(2);
          oss.fill('0');
          oss << date.month();
          oss.width(2);
          oss.fill('0');
          oss << date.day() << ".";
          outbase += oss.str();
        }
        else if (*I == 't')
        {
          oss.width(2);
          oss.fill('0');
          oss << date.hour();
          oss.width(2);
          oss.fill('0');
          oss << date.minute();
          oss.width(2);
          oss.fill('0');
          oss << date.second() << ".";
          outbase += oss.str();
        }
        else if (*I == 'u')
        {
          oss.width(2);
          oss.fill('0');
          oss << date.hour();
          oss.width(2);
          oss.fill('0');
          oss << date.minute();
          oss.width(2);
          oss.fill('0');
          oss << date.second() << ".";
          oss.width(3);
          oss.fill('0');
          oss << date.milsec();
          oss.width(3);
          oss.fill('0');
          oss << date.micsec() << ".";
          outbase += oss.str();
        }
        else if (*I == 's')
        {
          outbase += tfxx::string::trimws(header.wid2.station) + ".";
        }
        else if (*I == 'c')
        {
          outbase += tfxx::string::trimws(header.wid2.channel) + ".";
        }
        else if (*I == 'a')
        {
          outbase += tfxx::string::trimws(header.wid2.auxid) + ".";
        }
        else if (*I == 'i')
        {
          outbase += tfxx::string::trimws(header.wid2.instype) + ".";
        }
      }
    }

    if (opt.alltraces)
    {
      std::ostringstream oss;
      oss.width(4);
      oss.fill('0');
      oss << ++tracecounter[outbase] << ".";
      outbase += oss.str();
    }

    if (opt.verbose)
    {
      cout << "  basename for output: " << outbase << endl;
    }

    std::string outputtracename=outbase+std::string("asc");
    std::string outputheadername=outbase+std::string("hdr");

    /*----------------------------------------------------------------------*/
    // write data

    std::ofstream tos(outputtracename.c_str());
    if (opt.headerfiles)
    {
      std::ofstream hos(outputheadername.c_str());
      if (opt.hrdump)
      {
        writeheaderhr(hos, header);
      }
      else
      {
        writeheader(hos, header);
      }
    }
    else
    {
      if (opt.hrdump)
      {
        writeheaderhr(tos, header);
      }
      else
      {
        writeheader(tos, header);
      }
      tos << "data:" << endl;
    }

    if (opt.integerdata)
    {
      for (int isample=itimeseries.first(); isample<=itimeseries.last();
           ++isample)
      {
        if (opt.timecolumn)
        {
          tos.setf(std::ios_base::scientific,std::ios_base::floatfield);
          tos.precision(opt.precision);
          tos << (isample-itimeseries.f())*header.wid2.dt << " ";
        }
        tos << itimeseries(isample) << endl;
      }
    }
    else
    {
      for (int isample=timeseries.first(); isample<=timeseries.last();
           ++isample)
      {
        if (opt.timecolumn)
        {
          tos.setf(std::ios_base::scientific,std::ios_base::floatfield);
          tos.precision(opt.precision);
          tos << (isample-timeseries.f())*header.wid2.dt << " ";
        }
        tos.setf(std::ios_base::scientific,std::ios_base::floatfield);
        tos.precision(opt.precision);
        tos << timeseries(isample) << endl;
      }
    }

    hot=(!is.last());
    if (!opt.alltraces) { hot=false; }
  }

  if (opt.timecolumn) 
  {
    cout << "DEBUG: time column" << endl;
  }
}

/* ----- END OF any2ascii.cc ----- */
