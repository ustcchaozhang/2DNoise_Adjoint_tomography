/*! \file mgm2sff.cc
 * \brief Convert Monschau data to SFF
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/11/2006
 * 
 * Convert Monschau data to SFF
 * 
 * Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
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
 * ============================================================================
 * 
 * Format definition by Sebastion Schork:
 *
 * From s.schork@skytron-energy.com Fri Nov 17 17:48:19 2006
 * Date: Thu, 12 Oct 2006 18:43:23 +0200
 * From: Sebastian Schork <s.schork@skytron-energy.com>
 * To: Thomas.Forbriger@gpi.uni-karlsruhe.de
 * Subject: Monschauer Messdaten
 * 
 *     [ The following text is in the "UTF-8" character set. ]
 *     [ Your display is set for the "ISO-8859-1" character set.  ]
 *     [ Some characters may be displayed incorrectly. ]
 * 
 * Sehr geehrter Dr. Forbringer,
 * 
 * Herr Arndt schrieb mir, dass Sie auf unserer Jubiläumsveranstaltung
 * einen Vortrag halten werden und sich daher für die Messdaten
 * interessieren. 
 * 
 * Ich habe die - leider nie ganz fertiggestellte - Dokumentation zu der
 * Stationssoftware dieser Mail beigelegt. 
 * 
 * Auszug:
 * 
 * ---
 * 
 * Das Monschauer-Geo-Format
 *  
 * Ein Geo-Datensatz besteht aus zwei Dateien: geo.inf und geo.dat. geo.inf
 * enthält den Zeitraum, in dem die Daten gemessen wurden, die in geo.dat
 * gespeichert wurden. geo.dat enthält die Startsekunde und endet in dem
 * Moment, in dem die Endsekunde anfängt (alles klar? ;-) 
 * Erzeugt werden beide Dateien vom Datensammler gdata und können mit
 * gbrowser betrachtet werden. 
 * Aufbau der Datei geo.inf: 
 * <Dateianfang>
 * Offset 0: unsigned short int Start-Stunde
 * Offset 2: unsigned short int Start-Minute
 * Offset 4: unsigned short int Start-Sekunde
 * Offset 6: unsigned short int Start-Jahr
 * Offset 8: unsigned short int Start-Monat (1 bis 12)
 * Offset 10: unsigned short int Start-Tag (1 bis 31)
 * Offset 12: unsigned short int Start-Wochentag (0 bis 6, 0 = Sonntag)
 * Offset 14: unsigned short int End-Stunde
 * Offset 16: unsigned short int End-Minute
 * Offset 18: unsigned short int End-Sekunde
 * Offset 20: unsigned short int End-Jahr
 * Offset 22: unsigned short int End-Monat (1 bis 12)
 * Offset 24: unsigned short int End-Tag (1 bis 31)
 * Offset 26: unsigned short int End-Wochentag (0 bis 6, 0 = Sonntag)
 * <Dateiende>
 *  In geo.dat wiederholt sich stndig ein Block von 3 x 2 Bytes Lnge, in
 * den jeweils die Meßdaten von drei KanÃ¤len (den Seismographenspulen)
 * gespeichert werden. Die Meßwerte sind stets 16 Bit lang, die untersten 4
 * Bit sind 0 (Ausnahme Kanal 1, siehe unten). Ein Wert von 0 (0x0000)
 * bedeutet -1 Volt, 65520 (0xFFF0) bedeutet +1 Volt. 
 * In den nicht genutzten Bit vom ersten Kanal wird das DCF-Signal
 * versteckt: 
 * Bit 0: Sekundenmarke
 * Bit 1: Minutenmarke
 * <Dateianfang>
 * Offset 0: unsigned short int Kanal 1 und DCF-Signal, 1. Sample
 * Offset 2: unsigned short int Kanal 2, 1. Sample
 * Offset 4: unsigned short int Kanal 3, 1. Sample
 * Offset 6: unsigned short int Kanal 1 und DCF-Signal, 2. Sample
 * Offset 8: unsigned short int Kanal 2, 2. Sample.
 * usw.
 * <Dateiende>
 * 
 * ---
 * 
 * Hinzuzufügen wäre noch, dass die Bytereihenfolge "big endian" ist und
 * die Abtastrate etwa 16 Hz beträgt.
 * 
 * Unter http://www.schorkseiten.de/quake.html können Sie ein
 * Betrachtungsprogramm für Linux und Windows herunterladen.
 * 
 * Mit freundlichen Grüßen,
 * 
 *   Sebastian Schork
 * 
 * -- 
 * Sebastian Schork <s.schork@skytron-energy.com>
 * 
 *                                        \ /
 * Skytron-Energy                        - O -
 * Forschung und Entwicklung               |
 *                                       --)/\/\
 * Ernst-Augustin-Straße 12             ---)/\/\
 * 12489 Berlin                         ---)/\/\
 * 030-6185074                             |\
 *                                        _| /
 *
 * ============================================================================
 * 
 * REVISIONS and CHANGES 
 *  - 13/11/2006   V1.0   Thomas Forbriger (thof)
 *  - 01/02/2014 thof:    adjust code to use tsioxx/sfftimeseries.h instead
 *                        of tsxx/sffheaders.h; the former has replaced the
 *                        latter; the latter has vanished
 * 
 * ============================================================================
 */
#define MGM2SFF_VERSION \
  "MGM2SFF   V2014-02-01   Convert Monschau data to SFF"
#define MGM2SFF_CVSID \
  "$Id$"

#include <iostream>
#include <fstream>
#include <vector>
#include <aff/series.h>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <libtime++.h>
#include <sffxx.h>
#include <sffostream.h>
#include <tsioxx/sfftimeseries.h>


using std::cout;
using std::cerr;
using std::endl;

/*======================================================================*/

typedef float Tvalue;
typedef aff::Series<Tvalue> Tseries;
typedef ts::sff::SFFTimeSeries<Tseries> TSFFseries;
typedef TSFFseries::Theader Theader;

/*======================================================================*/
/*
 * class declarations
 * ==================
 */

/*
 * header file (geo.inf)
 * ===========
 */
class GeoInfFile {
  public:
    //! return time and date of first sample
    libtime::TAbsoluteTime first() const;
    //! return time and date of last sample
    libtime::TAbsoluteTime last() const;
  private:
    //! time and date of first sample
    unsigned short int Mshour, Msminute, Mssecond, Msyear, Msmonth, Msday,
      Msweekday;
    //! time and date of last sample
    unsigned short int Mehour, Meminute, Mesecond, Meyear, Memonth, Meday,
      Meweekday;
}; // class GeoInfFile

/*----------------------------------------------------------------------*/

/* 
 * data file (geo.dat)
 * =========
 */
class GeoDatFile {
  public:
    GeoDatFile(std::istream& is);
    Tseries series(const int& i) const;
    Tseries timemark() const { return(Mtm); }
  private:
    Tseries Ms1, Ms2, Ms3, Mtm;
}; // class GeoDatFile

/*
 * options
 * =======
 */
struct Options {
  bool verbose, readinf, readdat, overwrite, debug, usetruenumber;
  std::string inffile, datfile;
};

/*----------------------------------------------------------------------*/
/* 
 * Byte Block from data file
 * =========================
 */

class ByteBlock {
  public:
    Tvalue v(const int& i) const;
    Tvalue tm() const;
  private:
    unsigned short int w1, w2, w3;
}; // class ByteBlock

/*======================================================================*/
/*
 * class definitions
 * =================
 */

libtime::TAbsoluteTime GeoInfFile::first() const
{
  libtime::TAbsoluteTime retval(this->Msyear,
                                this->Msmonth,
                                this->Msday,
                                this->Mshour,
                                this->Msminute,
                                this->Mssecond);
  return(retval);
} // libtime::TAbsoluteTime GeoInfFile::first()

/*----------------------------------------------------------------------*/

libtime::TAbsoluteTime GeoInfFile::last() const
{
  libtime::TAbsoluteTime retval(this->Meyear,
                                this->Memonth,
                                this->Meday,
                                this->Mehour,
                                this->Meminute,
                                this->Mesecond);
  return(retval);
} // libtime::TAbsoluteTime GeoInfFile::last()

/*----------------------------------------------------------------------*/

GeoDatFile::GeoDatFile(std::istream& is)
{
  ByteBlock bb;
  std::vector<Tvalue> in1, in2, in3, intm;
  while (is.good())
  {
    typedef char* pchar;
    is.read(pchar(&bb), sizeof(ByteBlock));
    in1.push_back(bb.v(1));
    in2.push_back(bb.v(2));
    in3.push_back(bb.v(3));
    intm.push_back(bb.tm());
  }
  this->Ms1=Tseries(in1.size());
  for (int i=Ms1.f(); i<=Ms1.l(); ++i)
  { Ms1(i)=in1[i-Ms1.f()]; }
  this->Ms2=Tseries(in2.size());
  for (int i=Ms2.f(); i<=Ms2.l(); ++i)
  { Ms2(i)=in2[i-Ms2.f()]; }
  this->Ms3=Tseries(in3.size());
  for (int i=Ms3.f(); i<=Ms3.l(); ++i)
  { Ms3(i)=in3[i-Ms3.f()]; }
  this->Mtm=Tseries(intm.size());
  for (int i=Mtm.f(); i<=Mtm.l(); ++i)
  { Mtm(i)=intm[i-Mtm.f()]; }
} // GeoDatFile::GeoDatFile(std::istream& is)

/*----------------------------------------------------------------------*/

Tseries GeoDatFile::series(const int& i) const
{
  Tseries retval;
  switch(i) {
    case 1: retval=this->Ms1;
            break;
    case 2: retval=this->Ms2;
            break;
    case 3: retval=this->Ms3;
            break;
    default: TFXX_abort("GeoDatFile: illegal index!");
  }
  return(retval);
} // Tseries GeoDatFile::series(const int& i)

/*----------------------------------------------------------------------*/

Tvalue ByteBlock::v(const int& i) const
{
  unsigned short int w;
  switch(i) {
    case 1: w=this->w1;
            break;
    case 2: w=this->w2;
            break;
    case 3: w=this->w3;
            break;
    default: TFXX_abort("ByteBlock: illegal index!");
  }
  Tvalue v=float((w >> 4)-2048)/2048;
  return(v);
} // Tvalue ByteBlock::v(const int& i)

/*----------------------------------------------------------------------*/

Tvalue ByteBlock::tm() const
{
  Tvalue v=float(w1 & 0x03);
  return(v);
} // Tvalue ByteBlock::tm()

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    MGM2SFF_VERSION "\n"
    "usage: mgm2sff [-v] [-o] [-t] [-inf file] [-dat file] inbase outfile" "\n"
    "   or: mgm2sff --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    MGM2SFF_CVSID "\n"
    "-v           be verbose" "\n"
    "-o           overwrite existing output file" "\n"
    "-t           use true number of samples, if expected number" "\n"
    "             differs from the number of samples read from file" "\n"
    "-inf file    read inf file and dump" "\n"
    "-dat file    read dat file and dump" "\n"
    "inbase       basename for input data" "\n"
    "             inbase.inf and inbase.dat will be read" "\n"
    "outfile      filename for SFF output" "\n"
    "\n"
    "Data is converted to Volt."
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: read inf file and dump
    {"inf",arg_yes,"-"},
    // 3: read dat file and dump
    {"dat",arg_yes,"-"},
    // 4: overwrite existing output
    {"o",arg_no,"-"},
    // 5: debug mode
    {"D",arg_no,"-"},
    // 6: use true number of samples
    {"t",arg_no,"-"},
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

  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.readinf=cmdline.optset(2);
  opt.inffile=cmdline.string_arg(2);
  opt.readdat=cmdline.optset(3);
  opt.datfile=cmdline.string_arg(3);
  opt.overwrite=cmdline.optset(4);
  opt.debug=cmdline.optset(5);
  opt.usetruenumber=cmdline.optset(6);

  TFXX_assert(cmdline.extra(), "ERROR: missing input basename");
  std::string inbase=cmdline.next();
  TFXX_assert(cmdline.extra(), "ERROR: missing output filename");
  std::string outfile=cmdline.next();

  if (opt.readinf)
  {
    /*----------------------------------------------------------------------*/
    /* read inf and dump */

    if (opt.verbose) 
    { 
      cout << "read inf file " << opt.inffile 
        << " and dump contents" << endl;
    }
    std::ifstream ifs(opt.inffile.c_str());
    GeoInfFile gif;
    typedef char * pchar;
    ifs.read(pchar(&gif), sizeof(GeoInfFile));
    cout << "first sample at " << gif.first().timestring() << endl;
    cout << "last sample at " << gif.last().timestring() << endl;
  }
  else if (opt.readdat)
  {
    /*----------------------------------------------------------------------*/
    /* read dat and dump */

    if (opt.verbose) 
    { 
      cout << "read dat file " << opt.datfile 
        << " and dump parameters" << endl;
    }
    std::ifstream ifs(opt.datfile.c_str());
    GeoDatFile gdf(ifs);
    cout << "index range of series: " << endl;
    cout << "ch 1: " << gdf.series(1).f()
      << " - " << gdf.series(1).l() << endl;
    cout << "ch 2: " << gdf.series(2).f()
      << " - " << gdf.series(2).l() << endl;
    cout << "ch 3: " << gdf.series(3).f()
      << " - " << gdf.series(3).l() << endl;
    cout << "marks: " << gdf.timemark().f()
      << " - " << gdf.timemark().l() << endl;
    cout << "Assuming 16Hz sampling, " << gdf.timemark().l()+2
      << " intervals span a time of " 
      << double(gdf.timemark().l()+2)/16.
      << " seconds."
      << endl;
  }
  else
  {
    /*----------------------------------------------------------------------*/
    /* read data files and convert to SFF */
    /* ================================== */

    std::string ininf=inbase+".inf";
    std::string indat=inbase+".dat";
    const libtime::TRelativeTime onesecond(0,0,0,1);
    const libtime::TRelativeTime nominalsampling(0,0,0,0,62,500);
    if (opt.verbose)
    {
      cout << "reading from files: " 
        << ininf << " and " << indat << endl;
      cout << "writing to file " << outfile << endl;
    }

    /*----------------------------------------------------------------------*/
    /* extract time range from inf file */

    GeoInfFile gif;
    {
      std::ifstream ifs(ininf.c_str());
      typedef char * pchar;
      ifs.read(pchar(&gif), sizeof(GeoInfFile));
    }
    unsigned long int expectedsamples=
      (((gif.last()-gif.first())+onesecond)/nominalsampling);
    if (opt.verbose)
    { 
      cout << "first sample at " << gif.first().timestring() << endl;
      cout << "last sample at " << gif.last().timestring() << endl;
      cout << "nominal sampling is: "
        << onesecond/nominalsampling << " samples per" 
        << onesecond.timestring() << endl;
      cout << "file is expected to contain " 
        << expectedsamples << " samples." << endl;
    }

    /*----------------------------------------------------------------------*/
    /* extract samples from dat file */

    std::ifstream ifs(indat.c_str());
    GeoDatFile gdf(ifs);
    if (opt.verbose)
    {
      cout << "index range of series: " << endl;
      cout << "ch 1: " << gdf.series(1).f()
        << " - " << gdf.series(1).l() << endl;
      cout << "ch 2: " << gdf.series(2).f()
        << " - " << gdf.series(2).l() << endl;
      cout << "ch 3: " << gdf.series(3).f()
        << " - " << gdf.series(3).l() << endl;
      cout << "marks: " << gdf.timemark().f()
        << " - " << gdf.timemark().l() << endl;
      cout << "Assuming 16Hz sampling, " << gdf.timemark().l()+2
        << " intervals span a time of " 
        << double(gdf.timemark().l()+2)/16.
        << " seconds."
        << endl;
    }
    if (opt.usetruenumber)
    {
      if (gdf.series(1).size() != expectedsamples)
      {
        cout << "NOTICE: number of samples for channel 1"
          << " differs from expected number" << endl;
        cout << "        using number of samples in channel 1"
          << " as expected number of samples" << endl;
        expectedsamples = gdf.series(1).size();
      }
    }
    TFXX_assert(gdf.series(1).size() == expectedsamples,
                "channel 1 size does not meet expectation");
    TFXX_assert(gdf.series(2).size() == expectedsamples,
                "channel 2 size does not meet expectation");
    TFXX_assert(gdf.series(3).size() == expectedsamples,
                "channel 3 size does not meet expectation");
    TFXX_assert(gdf.timemark().size() == expectedsamples,
                "timemark size does not meet expectation");

    /*----------------------------------------------------------------------*/
    /* write SFF data */

    // check if output file exists and open
    if (!opt.overwrite)
    {
      std::ifstream file(outfile.c_str(),std::ios_base::in);
      TFXX_assert((!file.good()),"ERROR: output file exists!");
    }
    std::ofstream ofs(outfile.c_str());
    sff::SFFostream<Tseries> os(ofs, opt.debug);

    // prepare file free block
    sff::FREE filefree;
    filefree.append(MGM2SFF_VERSION); 
    filefree.append(MGM2SFF_CVSID);
    filefree.append("input was read from files:");
    filefree.append(ininf+" and "+indat);
    filefree.append("files contain data for");
    filefree.append(gif.first().timestring()+" - "+gif.last().timestring());
    filefree.append("data is converted to volt");

    os << filefree;
    if (opt.verbose)
    {
      cout << endl << "file FREE block:" << endl;
      filefree.write(cout);
    }

    /* prepare traces and write to output */
    TSFFseries sffseries;    
    Theader& header(sffseries.header);
    sff::WID2 wid2line;

    wid2line.date=gif.first();
    wid2line.station="MGM";
    wid2line.nsamples=expectedsamples;
    wid2line.dt=libtime::time2double(nominalsampling);

    {
      sffseries=gdf.series(3);
      wid2line.channel="Z";
      sff::FREE tracefree;
      tracefree.append("data from file "+indat);
      tracefree.append("contains vertical (UD) component");
      header.free(tracefree);
      header.wid2(wid2line);
      if (opt.verbose)
      {
        cout << endl << "trace header:" << endl;
        cout << wid2line.line();
        tracefree.write(cout);
      }
      os << sffseries;
    }

    {
      sffseries=gdf.series(1);
      wid2line.channel="N";
      sff::FREE tracefree;
      tracefree.append("data from file "+indat);
      tracefree.append("contains north-south (NS) component");
      header.free(tracefree);
      header.wid2(wid2line);
      if (opt.verbose)
      {
        cout << endl << "trace header:" << endl;
        cout << wid2line.line();
        tracefree.write(cout);
      }
      os << sffseries;
    }

    {
      sffseries=gdf.series(2);
      wid2line.channel="E";
      sff::FREE tracefree;
      tracefree.append("data from file "+indat);
      tracefree.append("contains east-west (EW) component");
      header.free(tracefree);
      header.wid2(wid2line);
      if (opt.verbose)
      {
        cout << endl << "trace header:" << endl;
        cout << wid2line.line();
        tracefree.write(cout);
      }
      os << sffseries;
    }

    {
      sffseries=gdf.timemark();
      wid2line.channel="TIM";
      sff::FREE tracefree;
      tracefree.append("data from file "+indat);
      tracefree.append("contains time marks");
      header.free(tracefree);
      header.wid2(wid2line);
      if (opt.verbose)
      {
        cout << endl << "trace header:" << endl;
        cout << wid2line.line();
        tracefree.write(cout);
      }
      os << sffseries;
    }
  }
}

/* ----- END OF mgm2sff.cc ----- */
