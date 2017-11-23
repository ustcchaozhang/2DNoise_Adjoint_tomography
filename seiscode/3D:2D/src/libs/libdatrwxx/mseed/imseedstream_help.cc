/*! \file imseedstream_help.cc
 * \brief imseedstream help function (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/07/2016
 * 
 * imseedstream help function (implementation)
 * 
 * Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.

 * ----
 *
 * REVISIONS and CHANGES 
 *  - 05/07/2016   V1.0   Thomas Forbriger
 *  - 12/07/2016   V1.1   thof: new consistency check ID "usec"
 * 
 * ============================================================================
 */
#define DATRW_IMSEEDSTREAM_HELP_CC_VERSION \
  "DATRW_IMSEEDSTREAM_HELP_CC   V1.1"

#include <datrwxx/mseed.h>
#include <datrwxx/formatmodifier.h>
#include <datrwxx/mseed_keywords.h>

namespace datrw {

  using std::endl;
  using namespace mseed;

  void imseedstream::help(std::ostream& os)
  {
    os << std::endl;
    os << "MiniSEED data reading functions" << std::endl;
    os << "-------------------------------" << std::endl;
    os << endl;
    os << "This module is designed to read MiniSEED (data only SEED) files"
      << endl;
    os << "created by several different data acquisition systems. It is"
      << endl;
    os << "tested with the following systems:"
      << endl;
    os << "  comserv with Q680 (Quanterra with SeisComP, GRSN setup)"
      << endl;
    os << "  seedlink (new SeisComP system with EDD plugin)"
      << endl;
    os << "  Earth Data Logger (EDL raw files)"
      << endl;
    os << "  Q330HR (Quanterra data acquisition system with Baler)"
      << endl;
    os << "  Taurus (Nanometrics data acquisition system)"
      << endl;
    os << "  Q330HR (data streamed from Q330HR)"
      << endl;
    os << "  Cube3 (data converted by cube2mseed)"
      << endl;
    os << endl;
    os << "Special features:"
      << endl;
    os << "Data files written by the datalog client of comserv commence"
      << endl;
    os << "with a telemetry volume header which is non-standard MiniSEED."
      << endl;
    os << "This module identifies and skips the telemetry volume headers."
      << endl << endl;
    os << "The module swaps data bytes if necessary. The SEED standard"
      << endl;
    os << "defines (page 12 of SEED manual) that all headers must have"
      << endl;
    os << "big-endian byte-sex. The byte-sex of MiniSEED data blocks is"
      << endl;
    os << "defined in the header. The Earth Data Logger violates this"
      << endl;
    os << "definition and writes headers in little-endian order. The"
      << endl;
    os << "module tries to guess the correct byte-sex in this case"
      << endl;
    os << "when reading the header blockettes."
      << endl << endl;
    os << "Several consistency checks (see below) are applied to the"
      << endl;
    os << "data file. The library module this way refuses to read data"
      << endl;
    os << "which obviously is corrupt or which header parameters are"
      << endl;
    os << "not plausible or are at variance with the data actually read."
      << endl;
    os << "There are data of known sources which fail these tests,"
      << endl;
    os << "although the data itself is intact. Such files are considered"
      << endl;
    os << "to be at variance with the definition of Data Only SEED (MiniSEED)."
      << endl;
    os << "To be able to work on such data, consistency checks can be made"
      << endl;
    os << "non-fatal or even be switched of by format modifiers (see below)."
      << endl;
    os << endl;
    {
      formatmodifiers::ModifierHelp mh(os, 24);
      os << "Available format modifiers:\n";
      mh(key::dumpascii) 
        << "dumps ASCII data blocks to stdout\n";
      mh(key::ttolerance, "T")
        << "tolerate time jitter, if not larger than\n";
      mh << "\"T\", where \"T\" is given in microseconds\n";
      mh(key::estimateNframes)
        << "estimate number of frames rather than reading\n";
      mh << "it from blockette1001; this is required for\n";
      mh << "data created by \"cube2mseed\" which contains\n";
      mh << "an invalid count of frames\n";
      mh(key::nonfatal, "ID[,ID,ID]")
        << "make consistency check \"ID\" non-fatal (report only)\n";
      mh << "one or more check \"ID\" values may be passed in a list\n";
      mh(key::skipcheck, "ID[,ID,ID]")
        << "skip consistency check \"ID\" entirely (ignore)\n";
      mh << "one or more check \"ID\" values may be passed in a list\n";
      os << endl;
    }
    {
      formatmodifiers::ModifierHelp mh(os, 12);
      os << "Available consistency checks (IDs to be used with modifiers):\n";
      mh(key::nframes) 
        << "The number of frames to be expected is specified in\n";
      mh << "the header in Blockette 1001. This value will be\n";
      mh << "compared against the number of frames actually\n";
      mh << "used to store data.\n";
      mh << "This test is only applied if the number of frames\n";
      mh << "actually is specified in the header. Some data require\n";
      mh << "the number of frames to be guessed (if Blockette 1001)\n";
      mh << "is missing.\n";
      mh(key::nsamples)
        << "The number of samples to be expected is provided in\n";
      mh << "the header in the fixed section data header. This\n";
      mh << "value will be compared against the number of\n";
      mh << "samples actually provided in the logical record.\n";
      mh(key::data)
        << "The sample value of the last sample in the record\n";
      mh << "is compared against the value of the reverse\n";
      mh << "integration constant as provided in the first data\n";
      mh << "frame of the record. Both must be identical.\n";
      mh(key::usec)
        << "The SEED Reference Manual (Version 2.4, August, 2012, page 124)\n";
      mh << "specifies the usec field in [1001] Data Extension Blockette:\n";
      mh << "The recommended value is from -50 to +49 usecs. At the users\n";
      mh << "option, this value may be from 0 to +99 usecs.\n";
      mh(key::all)
        << "Select all consistency checks\n";
      os << "Consistency checks will be selected upon a substring match\n"
            "against check IDs. This means that by passing only a substring\n"
            "in the argument list to the format modifier several checks\n"
            "can be selected at once,\n";
      os << endl;
    }
  } // void imseedstream::help(std::ostream& os)

} // namespace datrw

/* ----- END OF imseedstream_help.cc ----- */
