/*! \file formats.cc
 * \brief common description of formats (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 26/11/2010
 * 
 * common description of formats (implementation)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 26/11/2010   V1.0   Thomas Forbriger
 *  - 02/09/2011   V1.1   support seife format
 *  - 08/09/2011   V1.2   strip modifiers from format srting in anyID
 *  - 03/11/2011   V1.3   added format IDs for ASCII, binary, and Thies DL1
 *  - 04/11/2011   V1.4   make Thies DL1 module visible
 *  - 05/11/2011   V1.5   make ASCII output module visible
 *  - 08/11/2011   V1.6   make ASCII input module visible
 *  - 19/11/2011   V1.7   make binary module visible
 *  - 05/12/2011   V1.8   provide format specfic online help 
 *  - 23/07/2017   V1.9   report library version
 * 
 * ============================================================================
 */
#define DATRW_FORMATS_CC_VERSION \
  "DATRW_FORMATS_CC   V1.9"

#include <datrwxx/aalibdatrwxx.h>
#include <datrwxx/formats.h>
#include <datrwxx/formatmodifier.h>
#include <datrwxx/sff.h>
#include <datrwxx/pdas.h>
#include <datrwxx/hpmo.h>
#include <datrwxx/mseed.h>
#include <datrwxx/bonjer.h>
#include <datrwxx/sac.h>
#include <datrwxx/gse.h>
#include <datrwxx/tsoft.h>
#include <datrwxx/tfascii.h>
#include <datrwxx/su.h>
#include <datrwxx/seife.h>
#include <datrwxx/util.h>
#include <datrwxx/ascii.h>
#include <datrwxx/binary.h>
#include <datrwxx/thiesdl1.h>

namespace datrw {
  
  Eformat anyID(std::string formatstring)
  {
    std::string id=util::clipstring(formatstring);
    Eformat retval;
    if (id==pdas::streamID) { retval=Fpdas; }
    else if (id==sff::streamID) { retval=Fsff; }
    else if (id==hpmo::streamID) { retval=Fhpmo; }
    else if (id==mseed::streamID) { retval=Fmseed; }
    else if (id==bonjer::streamID) { retval=Fbonjer; }
    else if (id==sac::streamID) { retval=Fsac; }
    else if (id==gse::streamID) { retval=Fgse; }
    else if (id==tsoft::streamID) { retval=Ftsoft; }
    else if (id==tfascii::streamID) { retval=Ftfascii; }
    else if (id==su::streamID) { retval=Fsu; }
    else if (id==seife::streamID) { retval=Fseife; }
    else if (id==ascii::streamID) { retval=Fascii; }
    else if (id==binary::streamID) { retval=Fbinary; }
    else if (id==thiesdl1::streamID) { retval=Fthiesdl1; }
    else { 
      std::cerr << "selected data format ID: \"" << id << "\"" << std::endl;
      DATRW_abort("unknown data type identifier!"); 
    } 
    return(retval);
  } // Eformat anyID(const std::string& identifier)

  /*----------------------------------------------------------------------*/
  
  std::string anyID(const Eformat& id)
  {
    std::string retval="NSP";
    switch(id) {
      case Fpdas:     retval=pdas::streamID; break;
      case Fsff:      retval=sff::streamID; break;
      case Fhpmo:     retval=hpmo::streamID; break;
      case Fmseed:    retval=mseed::streamID; break;
      case Fbonjer:   retval=bonjer::streamID; break;
      case Fsac:      retval=sac::streamID; break;
      case Fgse:      retval=gse::streamID; break;
      case Ftsoft:    retval=tsoft::streamID; break;
      case Ftfascii:  retval=tfascii::streamID; break;
      case Fsu:       retval=su::streamID; break;
      case Fseife:    retval=seife::streamID; break;
      case Fascii:    retval=ascii::streamID; break;
      case Fbinary:   retval=binary::streamID; break;
      case Fthiesdl1: retval=thiesdl1::streamID; break;
    default: DATRW_abort("unknown data type ID#!");
    }
    return(retval);
  } // std::string anyID(const Eformat& id)

  /*----------------------------------------------------------------------*/

  void online_help(const std::string& format,
                   std::ostream& os,
                   const bool& modifierhelp)
  {
    online_help(anyID(format), os, modifierhelp);
  } // void online_help(const std::string& format, std::ostream& os)

  /*----------------------------------------------------------------------*/

  void online_help(const Eformat& format, 
                   std::ostream& os,
                   const bool& modifierhelp)
  {
    switch(format) {
      case Fsff:
        os << "SFF data input: "; isffstream::help(os);
        os << std::endl;
        os << "SFF data output: "; osffstream::help(os);
        os << std::endl;
        break;
      case Fhpmo:
        os << "HPMO data input: "; ihpmostream::help(os);
        os << std::endl;
        break;
      case Fmseed:
        os << "MiniSEED data input: "; imseedstream::help(os);
        os << std::endl;
        break;
      case Fpdas:
        os << "PDAS data input: "; ipdasstream::help(os);
        os << std::endl;
        break;
      case Fbonjer:
        os << "Bonjer data input: "; ibonjerstream::help(os);
        os << std::endl;
        break;
      case Fsac:
        os << "SAC data input: "; isacstream::help(os);
        os << std::endl;
        break;
      case Fgse:
        os << "GSE data input: "; igsestream::help(os);
        os << std::endl;
        os << "GSE data output: "; ogsestream::help(os);
        os << std::endl;
        break;
      case Ftsoft:
        os << "TSOFT data input: "; itsoftstream::help(os);
        os << std::endl;
        break;
      case Ftfascii:
        os << "TFASCII data input: "; itfasciistream::help(os);
        os << std::endl;
        break;
      case Fsu:
        os << "SU data input: "; isustream::help(os);
        os << std::endl;
        os << "SU data output: "; osustream::help(os);
        os << std::endl;
        break;
      case Fseife:
        os << "seife data input: "; iseifestream::help(os);
        os << std::endl;
        os << "seife data output: "; oseifestream::help(os);
        os << std::endl;
        break;
      case Fthiesdl1:
        os << "Thies DL1 data input: "; ithiesdl1stream::help(os);
        os << std::endl;
        break;
      case Fascii:
        os << "ASCII data input: "; iasciistream::help(os);
        os << std::endl;
        os << "ASCII data output: "; oasciistream::help(os);
        os << std::endl;
        break;
      case Fbinary:
        os << "binary data input: "; ibinarystream::help(os);
        os << std::endl;
        os << "binary data output: "; obinarystream::help(os);
        os << std::endl;
        break;
      default: DATRW_abort("unknown data type ID#!");
    }
    if (modifierhelp)
    {
      os << std::endl;
      datrw::formatmodifiers::online_help(os);
    }
  } // void online_help(const Eformat& format, std::ostream& os)

  /*----------------------------------------------------------------------*/

  void online_help(std::ostream& os)
  {
    os << std::endl
       << datrw::libversion << std::endl
       << "Online help obtained from I/O streams:"
       << std::endl;
    os << "--------------------------------------"
       << std::endl << std::endl;
    online_help(Fsff, os);
    online_help(Fhpmo, os);
    online_help(Fmseed, os);
    online_help(Fpdas, os);
    online_help(Fbonjer, os);
    online_help(Fsac, os);
    online_help(Fgse, os);
    online_help(Ftsoft, os);
    online_help(Ftfascii, os);
    online_help(Fsu, os);
    online_help(Fseife, os);
    online_help(Fthiesdl1, os);
    online_help(Fascii, os);
    online_help(Fbinary, os);
    datrw::formatmodifiers::online_help(os);
  } // void online_help(std::ostream& os)
    
  /*----------------------------------------------------------------------*/

  void supported_input_data_types(std::ostream& os)
  {
    os << datrw::libversion << std::endl;
    os << "data formats supported by ianystream:" << std::endl;
    os.width(13); os << sff::streamID 
      << ": Stuttgart File Format" << std::endl;
    os.width(13); os << hpmo::streamID 
      << ": HP-MO data format defined by W. Grossmann (BFO)" 
      << std::endl;
    os.width(13); os << pdas::streamID 
      << ": PDAS100 (i.e. DaDisp)" << std::endl;
    os.width(13); os << mseed::streamID 
      << ": MiniSEED (SeisComP, EDL, etc.)" << std::endl;
    os.width(13); os << bonjer::streamID 
      << ": K2 ASCII data format (defined by K. Bonjer?)" 
      << std::endl;
    os.width(13); os << sac::streamID << ": SAC binary format" << std::endl;
    os.width(13); os << gse::streamID << ": raw GSE format" << std::endl;
    os.width(13); os << tsoft::streamID << ": TSOFT format" << std::endl;
    os.width(13); os << tfascii::streamID 
      << ": ASCII format of T. Forbrigers any2ascii" << std::endl;
    os.width(13); os << su::streamID << ": SeismicUn*x format" << std::endl;
    os.width(13); os << seife::streamID 
      << ": seife format (E. Wielandt)" << std::endl;
    os.width(13); os << thiesdl1::streamID 
      << ": Thies DL1 pluviometer data at BFO" << std::endl;
    os.width(13); os << ascii::streamID 
      << ": simple single column ASCII data" << std::endl;
    os.width(13); os << binary::streamID 
      << ": binary data" << std::endl;
  }
    
  /*----------------------------------------------------------------------*/

  void supported_output_data_types(std::ostream& os)
  {
    os << datrw::libversion << std::endl;
    os << "data formats supported by oanystream:" << std::endl;
    os.width(13); os << sff::streamID 
      << ": Stuttgart File Format" << std::endl;
    os.width(13); os << gse::streamID << ": raw GSE format" << std::endl;
    os.width(13); os << su::streamID << ": SeismicUn*x format" << std::endl;
    os.width(13); os << seife::streamID 
      << ": seife format (E. Wielandt)" << std::endl;
    os.width(13); os << ascii::streamID 
      << ": simple single column ASCII data" << std::endl;
    os.width(13); os << binary::streamID 
      << ": binary data" << std::endl;
  }

  /*----------------------------------------------------------------------*/
    
  void supported_data_types(std::ostream& os)
  {
    os << "data formats supported for reading:" << std::endl;
    os << "-----------------------------------" << std::endl;
    supported_input_data_types(os);
    os << std::endl;
    os << "data formats supported for writing:" << std::endl;
    os << "-----------------------------------" << std::endl;
    supported_output_data_types(os);
  }

} // namespace datrw

/* ----- END OF formats.cc ----- */
