/*! \file readany.cc
 * \brief read any type (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/04/2004
 * 
 * read any type (implementation)
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 06/04/2004   V1.0   Thomas Forbriger
 *  - 16/09/2004   V1.1   activate isffstream
 *  - 23/12/2004   V1.2   activate ihpmostream
 *  - 29/06/2007   V1.3   added SAC binary format
 *  - 19/09/2007   V1.4   added raw GSE format
 *  - 12/11/2009   V1.5   added TSOFT format
 *  - 03/05/2010   V1.6   sac provides debugging
 *  - 06/10/2010   V1.7   added ASCII format of T. Forbrigers any2ascii
 *  - 23/11/2010   V1.8   use static member data
 *  - 25/11/2010   V1.9   add Seismic Unix format
 *  - 29/07/2011   V1.10  support format modifiers
 *  - 02/09/2011   V1.11  support seife format
 *  - 06/09/2011   V1.12  make non-subformat constructor deprecated
 *  - 07/09/2011   V1.13  more string type format ID support: openmode
 *  - 13/09/2011   V1.14  imseedstream now supports modifiers too
 *  - 04/11/2011   V1.15  make Thies DL1 input module visible
 *  - 07/11/2011   V1.16  make ASCII input module visible
 *  - 19/11/2011   V1.17  provide binary input
 *  - 05/12/2011   V1.18  itsoftstream takes format modifiers
 *  - 21/01/2012   V1.19  prepared isustream to take modifiers
 *  - 18/11/2016   V1.20  set debug mode in base class
 * 
 * ============================================================================
 */
#define DATRW_READANY_CC_VERSION \
  "DATRW_READANY_CC   V1.20"

#include <datrwxx/readany.h>
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
#include <datrwxx/error.h>
#include <datrwxx/formatmodifier.h>
#include <datrwxx/util.h>
#include <datrwxx/thiesdl1.h>
#include <datrwxx/ascii.h>
#include <datrwxx/binary.h>
#include <datrwxx/report.h>

namespace datrw {

  /*! return whether the specific format is a binary data format
   *
   * \param format selected data format
   * \return \c true, if data format is binary
   */
  bool isbinary(const Eformat& format)
  {
    bool retval=false;
    if (format==Fbonjer) { retval=bonjer::isbinary; }
    else if (format==Fpdas) { retval=pdas::isbinary; }
    else if (format==Fsff) { retval=sff::isbinary; }
    else if (format==Fhpmo) { retval=hpmo::isbinary; }
    else if (format==Fmseed) { retval=mseed::isbinary; }
    else if (format==Fsac) { retval=sac::isbinary; }
    else if (format==Fgse) { retval=gse::isbinary; }
    else if (format==Ftsoft) { retval=tsoft::isbinary; }
    else if (format==Ftfascii) { retval=tfascii::isbinary; }
    else if (format==Fsu) { retval=su::isbinary; }
    else if (format==Fseife) { retval=seife::isbinary; }
    else if (format==Fthiesdl1) { retval=thiesdl1::isbinary; }
    else if (format==Fascii) { retval=ascii::isbinary; }
    else if (format==Fbinary) { retval=binary::isbinary; }
    else 
    { DATRW_abort("ERROR (isbinary): unknown format!"); }
    return(retval);
  } // bool isbinary(const Eformat& format)

  /*----------------------------------------------------------------------*/

  /*! return appropriate file stream open mode for selected format
   */
  std::ios_base::openmode ianystream::openmode(const std::string& format)
  {
    return(ianystream::openmode(anyID(format)));
  } // std::ios_base::openmode ianystream::openmode(std::string format)

  /*----------------------------------------------------------------------*/

  /*! return appropriate file stream open mode for selected format
   */
  std::ios_base::openmode ianystream::openmode(const Eformat& format)
  {
    std::ios_base::openmode retval;
    if (format==Fbonjer) { retval=ibonjerstream::openmode; }
    else if (format==Fpdas) { retval=ipdasstream::openmode; }
    else if (format==Fsff) { retval=isffstream::openmode; }
    else if (format==Fhpmo) { retval=ihpmostream::openmode; }
    else if (format==Fmseed) { retval=imseedstream::openmode; }
    else if (format==Fsac) { retval=isacstream::openmode; }
    else if (format==Fgse) { retval=igsestream::openmode; }
    else if (format==Ftsoft) { retval=itsoftstream::openmode; }
    else if (format==Ftfascii) { retval=itfasciistream::openmode; }
    else if (format==Fsu) { retval=isustream::openmode; }
    else if (format==Fseife) { retval=iseifestream::openmode; }
    else if (format==Fthiesdl1) { retval=ithiesdl1stream::openmode; }
    else if (format==Fascii) { retval=iasciistream::openmode; }
    else if (format==Fbinary) { retval=ibinarystream::openmode; }
    else 
    { DATRW_abort("ERROR (openmode): unknown format!"); }
    return(retval);
  } // std::ios_base::openmode ianystream::openmode(const Eformat& format)

  /*----------------------------------------------------------------------*/

  ianystream::ianystream(std::istream& is, const Eformat& format,
                         const bool& debug)
   {
     datrw::util::report_deprecated("ianystream::ianystream(std::istream& is, "
                      "const Eformat& format)",
                      "this constructor does not support format modifiers");
     this->open(is, anyID(format), debug);
   }

  /*----------------------------------------------------------------------*/

  ianystream::ianystream(std::istream& is, const std::string& format,
                         const bool& debug)
   {
     this->open(is, format, debug);
   }

  /*----------------------------------------------------------------------*/

  void ianystream::open(std::istream& is, std::string format,
                        const bool& debug)
  {
    std::string formatstring=util::clipstring(format);
    std::string& modifiers=format;
    Mformat=anyID(formatstring);
    if (Mformat==Fbonjer) 
    {
      DATRW_expect_no_modifier(Mformat,modifiers);
      Mis=new ibonjerstream(is); 
    }
    else if (Mformat==Fpdas) 
    {
      DATRW_expect_no_modifier(Mformat,modifiers);
      Mis=new ipdasstream(is); 
    }
    else if (Mformat==Fsff) 
    {
      DATRW_expect_no_modifier(Mformat,modifiers);
      Mis=new isffstream(is); 
    }
    else if (Mformat==Fhpmo) 
    {
      DATRW_expect_no_modifier(Mformat,modifiers);
      Mis=new ihpmostream(is); 
    }
    else if (Mformat==Fmseed) 
    {
      Mis=new imseedstream(is, modifiers); 
    }
    else if (Mformat==Fsac) 
    {
      DATRW_expect_no_modifier(Mformat,modifiers);
      Mis=new isacstream(is); 
    }
    else if (Mformat==Fgse) 
    {
      DATRW_expect_no_modifier(Mformat,modifiers);
      Mis=new igsestream(is); 
    }
    else if (Mformat==Ftsoft) 
    { 
      Mis=new itsoftstream(is, modifiers); 
    }
    else if (Mformat==Ftfascii) 
    {
      DATRW_expect_no_modifier(Mformat,modifiers);
      Mis=new itfasciistream(is); 
    }
    else if (Mformat==Fsu) 
    {
      Mis=new isustream(is, modifiers); 
    }
    else if (Mformat==Fseife) 
    {
      Mis=new iseifestream(is, modifiers); 
    }
    else if (Mformat==Fthiesdl1) 
    {
      Mis=new ithiesdl1stream(is, modifiers); 
    }
    else if (Mformat==Fascii) 
    {
      Mis=new iasciistream(is, modifiers); 
    }
    else if (Mformat==Fbinary) 
    {
      DATRW_expect_no_modifier(Mformat,modifiers);
      Mis=new ibinarystream(is); 
    }
    else 
    { DATRW_abort("ERROR (ianystream): unknown format!"); }
    Mis->debug(debug);
  }

  /*----------------------------------------------------------------------*/

  ianystream::~ianystream()
  {
    delete Mis;
  }

} // namespace datrw

/*======================================================================*/

/*! \page page_formats Supported formats
 *

  Currently supported \b input \b formats are:
  - mseed:    MiniSEED (SeisComP, EDL, etc.), see: \subpage page_mseed
  - pdas:     PDAS100 (i.e. DaDisp)
  - sac:      SAC binary format
  - sff:      Stuttgart File Format
  - gse:      raw GSE format
  - hpmo:     HP-MO data format defined by W. Grossmann (BFO)
  - bonjer:   K2 ASCII data format (defined by K. Bonjer?)
  - tsoft:    TSOFT format (http://seismologie.oma.be/TSOFT/tsoft.html)
  - tfascii:  output data of T. Forbriger's any2ascii
  - su:       SeismicUn*x format
  - seife:    seife format (E. Wielandt), see: \subpage page_seife_format
  - thiesdl1: Thies DL1 pluviometer at BFO, see: \subpage page_thiesdl1_format
  - ascii:    simple one column raw ASCII data
  - bin:      binary data dumped from memory to disk
 */

/* ----- END OF readany.cc ----- */
