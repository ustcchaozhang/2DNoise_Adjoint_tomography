/*! \file writeany.cc
 * \brief common interface for all data types (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/02/2010
 * 
 * common interface for all data types (implementation)
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
 *  - 17/02/2010   V1.0   Thomas Forbriger
 *  - 08/12/2010   V1.1   provide SU writing
 *  - 29/07/2011   V1.2   support format modifiers
 *  - 02/09/2011   V1.3   support seife format
 *  - 06/09/2011   V1.4   make non-subformat constructor deprecated
 *  - 07/09/2011   V1.5   more string type format ID support: openmode
 *  - 05/11/2011   V1.6   provide ASCII output
 *  - 19/11/2011   V1.7   provide binary output
 *  - 21/11/2011   V1.8   sff output stream takes modifiers
 *  - 21/01/2012   V1.9   prepared osustream to take modifiers
 *  - 23/08/2012   V1.10  ASCII output supports modifiers
 *  - 18/11/2016   V1.11  set debug mode in base class
 * 
 * ============================================================================
 */
#define DATRW_WRITEANY_CC_VERSION \
  "DATRW_WRITEANY_CC   V1.11"

#include <datrwxx/writeany.h>
#include <datrwxx/error.h>
#include <datrwxx/sff.h>
#include <datrwxx/su.h>
#include <datrwxx/seife.h>
#include <datrwxx/ascii.h>
#include <datrwxx/binary.h>
#include <datrwxx/formatmodifier.h>
#include <datrwxx/util.h>
#include <datrwxx/report.h>

namespace datrw {

  oanystream::oanystream(std::ostream& os, const Eformat& format,
                         const bool& debug)
  {
    datrw::util::report_deprecated("oanystream::oanystream(std::ostream& os, "
                      "const Eformat& format)",
                      "this constructor does not support format modifiers");
    std::string fm(anyID(format));
    this->open(os, fm, debug);
  }

  /*----------------------------------------------------------------------*/

  oanystream::oanystream(std::ostream& os, const std::string& format,
                         const bool& debug)
  {
    this->open(os, format, debug);
  }

  /*----------------------------------------------------------------------*/

  void oanystream::open(std::ostream& os, std::string format,
                        const bool& debug)
  {
    std::string formatstring=util::clipstring(format);
    std::string& modifiers=format;
    Mformat=anyID(formatstring);
    if (Mformat==Fsff) 
    {
      Mos=new osffstream(os, modifiers); 
    }
    else if (Mformat==Fgse) 
    {
      DATRW_expect_no_modifier(Mformat,modifiers);
      Mos=new ogsestream(os);
    }
    else if (Mformat==Fsu) 
    {
      Mos=new osustream(os, modifiers); 
    }
    else if (Mformat==Fseife) 
    {
      Mos=new oseifestream(os, modifiers); 
    }
    else if (Mformat==Fascii) 
    {
      Mos=new oasciistream(os, modifiers); 
    }
    else if (Mformat==Fbinary) 
    {
      DATRW_expect_no_modifier(Mformat,modifiers);
      Mos=new obinarystream(os); 
    }
    else 
    {
      DATRW_abort("ERROR (oanystream): unsupported format!"); 
    }
    Mos->debug(debug);
  }

  /*----------------------------------------------------------------------*/

  oanystream::~oanystream()
  {
    delete Mos;
  }

  /*----------------------------------------------------------------------*/

  /*! return appropriate file stream open mode for selected format
   */
  std::ios_base::openmode oanystream::openmode(const std::string& format)
  {
    return(oanystream::openmode(anyID(format)));
  } // std::ios_base::openmode oanystream::openmode(std::string format)

  /*----------------------------------------------------------------------*/

  /*! return appropriate file stream open mode for selected format
   */
  std::ios_base::openmode oanystream::openmode(const Eformat& format)
  {
    std::ios_base::openmode retval;
    if (format==Fsff) { retval=osffstream::openmode; }
    else if (format==Fgse) { retval=ogsestream::openmode; }
    else if (format==Fsu) { retval=osustream::openmode; }
    else if (format==Fseife) { retval=oseifestream::openmode; }
    else if (format==Fascii) { retval=oasciistream::openmode; }
    else if (format==Fbinary) { retval=obinarystream::openmode; }
    else 
    { DATRW_abort("ERROR (openmode): unknown format!"); }
    return(retval);
  } // std::ios_base::openmode oanystream::openmode(const Eformat& format)

} // namespace datrw

/*! \page page_formats Supported formats
 *

  Currently supported \b output \b formats are:
  - sff:     Stuttgart File Format
  - gse:     raw GSE format
  - su:      SeismicUn*x format
  - seife:   seife format (E. Wielandt), see: \ref page_seife_format
  - ascii:   ASCII one column
  - bin:     binary data dumped from memory to disk
 */

/* ----- END OF writeany.cc ----- */
