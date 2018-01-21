/*! \file anyfilter.cc
 * \brief provide access to all filters (seife and other) (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/07/2005
 * 
 * provide access to all filters (seife and other) (implementation)
 * 
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 05/07/2005   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_ANYFILTER_CC_VERSION \
  "TF_ANYFILTER_CC   V1.0   "

#include <sstream>
#include <tsxx/anyfilter.h>
#include <tsxx/filter.h>
#include <tsxx/seifeclass.h>
#include <tsxx/debug.h>

namespace ts {

  namespace filter {

    Tfilterhandle make_any_filter(const std::string& s,
                                  const bool& debug)
    {
      TSXX_debug(debug, "make_any_filter", "process " + s );
      tfxx::error::Exception::dont_report_on_construct();
      typedef Tfilterhandle Tfh;
      Tfh fh(new Noop());
      bool hot=true;
      //! try C++ filters
      if (hot) {
        hot=false;
        try {
          fh=make_filter(s,debug);
        }
        catch (UnknownFilterException) {
          hot=true;
          TSXX_debug(debug, "make_any_filter", 
                     "caught unknown filter exception from my own" );
        }
        catch (Exception e) {
          TSXX_debug(debug, "make_any_filter", 
                     "caught other exception from my own" );
          tfxx::error::Exception::report_on_construct();
          e.report();
          throw;
        }
      }
      //! try seife filters
      if (hot) {
        hot=false;
        try {
          fh=ts::seife::make_seife_filter(s, debug);
        }
        catch (UnknownFilterException) {
          hot=true;
          TSXX_debug(debug, "make_any_filter", 
                     "caught unknown filter exception from seife" );
        }
        catch (Exception e) {
          TSXX_debug(debug, "make_any_filter", 
                     "caught other exception from seife" );
          tfxx::error::Exception::report_on_construct();
          e.report();
          throw;
        }
      }
      if (hot) {
        TSXX_debug(debug, "make_any_filter", 
                   "still hot after seife and my own" );
        tfxx::error::Exception::report_on_construct();
        TSXX_UnknownFilterAbort("ts::filter::make_any_filter", s);
      }
      tfxx::error::Exception::report_on_construct();
      return(fh);
    }

    //! print information on available filters
    void print_any_help(std::ostream& os)
    {
      ts::filter::print_help(os);
      os << std::endl;
      ts::seife::print_help(os);
    }

  } // namespace filter

} // namespace ts

/* ----- END OF anyfilter.cc ----- */
