/*! \file tsoftconfig.h
 * \brief tsoft configuration (prototypes)
 * \ingroup group_tsoft
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 02/12/2011
 * 
 * tsoft configuration (prototypes)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 02/12/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_TSOFTCONFIG_H_VERSION

#define DATRW_TSOFTCONFIG_H_VERSION \
  "DATRW_TSOFTCONFIG_H   V1.0   "

// #define DEBUG

#ifdef DEBUG
#include<iostream>
#endif

namespace datrw {

  namespace tsoft {

    /*! \brief config parameters for data extraction
     * \ingroup group_tsoft
     *
     * These parameters control the way gaps in the input data are handled.
     */
    struct ReaderConfig {
      /*! 
       * if true, read flagged (UNDETVAL) values, do not interrupt input
       */
      bool keepundetval;
      /*!
       * if true, replace UNDETVAL values by flagvalue
       * if values are set to flagvalue and flagvalue != UNDETVAL
       * then these sample values are read like normal sample values
       * (i.e. setundetval usually implies keepundetval)
       */
      bool setundetval;
      /*!
       * value to replace UNDETVAL values with
       */
      double flagvalue;
      /*!
       * if true, bridge values with time bridgetime by replacing the sample
       * time with the expected sample time
       */
      bool bridgesamples;
      /*!
       * unspecified date value which should be bridged
       * typically: 0001 01 01 01 01 01
       */
      libtime::TAbsoluteTime bridgetime;
      /*!
       * true if bridged samples shall be flagged
       */
      bool flagbridged;
      /*!
       * flag value for bridged samples
       */
      double bridgeflagvalue;
      /*!
       * constructor to set default values
       */
      ReaderConfig()
        : keepundetval(false), setundetval(false), flagvalue(9000.),
        bridgesamples(false), bridgetime(1,1,1,1,1,1),
        flagbridged(false), bridgeflagvalue(-9000.)
      { } // ReaderConfig()
    }; // struct ReaderConfig

  } // namespace tsoft

} // namespace datrw

#endif // DATRW_TSOFTCONFIG_H_VERSION (includeguard)

/* ----- END OF tsoftconfig.h ----- */
