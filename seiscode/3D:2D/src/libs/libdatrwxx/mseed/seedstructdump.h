/*! \file seedstructdump.h
 * \brief dump SEED structs in human readable form (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 15/03/2006
 * 
 * dump SEED structs in human readable form (prototypes)
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
 * Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 15/03/2006   V1.0   Thomas Forbriger
 *  - 09/05/2006   V1.1   support generic Steim Frame
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_SEEDSTRUCTDUMP_H_VERSION

#define DATRW_SEEDSTRUCTDUMP_H_VERSION \
  "DATRW_SEEDSTRUCTDUMP_H   V1.1"

#include<iostream>
#include<datrwxx/seedstructs.h>

namespace datrw {

  namespace mseed {

    namespace SEED {

      void dump(const ControlHeader& s, std::ostream& os);
      void dump(const BlocketteHeader& s, std::ostream& os);
      void dump(const BTIME& s, std::ostream& os);
      void dump(const DataExtensionBlockette& s, std::ostream& os);
      void dump(const DataOnlySEEDBlockette& s, std::ostream& os);
      void dump(const DataRecordBlocketteHeader& s, std::ostream& os);
      void dump(const FixedDataRecordHeader& s, std::ostream& os);
      void dump(const ActivityFlags& s, std::ostream& os);
      void dump(const QualityFlags& s, std::ostream& os);
      void dump(const IOFlags& s, std::ostream& os);
      void dump_aflags(const unsigned char& s, std::ostream& os);
      void dump_qflags(const unsigned char& s, std::ostream& os);
      void dump_ioflags(const unsigned char& s, std::ostream& os);
      void dump(const EEncodingFormat& e, std::ostream& os);
      void dump(const EByteOrder& e, std::ostream& os);
      void dump(SteimFrame& f, std::ostream& os);
      void dump(Steim1Frame f, std::ostream& os);
      void dump(Steim2Frame f, std::ostream& os);

    } // namespace SEED

  } // namespace mseed

} // namespace datrw

#endif // DATRW_SEEDSTRUCTDUMP_H_VERSION (includeguard)

/* ----- END OF seedstructdump.h ----- */
