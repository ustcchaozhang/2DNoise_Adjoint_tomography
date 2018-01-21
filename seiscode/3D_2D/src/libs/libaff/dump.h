/*! \file dump.h
 * \brief debug helpers (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/12/2002
 * 
 * debug helpers (prototypes)
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
 * This file just collects other header files. It provides dump and associates
 * presented in namespace aff.
 * \sa group_helpers
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 12/12/2002   V1.0   Thomas Forbriger
 *  - 15/12/2002   V1.1   (thof)
 *                        - no need to place this in namespace prebuilt
 *  - 17/12/2002   V1.2   (thof)
 *                        - introduced dump for class Series
 *  - 19/12/2002   V1.3   (thof)
 *                        - added array dump functions
 *                        - Factored out declarations to different header
 *                          files. This file just collects them. This is
 *                          essential to make the SimpleRigidArray dump
 *                          function useable in the Array definition (e.g.).
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_DUMP_H_VERSION

#define AFF_DUMP_H_VERSION \
  "AFF_DUMP_H   V1.3"

/*! \defgroup group_helpers Debug functions
 */

#include<aff/lib/dump_macros.h>
#include<aff/lib/dump_sharedheap.h>
#include<aff/lib/dump_strided.h>
#include<aff/lib/dump_array.h>
#include<aff/lib/dump_series.h>
#include<aff/lib/dump_simplerigidarray.h>

#endif // AFF_DUMP_H_VERSION (includeguard)

/* ----- END OF dump.h ----- */
