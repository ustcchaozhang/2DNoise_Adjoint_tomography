/*! \file dump_macros.h
 * \brief dump helper (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/12/2002
 * 
 * dump helper (prototypes)
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
 * This file provides some preprocessor macros that are usefull, when
 * debugging programs.
 * \sa DUMP
 * \sa CODE
 * \sa LOCATION
 * \sa group_helpers
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 19/12/2002   V1.0   Thomas Forbriger
 *  - 02/01/2003   V1.1   (thof)
 *                        - did not resolve cout and endl in namespace std
 *  - 19/12/2003   V1.2   (thof)
 *                        - there was a redundant semicolon in the output of
 *                           the CODE macro
 *                        - sorry: returned to previous definition to stay
 *                          consistent with older code. each line of CODE()
 *                          must be closed with a semicolon
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_DUMP_MACROS_H_VERSION

#define AFF_DUMP_MACROS_H_VERSION \
  "AFF_DUMP_MACROS_H   V1.2"

#include<iostream>

/*! \brief Dump any object through its dump function
 *
 * \ingroup group_helpers
 */
#define DUMP( A ) std::cout << "object \"" << #A << "\":" << std::endl; \
  aff::dump(A)

/*! \brief print program location
 *
 * \ingroup group_helpers
 */

#define LOCATION std::cout << "*** We are in \"" \
  << __FILE__ << "\" at line #" << __LINE__ << " ***" << std::endl;

/*! \brief Dump code and execute (works like echo)
 *
 * \note 
 * If the code contains a comma, this will not work, because the preprocessor
 * will split the code into two macro arguments. In some cases you can resolve
 * that problem by putting the code in brackets.
 *
 * \ingroup group_helpers
 */
#define CODE( C ) std::cout << #C << ";" << std::endl; C

#endif // AFF_DUMP_MACROS_H_VERSION (includeguard)

/* ----- END OF dump_macros.h ----- */
