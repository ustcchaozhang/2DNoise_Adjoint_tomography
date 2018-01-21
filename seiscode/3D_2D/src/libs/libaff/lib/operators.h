/*! \file operators.h
 * \brief provide operators for iteratable objects (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 10/02/2004
 * 
 * provide operators for iteratable objects (prototypes)
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
 *  - 10/02/2004   V1.0   Thomas Forbriger
 *  - 05/07/2005   V1.1   support modification of data in containers that are
 *                        declared const
 *  - 15/05/2011   V1.2   added group name (thof)
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_OPERATORS_CLASS
#error "never ever inlcude this from your code!"
#endif

#ifndef AFF_OPERATORS_CONSTCLASS
#error "definition of container of const is missing!"
#endif

#include<aff/iterator.h>

namespace aff {

/*! \brief Container operators
 * \defgroup group_operators Container operators
 *
 * Operators for containers are defined through a macro expansion mechanism,
 * since the basic algorithm is the same for all containers.
 * We make use of iterators to traverse the elements of the containers.
 */
  
/*! \brief Basic macro type definitions for operators (see details).
 * \ingroup group_operators
 *
 * The macros
 * - #AFF_OPERATORS_CLASS
 * - #AFF_OPERATORS_CONSTCLASS
 * 
 * are defined by the header file including this header file.
 * \sa arrayoperators.h, seriesoperators.h
 */
//@{
#define AFF_OPERATORS_TEMPAR T
#define AFF_OPERATORS_CONT AFF_OPERATORS_CLASS< AFF_OPERATORS_TEMPAR >
#define AFF_OPERATORS_CONSTCONT AFF_OPERATORS_CONSTCLASS< AFF_OPERATORS_TEMPAR >
//@}

/*----------------------------------------------------------------------*/

/*! \brief unary operator macro for containers with common value type
 * \ingroup group_operators
 */
#define AFF_OPERATORS_UNOP( CONT, OP ) \
  template<class AFF_OPERATORS_TEMPAR > \
  const CONT& operator OP ## =(const CONT& container, \
                         typename CONT::Tconst_reference value) \
  { \
    for (aff::Iterator< CONT > i(container); i.valid(); ++i) \
    { (*i) OP ## = value; } \
    return(container); \
  }

/*----------------------------------------------------------------------*/

/*! \brief binary operator macro for containers with common value type
 * \ingroup group_operators
 */
#define AFF_OPERATORS_BINOP( RETCONT, INCONT, OP ) \
  template<class AFF_OPERATORS_TEMPAR > \
  RETCONT operator OP(const INCONT& container, \
                       typename INCONT::Tconst_reference value) \
  { \
    RETCONT retval(container.shape()); \
    aff::Browser< typename INCONT::Tcontainer > i(container); \
    aff::Iterator< typename RETCONT::Tcontainer > o(retval); \
    while ( i.valid() && o. valid() ) \
    {  (*o) = (*i) OP value;  ++o; ++i;  } \
    return(retval); \
  } \
  template<class AFF_OPERATORS_TEMPAR > \
  RETCONT operator OP(typename INCONT::Tconst_reference value, \
                      const INCONT& container) \
  { return( container OP value ); }

/*----------------------------------------------------------------------*/

/*! \brief unary operator macro for containers with different value type
 * \ingroup group_operators
 */
#define AFF_OPERATORS_UNOPB( RETCONT, INCONT, OP ) \
  template<class A , class B > \
  RETCONT< A >& operator OP ## =(RETCONT< A >& container1, \
                           const INCONT< B >& container2) \
  { \
    aff::Iterator< RETCONT< A > > i1(container1); \
    aff::Browser< INCONT< B > > i2(container2); \
    while ( i1.valid() && i2.valid() ) \
    {  (*i1) OP ## = (*i2); ++i1; ++i2; } \
    return(container1); \
  }

/*----------------------------------------------------------------------*/

/*! \brief binary operator macro for containers with different value type
 * \ingroup group_operators
 */
#define AFF_OPERATORS_BINOPB( RETCONT, INCONT, OP ) \
  template<class A , class B > \
  RETCONT< A > operator OP(const INCONT< A >& container1, \
                           const INCONT< B >& container2) \
  { \
    RETCONT< A > retval(container1.shape()); \
    retval=A(0); \
    aff::Browser< INCONT< A > > i1(container1); \
    aff::Browser< INCONT< B > > i2(container2); \
    aff::Iterator< RETCONT< A > > o(retval); \
    while ( i1.valid() && o.valid() && i2.valid() ) \
    {  (*o) = (*i1) OP (*i2);  ++o; ++i1; ++i2; } \
    return(retval); \
  }

/*======================================================================*/

/*! \brief unary operators
 * \ingroup group_operators
 * This makes use of #AFF_OPERATORS_THEUNOP and #AFF_OPERATORS_UNOP
 */
//@{
#define AFF_OPERATORS_THEUNOP( OP ) \
  AFF_OPERATORS_UNOP( AFF_OPERATORS_CONT , OP )
  AFF_OPERATORS_THEUNOP( + )
  AFF_OPERATORS_THEUNOP( * )
  AFF_OPERATORS_THEUNOP( / )
  AFF_OPERATORS_THEUNOP( - )
//@}
#undef AFF_OPERATORS_THEUNOP

/*! \brief binary operators
 * \ingroup group_operators
 * This makes use of #AFF_OPERATORS_THEBINOP and #AFF_OPERATORS_BINOP
 */
//@{
#define AFF_OPERATORS_THEBINOP( OP ) \
  AFF_OPERATORS_BINOP( AFF_OPERATORS_CONT , AFF_OPERATORS_CONSTCONT, OP )
  AFF_OPERATORS_THEBINOP( + )
  AFF_OPERATORS_THEBINOP( * )
  AFF_OPERATORS_THEBINOP( / )
  AFF_OPERATORS_THEBINOP( - )
//@}
#undef AFF_OPERATORS_THEBINOP

/*! \brief binary operators
 * \ingroup group_operators
 * This makes use of #AFF_OPERATORS_THEBINOP and #AFF_OPERATORS_BINOPB
 */
//@{
#define AFF_OPERATORS_THEBINOP( OP ) \
  AFF_OPERATORS_BINOPB( AFF_OPERATORS_CLASS , AFF_OPERATORS_CONSTCLASS , OP )
  AFF_OPERATORS_THEBINOP( + )
  AFF_OPERATORS_THEBINOP( * )
  AFF_OPERATORS_THEBINOP( / )
  AFF_OPERATORS_THEBINOP( - )
//@}
#undef AFF_OPERATORS_THEBINOP

/*! \brief unary operators
 * \ingroup group_operators
 * This makes use of #AFF_OPERATORS_THEUNOP and #AFF_OPERATORS_UNOPB
 */
//@{
#define AFF_OPERATORS_THEUNOP( OP ) \
  AFF_OPERATORS_UNOPB( AFF_OPERATORS_CLASS , AFF_OPERATORS_CONSTCLASS , OP )
  AFF_OPERATORS_THEUNOP( + )
  AFF_OPERATORS_THEUNOP( * )
  AFF_OPERATORS_THEUNOP( / )
  AFF_OPERATORS_THEUNOP( - )
//@}
#undef AFF_OPERATORS_THEUNOP

#undef AFF_OPERATORS_UNOP
#undef AFF_OPERATORS_BINOP
#undef AFF_OPERATORS_BINOPB
#undef AFF_OPERATORS_CONSTCONT
#undef AFF_OPERATORS_CONT
#undef AFF_OPERATORS_TEMPAR
}

/* ----- END OF operators.h ----- */
