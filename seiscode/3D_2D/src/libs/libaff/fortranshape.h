/*! \file fortranshape.h
 * \brief prepare information to pass to Fortran subroutines (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 23/12/2002
 * 
 * prepare information to pass to Fortran subroutines (prototypes)
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
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 23/12/2002   V1.0   Thomas Forbriger
 *  - 03/01/2003   V1.1   (thof)
 *                        - now offers a casted pointer
 *                        - FortranArray now takes container type as template
 *                          argument
 *  - 10/12/2007   V1.2   (thof)
 *                        - we need to use the types of the representation
 *                          class to distinguish correctly for const
 *                          correctness
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_FORTRANSHAPE_H_VERSION

#define AFF_FORTRANSHAPE_H_VERSION \
  "AFF_FORTRANSHAPE_H   V1.2"

#include<aff/array.h>
#include<aff/lib/checkedcast.h>

namespace aff {

namespace util {

  /*! \brief find appropriate leading dimensions
   *
   * This class is used to calculate appropriate leading dimensions from a
   * aff::Strided shape. These values are needed to pass an array to a Fortran
   * 77 subroutine.
   *
   * \sa aff::FortranArray
   */
  class FortranShape {
    public:
      //! This is coded for Strided shapes
      typedef aff::Strided Tshape;
      //! Vector of index values
      typedef Tshape::TIndexVec TIndexVec;
      //! Vector of size values
      typedef Tshape::TSizeVec TSizeVec;
      //! construct from shape
      explicit FortranShape(const Tshape& shape, const bool& BaseOne=true);
      //! first index in dimension \p i
      const Tsubscript& first(const Tdim& i) const { return(Mfirst[i]); }
      //! last index in dimension \p i
      const Tsubscript& last(const Tdim& i) const { return(Mlast[i]); }
      //! last index as dimensioned in dimension \p i
      const Tsubscript& dimlast(const Tdim& i) const { return(Mdimlast[i]); }
      //! offset to access representation
      const Tsubscript& offset() const { return(Moffset); }
      //! full first vector
      const TIndexVec& first() const { return(Mfirst); }
      //! full last vector
      const TIndexVec& last() const { return(Mlast); }
      //! full dimlast vector
      const TIndexVec& dimlast() const { return(Mdimlast); }
    private:
      //! first index
      TIndexVec Mfirst;
      //! last index
      TIndexVec Mlast;
      //! last index as dimensioned in memory
      TIndexVec Mdimlast;
      //! offset of first element in representation
      Tsubscript Moffset;
  }; // class FortranShape
} // namespace util

/*----------------------------------------------------------------------*/

/*! \brief Class to provide Fortran interface values
 *
 * \sa aff::util::FortranShape
 */
template<class C>
class FortranArray: private aff::util::FortranShape {
  public:
    //! type of corresponding array
    typedef C Tarray;
    //! shape class
    typedef typename Tarray::Tshape Tshape;
    //! representation class
    typedef typename Tarray::Trepresentation Trepresentation;
    //! pointer to value used in representation class
    typedef typename Trepresentation::Tpointer Treppointer;
    //! base class
    typedef aff::util::FortranShape Tbase;
    //! pointer to array base in memory
    typedef typename Tarray::Tpointer Tpointer;
    //! value type in array class
    typedef typename Tarray::Tvalue Tvalue;
    //! value type in representation class
    typedef typename Trepresentation::Tvalue Trepvalue;
    //! create
    FortranArray(Tarray array, const bool& BaseOne=true):
      Tbase(array.shape(), BaseOne)
      {
        Trepresentation repr=array.representation();
        Mpointer=&repr[this->Tbase::offset()];
      }
    //! access declarations
    //@{
    Tbase::first;
    Tbase::last;
    Tbase::dimlast;
    //@}
    //! return pointer to first element in Fortran layout
    Treppointer pointer() const { return(Mpointer); }
    /*! \brief return type-casted pointer to first element in Fortran layout
     *
     * The cast checks for const-correctness and type-size. But you have to
     * ensure that there is a meaningful relation between both types involved.
     *
     * \sa aff::util::SizeCheckedCast
     */
    template<class TT>
      TT* castedpointer() const
      { return(SizeCheckedCast<Trepvalue,TT>::cast(Mpointer)); }
  private:
    //! pointer to memory
    Treppointer Mpointer;
}; // class FortranArray

} // namespace aff

#endif // AFF_FORTRANSHAPE_H_VERSION (includeguard)

/* ----- END OF fortranshape.h ----- */
