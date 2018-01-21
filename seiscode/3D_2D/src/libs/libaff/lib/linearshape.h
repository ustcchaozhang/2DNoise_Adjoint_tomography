/*! \file linearshape.h
 * \brief Shape for Series class (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 23/12/2003
 * 
 * Shape for Series class (prototypes)
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
 * Copyright (c) 2003 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 23/12/2003   V1.0   Thomas Forbriger
 *  - 11/01/2004   V1.1   now offers index range modifiers
 *  - 13/01/2004   V1.2   
 *                        - now provide Mmax_dimen
 *                        - renamed reshaping functions to match the class
 *                          Strided
 *  - 05/07/2005   V1.3   provide index shifting
 *  - 27/04/2006   V1.4   assert: last >= first
 *                        was: last > first which is too strict
 *  - 20/06/2006   V1.5   allow arrays of size zero
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_LINEARSHAPE_H_VERSION

#define AFF_LINEARSHAPE_H_VERSION \
  "AFF_LINEARSHAPE_H   V1.4"

namespace aff {

  namespace util {
    //! forward declaration of stepper class
    class SeriesStepper;
  } // namespace util

  /*! \brief Shape for class aff::Series
   *
   * aff::Series needs its own shape class to provide an Array compatible
   * interface to Iterator.
   *
   * The shape ist defined by three parameters, which are
   * \a first, \a last, and \a base.
   * The index \a k of the corresponding series must be in
   * (\a first, \a last).
   * While the range acessed within the representation is
   * (\a first-base, \a last-base).
   * Thus the element \a k of the series will be found at index \a k-base
   * in the representation.
   *
   * The shape guarantees that the memory layout is dense. This means that the
   * elements of a series can be accessed like a C-style array through a
   * pointer to the first element of the series.
   *
   * Since we do not need to handle multiple dimensions, we offer in-class
   * reshaping functions.
   */
  class LinearShape 
  {
    public:
      /*! maximum dimensionality
       *
       * This is defined to make Subarray useable with the Series class.
       *
       * \note
       * Some parts of the code rely on \c Mmax_dimen>=2. This condition is
       * intrinsically violated here.
       */
      //! the linear shape is used to address one-dimensional arrays
      static const Tdim Mmax_dimen=1;
      //! stepper class for LinearShape (useid through Series by Iterator e.g.)
      typedef aff::util::SeriesStepper Tstepper;
      //! default constructor defines range of size 1
      LinearShape(): Mfirst(0), Mlast(0), Mbase(0) { }
      //! constructor to use for full definition
      LinearShape(const Tsubscript& first,
                  const Tsubscript& last,
                  const Tsubscript& firstinrepr):
        Mfirst(first), Mlast(last), Mbase(first-firstinrepr) 
        { 
          AFF_assert((this->size()>=0),
              "ERROR (LinearShape): inconsistent constructor arguments");
        }
      //! return first legal index
      const Tsubscript& first() const { return(Mfirst); }
      //! return last legal index
      const Tsubscript& last() const { return(Mlast); }
      //! return offset in representation for indes \p i
      Tsubscript offset(const Tsubscript& i) const { return(i-Mbase); }
      //! return base for access to representation
      const Tsize& base() const { return(Mbase); }
      //! by size we mean the size defined by the shape
      Tsize size() const
      { return(static_cast<Tsize>(Mlast-Mfirst+1)); }
      //! return size of addressed memory
      Tsize memory_size() const { return(this->size()); }
      //! set index range [ \p first , \p last ] for dimension \p i
      //! (used by aff::util::Subarray)
      LinearShape& shrink(const Tdim& i, 
                      const Tsubscript& first, const Tsubscript& last)
      {
        AFF_assert((i==0),"ERROR (LinearShape::shrink): illegal dimension!");
        this->setindexrange(first, last);
        return(*this);
      }
      //! set last index of dimension \p i to \p last
      //! (used by aff::util::Subarray)
      LinearShape& shrink(const Tdim& i, 
                      const Tsubscript& last)
      {
        AFF_assert((i==0),"ERROR (LinearShape::shrink): illegal dimension!");
        this->setlastindex(last);
        return(*this);
      }
      //! set index range [ \p first , \p last ] 
      void setindexrange(const Tsubscript& first,
                         const Tsubscript last)
      {
          AFF_assert((last>=first),
              "ERROR (LinearShape::setindexrange): inconsistent arguments");
          setfirstindex(first);
          setlastindex(last);
      }
      //! set first index to \p first
      void setfirstindex(const Tsubscript& first)
      {
          AFF_assert(((first>=Mfirst)&&(first<=Mlast)),
              "ERROR (LinearShape::setfirstindex): index value out of range");
          Mfirst=first;
      }
      //! set last index to \p last
      void setlastindex(const Tsubscript& last)
      {
          AFF_assert(((last>=Mfirst)&&(last<=Mlast)),
              "ERROR (LinearShape::setlastindex): index value out of range");
          Mlast=last;
      }
      //! shift effective index range by \p i
      void shift(const Tsubscript& i)
      {
        Mfirst+=i;
        Mlast+=i;
        Mbase+=i;
      }
    private:
      Tsubscript Mfirst; //!< first valid index
      Tsubscript Mlast;  //!< last valid index
      Tsize Mbase;       //!< base for access to representation
  }; // LinearShape

}

#endif // AFF_LINEARSHAPE_H_VERSION (includeguard)

/* ----- END OF linearshape.h ----- */
