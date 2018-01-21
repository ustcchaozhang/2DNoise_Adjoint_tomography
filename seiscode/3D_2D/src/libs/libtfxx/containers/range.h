/*! \file range.h
 * \brief deal with number ranges
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 09/06/2002
 * 
 * deal with number ranges
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 09/06/2002   V1.0   Thomas Forbriger
 *  - 27/11/2002   V1.1   copied and modified from libcontxx sources
 *  - 27/04/2009   V1.2   added range stepper
 * 
 * ============================================================================
 */

// include guard
#ifndef TFXX_RANGE_H_VERSION

#define TFXX_RANGE_H_VERSION \
  "TFXX_RANGE_H   V1.2"

namespace tfxx {

/*! \brief A class to deal with numerical ranges
 *
 * \param T type of value (default=int)
 */
template<class T=int>
  class Range {
    public:
      //! element type
      typedef T Tvalue;
      //! empty range
      Range(): Mfirst(0), Mlast(0) { }
      //! range containing exactly one element
      Range(const Tvalue& index): Mfirst(index), Mlast(index) { }
      //! set range
      Range(const Tvalue& first, const Tvalue& last):
        Mfirst(first<last ? first:last), 
        Mlast(last>first ? last:first) { }
      //! access start of range
      Tvalue& first() { return Mfirst; }
      //! access end of range
      Tvalue& last() { return Mlast; }
      //! read start of range
      const Tvalue& first() const { return Mfirst; }
      //! read end of range
      const Tvalue& last() const { return Mlast; }
      //! is this range inside the other
      bool isinside(const Range& other) const
      { return ((Mfirst >= other.first()) && (Mlast <= other.last())); }
      //! does this range contain the other
      bool contains(const Range& other) const
      { return ((Mfirst <= other.first()) && (Mlast >= other.last())); }
      //! does this range contain then value v
      bool contains(const Tvalue& v) const
      { return ((Mfirst <= v) && (Mlast >= v)); }
      //! shrink to smaller of this and the other
      Range& shrink(const Range& other) 
      { 
        Mfirst=Mfirst>other.first() ? Mfirst:other.first();
        Mlast=Mlast<other.last() ? Mlast:other.last();
        return *this;
      }
      //! expand to larger of this and the other
      Range& expand(const Range& other) 
      { 
        Mfirst=Mfirst<other.first() ? Mfirst:other.first();
        Mlast=Mlast>other.last() ? Mlast:other.last();
        return *this;
      }
      //! shift by n
      Range& shift(const Tvalue& n)
      { Mfirst+=n; Mlast+=n; return *this; }
      //! shift by n
      Range& operator+=(const Tvalue& n)
      { return this->shift(n); }
      //! shift by -n
      Range& operator-=(const Tvalue& n)
      { return this->shift(-n); }
    private:
      Tvalue Mfirst; //!< start of range.
      Tvalue Mlast;  //!< end of range.
  }; // class Range

/*----------------------------------------------------------------------*/

  /*! a class to step through a range
   *
   * This class is only meaningful for ranges of integer value
   */
  template<class T=int>
    class RangeStepper {
      public:
        typedef Range<T> Trange;
        typedef typename Trange::Tvalue Tvalue;
        RangeStepper(const Trange& range,
                     const Tvalue& stepsize=1):
          Mrange(range), Mstepsize(stepsize), Mcurrent(Mrange.first()) { }
        //! \brief return current value
        operator Tvalue() const { return(this->current()); }
        //! \brief return current value
        Tvalue current() const { return(Mcurrent); }
        /*! \brief true if stepper is still in range and can return a current
         * value
         */
        bool valid() const { return(Mcurrent<=Mrange.last()); }
        //! \brief true if stepper will still be in range after next advance
        bool more() const { return(Mcurrent<=(Mrange.last()-Mstepsize)); }
        //! \brief advance to next value and return current value
        Tvalue next() 
          { 
            if (this->valid()) { Mcurrent += Mstepsize; }
            return(this->current()); 
          }
        //! \brief advance to next value and return current value
        Tvalue operator++() { return(this->next()); }
      private:
        Trange Mrange;
        Tvalue Mstepsize;
        Tvalue Mcurrent;
    };// class RangeStepper

} // namespace tfxx

#endif // TFXX_RANGE_H_VERSION (includeguard)

/* ----- END OF range.h ----- */
