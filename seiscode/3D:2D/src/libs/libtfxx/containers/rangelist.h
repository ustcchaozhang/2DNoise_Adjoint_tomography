/*! \file rangelist.h
 * \brief handle a list of ranges (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/06/2005
 * 
 * handle a list of ranges (prototypes)
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
 *  - 30/06/2005   V1.0   Thomas Forbriger
 *  - 27/04/2009   V1.1   added range list stepper
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_RANGELIST_H_VERSION

#define TF_RANGELIST_H_VERSION \
  "TF_RANGELIST_H   V1.1"

#include<tfxx/range.h>
#include<list>

namespace tfxx {

  template<class T>
    class RangeList {
      public:
        typedef T Tvalue;
        typedef tfxx::Range<Tvalue> Trange;
        typedef std::list<Trange> Tlist;
        RangeList() { }
        RangeList& append(const Trange& r)
        { Mlist.push_back(r); return *this; }
        void clear() 
        { Mlist.erase(Mlist.begin(), Mlist.end()); }
        typename Tlist::size_type size() const { return Mlist.size(); }
        bool contains(const Tvalue& v) const
        {
          bool retval=false;
          typename Tlist::const_iterator i=Mlist.begin();
          while ((i!=Mlist.end()) && (!retval))
          { retval = i->contains(v); ++i; }
          return retval;
        }
        Tlist list() const { return Mlist; }
      private:
        Tlist Mlist;
    }; // template class RangeList

  /*----------------------------------------------------------------------*/

  template<class T>
    class RangeListStepper {
      public:
        typedef RangeList<T> Trangelist;
        typedef typename Trangelist::Tvalue Tvalue;
        typedef typename Trangelist::Tlist Tlist;
        typedef RangeStepper<Tvalue> Trangestepper;
        RangeListStepper(const Trangelist& rangelist,
                         const Tvalue& stepsize=1):
          Mlist(rangelist.list()), Mstepsize(stepsize),
            Miterator(Mlist.begin()),
            Mrangestepper(Trangestepper(*Miterator, Mstepsize))
          { }
        //! \brief return current value in range list
        Tvalue current() const { return(Mrangestepper.current()); }
        //! \brief return current value in range list
        operator Tvalue() const { return(this->current()); }
        /*! \brief true if stepper is still in range and can return a current
         * value
         */
        bool valid() const 
          { 
            return(Mrangestepper.valid() && (Miterator!=Mlist.end()));
          }
        //! \brief true if stepper is still in range after next step forward
        bool more() const 
          { 
            bool retval=this->valid();
            if (retval && (!Mrangestepper.more()))
            {
              typename Tlist::const_iterator I=Miterator;
              ++I;
              if (!(I!=Mlist.end())) { retval=false; }
            }
            return(retval);
          }
        //! \brief step forward and return current value
        Tvalue next() 
          {
            if (this->valid())
            {
              if (Mrangestepper.more()) { Mrangestepper.next(); }
              else {
                ++Miterator;
                if (Miterator != Mlist.end()) 
                {
                  Mrangestepper=Trangestepper(*Miterator, Mstepsize);
                }
              }
            }
            return(this->current()); 
          }
        //! \brief step forward and return current value
        Tvalue operator++() { return(this->next()); }
      private:
        Tlist Mlist;
        Tvalue Mstepsize;
        typename Tlist::const_iterator Miterator;
        Trangestepper Mrangestepper;
    }; // template class RangeListStepper

  
}

#endif // TF_RANGELIST_H_VERSION (includeguard)

/* ----- END OF rangelist.h ----- */
