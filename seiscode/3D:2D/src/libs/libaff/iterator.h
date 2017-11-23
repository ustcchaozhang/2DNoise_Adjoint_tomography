/*! \file iterator.h
 * \brief Define the iterator class template (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 08/12/2002
 * 
 * Define the iterator class template (prototypes)
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
 * \sa aff::Iterator
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 08/12/2002   V1.0   copied from libcontxx
 *  - 23/12/2002   V1.1   (thof)
 *                        - reorganized code: do not inherit from Stepper
 *  - 28/12/2002   ----   (thof)
 *                        - revised: is not affected by new style of base
 *                          classes for constant elements
 *  - 23/12/2003   V1.2   (thof)
 *                        - introduced Browser. Iterator may not be
 *                          instantiated from a const object. It is not
 *                          allowed to create a copy of SharedHeap from a
 *                          const reference to a container.
 *  - 05/07/2005   V1.3   (thof)
 *                        - promise constness of Iterator argument
 *  - 19/06/2006   V1.4   (thof)
 *                        - Browser must use const value types
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_ITERATOR_H_VERSION

#define AFF_ITERATOR_H_VERSION \
  "AFF_ITERATOR_H   V1.4"

#include<aff/lib/types.h>

namespace aff {

  /*! \brief Iterator
   *
   * \note
   * This class holds an intrinsic reference to the Container components.
   *
   * \param C any container with appropriate interface (e.g. aff::Array,
   *          aff::Series)
   *
   * \todo
   * Define requirements for the interface a container must provide.
   */
  template<class C>
  class Iterator {
    public:
      //! type of container
      typedef C Tcontainer;
      //! representation class
      typedef typename C::Trepresentation Trepresentation;
      //! shape class
      typedef typename C::Tshape Tshape;
      //! stepper base class
      typedef typename C::Tshape::Tstepper Tstepper;
      //! value type
      typedef typename C::Tvalue Tvalue;
      //! value type
      typedef typename C::Treference Treference;
      //! value type
      typedef typename C::Tpointer Tpointer;
      //! only this constructor
      Iterator(const C& c): 
        Mstepper(c.shape()), Mrepresentation(c.representation()) { }
      
      //! delegate to stepper
      //@{
      bool more() const { return(Mstepper.more()); }
      bool less() const { return(Mstepper.less()); }
      bool valid() const { return(Mstepper.valid()); }
      Iterator& incr() { Mstepper.incr(); return(*this); }
      Iterator& decr() { Mstepper.decr(); return(*this); }
      Iterator& tofirst() { Mstepper.tofirst(); return(*this); }
      Iterator& tolast() { Mstepper.tolast(); return(*this); }
      //@}

      //! synonym for increment
      Iterator& operator++()
      { return(this->incr()); }

      //! synonym for decrement
      Iterator& operator--()
      { return(this->decr()); }

      //! access element
      Treference operator*() 
      { return(Mrepresentation[Mstepper.current()]); }

      //! member selection operator
      Tpointer operator->() 
      { return(&(Mrepresentation[Mstepper.current()])); }

    private:
      //! Stepper to use
      Tstepper Mstepper;
      //! Representation to access
      Trepresentation Mrepresentation;
  }; // class Iterator

  /*! \brief Browser
   *
   * \note
   * This class holds an intrinsic reference to the Container components.
   *
   * \param C any container with appropriate interface (e.g. aff::Array,
   *          aff::Series)
   *
   * \todo
   * Define requirements for the interface a container must provide.
   */
  template<class C>
  class Browser {
    public:
      //! type of container
      typedef typename C::Tcontainer_of_const Tcontainer;
      //! representation class
      typedef typename Tcontainer::Trepresentation::Tcoc Trepresentation;
      //! shape class
      typedef typename Tcontainer::Tshape Tshape;
      //! stepper base class
      typedef typename Tcontainer::Tshape::Tstepper Tstepper;
      //! value type
      typedef typename Tcontainer::Tconst_value Tvalue;
      //! value type
      typedef typename Tcontainer::Tconst_reference Treference;
      //! value type
      typedef typename Tcontainer::Tconst_pointer Tpointer;
      //! only this constructor
      Browser(const Tcontainer& c): 
        Mstepper(c.shape()), Mrepresentation(c.representation()) { }
      
      //! delegate to stepper
      //@{
      bool more() const { return(Mstepper.more()); }
      bool less() const { return(Mstepper.less()); }
      bool valid() const { return(Mstepper.valid()); }
      Browser& incr() { Mstepper.incr(); return(*this); }
      Browser& decr() { Mstepper.decr(); return(*this); }
      Browser& tofirst() { Mstepper.tofirst(); return(*this); }
      Browser& tolast() { Mstepper.tolast(); return(*this); }
      //@}

      //! synonym for increment
      Browser& operator++()
      { return(this->incr()); }

      //! synonym for decrement
      Browser& operator--()
      { return(this->decr()); }

      //! access element
      Treference operator*() const
      { return(Mrepresentation[Mstepper.current()]); }

      //! member selection operator
      Tpointer operator->() const
      { return(&(Mrepresentation[Mstepper.current()])); }

    private:
      //! Stepper to use
      Tstepper Mstepper;
      //! Representation to access
      Trepresentation Mrepresentation;
  }; // class Browser

} // namespace aff

#endif // AFF_ITERATOR_H_VERSION (includeguard)

/* ----- END OF iterator.h ----- */
