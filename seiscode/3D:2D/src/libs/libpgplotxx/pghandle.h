/*! \file pghandle.h
 * \brief A handle class used within the PGPLOT++ library (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 28/12/2008
 * 
 * A handle class used within the PGPLOT++ library (prototypes)
 * 
 * Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 28/12/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_PGHANDLE_H_VERSION

#define TF_PGHANDLE_H_VERSION \
  "TF_PGHANDLE_H   V1.0   "
#define TF_PGHANDLE_H_CVSID \
  "$Id$"

namespace pgplot {

/*! \brief Interface provided through pghandle.h
 *
 * \defgroup handle_h Interface provided through pghandle.h
 */
/*@{*/

  /*! \brief Base class for handles.
   *
   * The base class is necessary to promise constness for values that are passed
   * around using handles.
   * Consider a code that uses a handle to pass the contents of a class of type
   * A as defined by
   * \code
   * class A {
   * ...
   * };
   * Handle<A> a;
   * \endcode
   * A function sample can accept a handle providing access to the values of
   * type A but promising not to changes these values if declared
   * \code
   * void sample(const Handle<A>::Tcontainer_of_const& h);
   * \endcode
   * or briefly
   * \code
   * void sample(const Handle<A>::Tcoc& h);
   * \endcode
   * Declaring the function
   * \code
   * void sample(const Handle<A>& h);
   * \endcode
   * is not sufficient, since this only promises constness of the handle but not
   * of the values that can be accessed through the handle.
   *
   * \sa Example code is available in pgtestxx.cc
   * \sa Therein example code is available in functions
   *       #testhandle() and #testhandlefunction()
   */
  template<class X>
    class HandleOfConst {
      public:
        /*! \brief friend declaration to other Handle templates is essential
         * for inheritance transparency.
         */
        template<class Y> friend class HandleOfConst;

        /*! \name Typedefs for class template HandleOfConst.
         *
         * \note
         * \code
         * typedef const X* Tpointer_to_const;
         * \endcode
         * is not the same as
         * \code
         * typedef const Tpointer Tpointer_to_const;
         * \endcode
         */
        /*@{*/
        typedef X Tvalue;
        typedef X* Tpointer;
        typedef X& Treference;
        typedef const X* Tpointer_to_const;
        typedef const X& Treference_to_const;
        typedef HandleOfConst<Tvalue> Tcontainer;
        typedef Tcontainer Tcontainer_of_const;
        typedef Tcontainer_of_const Tcoc;
        /*@}*/

        //! \name Constructors
        //@{
        //! Default constructor if class X provides one too.
        HandleOfConst():
          Mpointer(new Tvalue()), Mpcount(new int(1)) { }

        //! Constructor to initialize the handle.
        HandleOfConst(Treference_to_const x):
          Mpointer(new Tvalue(x)), Mpcount(new int(1)) { }

        //! Copy constructor that provides transparency for inheritance.
        template<class Y>
          HandleOfConst<X>(const HandleOfConst<Y>& h): 
          Mpointer(h.Mpointer), Mpcount(h.Mpcount) 
          { (*Mpcount)++; }
        //@}

        //! Destructor that takes care of reference count.
        ~HandleOfConst() 
        { if (--(*Mpcount)==0) { delete Mpointer; delete Mpcount; } }

        //! \name Read access operators
        //@{
        //! Read access operator with pointer semantics.
        Tpointer_to_const operator->() const { return Mpointer; }

        //! Read access operator with pointer semantics.
        Treference_to_const operator*() const { return *Mpointer; }
        //@}

        //! Assignement operator that provide transparency for inheritance.
        template<class Y>
          Tcontainer& operator=(const HandleOfConst<Y>& h)
          {
            if (Mpointer == h.Mpointer) return *this;
            if (--(*Mpcount) == 0) { delete Mpointer; delete Mpcount; }
            Mpointer=h.Mpointer;
            Mpcount=h.Mpcount;
            (*Mpcount)++;
            return *this;
          }

      protected:
        //! \name Write access operators must be protected
        //@{
        //! expose the pointer to derived classes
        Tpointer pointer() const { return Mpointer;}

        //! expose the object to derived classes
        Treference reference() const { return *Mpointer;}
        //@}

      private:
        //! internal pointer to the handled object
        Tpointer Mpointer;
        //! reference counter
        int* Mpcount;
    }; // template class HandleOfConst

  /*----------------------------------------------------------------------*/

  /*! \brief The handle class.
   *
   * This handle class provides transparency to class inheritance.
   * Consider a base class A
   * \code
   * class A {
   * ...
   * };
   * \endcode
   * and a derived class B
   * \code
   * class B: public A {
   *   public:
   *     // constructor
   *     B(const int& i, const int& j);
   * ...
   * };
   * \endcode
   * where the contstructor is just an example to illustrate code below.
   * If you want to provide access to the same instance of B at more than one
   * locations within your code, it is safe to use a %Handle\<B\> that can be
   * initialized in a straight forward way like
   * \code
   * Handle<B> b(B(3,4));
   * \endcode
   * The value b can be passed around safely, providing access to the
   * contained value of type B with pointer semantics.
   * No consider a function
   * \code
   * void sample(const Handle<A>& a);
   * \endcode
   * that accepts a handle to the base class you can simply call this
   * function by
   * \code
   * sample(b);
   * \endcode
   * and the contents of the object of type B will be available within
   * function sample through virtual functions.
   *
   * \sa Example code is available in pgtestxx.cc
   * \sa Therein example code is available in 
   *       functions #testhandle() and #testhandlefunction()
   */
  template<class X>
    class Handle: public HandleOfConst<X> {
      public:
        /*! \name Typedefs for class template Handle.
         *
         * \note
         * \code
         * typedef const X* Tpointer_to_const;
         * \endcode
         * is not the same as
         * \code
         * typedef const Tpointer Tpointer_to_const;
         * \endcode
         */
        /*@{*/
        typedef X Tvalue;
        typedef X* Tpointer;
        typedef X& Treference;
        typedef const X* Tpointer_to_const;
        typedef const X& Treference_to_const;
        typedef HandleOfConst<Tvalue> Tbase;
        typedef Handle<Tvalue> Tcontainer;
        typedef Tbase Tcontainer_of_const;
        typedef Tcontainer_of_const Tcoc;
        /*@}*/

        /*! \name Constructors
         *
         * Action is provided by base class.
         */
        //@{
        //! \brief Default constructor.
        Handle(): Tbase() { }

        //! \brief Initializing constructor.
        Handle(Treference_to_const x): 
          Tbase(x) { }

        //! \brief Copy constructor providing transparency to inheritance.
        template<class Y>
          Handle(const Handle<Y>& h): Tbase(h) { }
        //@}

        //! \name Write access operators.
        //@{
        //! \brief Write access operator with pointer semantics.
        Tpointer operator->() const { return Tbase::pointer(); }

        //! \brief Write access operator with pointer semantics.
        Treference operator*() const { return Tbase::reference(); }
        //@}

        //! \brief Assignement operator providing transparency to inheritance.
        template<class Y>
          Tcontainer& operator=(const Handle<Y>& h)
          { 
            Tbase::operator=(h);
            return *this;
          }

    }; // template class Handle

/*@}*/

} // namespace pgplot

#endif // TF_PGHANDLE_H_VERSION (includeguard)

/* ----- END OF pghandle.h ----- */
