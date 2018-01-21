/*! \file handle.h
 * \brief provide a pointer with reference counting (prototypes)
 * 
 * \addtogroup handle_h
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/06/2005
 * 
 * provide a pointer with reference counting (prototypes)
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
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_HANDLE_H_VERSION

#define TF_HANDLE_H_VERSION \
  "TF_HANDLE_H   V1.0   "

namespace tfxx {

/*! \brief Interface provided through handle.h
 *
 * \defgroup handle_h Interface provided through handle.h
 */
/*@{*/

/** template #tfxx::THandle<class TObj>#:
 *
 * THandle is a class template published by B. Stroustrup in Chapter 25.7 of
 * "The C++ programming Language" (3rd edition). This is a slightly modified
 * version.
 *
 * Example:
 * To create a handle #hs# to a string #s# and then output the string value
 * you would code:
 *
 *   #THandle<string> hs(new string(s)); cout << *hs << endl;#
 *
 * Thus the handle always represents a kind of pointer to your string. You may
 * copy and pass this pointer as you like. And all these copies will allocated
 * just memory for that pointer and any modification applied to the string
 * will be accessible through every of these handles. However if the last
 * handle is removed during program execution it will also remove the string
 * representation from the memory. This is an convenient way to pass large
 * objects without worrying about memory usage and management.
 *
 * It is not possible to create an empty handle - that wouldn't make any
 * sense. You always have to provide another handle to copy from or an element
 * to be handled.
 *
 * \sa tfxx::Handle
 *
 * @author Bjarne Stroustrup,
 * @author Thomas Forbriger
 * @version V1.0 (#$Revision: 1.6 $ $Date: 2006-03-28 18:14:15 $#)
 * @memo template providing a handle to an object of any type
 */
template<class X>
class ConstHandle {
  public:
    typedef X Tmutableobject;
    typedef const X Tobject;
    typedef ConstHandle<Tobject> Tcontainer;
    typedef Tcontainer Tcoc;
    typedef Tcoc Tcontainer_of_const;
    typedef const Tobject* Tpointer;
    typedef const Tobject& Treference;
    
    /** constructor #tfxx::THandle<class X>::THandle#:
     *
     * This constructor is used in the following way:
     *
     *   #THandle<X> handle(X);#
     *
     * @memo init constructor
     */
    ConstHandle(Treference p):
      Mrep(new Tmutableobject(p)), Mpcount(new int(1)) { }

    /*! Constructor to handle virtual base classes
     *
     * This constructor \b must be used in the following way:
     *
     *   #THandle<X> handle(new X);#
     *
     * \note
     * Do not pass any other pointer, since the destructor will call the
     * delete operator on p.
     */
    ConstHandle(Tmutableobject* p):
      Mrep(p), Mpcount(new int(1)) { }

    /** constructor #tfxx::THandle<class X>::THandle#:
     *
     * This constructor is used in the following way together with another
     * #handle2# of type #THandle<X>#:
     *
     *   #THandle<X> handle2=handle1;#
     *
     * @memo copy constructor
     */
    ConstHandle(const ConstHandle& h): Mrep(h.Mrep), Mpcount(h.Mpcount) 
      { (*Mpcount)++; }

    /** desctructor #tfxx::THandle<class X>::~THandle#
     * @memo book-keeping destructor
     */
    ~ConstHandle() { if (--(*Mpcount)==0) { delete Mrep; delete Mpcount; } }

    /** operator #tfxx::THandle<class X>::operator->()#
     * @return returns pointer to object of type #class X#
     * @memo dereferencing operator
     */
    Tpointer operator->() const { return Mrep; }

    /** operator #tfxx::THandle<class X>::operator*()#
     * @return returns reference to object of type #class X#
     * @memo dereferencing operator
     */
    Treference operator*() const { return *Mrep; }

    /** operator #tfxx::THandle<class X>::operator=()#
     * @memo book-keeping asignment operator
     */
    ConstHandle& operator=(const ConstHandle& h)
    {
      if (Mrep == h.Mrep) return *this;
      if (--(*Mpcount) == 0) { delete Mrep; delete Mpcount; }
      Mrep=h.Mrep;
      Mpcount=h.Mpcount;
      (*Mpcount)++;
      return *this;
    }

  protected:
    //! expose the pointer to derived classes
    Tmutableobject* pointer() const { return Mrep;}

    //! expose the object to derived classes
    Tmutableobject& reference() const { return *Mrep;}

  private:
    /// internal pointer to the handled object
    Tmutableobject* Mrep;
    /// usage counter
    int* Mpcount;
}; // template class ConstHandle

/*! The handle class
 */
template<class X>
class Handle: public ConstHandle<X> {
  public:
    typedef X Tobject;
    typedef ConstHandle<Tobject> Tbase;
    typedef Handle<Tobject> Tcontainer;
    typedef Tbase Tcoc;
    typedef Tcoc Tcontainer_of_const;
    typedef Tobject* Tpointer;
    typedef Tobject& Treference;
    
    /** constructor #tfxx::THandle<class X>::THandle#:
     *
     * This constructor is used in the following way:
     *
     *   #THandle<X> handle(new X);#
     *
     * @memo init constructor
    Handle(X* p): Mrep(p), Mpcount(new int(1)) { }
     */
    Handle(const Treference p): Tbase(p) { }

    /*! Constructor to handle virtual base classes
     *
     * This constructor \b must be used in the following way:
     *
     *   #THandle<X> handle(new X);#
     *
     * \note
     * Do not pass any other pointer, since the destructor will call the
     * delete operator on p.
     */
    Handle(Tpointer p): Tbase(p) { }

    /** constructor #tfxx::THandle<class X>::THandle#:
     *
     * This constructor is used in the following way together with another
     * #handle2# of type #THandle<X>#:
     *
     *   #THandle<X> handle2=handle1;#
     *
     * @memo copy constructor
     */
    Handle(const Handle& h): Tbase(h) { }

    /** operator #tfxx::THandle<class X>::operator->()#
     * @return returns pointer to object of type #class X#
     * @memo dereferencing operator
     */
    Tpointer operator->() const { return Tbase::pointer(); }

    /** operator #tfxx::THandle<class X>::operator*()#
     * @return returns reference to object of type #class X#
     * @memo dereferencing operator
     */
    Treference operator*() const { return Tbase::reference(); }

    /** operator #tfxx::THandle<class X>::operator=()#
     * @memo book-keeping asignment operator
     */
    Handle& operator=(const Handle& h)
    { 
      Tbase::operator=(h);
      return *this;
    }
}; // template class Handle

/*@}*/

} // namespace tfxx

#endif // TF_HANDLE_H_VERSION (includeguard)

/* ----- END OF handle.h ----- */
