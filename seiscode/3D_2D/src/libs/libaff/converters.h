/*! \file converters.h
 * \brief converters for AFF containers
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 15/05/2011
 * 
 * converters for AFF containers
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 * 
 * REVISIONS and CHANGES 
 *  - 15/05/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_CONVERTERS_H_VERSION

#define AFF_CONVERTERS_H_VERSION \
  "AFF_CONVERTERS_H   V1.0   "

#include<aff/array.h>
#include<aff/series.h>
#include<aff/lib/error.h>

namespace aff {

  /*! \brief create a series container from an array container.
   *
   * \ingroup group_series_extensions group_array_extensions 
   */
  template<class T>
    aff::Series<T> series_from_array(const aff::Array<T>& array)
    {
      typedef aff::Series<T> Tseries;
      typedef aff::Array<T> Tarray;
      typename Tarray::Tshape shape(array.shape());
      AFF_assert(aff::util::is_dense_1D_array(shape),
                 "ERROR: array is not suitable to be converted to series");
      typename Tseries::Trepresentation representation=array.representation();
      typename Tseries::Tshape seriesshape(shape.first(0), 
                                           shape.last(0),
                                           shape.first_offset());
      Tseries retval(seriesshape, representation);
      return retval;
    } // aff::Series<T> series_from_array(const aff::Array<T>& array)

  /*----------------------------------------------------------------------*/

  /*! \brief create a series container from an array container.
   *
   * \ingroup group_series_extensions group_array_extensions 
   */
  template<class T>
    aff::ConstSeries<T> series_from_array(const aff::ConstArray<T>& array)
    {
      typedef aff::ConstSeries<T> Tseries;
      typedef aff::ConstArray<T> Tarray;
      typename Tarray::Tshape shape(array.shape());
      AFF_assert(aff::util::is_dense_1D_array(shape),
                 "ERROR: array is not suitable to be converted to series");
      typename Tseries::Trepresentation representation=array.representation();
      typename Tseries::Tshape seriesshape(shape.first(0), 
                                           shape.last(0),
                                           shape.first_offset());
      Tseries retval(seriesshape, representation);
      return retval;
    } // aff::ConstSeries<T> series_from_array(const aff::ConstArray<T>& array)

  /*----------------------------------------------------------------------*/

  /*! \brief create an array container from a series container.
   *
   * \ingroup group_series_extensions group_array_extensions 
   * \todo
   * implement aff::Array<T> array_from_series(const aff::Series<T>& array)
   */
  template<class T>
    aff::Array<T> array_from_series(const aff::Series<T>& array)
    {
      AFF_abort("not yet implemented");
    } // aff::Series<T> series_from_array(const aff::Array<T>& array)

  /*----------------------------------------------------------------------*/

  /*! \brief create an array container from a series container.
   *
   * \ingroup group_series_extensions group_array_extensions 
   * \todo
   * implement aff::ConstArray<T> array_from_series(const aff::ConstSeries<T>& array)
   */
  template<class T>
    aff::ConstArray<T> array_from_series(const aff::ConstSeries<T>& array)
    {
      AFF_abort("not yet implemented");
    } // aff::Series<T> series_from_array(const aff::Array<T>& array)

  /*----------------------------------------------------------------------*/

  /*! \brief create a series class from raw memory.
   *
   * \ingroup group_series_extensions
   *
   * \param pointer pointer to first element in raw memory
   * \param size number of elements allocated in raw memory
   * \return series container accessing raw memory
   */
  template<class T>
    aff::Series<T> series_from_raw_memory(T* pointer,
                                          const unsigned int size)
    {
      typedef aff::Series<T> Tseries;
      typename Tseries::Tshape shape(0, size-1, 0);
      typename Tseries::Trepresentation representation(pointer, size);
      Tseries retval(shape, representation);
      return(retval);
    } // aff::Series<T> series_from_raw_memory

  /*----------------------------------------------------------------------*/

  /*! \brief access Series contents through raw memory
   *
   * \ingroup group_series_extensions
   */
  template<class T>
    class CSeries {
      public:
        /*! \name Various types
         *
         * In particular due to our concept of const-correctness we need
         * several typedefs to declare types derived from the element type of
         * the array.
         *
         * \sa \ref sec_design_interface_typedef
         * \sa \ref sec_design_const
         */
        //@{
        //! Type of array to be interfaced
        typedef aff::Series<T> Tseries;
        //! Type of representation
        typedef typename Tseries::Trepresentation Trepresentation;
        //! Type of shape
        typedef typename Tseries::Tshape Tshape;
        //! Element type
        typedef typename Tseries::Tvalue Tvalue;
        //! Type of pointer to element
        typedef typename Tseries::Tpointer Tpointer;
        //! Type of reference to element
        typedef typename Tseries::Treference Treference;
        //! const element type
        typedef typename Tseries::Tconst_value Tconst_value;
        //! Type of pointer to const element
        typedef typename Tseries::Tconst_pointer Tconst_pointer;
        //! Type of reference to const element
        typedef typename Tseries::Tconst_reference Tconst_reference;
        //! Type of this array
        typedef CSeries<T> Tcontainer;
        //@}

        /*-----------------------------------------------------------------*/
        
        /*! \name Constructors
         *
         * No copy constructors or copy operators are provided since this is
         * provided as an interface class only.
         */
        //@{
        //! construct from shape and representation
        CSeries(const Tseries& series)
          : Mrepresentation(series.representation())
        {
          Tshape shape=series.shape();
          Moffset=shape.offset(shape.first());
          Msize=shape.size();
        }
        //@}

        /*------------------------------------------------------------------*/

        /*! \name Shape access
         */
        //@{
        //! size of dimension \par i
        const Tsize& size() const
        { return (Msize); }
        //@}

        /*-----------------------------------------------------------------*/
        
        /*! \name Memory access
         *
         */
        //@{
        //! return pointer to first element in Fortran layout
        Tpointer pointer() const 
        { return(&Mrepresentation[Moffset]); }
        /*! \brief return type-casted pointer to first element in Fortran
         * layout
         *
         * The cast checks for const-correctness and type-size. But you have
         * to ensure that there is a meaningful relation between both types
         * involved.
         *
         * \sa aff::util::SizeCheckedCast
         */
        template<class TT>
          TT* castedpointer() const
          { return(SizeCheckedCast<Tvalue,TT>::cast(this->pointer())); }
        //@}

      private:
        //! representation member
        Trepresentation Mrepresentation;
        //! sizes of series
        aff::Tsize Msize;
        //! offset of memory location of first element
        aff::Tsize Moffset;
    }; // class CSeries

} // namespace aff

#endif // AFF_CONVERTERS_H_VERSION (includeguard)

/* ----- END OF  converters.h ----- */
