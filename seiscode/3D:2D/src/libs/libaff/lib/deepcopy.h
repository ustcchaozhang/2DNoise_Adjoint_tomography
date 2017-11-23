/*! \file deepcopy.h
 * \brief external deep copy function (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 29/12/2002
 * 
 * external deep copy function (prototypes)
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
 * \sa aff::deepcopy
 * \sa aff::Array::copyin
 * \sa aff::Series::copyin
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 29/12/2002   V1.0   Thomas Forbriger
 *  - 03/01/2003   V1.1   (thof)
 *                        - placed deepcopy in namespace aff
 *  - 04/01/2003   V1.2   (thof)
 *                        - now is able to handle slice and subarray classes
 *                          if they provide an appropriate Tcontainer typedef
 *                        - uhh source/target type mismatch resolved
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_DEEPCOPY_H_VERSION

#define AFF_DEEPCOPY_H_VERSION \
  "AFF_DEEPCOPY_H   V1.2"

namespace aff {

  /*! \brief deep copy
   *
   * Takes any two containers with appropriate interface and copies as many
   * values as possible from source to target.
   * 
   * \param source container to read values from
   * \param target container to write values to
   *
   * \sa aff::Array::copyin
   * \sa aff::Series::copyin
   */
  template<class S, class T>
    void deepcopy(const S& source, T& target)
    {
      // acces source through container of const (read only access)
      typedef typename S::Tcontainer::Tcoc Tscoc;
      typedef typename Tscoc::Trepresentation Tsrep;
      typedef typename Tscoc::Tstepper Tsstp;
      // access types for target
      typedef typename T::Tcontainer Ttcon;
      typedef typename Ttcon::Trepresentation Ttrep;
      typedef typename Ttcon::Tstepper Ttstp;
      // alias for source and target
      const Tscoc& csource(source);
      Ttcon ctarget(target);
      // representations
      Tsrep srep(csource.representation());
      Ttrep trep(ctarget.representation());
      // steppers
      Tsstp sstp(csource.shape());
      Ttstp tstp(ctarget.shape());
      sstp.tofirst();
      tstp.tofirst();
      while (sstp.valid() && tstp.valid())
      {
        trep[tstp.current()]=srep[sstp.current()];
        tstp.incr();
        sstp.incr();
      }
    } // deepcopy

} // namespace aff

#endif // TF_DEEPCOPY_H_VERSION (includeguard)

/* ----- END OF deepcopy.h ----- */
