/*! \file regexx.h
 * \brief provide regular expression functionality (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/12/2007
 * 
 * provide regular expression functionality (prototypes)
 * 
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 05/12/2007   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_REGEXX_H_VERSION

#define TF_REGEXX_H_VERSION \
  "TF_REGEXX_H   V1.0"

#include<string>

namespace tfxx {

  namespace string {

    namespace helper {

        /*! we need a helper class to hide the internals.
         *
         * delegate everything to the container class.
         */
        class regexxcontainer {
          public:
            //! create instance.
            regexxcontainer();
            //! create instance and set expression to \c e.
            regexxcontainer(const std::string& e);
            //! create instance and set expression to \c e.
            virtual ~regexxcontainer();
            //! set expression to \c e.
            virtual void expression(const std::string& e) =0;
            //! return expression
            virtual std::string expression() const =0;
            //! return true is \c s matches expression.
            virtual bool match(const std::string s) =0;
        }; // class regexxcontainer

    } // namespace helper

    /*! interface to regular expression functionality.
     *
     * This class provides an interface either to libregexx or libboost_regex,
     * depending on the setting of the preprocessor flag OLDLIBREGEXX.
     *
     * Create an instance of this class and set a regular expression. Using
     * the match function you can check strings against the expression.
     */
    class regexx {
      public:
        //! create instance.
        regexx();
        //! create instance and set expression to \c e.
        regexx(const std::string& e);
        //! remove instance.
        ~regexx();
        //! set expression to \c e.
        void expression(const std::string& e)
        { Mcontainer->expression(e); }
        //! return expression
        std::string expression() const
        { return(Mcontainer->expression()); }
        //! return true is \c s matches expression.
        bool match(const std::string s)
        { return(Mcontainer->match(s)); }
      private:
        tfxx::string::helper::regexxcontainer* Mcontainer;
    }; // class regexx

  } // namespace string

} // namespace tfxx

#endif // TF_REGEXX_H_VERSION (includeguard)

/* ----- END OF regexx.h ----- */
