/*! \file regexx.cc
 * \brief provide regular expression functionality (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/12/2007
 * 
 * provide regular expression functionality (implementation)
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
 *  - 13/12/2007   V1.1   traditional regexx.hh uses namespace regexx too
 *                        use explicit root reference for namespace
 * 
 * ============================================================================
 */
#define TF_REGEXX_CC_VERSION \
  "TF_REGEXX_CC   V1.1"

#include <tfxx/regexx.h>

#ifdef OLDLIBREGEXX
#include<regexx.hh>
#ifndef REGEXX_HH
#error read wrong header
#endif
#else
#include<boost/regex.hpp>
#endif

namespace tfxx {

  namespace string {

    namespace helper {

      //! create instance.
      regexxcontainer::regexxcontainer() { }
      //! create instance and set expression to \c e.
      regexxcontainer::regexxcontainer(const std::string& e) { }
      //! create instance and set expression to \c e.
      regexxcontainer::~regexxcontainer() { }

/*======================================================================*/
// use old libregexx
#ifdef OLDLIBREGEXX

      /*! we need a helper class to hide the internals.
       *
       * delegate everything to the container class.
       */
      class myregexxcontainer: 
        public tfxx::string::helper::regexxcontainer {
          public:
            //! create instance.
            myregexxcontainer():
              tfxx::string::helper::regexxcontainer()
            { this->expression(std::string(".*")); }
            //! create instance and set expression to \c e.
            myregexxcontainer(const std::string& e):
              tfxx::string::helper::regexxcontainer(e)
            { this->expression(e); }
            //! create instance and set expression to \c e.
            virtual ~myregexxcontainer() { }
            //! set expression to \c e.
            virtual void expression(const std::string& e)
            { Mregex.expr(e); }
            //! return expression
            virtual std::string expression() const
            { return(Mregex.expr()); }
            //! return true is \c s matches expression.
            virtual bool match(const std::string s)
            {
              Mregex.str(s);
              return (Mregex.exec());
            }
          private:
            ::regexx::Regexx Mregex;
      }; // class myregexxcontainer

/*======================================================================*/
// use libboost_regex
#else

      /*! we need a helper class to hide the internals.
       *
       * delegate everything to the container class.
       */
      class myregexxcontainer: 
        public tfxx::string::helper::regexxcontainer {
          public:
            //! create instance.
            myregexxcontainer():
              tfxx::string::helper::regexxcontainer()
            { this->expression(std::string(".*")); }
            //! create instance and set expression to \c e.
            myregexxcontainer(const std::string& e):
              tfxx::string::helper::regexxcontainer(e)
            { this->expression(e); }
            //! create instance and set expression to \c e.
            virtual ~myregexxcontainer() { }
            //! set expression to \c e.
            virtual void expression(const std::string& e)
            { Mregex.assign(e); }
            //! return expression
            virtual std::string expression() const
            { return(Mregex.str()); }
            //! return true is \c s matches expression.
            virtual bool match(const std::string s)
            { return(boost::regex_match(s, Mregex)); }
          private:
            boost::regex Mregex;
      }; // class myregexxcontainer

#endif

    } // namespace helper

/*======================================================================*/
// common code

    //! create instance.
    regexx::regexx() 
    { Mcontainer=new tfxx::string::helper::myregexxcontainer; }
    //! create instance and set expression to \c e.
    regexx::regexx(const std::string& e)
    { Mcontainer=new tfxx::string::helper::myregexxcontainer(e); }
    //! remove instance.
    regexx::~regexx()
    { delete(Mcontainer); }

  } // namespace string

} // namespace tfxx

/* ----- END OF regexx.cc ----- */
