/*! \file formatmodifier.h
 * \brief provide format modifiers (prototypes)
 * 
 * \ingroup group_formatmodifiers
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/07/2011
 * 
 * provide format modifiers (prototypes)
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
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 28/07/2011   V1.0   Thomas Forbriger (thof)
 *  - 21/01/2012   V1.1   provide online help output support class
 *  - 05/07/2016   V1.2   thof: provide output operator for line indentation
 *                        with modifier help
 *  - 08/07/2016   V1.3   make correct use of new DATRW_report_assert
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_FORMATMODIFIER_H_VERSION

#define DATRW_FORMATMODIFIER_H_VERSION \
  "DATRW_FORMATMODIFIER_H   V1.3"

#include <string>
#include <sstream>
#include <map>
#include <datrwxx/formats.h>
#include <datrwxx/error.h>

/*! \brief Provide mechanism to extract format modifiers
 *
 * \defgroup group_formatmodifiers Internal utility: Support for format modifiers
 */

/*! \brief Issue warning if modifiers are passed but not evaluated
 *
 * \ingroup group_formatmodifiers
 * \param F format enum
 * \param M modifier string
 */
#define DATRW_expect_no_modifier(F,M) \
  DATRW_report_assert(M.length()<1, \
                      "no format modifier should be passed!\n" << \
                      "format " << anyID( F )  \
                      << " is not expected to handle format modifiers\n" \
                      << "any modifiers are silently ignored!"); 

/*! \brief abort if user passed unused modifiers
 *
 * \ingroup group_formatmodifiers
 * \param S instance of type datrw::Subformat to check
 * \param F name of function issuing the message
 */
#define DATRW_assert_modifiers_are_recognized(S,F) \
  if (!S.allarechecked()) \
  { \
    std::cerr << "ERROR (" << F << "):" << std::endl; \
    S.notchecked(std::cerr); \
  } \
  DATRW_assert(S.allarechecked(), "ERROR: additional unrecognized modifiers");

namespace datrw {

  /*! \brief Namespace containing components of format modifiers
   * \ingroup group_formatmodifiers
   */
  namespace formatmodifiers {

    /*----------------------------------------------------------------------*/

    /*! \brief A struct to store values for a given key
     * \ingroup group_formatmodifiers
     */
    struct Value {
      Value(const std::string& v):
        value(v), checked(false) { } 
      Value():
        value(""), checked(false) { } 
      std::string value; //!< value passed with parameter string
      mutable bool checked; //!< true if value was checked
    }; // struct Value

    /*----------------------------------------------------------------------*/

    /*! \brief A map to store parameters.
     * \ingroup group_formatmodifiers
     */
    typedef std::map<std::string,Value> Tparamap;

    /*----------------------------------------------------------------------*/

    /*! \brief Create a parameter map from a parameter string
     * \ingroup group_formatmodifiers
     *
     * \param p parameter string
     * \param delimiter delimiter which separates two parameters
     * \param assign symbol seprating key and value
     * \return a multimap created from the parameter string
     */
    Tparamap makeparamap(const std::string& p,
                         const std::string& delimiter=":",
                         const std::string& assign="=");
    /*----------------------------------------------------------------------*/

    /*! \brief Provide online help on modifiers
     * \ingroup group_formatmodifiers
     *
     * \param os output stream to which online help should be written
     */
    void online_help(std::ostream& os);

    /*----------------------------------------------------------------------*/

    /*! \brief Help formatting modifier online help
     * \ingroup group_formatmodifiers
     * \sa datrw::osustream::help()
     */
    class ModifierHelp
    {
      public:
        ModifierHelp(std::ostream& os,
                     const unsigned int& width):
          Mos(os), Mwidth(width) { }
        std::ostream& operator()(const std::string& key,
                                 const std::string& val);
        std::ostream& operator()(const std::string& key);
        std::ostream& operator()();
      private:
        std::ostream& Mos;
        unsigned int Mwidth;
    }; // class ModifierHelp

    /*! \brief Output operator for ModifierHelp
     * \ingroup group_formatmodifiers
     * 
     * For line indentation an object of type ModifierHelp can be used like an
     * output stream;
     *
     * \sa datrw::mseed::help()
     */
    template<class C>
      std::ostream& operator<<(ModifierHelp& mh, const C& c)
      { return(mh() << c); }

  } // namespace formatmodifiers

  /*----------------------------------------------------------------------*/

  /*! \brief Class to handle format modifiers
   * \ingroup group_formatmodifiers
   *
   * This class is used to parse a format modifier string.
   * Detailed instructions will be given upon request.
   * For some hints have a look at tests/libdatrwxxtests.c
   */
  class Subformat {
    public:
      //! \brief constructor parses format modifier string
      Subformat(const std::string& modifiers):
        Mparamap(formatmodifiers::makeparamap(modifiers, ":", "=")),
        Mstream("")
    { }
      /*! \brief function operator returns string 
       * \param k key for which parameters should be read
       * \param dev default value to be used, if parameter is not set
       * \return string containing parameter value
       */
      std::string value(const std::string& k,
                        const std::string& dev="false") const;
      /*! \brief function operator returns string stream 
       * \param k key for which parameters should be read
       * \param dev default value to be used, if parameter is not set
       * \return string stream from which parameters can be read using
       *         stream input operator
       * This function replaces commas by whitespace before passing the
       * parameter value to the string stream.
       */
      std::istringstream& operator()(const std::string& k,
                                     const std::string& dev="false") const;
      /*! \brief check if user provided this key
       * \param k key to check for
       * \return true if key is set in modifier string
       */
      bool isset(const std::string& k) const;
      /*! \brief check if user provided keys not being recognized
       * \return true is all modifiers passed by the user haven been evaluated
       */
      bool allarechecked() const;
      /*! \brief output all modifier which have not been checked yet
       */
      void notchecked(std::ostream& os) const;
    private:
      //! actual container for parameters
      formatmodifiers::Tparamap Mparamap;
      //! string stream used by function operator member function
      mutable std::istringstream Mstream;
  }; // class Subformat

} // namespace datrw

#endif // DATRW_FORMATMODIFIER_H_VERSION (includeguard)

/* ----- END OF formatmodifier.h ----- */
