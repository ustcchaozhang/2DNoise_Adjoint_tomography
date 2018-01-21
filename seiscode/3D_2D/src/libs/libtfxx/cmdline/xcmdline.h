/*! \file xcmdline.h
 * \brief parse extra commandline values (prototypes)
 * 
 * \ingroup xcmdline_h
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/06/2005
 * 
 * parse extra commandline values (prototypes)
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
 *  - 28/06/2005   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_XCMDLINE_H_VERSION

#define TF_XCMDLINE_H_VERSION \
  "TF_XCMDLINE_H   V1.0   "

#include <tfxx/commandline.h>
#include <string>
#include <map>
#include <list>

namespace tfxx {
  namespace cmdline {

    /*! \brief Interface provided through xcmdline.h
     *
     * \defgroup xcmdline_h Interface provided through xcmdline.h
     *
     * This module is used to read a list o filenames from the command line
     * together with options and arguments that are given per filename.
     *
     * In case you want to pass options in the form
     * \verbatim filename f:option1 dl:option2 k:option3 \endverbatim
     * you should generate keys of the form
     * \code
     *   static const char* keys[]={"f", "dl", "k", 0};
     * \endcode
     *
     * With this list of key strings, you call the function
     * tfxx::cmdline::parse_cmdline to read the rest of the command line:
     * \code
     * tfxx::cmdline::Tparsed 
     *   filenames=tfxx::cmdline::parse_cmdline(cmdline, 
     *                                          keys, 
     *                                          verbose);
     * \endcode
     * The arguments to this function have the following meaning:
     * - \p cmdline is an object of type 
     *   \code
     *   tfxx::cmdline::Commandline cmdline(iargc, argv, options);
     *   \endcode
     *   that is initialized with the contents of the command line and that is
     *   usually available anyway.
     * - \p keys is a list of character strings declared as
     *   \code
     *   static const char* keys[]={"f", "dl", "k", 0};
     *   \endcode
     *   that contains the keys for filename specific parameters
     * - \p debug is a switch of type
     *   \code
     *   bool verbose;
     *   \endcode
     *   that controls verbosity
     *
     * The functions returns an object of type tfxx::cmdline::Tparsed.
     * This contains the filenames together with their parameters.
     * It is a list of objects of type tfxx::cmdline::Filename actually
     * defined as
     * \code
     * std::list<tfxx::cmdline::Filename> filenames;
     * \endcode
     * It can thus be scanned by the means of STL iterators.
     * Each entry contains exactly one filename together with its parameters.
     * See tfxx::cmdline::Filename for details.
     *
     * \sa example_commandlinetest
     *
     * \ingroup group_commandline
     */

    /*! \brief pair to hold file option
     * \ingroup group_commandline, xcmdline_h
     *
     * A std::pair is an STL container.
     * In this it contains a key together with the parameter read from the
     * command line. 
     * Given an option
     * \code
     * Tfileoption option;
     * \endcode
     * The key value is available through 
     * \code
     * std::string key=option.first;
     * \endcode
     * and the parameter is available though
     * \code
     * std::string parameter=option.second;
     * \endcode
     *
     * A tutorial is available in the detailed description of
     * the \ref xcmdline_h
     *
     * \sa example_commandlinetest
     */
    typedef std::pair<std::string, std::string> Tfileoption;

    /*! \brief map to hold file options
     * \ingroup group_commandline, xcmdline_h
     *
     * A std::multimap is an STL container of pairs.
     * It provides STL iterators to scan the collection.
     * In this context each element in a 
     * \c tfxx::cmdline::Toptionmap is a \c tfxx::cmdline::Tfileoption.
     *
     * A tutorial is available in the detailed description of
     * the \ref xcmdline_h
     *
     * \sa example_commandlinetest
     */
    typedef std::multimap<std::string, std::string> Toptionmap;

    /*! \brief A struct to hold filename together with options
     * \ingroup group_commandline, xcmdline_h
     *
     * If a filename is given together with its specific parameters in
     * \code
     * tfxx::cmdline::Filename fname;
     * \endcode
     * You can
     * - extract obtain the filename itself from field \p name:
     *   \code
     *   std::string name=fname.name;
     *   \endcode
     * - extract all parameters together with their keys in a multimap from 
     *   field \p options:
     *   \code
     *   std::multimap<std::string, std::string> parameters=fname.options;
     *   \endcode
     * - extract all parameters matching a specific key \c "keyval" through
     *   function \c extract:
     *   \code
     *   std::multimap<std::string, std::string> 
     *     parameters=fname.extract("keyval");
     *   \endcode
     * - find out whether a key was used to specify parameters through
     *   function \c haskey:
     *   \code
     *   bool keysfound=fname.haskey("keyval");
     *   \endcode
     * - find out how many entries are available for a specific key through
     *   function \c count:
     *   \code
     *   int nentries=fname.count("keyval");
     *   \endcode
     * - extract the first parameter used together with a specific key through
     *   function \c value:
     *   \code
     *   std::string parameter=fname.value("keyval");
     *   \endcode
     *
     * Lists of parameters are provided in a multimap defined as 
     * \c tfxx::cmdline::Toptionmap
     *
     * \sa Toptionmap
     * \sa example_commandlinetest
     */
    struct Filename {
      //! filename
      std::string name;
      //! multimap containing all arguments together with their keys
      Toptionmap options;
      //! return all entries for a given key
      Toptionmap extract(const std::string& key) const;
      //! return values for option with given key
      std::string value(const std::string& key) const;
      //! number of entries for key
      Toptionmap::size_type count(const std::string& key) const
      { return options.count(key); }
      //! tell if the key is present
      bool haskey(const std::string& key) const
      { return (this->count(key) > 0); }
    }; // struct Filename


    /*! \brief list to hold file names with options
     * \ingroup group_commandline, xcmdline_h
     *
     * A tutorial is available in the detailed description of
     * the \ref xcmdline_h
     *
     * \sa example_commandlinetest
     */
    typedef std::list<Filename> Tparsed;

    /*! \brief parse command line arguments for file names and options
     * \ingroup group_commandline, xcmdline_h
     *
     * A tutorial is available in the detailed description of
     * the \ref xcmdline_h
     *
     * \sa example_commandlinetest
     */
    Tparsed parse_cmdline(tfxx::cmdline::Commandline& c,
                          const char** keys,
                          const bool& debug=false);

  } // namespace cmdline

} // namespace tfxx

#endif // TF_XCMDLINE_H_VERSION (includeguard)

/* ----- END OF xcmdline.h ----- */
