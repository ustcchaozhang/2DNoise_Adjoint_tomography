/*! \file stringfunc.h
 * \brief string functions copied from former class library (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/06/2005
 * 
 * string functions copied from former class library (prototypes)
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
 * 
 * REVISIONS and CHANGES 
 *  - 30/06/2005   V1.0   Thomas Forbriger
 *  - 11/11/2009   V1.1   \b !! changed interface of gen_split, since
 *                        new g++ does not handle template template functions
 *  - 25/11/2010   V1.2   implemented more efficient method to strip
 *                        whitespace
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_STRINGFUNC_H_VERSION

#define TF_STRINGFUNC_H_VERSION \
  "TF_STRINGFUNC_H   V1.2"

#include<string>

  namespace tfxx {

    namespace string {

      /*! template #tf::join#:
       *
       * The following takes any Container of string elements and joins all 
       * strings into one single string.
       *
       * This is the inverse operation to #tf::split#.
       *
       * \return concatenated string
       * \param v any sequence container containing strings
       * \param delimiter a string sequence that will put in between 
       *    the individual strings when concatenating them
       *
       * \author Thomas Forbriger
       * \version V1.0 (#$Revision: 1.4 $ $Date: 2009-11-11 12:34:04 $#)
       * \memo template to join strings in any sequence container
       * \see split
       * \see gen_split
       */
      template<template<class> class C>
        std::string join(const C<std::string>& v, 
                         const std::string& delimiter="")
        {
          std::string retval;
          for (typename C<std::string>::const_iterator i=v.begin(); 
               i!=v.end(); i++)
          { retval.append(*i); retval.append(delimiter); }
          return(retval);
        }

      /*! template #tf:gen_split#:
       *
       * The following takes a string and splits it into substrings delimited
       * by #delimiter#. The resulting substrings will be written into a
       * container of strings.
       *
       * Neither the template mechanism nor the linker mechanism linker is
       * able to deduce the correct function definition from the return value.
       * Hence we have to use the container class template parameter in the
       * functions formal parameter list.
       *
       * This is the inverse operation to #tf::join#.
       *
       * \param v               sequence container that will receive the
       *                        substrings
       * \param s               string to be split into substrings
       * \param delimiter       string sequence that intersects substrings
       * \param dropdelimiter   if #true# the delimiter itself will be
       *                        excluded from the output
       *
       * \author Thomas Forbriger
       * \version V1.0 (#$Revision: 1.4 $ $Date: 2009-11-11 12:34:04 $#)
       * \memo split string into substrings
       * \see join
       * \see split
       */
      /*
      template<template<class> class C>
        void gen_split(C<std::string>& v, const std::string& s, 
        */
      template<class C>
        void gen_split(C& v, const std::string& s, 
                       const std::string& delimiter=" ",
                       const bool& dropdelimiter=false)
        {
          v.clear();
          if (!s.empty()) {
            std::string::size_type posend=0, pos=0;
            std::string::size_type len=0;
            while (posend!=std::string::npos) {
              posend=s.find(delimiter, pos);
              len=posend-pos;
              if (dropdelimiter){
                v.push_back(s.substr(pos, len));
                if (posend!=std::string::npos) { len+=delimiter.length(); }
              } else {
                if (posend!=std::string::npos) { len+=delimiter.length(); }
                v.push_back(s.substr(pos, len));
              }
              pos+=len;
            }
          }
        }

      /*! template #tf::split#
       *
       * Use the following interface to the gen_split function by explicitely
       * passing a template type argument like:
       * 
       *   #list<string> l;#
       *   #string s="a,b,c,d,e";#
       *   #l=split<list>(s);#
       * 
       * This is the inverse operation to #tf::join#.
       *
       * \return seqeunce container containing substrings
       * \param s               string to be split into substrings
       * \param delimiter       string sequence that intersects substrings
       * \param dropdelimiter   if #true# the delimiter itself will be
       *                        excluded from the output
       *
       * \author Thomas Forbriger
       * \version V1.0 (#$Revision: 1.4 $ $Date: 2009-11-11 12:34:04 $#)
       * \see join
       * \see gen_split
       * \memo adapter to gen_split
       */
      template<template<class> class C>
        C<std::string> split(const std::string& s, 
                             const std::string& delimiter=" ",
                             const bool& dropdelimiter=false)
        { 
          C<std::string> v; 
          gen_split(v, s, delimiter, dropdelimiter); 
          return(v); 
        }

      /*! function #tf::trimws_begin#:
       * Erase leading whitespace from a string.
       *
       * \return string with leading whitespace removed
       * \param s string to remove leading whitespace from
       * \author Thomas Forbriger
       * \version V1.0 (#$Revision: 1.4 $ $Date: 2009-11-11 12:34:04 $#)
       * \memo remove leading whitespace
       * \see trimws_end
       * \see trimws
       */
      std::string trimws_begin(std::string s);

      /*! function #tf::trimws_end#:
       * Erase trailing whitespace from a string.
       *
       * \return string with trailing whitespace removed
       * \param s string to remove trailing whitespace from
       * \author Thomas Forbriger
       * \version V1.0 (#$Revision: 1.4 $ $Date: 2009-11-11 12:34:04 $#)
       * \memo remove trailing whitespace
       * \see trimws_begin
       * \see trimws
       */
      std::string trimws_end(std::string s);

      /*! function #tf::trimws#:
       * Erase leading and trailing whitespace from a string.
       * This in fact is an inline adapter to #trimws_begin# and #trimws_end#.
       *
       * \return string with leading and trailing whitespace removed
       * \param s string to remove leading and trailing whitespace from
       * \author Thomas Forbriger
       * \version V1.0 (#$Revision: 1.4 $ $Date: 2009-11-11 12:34:04 $#)
       * \memo remove leading and trailing whitespace
       * \see trimws_begin
       * \see trimws_end
       */
      std::string trimws(std::string s);

      /*! strip substring
       *
       * \param c character to find in string
       * \param delim delimiter to look for
       * \return number of c characters in string
       */
      std::string strip_string(std::string& s, const char& delim=',');

      /*! count characters in string
       *
       * \param s string to look for c (substring is stripped from this)
       * \param c character to find in string
       * \return number of c characters in string
       */
      int count_char(const std::string& s, const char& c);

      /*! replace pattern by string
       *
       * \param s string that contains pattern
       * \param p pattern string
       * \param r replacement for pattern
       * \return string \p s with all instances of pattern \p p 
       *                replaced by \p r
       */
      std::string patsubst(const std::string& s,
                           const std::string& p,
                           const std::string& r);

    } // namespace string

  } // namesapce tfxx

#endif // TF_STRINGFUNC_H_VERSION (includeguard)

/* ----- END OF stringfunc.h ----- */
