/*! \file formatmodifier.cc
 * \brief provide format modifiers (implementation)
 * 
 * \ingroup group_formatmodifiers
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/07/2011
 * 
 * provide format modifiers (implementation)
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
 *  - 28/07/2011   V1.0   Thomas Forbriger
 *  - 21/01/2012   V1.1   provide online help output support class
 * 
 * ============================================================================
 */
#define DATRW_FORMATMODIFIER_CC_VERSION \
  "DATRW_FORMATMODIFIER_CC   V1.1"

#include <iostream>
#include <iomanip>
#include <datrwxx/formatmodifier.h>
#include <datrwxx/util.h>
#include <algorithm>

namespace datrw {

  namespace formatmodifiers {

    /*----------------------------------------------------------------------*/

    Tparamap makeparamap(const std::string& p,
                         const std::string& delimiter,
                         const std::string& assign)
    {
      std::string parameters=p;
      Tparamap retval;
      while (parameters.length()>0)
      {
        std::string value=datrw::util::clipstring(parameters, delimiter);
        std::string key
          =datrw::util::trimws(datrw::util::clipstring(value, assign));
        if (value.length()==0)
        {
          retval[key]=Value("true");
        }
        else
        {
          retval[key]=Value(value);
        }
      } // while (parameters.length()>0)
      return retval;
    } // Tparamap makeparamap()

    /*----------------------------------------------------------------------*/

    void online_help(std::ostream& os)
    {
      os << "format modifiers" << std::endl;
      os << "----------------" << std::endl;
      os << 
        "Some I/O streams support format modifiers. These can be used to\n"
        "control the behaviour of the specific I/O stream. The modifiers\n"
        "are passed through a string to the I/O stream. Each modifier\n"
        "consists of a keyword and an optional parameter string. Several\n"
        "modifiers can be concatenated being separated by a colon (:).\n"
        "Keyword and parameter are separated by an equal sign (=).\n"
        "If the program takes format identifiers as string values,\n"
        "format modifiers usually can be appended to the format ID with\n"
        "a separating colon in between.\n"
        "\n"
        "Example: To read a seife format file, using 4.7.2011 as the day\n"
        "of recording and \"BFO\" as recording station, you could use\n"
        "\n"
        "  seife:date=2011/7/4:station=BFO\n"
        "\n"
        "to specify the input format.";
      os << std::endl;
    } // void online_help(std::ostream& os)

  } // namespace formatmodifiers

  /*======================================================================*/
  // class Subformat

  std::istringstream& 
    Subformat::operator()(const std::string& k,
                          const std::string& def) const
    {
      Mstream.clear();
      Mstream.str(util::commatospace(this->value(k, def)));
      return(Mstream);
    } // std::istringstream& 
      //   Subformat::operator()(const std::string& k,
      //                         const std::string& def) const

  /*----------------------------------------------------------------------*/

  std::string
    Subformat::value(const std::string& k,
                     const std::string& def) const
    {
      std::string retval;
      formatmodifiers::Tparamap::const_iterator P=Mparamap.find(k);
      if (P!=Mparamap.end())
      {
        retval=P->second.value;
        P->second.checked=true;
      }
      else
      {
        retval=def;
      }
      return retval;
    } // std::istringstream& 
      //   Subformat::value(const std::string& k,
      //                    const std::string& def) const


  /*----------------------------------------------------------------------*/

  bool Subformat::isset(const std::string& k) const
  {
    formatmodifiers::Tparamap::const_iterator P=Mparamap.find(k);
    bool retval=(P!=Mparamap.end());
    if (retval)
    {
      P->second.checked=true;
    }
    return(retval);
  } // bool Subformat::isset(const std::string& k) const

  /*----------------------------------------------------------------------*/

  bool Subformat::allarechecked() const
  {
    bool retval=true;
    formatmodifiers::Tparamap::const_iterator I=Mparamap.begin(); 
    while (I!=Mparamap.end())
    {
      if (!I->second.checked) { retval=false; }
      ++I;
    }
    return(retval);
  } // bool Subformat::allarechecked() const

  /*----------------------------------------------------------------------*/

  void Subformat::notchecked(std::ostream& os) const
  {
    if (!this->allarechecked())
    {
      formatmodifiers::Tparamap::const_iterator I=Mparamap.begin(); 
      os << "Format modifiers which were not recognized:" << std::endl;
      while (I!=Mparamap.end())
      {
        if (!I->second.checked) 
        { 
          os << "  " << I->first << "=" << I->second.value << std::endl;
        }
        ++I;
      }
    }
  } // void Subformat::notchecked(std::ostream& os) const

  /*======================================================================*/

  namespace formatmodifiers {

    std::ostream& ModifierHelp::operator() ()
    {
      Mos.width(Mwidth);
      Mos << std::left << std::setfill(' ') << " " << " ";
      return(Mos);
    } // std::ostream& ModifierHelp::operator() ()

    /*----------------------------------------------------------------------*/

    std::ostream& ModifierHelp::operator() (const std::string& key)
    {
      Mos.width(Mwidth);
      std::string keyval=key;
      keyval += ": ";
      Mos << std::left << std::setfill('.') << keyval << " ";
      return(Mos);
    } // std::ostream& ModifierHelp::operator() (key)

    /*----------------------------------------------------------------------*/

    std::ostream& ModifierHelp::operator() (const std::string& key,
                                            const std::string& val)
    {
      Mos.width(Mwidth);
      std::string keyval=key;
      keyval += "=" + val + ": ";
      Mos << std::left << std::setfill('.') << keyval << " ";
      return(Mos);
    } // std::ostream& ModifierHelp::operator() (key, val)

  } // namespace formatmodifiers

} // namespace datrw

/* ----- END OF formatmodifier.cc ----- */
