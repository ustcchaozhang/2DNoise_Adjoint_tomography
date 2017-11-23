/*! \file xcmdline.cc
 * \brief parse extra commandline values (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/06/2005
 * 
 * parse extra commandline values (implementation)
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
#define TF_XCMDLINE_CC_VERSION \
  "TF_XCMDLINE_CC   V1.0   "

#include <tfxx/xcmdline.h>

namespace tfxx {
  namespace cmdline {

    namespace helper {

      typedef std::list<std::string> Tkeylist;

      class CmdlineArgument {
        public:
          CmdlineArgument(const std::string& arg, 
                          const Tkeylist& keys);
          bool isoption() const { return(Misoption); }
          std::string value() const { return(Mvalue); }
          Tfileoption option() const { return(Moption); }
        private:
          std::string Mvalue;
          Tfileoption Moption;
          bool Misoption;
      }; // class CmdlineArgument

      CmdlineArgument::CmdlineArgument(const std::string& arg,
                                       const Tkeylist& keys)
      {
        Mvalue=arg;
        Misoption=false;
        helper::Tkeylist::const_iterator i=keys.begin();
        // std::cout << Mvalue << std::endl;
        while ((i!=keys.end()) && (!Misoption))
        {
          // std::cout << *i << " " << Mvalue.find(*i) << std::endl;
          if (Mvalue.find(*i) == 0)
          {
            Misoption=true;
            Moption.first = i->substr(0, i->size()-1);
            Moption.second= Mvalue.substr(Moption.first.size()+1);
          }
          ++i;
        }
      } // CmdlineArgument constructor

    } // namespace helper

    /*----------------------------------------------------------------------*/

    Tparsed parse_cmdline(tfxx::cmdline::Commandline& c,
                          const char** keys, 
                          const bool& debug)
    {
      // fill a search list
      // ------------------
      helper::Tkeylist keylist;
      const char* key=*keys;
      while (key != NULL)
      {
        std::string keystring(key);
        keylist.push_back(keystring+":");
        ++keys;
        key=*keys;
      }
      // report search list if requested
      if (debug)
      {
        std::cout << "DEBUG (parse_cmdline): keys are ";
        helper::Tkeylist::const_iterator i=keylist.begin();
        while (i!=keylist.end())
        {
          std::cout << "\"" << *i << "\" ";
          ++i;
        }
        std::cout << std::endl;
      }

      // parse rest of command line
      // --------------------------
      Tparsed retval;
      Filename filename;
      bool first=true;
      while (c.extra())
      {
        helper::CmdlineArgument ca(c.next(), keylist);
        if (first)
        {
          first=false;
          filename.name=ca.value();
        }
        else
        {
          if (ca.isoption())
          {
            filename.options.insert(ca.option());
          }
          else
          {
            retval.push_back(filename);
            filename.options.clear();
            filename.name=ca.value();
          }
        }
      }
      if (!first) { retval.push_back(filename); }
      
      return(retval);
    } // parse_cmdline

    /*======================================================================*/

    Toptionmap Filename::extract(const std::string& key) const
    {
      typedef Toptionmap::const_iterator Tomi;
      typedef std::pair<Tomi, Tomi> Tomipair;
      Tomipair equalrange=options.equal_range(key);
      Toptionmap retval(equalrange.first, equalrange.second);
      return retval;
    }

    /*----------------------------------------------------------------------*/

    std::string Filename::value(const std::string& key) const
    {
      std::string retval;
      Toptionmap::const_iterator p=options.find(key);
      if (p != options.end())
      { retval=p->second; }
      return retval;
    } // Filename::value

  } // namespace cmdline

} // namespace tfxx

/* ----- END OF xcmdline.cc ----- */
