/*! \file commandlinetest.cc
 * \brief test commandline class
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 15/11/2002
 * 
 * test commandline class
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 15/11/2002   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define COMMANDLINETEST_VERSION \
  "COMMANDLINETEST   V1.0   test commandline class"

#include <iostream>
#include <tfxx/commandline.h>
#include <tfxx/xcmdline.h>

using std::cout;
using std::cerr;
using std::endl;

/*! \defgroup example_commandlinetest TEST: Commandline evaluation.
 */

/*! \brief Program to test class tfxx::comdline::Commandline.
 * \ingroup example_commandlinetest
 * \sa tfxx::cmdline::Commandline
 * \sa tfxx::cmdline
 * \sa group_commandline
 */
int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    COMMANDLINETEST_VERSION "\n"
    "usage: commandlinetest argument [-v] [-o n] [-f name] [-q [r]]" "\n"
    "                       [-longopt f] [-F] [value [value ...]]" "\n"
    "   or: commandlinetest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "argument     one argument is mandatory" "\n"
    "value        additional arguments (optional)" "\n"
    " " "\n"
    "options:" "\n"
    "-v           set a flag (no argument is allowed)" "\n"
    "-o n         set an integer value (argument mandatory)" "\n"
    "-f name      set a string value (argument mandatory)" "\n"
    "-q r         set an integer value (argument optional)" "\n"
    "-longopt f   set the value for a long option name" "\n"
    "-F           evaluate file name options" "\n"
    "             availabe keys are: \"f\", \"dl\", \"k\"" "\n"
    " " "\n"
    "Long options may be abbreviated - test it!" "\n"
    " " "\n"
    "Notice that you have to use the syntax" "\n"
    " " "\n"
    "  -q=argument" "\n"
    " " "\n"
    "with the optional argument for option -q. This syntax" "\n"
    "is mandatory with optional arguments, but may also be used" "\n"
    "with mandatory arguments." "\n"
    " " "\n"
    COMMANDLINETEST_CVSID
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: verbose mode
    {"o",arg_yes,"45"},
    // 3: verbose mode
    {"f",arg_yes,"a_string"},
    // 4: verbose mode
    {"q",arg_opt,"10.5"},
    // 5: verbose mode
    {"longopt",arg_yes,"default"},
    // 6: evaluate filename options
    {"F",arg_no,"-"},
    {NULL}
  };

  // define filename options
  static const char* keys[]={"f", "dl", "k", 0};

  // no arguments? print usage...
  if (iargc<2) 
  {
    cerr << usage_text << endl;
    exit(0);
  }

  // collect options from commandline
  tfxx::cmdline::Commandline cmdline(iargc, argv, options);

  // help requested? print full help text...
  if (cmdline.optset(0))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    exit(0);
  }

  cout << "Here I tell you about the options you supplied:" << endl;
  // dummy operation: print option settings
  for (int iopt=0; iopt<6; iopt++)
  {
    cout << endl;
    cout << "option: '" << options[iopt].opt_string << "'" << endl;
    if (cmdline.optset(iopt)) {  cout << "    option was set"; }
    else { cout << "  option was not set"; }
    cout << endl;
    cout << "    argument (string): '" 
      << cmdline.string_arg(iopt) << "'" << endl;
    cout << "       argument (int): '" 
      << cmdline.int_arg(iopt) << "'" << endl;
    cout << "      argument (long): '" 
      << cmdline.long_arg(iopt) << "'" << endl;
    cout << "     argument (float): '" 
      << cmdline.float_arg(iopt) << "'" << endl;
    cout << "    argument (double): '" 
      << cmdline.double_arg(iopt) << "'" << endl;
    cout << "      argument (bool): '";
    if (cmdline.bool_arg(iopt))
    { cout << "true"; } else { cout << "false"; }
    cout << "'" << endl;
  }

  // print options in the expected way
  cout << endl;
  cout << "evaluating your choice:" << endl;
  cout << "  verbose mode switched ";
  if (cmdline.optset(1)) { cout << "ON"; } else { cout << "OFF"; } 
  cout << endl; 
  if (cmdline.optset(2)) { cout << "      set:"; } 
  else {  cout << "  default:"; }
    cout << "       -o: '" << cmdline.int_arg(2) << "'" << endl;
  if (cmdline.optset(3)) { cout << "      set:"; } 
  else {  cout << "  default:"; }
    cout << "       -f: '" << cmdline.string_arg(3) << "'" << endl;
  if (cmdline.optset(4)) { cout << "      set:"; } 
  else {  cout << "  default:"; }
    cout << "       -q: '" << cmdline.float_arg(4) << "'" << endl;
  if (cmdline.optset(5)) { cout << "      set:"; } 
  else {  cout << "  default:"; }
    cout << " -longopt: '" << cmdline.string_arg(5) << "'" << endl;

  if (!cmdline.optset(6))
  {
    // dummy operation: print rest of command line
    cout << endl;
    cout << "And here is the rest of your command line arguments:" << endl;
    while (cmdline.extra()) { cout << cmdline.next() << endl; }
  }
  else
  {
    cout << endl;
    cout << "Parse file names and options" << endl;
    cout << "Supported options keys are:" << endl;
    const char** thekeys=keys;
    const char* key=*thekeys;
    while (key != NULL)
    {
      std::cout << "  next key is: \"" << key << "\"" << std::endl;
      ++thekeys;
      key=*thekeys;
    }
    Tparsed result=parse_cmdline(cmdline, keys, cmdline.optset(1));
    Tparsed::const_iterator i=result.begin();
    while(i!=result.end())
    {
      std::cout << "filename: \"" << i->name << "\"" << std::endl;
      Toptionmap::const_iterator j=i->options.begin();
      while(j!=i->options.end())
      {
        std::cout << "  " << j->first 
          << " \"" << j->second << "\"" << std::endl;
        ++j;
      }
      const char ** k=keys;
      while (*k != NULL)
      {
        if (i->haskey(*k))
        {
          std::cout << "  key " << *k << ": found " << i->count(*k)
            << " entries" << std::endl;
          std::cout << "    first entry is \"" << i->value(*k) <<
            "\"" << std::endl;
          Toptionmap e=i->extract(*k);
          std::cout << "    all entries are:" << std::endl;
          Toptionmap::const_iterator p=e.begin();
          while(p != e.end())
          {
            std::cout << "      \"" << p->second << "\"" << std::endl;
            ++p;
          }
        }
        ++k;
        key=*thekeys;
      }
      // next filename
      ++i;
    }
  }
}

/* ----- END OF commandlinetest.cc ----- */
