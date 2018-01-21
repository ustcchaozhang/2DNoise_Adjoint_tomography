/*! \file commandline.h
 * \brief Interface of class commandline.
 *
 * \addtogroup commandline_h 
 * ----------------------------------------------------------------------------
 *
 * \date 12/01/2002
 * \author Thomas Forbriger
 *
 * Copyright (c) 2002 by Thomas Forbriger (IMGF Frankfurt)
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
 *  - 12/01/2002   V1.0   Thomas Forbriger
 *  - 08/05/2002   V1.1   changed char* types in Tdeclare to const char*
 *                        since they are initialized from literal constants
 *  - 14/11/2002   V1.2   copied from libclass
 *  - 19/11/2002   V1.3   Tresult --> Result
 *  - 19/07/2005   V1.4   use the nice TFXX_abort()
 *  - 11/11/2009   V1.5   header cstdlib is required for atoi and friends
 *
 * ============================================================================
 */
 
#ifndef TF_COMMANDLINE_H_VERSION

#define TF_COMMANDLINE_H_VERSION \
  "TF_COMMANDLINE_H   V1.5"

#include<tfxx/error.h>
#include<iostream>
#include<string>
#include<cstdlib>

/*! \defgroup group_commandline Commandline options and arguments.
 * \brief Evaluate options and arguments given on the command line.
 *
 * The module is presented in namespace tfxx::cmdline.
 * You will find an example in tests/commandlinetest.cc.
 *
 * \sa tfxx::cmdline::Commandline
 * \sa tfxx::cmdline
 * \sa example_commandlinetest
 */

/*! \brief Interface provided through commandline.h
 *
 * \defgroup commandline_h Interface provided through commandline.h
 *
 * The main classes providing this interface are
 * - tfxx::cmdline::Commandline
 * - tfxx::cmdline::Declare
 *
 * A tutorial for their use will be given there.
 *
 * \sa example_commandlinetest
 * 
 * \ingroup group_commandline
 */
 
namespace tfxx {

/*! \namespace tfxx::cmdline
 * \brief Namespace containing all components of module commandline.
 * \ingroup group_commandline, commandline_h
 * \sa example_commandlinetest
 * \sa group_commandline
 * \sa tfxx::cmdline::Commandline
 */
namespace cmdline { 

  /*! \brief option modes
   * \ingroup group_commandline, commandline_h
   *
   * The following enums are used to define whether an option has or has not
   * an argument. It is used within Declare. It tells Commandline whether it
   * has to look for arguments to the option.
   *
   * \sa tfxx::cmdline::Commandline
   * \sa tfxx::cmdline::Declare
   */
  enum Eopt_arg_mode 
  {
    //! option has no argument
    arg_no=0, 
    //! option requires an argument
    arg_yes=1, 
    //! option may have an optional argument
    arg_opt=2 
  };

  /*!
   * \brief struct to define options
   * \ingroup group_commandline, commandline_h
   *
   * This struct is used to define a list of options. An example is:
   *
   * \code
   *   // define commandline options
   *   using namespace tf::cmdline;
   *   static Declare options[]=
   *   {
   *     // 0: print help
   *     {"help",arg_no,"-"},
   *     // 1: verbose mode
   *     {"v",arg_no,"-"},
   *     // 2: output device
   *     {"d",arg_yes,"x11"},
   *     {NULL}
   *   };
   * \endcode
   *
   * It defines the options \c -help, \c -v, and \c -d, where the latter does
   * take an argument and the first two not. The array \c options[] is passed
   * to tfxx::cmdline::Commandline. The first entry in each Declare defines
   * the option anme, the second the argument mode and the third the default
   * argument.
   *
   * \sa tfxx::cmdline::Commandline
   */
  struct Declare {
    //! option name
    const char* opt_string;
    //! option argument mode
    Eopt_arg_mode opt_arg_mode;
    //! option default argument
    const char* arg_default;
  };

/*! \class Commandline
 * \brief Evaluates commandline by calling \c long_getopt.
 * \ingroup group_commandline, commandline_h
 *
 * You may instantiate a Commandline object by passing the C-variables \c
 * iargc and \c argv (which are passed to \c main) and a Declare-list to the
 * constructor. The constructor itself will call the function \c long_getopt
 * to evaluate the command line and stores the results.
 *
 * Option settings and options arguments (in various types) may be received
 * through query functions. Two other query functions allow reading of the
 * rest of the commandline arguments (filenames etc.).
 *
 * Usage example:
 * \code
 *  // collect options from commandline
 *  tfxx::cmdline::Commandline cmdline(iargc, argv, options);
 *
 *  // help requested? print full help text...
 *  if (cmdline.optset(0))
 *  {
 *    cerr << usage_text << endl;
 *    cerr << help_text << endl;
 *    exit(0);
 *  }
 *
 *  // dummy operation: print option settings
 *  for (int iopt=0; iopt<3; iopt++)
 *  {
 *    cout << "option: '" << options[iopt].opt_string << "'" << endl;
 *    if (cmdline.optset(iopt)) {  cout << "  option was set"; }
 *    else { cout << "option was not set"; }
 *    cout << endl;
 *    cout << "  argument (string): '" << cmdline.string_arg(iopt) << "'" << endl;
 *    cout << "     argument (int): '" << cmdline.int_arg(iopt) << "'" << endl;
 *    cout << "    argument (long): '" << cmdline.long_arg(iopt) << "'" << endl;
 *    cout << "   argument (float): '" << cmdline.float_arg(iopt) << "'" << endl;
 *    cout << "  argument (double): '" << cmdline.double_arg(iopt) << "'" << endl;
 *    cout << "    argument (bool): '";
 *    if (cmdline.bool_arg(iopt))
 *    { cout << "true"; } else { cout << "false"; }
 *    cout << "'" << endl;
 *  }
 *
 *  // dummy operation: print rest of command line
 *  while (cmdline.extra()) { cout << cmdline.next() << endl; }
 * \endcode
 *
 * Consult the documentation of tfxx::cmdline::Declare on the construction of
 * the \c options array.
 *
 * \sa tfxx::cmdline::Declare
 * \sa example_commandlinetest
 * \sa group_commandline
 * \sa tfxx::cmdline
 */
class Commandline {
  public:
    //! Constructor evaluates commandline by calling \c long_getopt.
    Commandline(int& iargc, char** argv, Declare* declarations);
    //! Destructor has to remove result array.
    ~Commandline() { delete[] Mresult; }

    /*! \name Option argument query functions.
     * \brief Functions to query commandline option information.
     */
    //@{
    //! true if option # \c iopt was set on commandline
    bool optset(const int& iopt) const
    { check_index(iopt); return(Mresult[iopt].opt_set); }
    //! return argument of option # \c iopt as \c string value
    std::string string_arg(const int& iopt) const
    { check_index(iopt); return(Mresult[iopt].arg); }
    //! return argument of option # \c iopt as \c int value
    int int_arg(const int& iopt) const
    { check_index(iopt); return(atoi(Mresult[iopt].arg.c_str())); }
    //! return argument of option # \c iopt as \c float value
    float float_arg(const int& iopt) const
    { check_index(iopt); return(float(atof(Mresult[iopt].arg.c_str()))); }
    //! return argument of option # \c iopt as \c double value
    double double_arg(const int& iopt) const
    { check_index(iopt); return(atof(Mresult[iopt].arg.c_str())); }
    //! return argument of option # \c iopt as \c long value
    long long_arg(const int& iopt) const
    { check_index(iopt); return(atol(Mresult[iopt].arg.c_str())); }
    //! return argument of option # \c iopt as \c bool value
    bool bool_arg(const int& iopt) const
    { check_index(iopt); return(bool(atoi(Mresult[iopt].arg.c_str()))); }
    //@}

    /*! \name Commandline argument query functions.
     * \brief Functions to query extra commandline arguments.
     */
    //@{
    //! true if there are more commandline arguments
    bool extra() const
    { return(Moptind<Miargc); }
    //! returns char-array of next commandline argument
    char* next()
    { if (Moptind<Miargc) {return(Margv[Moptind++]);} else {return(NULL);} }
    //@}
  private:
    //! Check requested index before query.
    void check_index(const int& iopt) const
    {
      if ((iopt < 0) || (iopt >= Mnopt))
      {
        std::cerr << "Commandline: illegal option index '" 
             << iopt << "' on query" << std::endl;
        TFXX_abort("bailing out...");
      }
    }

    /*! \brief Structure to store result of command line scanning.
     *
     * This is privately contained (nested class) in
     * tfxx::cmdline::Commandline.
     */
    struct Result {
      //! true if option was used on command line
      bool opt_set;
      //! holds option argument
      std::string arg;
    };

    //! Array of command line contents
    Result* Mresult; 
    //! Total number of options evaluated ot Mresult.
    int Mnopt;
    //! Number of command line strings (as passed to main)
    int Miargc;
    //! Pointer to array of command line strings (as passed to main)
    char** Margv;
    //! Next option string to read after last defined option.
    int Moptind;
};

/*! \example tests/commandlinetest.cc
 * \ingroup group_commandline, commandline_h
 *
 * You will find and example for the module commandline in
 * tests/commandlinetest.cc.
 *
 * \sa tfxx::cmdline::Commandline
 * \sa tfxx::cmdline::Declare
 * \sa tfxx::cmdline
 * \sa group_commandline
 * \sa example_commandlinetest
 */

} // namespace cmdline
 
} // namespace tfxx
 
#endif // TF_COMMANDLINE_H_
 
/* ----- END OF commandline.h ----- */
