/*! \file commandline.cc
 * \brief Implementation of class commandline.
 *
 * ----------------------------------------------------------------------------
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
 *  - 14/11/2002   V1.2   
 *                        - copied from libclass
 *                        - changed name and namespace
 *  - 19/11/2002   V1.3   Tresult --> Result
 *  - 19/07/2005   V1.4   use the nice TFXX_abort()
 *
 * ============================================================================
 */
 
#include <iostream>
#include <getopt.h>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
 
namespace tfxx {
namespace cmdline {
 
  //! create and parse
  Commandline::Commandline(int& iargc, 
                           char** argv, 
                           Declare* declarations):
    Miargc(iargc), Margv(argv)
  {
//    cout << "build getopt array" << endl;
//    cout << declarations[0].opt_string << endl;
//    cout << declarations[1].opt_string << endl;
    
    // count the number of options by looking for the last one
    // which has to have a NULL pointer in the name
    Mnopt=0;
//    cout << "count options" << endl;
    while (declarations[Mnopt].opt_string != NULL) { Mnopt++; }
//    cout << Mnopt << " options found" << endl;

    // allocate temporary array long_optins for function
    // getopt_long_only
    struct option* long_options=new struct option[Mnopt];
    // allocate result array
    Mresult=new Result[Mnopt];

//    cout << "fill getopt array" << endl;
    // fill array long_options with required values
    // and Mresult with default values
    int iopt=0;
    while (declarations[iopt].opt_string != NULL) 
    {
      long_options[iopt].name    =declarations[iopt].opt_string;
      long_options[iopt].has_arg =declarations[iopt].opt_arg_mode;
      long_options[iopt].flag    =NULL;
      long_options[iopt].val     =iopt;
      Mresult[iopt].opt_set      =false;
      Mresult[iopt].arg          =std::string(declarations[iopt].arg_default);
      iopt++;
    }

//    cout << "evaluate getopt" << endl;
    // evaluate command line by calling getopt_long_only
    int c;
    while (1)
    {
      iopt=0;
      c=getopt_long_only(iargc, argv, "", long_options, &iopt);
//      cout << "returned " << c << " and " << iopt << endl;

      // finish on return value -1
      if (c == -1) break;

      // check for error condition
      if ((c < 0) || (c >= Mnopt))
      {
        std::cerr << "Commandline: ERROR on option '" 
             << long_options[iopt].name << "'" << std::endl;
        TFXX_abort("bailing out...");
      }

      // store results
      Mresult[iopt].opt_set = true;
      if (optarg) Mresult[iopt].arg=optarg;
//      cout << "** option '" << long_options[iopt].name <<
//              "' with argument " << Mresult[iopt].arg << endl;
    };
   
    // remember local optind (for later scan)
    Moptind=optind;

    // remove temporaray array
    delete[] long_options;
//    cout << "initialized" << endl;
  }

} // namespace cmdline
} // namespace tf
 
/* ----- END OF commandline.cc ----- */
