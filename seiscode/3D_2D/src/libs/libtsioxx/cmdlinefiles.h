/*! \file cmdlinefiles.h
 * \brief containers for data files with file specific command line parameters
 *        (prototypes).
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 31/01/2007
 * \date 30/01/2014
 * 
 * Copyright (c) 2007, 2010, 2011, 2014 by Thomas Forbriger (BFO Schiltach) 
 * 
 * containers for data files with file specific command line parameters
 * (prototypes)
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
 *  - 30/01/2014   V1.0   Thomas Forbriger (thof):
 *                        copied from readtsdata.h
 * 
 * ============================================================================
 */

// include guard
#ifndef TSIO_CMDLINEFILES_H_VERSION

#define TSIO_CMDLINEFILES_H_VERSION \
  "TF_CMDLINEFILES_H   2014/01/30"

#include<tsioxx/sfftsfile.h>
#include<tfxx/xcmdline.h>
#include<datrwxx/readany.h>

namespace ts {

  namespace sff {

    /*! \brief Full commandline data file parsing
     * \defgroup group_cmdlinefiles Full commandline data file parsing
     *
     * This module is presented through cmdlinefiles.h
     *
     * @{
     */

    /*! \brief type of trace selection list.
     *
     * This is the rangelist class that holds a selection of traces in
     * numerical form. It can be generated generated from an approriate string
     * on the command line through:
     * \code
     * Ttracelist selection=
     *   tfxx::string::rangelist<Ttracelist::Tvalue>(filename.value(tracekey));
     * \endcode
     */
    typedef tfxx::RangeList<int> Ttracelist;

    /*! \brief type of trace selection key.
     *
     * This typedef is given here to make the argument definitions more
     * verbose.
     */
    typedef const char* Ttracelistkey;


    /*! \brief structure to contain file data together with command line
     * arguments (single precision).
     *
     * A struct to hold a complete data file including
     * -# command line arguments associated with the file
     * -# the file header
     * -# headers and samples from all selected traces
     *
     * This specific struct will hold samples in form of doubles.
     */
    struct SFile {
      //! provides reading into doubles
      typedef float Tvalue;
      //! container to be used
      typedef aff::Series<Tvalue> Tseries;
      //! file container
      typedef ts::sff::File<Tseries> Tfile;
      //! command line arguments
      tfxx::cmdline::Filename arguments;
      //! container to hold data headers and samples
      ts::sff::File<Tseries> data;
    }; // struct SFile

    /*! \brief structure to contain file data together with command line
     * arguments.
     *
     * A struct to hold a complete data file including
     * -# command line arguments associated with the file
     * -# the file header
     * -# headers and samples from all selected traces
     *
     * This specific struct will hold samples in form of doubles.
     */
    struct DFile {
      //! provides reading into doubles
      typedef double Tvalue;
      //! container to be used
      typedef aff::Series<Tvalue> Tseries;
      //! file container
      typedef ts::sff::File<Tseries> Tfile;
      //! command line arguments
      tfxx::cmdline::Filename arguments;
      //! container to hold data headers and samples
      ts::sff::File<Tseries> data;
    }; // struct DFile

    /*! \brief complete list of files.
     *
     * A list of DFile structs, to store all data files provided on the
     * command line at once.
     */
    typedef std::list<DFile> TDFileList;

    /*! \brief complete list of files (single precision version).
     *
     * A list of DFile structs, to store all data files provided on the
     * command line at once.
     */
    typedef std::list<SFile> TSFileList;
    
    /*======================================================================*/

    /*! \brief read complete SFF file.
     *
     * Read a complete data file with headers and samples at once.
     *
     * \return structure that contains file data together with command line
     *    arguments
     * \param filename input file name of file to be read
     * \param verbose select verbosity
     * \param format data format to be read
     */
    DFile readDSFF(const std::string& filename,
                   const bool& verbose=false,
                   const std::string& format=datrw::anyID(datrw::Fsff));

    /*----------------------------------------------------------------------*/

    /*! \brief read complete SFF file with trace selection.
     *
     * Read a complete data file with all headers and samples but only for the
     * selected traces.
     *
     * \return structure that contains file data together with command line
     *    arguments (here: only file name)
     * \param filename input file name of file to be read
     * \param selection numerical list of selected traces
     * \param verbose select verbosity
     * \param format data format to be read
     */
    DFile readDSFF(const std::string& filename,
                   const Ttracelist& selection,
                   const bool& verbose=false,
                   const std::string& format=datrw::anyID(datrw::Fsff));

    /*----------------------------------------------------------------------*/

    /*! \brief read complete SFF file with trace selection.
     *
     * Read a complete data file, but take selection from the structure of
     * command line arguments.
     *
     * \return structure that contains file data together with command line
     *    arguments (here: only file name)
     * \param filename input file name together with command line options and
     *    arguments
     * \param verbose select verbosity
     * \param tracekey key string for command line option that defines trace
     *   selection
     * \param format data format to be read
     */
    DFile readDSFF(const tfxx::cmdline::Filename& filename,
                   const bool& verbose=false,
                   Ttracelistkey tracekey="t",
                   const std::string& format=datrw::anyID(datrw::Fsff));

    /*----------------------------------------------------------------------*/

    /*! \brief read complete list of SFF files with trace selection.
     *
     * Read a complete set of files like defined in the structure of command
     * line arguments.
     *
     * \return structure that contains file data together with command line
     *    arguments
     * \param flist list of file names together with
     *   command line options and arguments
     * \param verbose select verbosity
     * \param tracekey key string for command line option that defines trace
     *   selection
     * \param format data format to be read
     */
    TDFileList readDSFF(const tfxx::cmdline::Tparsed& flist,
                        const bool& verbose=false,
                        Ttracelistkey tracekey="t",
                        const std::string& format=datrw::anyID(datrw::Fsff));
    
    /*----------------------------------------------------------------------*/

    /*! \brief read complete SFF file (single precision version).
     *
     * Read a complete data file with headers and samples at once.
     *
     * \return structure that contains file data together with command line
     *    arguments
     * \param filename input file name of file to be read
     * \param verbose select verbosity
     * \param format data format to be read
     */
    SFile readSSFF(const std::string& filename,
                   const bool& verbose=false,
                   const std::string& format=datrw::anyID(datrw::Fsff));

    /*----------------------------------------------------------------------*/

    /*! \brief read complete SFF file with trace selection (single precision
     * version).
     *
     * Read a complete data file with all headers and samples but only for the
     * selected traces.
     *
     * \return structure that contains file data together with command line
     *    arguments (here: only file name)
     * \param filename input file name of file to be read
     * \param selection numerical list of selected traces
     * \param verbose select verbosity
     * \param format data format to be read
     */
    SFile readSSFF(const std::string& filename,
                   const Ttracelist& selection,
                   const bool& verbose=false,
                   const std::string& format=datrw::anyID(datrw::Fsff));

    /*----------------------------------------------------------------------*/

    /*! \brief read complete SFF file with trace selection (single precision
     * version).
     *
     * Read a complete data file, but take selection from the structure of
     * command line arguments.
     *
     * \return structure that contains file data together with command line
     *    arguments (here: only file name)
     * \param filename input file name together with command line options and
     *    arguments
     * \param verbose select verbosity
     * \param tracekey key string for command line option that defines trace
     *   selection
     * \param format data format to be read
     */
    SFile readSSFF(const tfxx::cmdline::Filename& filename,
                   const bool& verbose=false,
                   Ttracelistkey tracekey="t",
                   const std::string& format=datrw::anyID(datrw::Fsff));

    /*----------------------------------------------------------------------*/

    /*! \brief read complete list of SFF files with trace selection (single
     * precision version).
     *
     * Read a complete set of files like defined in the structure of command
     * line arguments.
     *
     * \return structure that contains file data together with command line
     *    arguments
     * \param flist list of file names together with
     *   command line options and arguments
     * \param verbose select verbosity
     * \param tracekey key string for command line option that defines trace
     *   selection
     * \param format data format to be read
     */
    TSFileList readSSFF(const tfxx::cmdline::Tparsed& flist,
                        const bool& verbose=false,
                        Ttracelistkey tracekey="t",
                        const std::string& format=datrw::anyID(datrw::Fsff));

    /*======================================================================*/

    /*! \brief read complete SFF file.
     *
     * Read a complete data file with headers and samples at once.
     *
     * \return structure that contains file data together with command line
     *    arguments
     * \param filename input file name of file to be read
     * \param verbose select verbosity
     * \param format data format to be read
     */
    DFile readDSFF(const std::string& filename,
                   const bool& verbose=false,
                   const datrw::Eformat& format=datrw::Fsff);

    /*----------------------------------------------------------------------*/

    /*! \brief read complete SFF file with trace selection.
     *
     * Read a complete data file with all headers and samples but only for the
     * selected traces.
     *
     * \return structure that contains file data together with command line
     *    arguments (here: only file name)
     * \param filename input file name of file to be read
     * \param selection numerical list of selected traces
     * \param verbose select verbosity
     * \param format data format to be read
     */
    DFile readDSFF(const std::string& filename,
                   const Ttracelist& selection,
                   const bool& verbose=false,
                   const datrw::Eformat& format=datrw::Fsff);

    /*----------------------------------------------------------------------*/

    /*! \brief read complete SFF file with trace selection.
     *
     * Read a complete data file, but take selection from the structure of
     * command line arguments.
     *
     * \return structure that contains file data together with command line
     *    arguments (here: only file name)
     * \param filename input file name together with command line options and
     *    arguments
     * \param verbose select verbosity
     * \param tracekey key string for command line option that defines trace
     *   selection
     * \param format data format to be read
     */
    DFile readDSFF(const tfxx::cmdline::Filename& filename,
                   const bool& verbose=false,
                   Ttracelistkey tracekey="t",
                   const datrw::Eformat& format=datrw::Fsff);

    /*----------------------------------------------------------------------*/

    /*! \brief read complete list of SFF files with trace selection.
     *
     * Read a complete set of files like defined in the structure of command
     * line arguments.
     *
     * \return structure that contains file data together with command line
     *    arguments
     * \param flist list of file names together with
     *   command line options and arguments
     * \param verbose select verbosity
     * \param tracekey key string for command line option that defines trace
     *   selection
     * \param format data format to be read
     */
    TDFileList readDSFF(const tfxx::cmdline::Tparsed& flist,
                        const bool& verbose=false,
                        Ttracelistkey tracekey="t",
                        const datrw::Eformat& format=datrw::Fsff);
    
    /*----------------------------------------------------------------------*/

    /*! \brief read complete SFF file (single precision version).
     *
     * Read a complete data file with headers and samples at once.
     *
     * \return structure that contains file data together with command line
     *    arguments
     * \param filename input file name of file to be read
     * \param verbose select verbosity
     * \param format data format to be read
     */
    SFile readSSFF(const std::string& filename,
                   const bool& verbose=false,
                   const datrw::Eformat& format=datrw::Fsff);

    /*----------------------------------------------------------------------*/

    /*! \brief read complete SFF file with trace selection (single precision
     * version).
     *
     * Read a complete data file with all headers and samples but only for the
     * selected traces.
     *
     * \return structure that contains file data together with command line
     *    arguments (here: only file name)
     * \param filename input file name of file to be read
     * \param selection numerical list of selected traces
     * \param verbose select verbosity
     * \param format data format to be read
     */
    SFile readSSFF(const std::string& filename,
                   const Ttracelist& selection,
                   const bool& verbose=false,
                   const datrw::Eformat& format=datrw::Fsff);

    /*----------------------------------------------------------------------*/

    /*! \brief read complete SFF file with trace selection (single precision
     * version).
     *
     * Read a complete data file, but take selection from the structure of
     * command line arguments.
     *
     * \return structure that contains file data together with command line
     *    arguments (here: only file name)
     * \param filename input file name together with command line options and
     *    arguments
     * \param verbose select verbosity
     * \param tracekey key string for command line option that defines trace
     *   selection
     * \param format data format to be read
     */
    SFile readSSFF(const tfxx::cmdline::Filename& filename,
                   const bool& verbose=false,
                   Ttracelistkey tracekey="t",
                   const datrw::Eformat& format=datrw::Fsff);

    /*----------------------------------------------------------------------*/

    /*! \brief read complete list of SFF files with trace selection (single
     * precision version).
     *
     * Read a complete set of files like defined in the structure of command
     * line arguments.
     *
     * \return structure that contains file data together with command line
     *    arguments
     * \param flist list of file names together with
     *   command line options and arguments
     * \param verbose select verbosity
     * \param tracekey key string for command line option that defines trace
     *   selection
     * \param format data format to be read
     */
    TSFileList readSSFF(const tfxx::cmdline::Tparsed& flist,
                        const bool& verbose=false,
                        Ttracelistkey tracekey="t",
                        const datrw::Eformat& format=datrw::Fsff);

  } // namespace sff
} // namespace ts

#endif // TSIO_CMDLINEFILES_H_VERSION (includeguard)

/* ----- END OF cmdlinefiles.h ----- */
