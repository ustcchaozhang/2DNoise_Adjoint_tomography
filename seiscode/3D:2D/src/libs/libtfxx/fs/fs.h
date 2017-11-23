/*! \file fs.h
 * \brief file system utilities (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/12/2012
 * 
 * file system utilities (prototypes)
 * 
 * Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 16/12/2012   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_FS_H_VERSION

#define TF_FS_H_VERSION \
  "TF_FS_H   V1.0   "

#include<string>
#include<tfxx/error.h>

/*! \defgroup group_fs File system utilities.
 *
 * \brief An interface to libc file system functions.
 *
 * The module is presented in namespace tfxx::fs.
 * You will find an example in tests/fstest.cc.
 *
 * \sa group_fs_h
 *
 */

/*! \brief Interface provided through fs.h
 *
 * \defgroup group_fs_h Interface provided through fs.h
 *
 * \ingroup group_fs
 */

namespace tfxx {

    namespace error {

    /*! \brief exception class for file system utilities
     *
     * \ingroup group_fs, group_fs_h
     * \sa TFXX_Xassert
     * \sa tfxx::error::Exception
     */
    class FSException: public tfxx::error::Exception
    {
      public:
        //! base class
        typedef tfxx::error::Exception TBase;
        //! Create with message, failed assertion, and code position
        FSException(const char* message, 
                    const char* file,
                    const int& line,
                    const int& en);
        //! provide explicit virtual destructor
        virtual ~FSException() { }
        //! Screen report
        virtual void report() const;
        //! true, if exception reports error en
        bool iserrno(const int& en) const { return (en==Merrno); }
      private:
        //! my report
        void fs_report() const;
        //! error number
        int Merrno;
    }; // class exception

  } // namespace error

  /*! \brief Namespace for all file system utilities.
   *
   * \ingroup group_fs
   * \sa group_fs
   * \sa group_fs_h
   */
  namespace fs {

    /*----------------------------------------------------------------------*/
    // functions

    /*! \brief return path with its last non-slash component removed
     *
     * \ingroup group_fs
     * C++ interface to dirname(3)
     */
    std::string dirname(const std::string& path);

    /*! \brief create a directory
     *
     * \ingroup group_fs
     * C++ interface to mkdir(2)
     */
    void mkdir(const std::string& path);

    /*! \brief create a directory with all parents
     *
     * \ingroup group_fs
     * C++ interface to mkdir(2)
     */
    void mkdirp(const std::string& path);

  } // namespace fs

} // namespace tfxx

#endif // TF_FS_H_VERSION (includeguard)

/* ----- END OF fs.h ----- */
