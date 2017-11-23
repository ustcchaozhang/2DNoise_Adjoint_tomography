/*! \file label.h
 * \brief label for graphs (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/03/2008
 * 
 * label for graphs (prototypes)
 * 
 * Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 08/03/2008   V1.0   Thomas Forbriger
 *  - 17/03/2015   V1.1   adjust libpgplotxx interface
 * 
 * ============================================================================
 */

// include guard
#ifndef STUPLO_LABEL_H_VERSION

#define STUPLO_LABEL_H_VERSION \
  "STUPLO_LABEL_H   V1.0   (17-03-2015)"

#include<string>
#include<list>
#include<tfxx/stringfunc.h>
#include<pgplotxx/pgplotxx.h>
#include<pgplotxx/xpgplotxx.h>

#include"utilitystructures.h"

namespace stuplo {

  /*! \brief a single label for one graph
   */
  class Label {
    public:
      Label(const std::string& text,
            const GLstyle& glstyle,
            const pgplot::Tlinestyle& linestyle):
        Mtext(text), Mglstyle(glstyle), Mls(linestyle) 
      { Mtext=tfxx::string::trimws(Mtext); }
      //! draw graph label text
      void draw(pgplot::basic_device& dev, pgplot::Ttext& pgt) const;
      //! true if not equal
      bool operator!=(const Label& l) const;
      //! return length text
      int length() const { return(Mtext.length()); }
    private:
      //! label text string
      std::string Mtext;
      //! graph label style options
      GLstyle Mglstyle;
      //! line style for graph
      pgplot::Tlinestyle Mls;
  }; // class Label

  //! comparison operator
  inline bool operator==(const Label& a, const Label& b) { return(!(a!=b)); }

  /*----------------------------------------------------------------------*/

  /*! \brief a collection of labels for all graphs in a panel
   */
  class Labelset {
    public:
      //! add a label to the set if the new is unique
      void add(const Label& l);
      //! draw graph label text
      void draw(pgplot::basic_device& dev, pgplot::Ttext& pgt) const;
    private:
      //! my type for label collection
      typedef std::list<Label> Tlist;
      //! my collection of labels
      Tlist Mlist;
  }; // class Labelset

} // namespace stuplo

#endif // STUPLO_LABEL_H_VERSION (includeguard)

/* ----- END OF label.h ----- */
