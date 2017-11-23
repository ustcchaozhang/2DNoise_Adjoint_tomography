/*! \file label.cc
 * \brief label for graphs (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/03/2008
 * 
 * label for graphs (implementation)
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
 * 
 * ============================================================================
 */
#define STUPLO_LABEL_CC_VERSION \
  "STUPLO_LABEL_CC   V1.0   "

#include "label.h"

namespace stuplo {

  //! draw label on panel
  void Label::draw(pgplot::basic_device& dev, pgplot::Ttext& pgt) const
  {
    dev.save();
    pgplot::Tlinestyle tls=Mls;
    tls.cci().clw();
    if (Mglstyle.colourlabel) tls(dev);
    if (Mglstyle.eraselabelbox) { pgt.erase(); } else { pgt.noerase(); }
    pgt.print(dev, Mtext.c_str(), false, 0.);
    if (Mglstyle.underlinelabel)
    {
      pgplot::Tbbox bbox=pgt.bbox();
      Mls(dev).move(bbox.coor[0]).draw(bbox.coor[3]);
    }
    dev.unsa();
  } // void Label::draw(pgplot::basic_device& dev, pgplot::Ttext& pgt) const

  /*----------------------------------------------------------------------*/

  //! comparison operator
  bool Label::operator!=(const Label& l) const
  {
    if (this->Mtext != l.Mtext) return(true);
    if (this->Mglstyle != l.Mglstyle) return(true);
    if (this->Mls != l.Mls) return(true);
    return(false);
  } // bool Label::operator!=(const Label& l) const

  /*======================================================================*/

  //! add a new label if unique
  void Labelset::add(const Label& l)
  {
    if (l.length()>0)
    {
      bool doadd=true;
      Tlist::const_iterator I=Mlist.begin();
      while (I!=Mlist.end())
      {
        if (*I == l)
        {
          doadd=false;
          I=Mlist.end();
        }
        else
        {
          ++I;
        }
      }
      if (doadd) Mlist.push_back(l);
    }
  }

  /*----------------------------------------------------------------------*/

  //! draw label on panel
  void Labelset::draw(pgplot::basic_device& dev, pgplot::Ttext& pgt) const
  {
    Tlist::const_iterator I=Mlist.begin();
    while (I!=Mlist.end())
    {
      I->draw(dev, pgt);
      pgt.space(dev);
      ++I;
    }
  } // void Labelset::draw(pgplot::basic_device& dev, pgplot::Ttext& pgt) const

} // namespace stuplo

/* ----- END OF label.cc ----- */
