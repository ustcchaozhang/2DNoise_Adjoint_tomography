/*! \file structs.cc
 * \brief provide useful structs for pgplotxx (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/03/2015
 * 
 * provide useful structs for pgplotxx (implementation)
 * 
 * Copyright (c) 2015 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 17/03/2015   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_STRUCTS_CC_VERSION \
  "TF_STRUCTS_CC   V1.0   (17-03-2015)"

#include <pgplotxx/structs.h>

namespace pgplot {

  //! default constructor
  Tbbox::Tbbox()
  {
    for (int i=0; i<4 ; ++i)
    {
      this->coor[i].x=0;
      this->coor[i].y=0;
    }
  }

  /*----------------------------------------------------------------------*/

  //! initialize from C arrays \p x and \p y
  Tbbox::Tbbox(const float x[4], const float y[4])
  {
    for (int i=0; i<4 ; ++i)
    {
      this->coor[i].x=x[i];
      this->coor[i].y=y[i];
    }
  }

} // namespace pgplot

/* ----- END OF structs.cc ----- */
