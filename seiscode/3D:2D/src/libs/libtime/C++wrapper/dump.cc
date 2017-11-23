/*! \file dump.cc
 * \brief dump internals of time class object (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/11/2017
 * 
 * dump internals of time class object (implementation)
 * 
 * Copyright (c) 2017 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 07/11/2017   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_DUMP_CC_VERSION \
  "TF_DUMP_CC   V1.0"

#include <libtime++.h>
#include<iostream>

namespace libtime {

  void dump(std::ostream& os, const TBaseClassTime& t)
  {
    time_kernel::time_Ts mytime_Ts(t);
    os << " ** dump internals of " << t.timestring() << std::endl;
    os << " **              year " << mytime_Ts.year << std::endl;
    os << " **               doy " << mytime_Ts.doy << std::endl;
    os << " **              hour " << mytime_Ts.hour << std::endl;
    os << " **            minute " << mytime_Ts.minute << std::endl;
    os << " **            second " << mytime_Ts.second << std::endl;
    os << " **      milli second " << mytime_Ts.milsec << std::endl;
    os << " **      micro second " << mytime_Ts.micsec << std::endl;
  }

} // namespace libtime

/* ----- END OF dump.cc ----- */
