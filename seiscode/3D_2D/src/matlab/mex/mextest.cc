/*! \file mextest.cc
 * \brief test mex compiler
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 27/04/2010
 * 
 * test mex compiler
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 27/04/2010   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define MEXTEST_VERSION \
  "MEXTEST   V1.0   test mex compiler"
#define MEXTEST_CVSID \
  "$Id$"

#include "mex.h" 
#include <libtime++.h>
void mexFunction(int nlhs, mxArray *plhs[],
                     int nrhs, const mxArray *prhs[]) {
    mexPrintf("Hello, world!\n"); 
    libtime::TAbsoluteTime date=libtime::now();
    mexPrintf("Year: %d Day of year: %d\n", date.year(), date.doy());
} 

/* ----- END OF mextest.cc ----- */
