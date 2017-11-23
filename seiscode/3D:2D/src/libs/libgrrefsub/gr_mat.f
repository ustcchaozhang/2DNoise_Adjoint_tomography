c this is <gr_mat.f>
c------------------------------------------------------------------------------
c
c These are matrix inversion and matrix multiplication routines written
c by Joachim Ungerer for his reflectivity program refseis
c
c This is part of a library version of the original code 'refseis.f'
c
c Copyright 1990 by Joachim Ungerer
c
c refseis.f is published as part of the diploma thesis of Joachim Ungerer
c without a specific license
c
c----------------------------------------------------------------------
c
c Copyright 1997, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c ----
c This program is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c 
c You should have received a copy of the GNU General Public License
c along with this program; if not, write to the Free Software
c Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
c ----
c
c REVISIONS and CHANGES
c    24/07/97   V1.0   Thomas Forbriger
c
c==============================================================================
c
C23456789012345678901234567890123456789012345678901234567890123456789012
C Subroutine fuer Matrizeninversion
C Bem: DOUBLE COMPLEX muss implementiert sein
 
      SUBROUTINE MATINV(A11,A12,A21,A22,B11,B12,B21,B22)
      COMPLEX*16   A11,A12,A21,A22,B11,B12,B21,B22,D
      D=1.D0/(A11*A22-A12*A21)
      B11= A22*D
      B12=-A12*D
      B21=-A21*D
      B22=A11*D
      RETURN
      END



C23456789012345678901234567890123456789012345678901234567890123456789012
C Subroutine fuer Matrizenmultiplikation
C Bem: DOUBLE COMPLEX muss implementiert sein

      SUBROUTINE MATMUL(A11,A12,A21,A22,B11,B12,B21,B22,C11,C12,C21,C22)
      COMPLEX*16   A11,A12,A21,A22,B11,B12,B21,B22,C11,C12,C21,C22
      C11=A11*B11+A12*B21
      C12=A11*B12+A12*B22
      C21=A21*B11+A22*B21
      C22=A21*B12+A22*B22
      RETURN
      END

c
c ----- END OF gr_mat.f -----
