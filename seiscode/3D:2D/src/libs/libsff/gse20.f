c this is <gse20.f>
c
c This file is part of libsff.
c
c The code is taken from codeco.f which is put into the public domain at
c http://www.seismo.ethz.ch/prod/autodrm/Software/index_EN
c
c No copying information is given in the package
c http://www.seismo.ethz.ch/prod/autodrm/autodrm_3.00beta.tar.tar
c 
c Copyright statements for the code used here could be resolved to be:
c
c Copyright (c) 1989 by Shane Ingate (Australian Seismological Centre)
c Copyright (c) 1989 by Ken Muirhead (Australian Seismological Centre)
c Copyright (c) 1990 by Urs Kradolfer (SED, Swiss Seismological c Service)
c
c The file in its present form (gse20.f) is put under the 
c GNU General Public License
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
c 17/1/2008       modified function intand to satisfy gfortran (thof)
c
c======================================================================
c 
c We use the following routines from codeco.f:
c DIF1       calculates first differences (compress)
c REMDIF1    remove first differences (decompress)
c CMPRS6     compress integer data in a six bit ascii character scheme
c DCOMP6     unpack character scheme to reveal the integer data
c INTAND     do a bitwise AND operation
c 
c These routines are not ment to be called directly.
c
c--------------------------------------------------------------------
c
      subroutine DIF1(iy,nmax)
c
c     Do Data-compression (first differences)
c
c     Urs Kradolfer, January 1990
c
      implicit none
      integer nmax, iy(nmax), k, xim1, xtemp
c
      xim1=iy(1)
      do k=2,nmax
         xtemp=iy(k)
         iy(k)=xtemp-xim1
         xim1=xtemp
      enddo
c
      RETURN
      end
c
c***************************************************************************
c
      subroutine REMDIF1(iy,nmax)
c
c     Remove Data-compression (first differences)
c
c     Urs Kradolfer, January 1990
c
      implicit none
      integer nmax, iy(nmax), k
c
      do k=2,nmax
         iy(k)=iy(k)+iy(k-1)
      enddo
c
      RETURN
      end
c
C******************************************************************************
c#    The following data compression and decompression routines have
c#    been developped by Dr. Shane Ingate and Dr. Ken Muirhead, at the
c#    Australian Seismological Centre, Bureau of Mineral Resources,
c#    Canberra, Australia.
c#    They provided me with these routines in March 1989 during the
c#    session of the Group of Scientific Experts (GSE) in Geneva.
c#    These compression/decompression algorithms are used by all members
c#    of the GSE during the large-scale data exchange experiment GSETT-2,
c#    carried out from 1989 to 1991.
c#    It is recommended to use second differences and the six-bit compression.
c#    The second differences can be computed by two subsequent calls of
c#    subroutine DIF1 (see above).
c#    These routines are already running on many different machines.
c#    Because the AND function is not standard FORTRAN 77, this operation
c#    has been moved to a separate subroutine INTAND. All users of these
c#    routines will have to modify INTAND so that it performs correctly on
c#    their computers.
c#                                 Urs Kradolfer, Swiss Seismological Service
c#
C******************************************************************************
C                                                                       *
C     SUBROUTINE CMPRS6(LX,IX,LC,CBUFF,IERROR)                          *
C                                                                       *
C     ROUTINE TO COMPRESS INTEGER DATA INTO PRINTABLE ASCII CHARACTERS. *
C     INPUTS ARE INTEGER*4 ARRAY IX OF LENGTH LX.                       *
C     LC SHOULD CONTAIN THE DIMENSION OF THE ARRAY CBUFF.               *
C     OUTPUTS ARE CHARACTER*1 ARRAY CBUFF CONTAINING LC CHARACTERS.     * 
C     IF THE ARRAY CBUFF IS NOT LARGE ENOUGH TO CONTAIN ALL OF THE DATA *
C     IERROR IS SET TO -1, OTHERWISE IT IS SET TO ZERO.                 *
C     OUTPUT IS PADDED TO A MULTIPLE OF 80 CHARACTERS USING SPACES.     *
C                                                                       *
C     METHOD OF COMPRESSION IS TO USE THE 6 LEAST SIGNIFICANT SIX BITS  *
C     OF AN EIGHT BIT BYTE SO THAT ALL DATA CAN BE TRANSMITTED AS ASCII *
C     CHARACTERS.                                                       *
C     OF THESE SIX BITS, THE MOST SIGNIFICANT IS USED AS A CONTINUATION *
C     BIT. IF IT IS SET TO ONE THE FOLLOWING BYTE ALSO FORMS PART OF    *
C     THE PRESENT SAMPLE. IF ZERO, THIS IS THE LAST BYTE IN THE SAMPLE. *
C     THE SECOND LEAST SIGNIFICANT BIT IS USED AS A SIGN BIT IN THE     *
C     FIRST BYTE AND FOR DATA IN SUBSEQUENT BYTES (ORIGINAL DATA IS     *
C     FIRST CONVERTED TO SIGN AND MAGNITUDE). ALL OTHER BITS OF FIRST   *
C     AND SUBSEQUENT BYTES FORM THE MAGNITUDE OF THE NUMBER.            *
C                                                                       *
C     TO ENABLE TRANSMISSION OVER AS MANY LINKS AS POSSIBLE DATA IS     *
C     FURTHER TRANSFORMED TO THE CHARACTERS +,-,0 - 9,A - Z, a - z      *
C     USING A LOOKUP TABLE.                                             *
C                                                                       *
C     SUBROUTINES CALLED INTAND (THIS SUBROUTINE IS MACHINE SPECIFIC)   *
C                                                                       *
C     DATA MAY BE DECOMPRESSED USING SUBROUTINE DCOMP6.                 *
C                                                                       *
C************************************************************************ 
C
      SUBROUTINE CMPRS6(LX,IX,LC,CBUFF,IERROR)
      IMPLICIT INTEGER*4 (I-N)
cuk      CHARACTER *1 CBUFF(4),TEMP(4),TEST(4),ACHAR(64),BLANK
cuu91      CHARACTER *1 CBUFF(1),TEMP(4),TEST(4),ACHAR(64),BLANK
      CHARACTER *1 CBUFF(lc),TEMP(4),TEST(4),ACHAR(64),BLANK
cuu91      INTEGER *4 IX(1)
      INTEGER *4 IX(lx)
      EQUIVALENCE (J,TEMP(1)),(TEST,ITEST)
      DATA TEST/'1','2','3','4'/
      DATA ACHAR / '+','-','0','1','2','3','4','5','6','7','8','9','A',
     1 'B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q',
     2 'R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g',
     3 'h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w',
     4 'x','y','z'/
      DATA BLANK /' '/
      DATA N4/16/, N5/32/, N5M1/31/, N6/64/, N9/512/, N10/1024/, 
     1 N10M1/1023/, N14/16384/, N15/32768/, N15M1/32767/, N19/524288/, 
     2 N20/1048576/, N20M1/1048575/, N24/16777216/, N25/33554432/, 
     3 N25M1/33554431/, N28/134217728/, N28M1/134217727/
      IERROR = 0
      IMAX = LC
      MFLAG = N5
      IOFF = 0
      DO 60 I = 1,LX
        JN = IX(I)
C       SET NFLAG TO 1 TO POINT TO FIRST ELEMENT IN LOOKUP TABLE
        NFLAG = 1
C
C       SEE IF NUMBER IS -VE IF SO CONVERT TO SIGN AND MAGNITUDE.
C
        IF(JN .LT. 0) THEN
           NFLAG = NFLAG + N4
           JN = -JN
        END IF
        IF(JN .LT. N4) GO TO 50
C        IF HERE, DATA REQUIRES MORE THAN 1 BYTE
         IF(JN .LT. N9) GO TO 40
C         IF HERE, DATA REQUIRES MORE THAN 2 BYTES
          IF(JN .LT. N14) GO TO 30
C          IF HERE, DATA REQUIRES MORE THAN 3 BYTES.
           IF(JN. LT. N19) GO TO 20
C           IF HERE, DATA REQUIRES MORE THAN 4 BYTES.
            IF(JN. LT. N24) GO TO 10
C            IF HERE, DATA REQUIRES MORE THAN 5 BYTES.
             IF(JN .GT. N28M1) JN=N28M1
C            FILL A BYTE IF HERE NUMBER WILL REQUIRE SIX BYTES
             J = JN/N25 + NFLAG + MFLAG
             IOFF = IOFF + 1
             CBUFF(IOFF) = ACHAR(J)
             CALL INTAND(JN,N25M1,JN)
             NFLAG = 1
   10       CONTINUE
C           FIVE CHARACTERS TO GO
            J = JN/N20 + NFLAG + MFLAG
            IOFF = IOFF + 1
            IF (IOFF .GT. IMAX) GO TO 80
            CBUFF(IOFF) = ACHAR(J)
            CALL INTAND(JN,N20M1,JN)
            NFLAG = 1
   20      CONTINUE
C          FOUR CHARACTERS TO GO
           J = JN/N15 + NFLAG + MFLAG
           IOFF = IOFF + 1
           IF (IOFF .GT. IMAX) GO TO 80
           CBUFF(IOFF) = ACHAR(J)
           CALL INTAND(JN,N15M1,JN)
           NFLAG = 1
   30     CONTINUE
C         THREE CHARACTERS TO GO
          J = JN/N10 + NFLAG + MFLAG
          IOFF = IOFF + 1
          IF (IOFF .GT. IMAX) GO TO 80
          CBUFF(IOFF) = ACHAR(J)
          CALL INTAND(JN,N10M1,JN)
          NFLAG = 1
   40    CONTINUE
C        TWO CHARACTERS TO GO
         J = JN/N5 + NFLAG + MFLAG
         IOFF = IOFF + 1
         IF (IOFF .GT. IMAX) GO TO 80
         CBUFF(IOFF) = ACHAR(J)
         CALL INTAND(JN,N5M1,JN)
         NFLAG = 1
   50   CONTINUE
C       ONE CHARACTER TO GO
        J = JN + NFLAG
        IOFF = IOFF + 1
        IF (IOFF .GT. IMAX) GO TO 80
        CBUFF(IOFF) = ACHAR(J)
   60 CONTINUE
C     NOW MAKE OUTPUT BUFFER UP TO A MULTIPLE OF 80 CHARACTERS
C     WITH BLANKS
      K = 80 - (IOFF - ((IOFF/80) * 80))
      IF (K .LT. 2) K = K + 80
C     PUT IN AT LEAST TWO ADJACENT SPACE CHARACTERS TO ASSIST IN
C     DECODING
      DO 70 I = 1, K
         IOFF = IOFF + 1
         IF (IOFF .GT. IMAX) GO TO 80
         CBUFF(IOFF) = BLANK
   70 CONTINUE
   80 CONTINUE
      LC = IOFF
      IF (IOFF .GT. IMAX) THEN
         LC = IMAX
         IERROR = -1
      ENDIF
      RETURN
      END
C***********************************************************************
C                                                                      *
C     SUBROUTINE DCOMP6(LB,IBUF,LOUT,IOUT,IERROR)                      *
C                                                                      *
C     SUBROUTINE DECOMPRESS INTEGER DATA THAT HAS BEEN COMPRESSED      *
C     INTO ASCII CHARACTERS AND RETURNS VALUES IN INTEGER *4 FORMAT    *
C     SEE SUBROUTINE CMPRS6 FOR COMPRESSION FORMAT                     *
C     INPUT - IBUF AN ARRAY OF CONTAINING  LB CHARACTERS.              *
C     LOUT SHOULD CONTAIN THE DIMENSION OF THE INTEGER *4 ARRAY IOUT.  * 
C     ON RETURN ARRAY LOUT WILL BE SET TO CONTAIN THE NUMBER OF INTEGER*
C     VALUES WHICH HAVE BEEN PUT IN THE ARRAY IOUT.                    *
C     IF THE ARRAY IOUT IS NOT LARGE ENOUGH TO CONTAIN ALL OF THE      *
C     DECOMPRESSED VALUES IERROR WILL BE SET TO -1 OTHERWISE IT WILL   *
C     SET TO ZERO                                                      *
C***********************************************************************
C
      SUBROUTINE DCOMP6(LB,IBUF,LOUT,IOUT,IERROR)
      IMPLICIT INTEGER *4 (I-N)
cuk      INTEGER *4 IOUT(*),ICHAR(128)
cuu91      INTEGER *4 IOUT(1),ICHAR(128)
      INTEGER *4 IOUT(lout),ICHAR(128)
cuu91      CHARACTER *1 IBUF(1), ACHAR(4), TEST(4)
      CHARACTER *1 IBUF(lb), ACHAR(4), TEST(4)
      CHARACTER *1 ASPACE,LFEED,CRETN
      EQUIVALENCE (INN,ACHAR),(ITEST,TEST)
      DATA TEST/'1','2','3','4'/
      data ichar/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     1 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     2 0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,2,
     3 3,4,5,6,7,8,9,10,11,0,0,0,0,0,0,0,
     4 12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,
     5 28,29,30,31,32,33,34,35,36,37,0,0,0,0,0,0,
     6 38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,
     7 54,55,56,57,58,59,60,61,62,63,0,0,0,0,0,0/
      IERROR = 0
      CRETN = CHAR(13)
      LFEED = CHAR(10)
      ASPACE = CHAR(32)
      IMAX = LOUT
      ISIGN = 16
      IOFLOW = 32
      MASK1 = 15
      MASK2 = 31
C     WORK OUT WHICH WAY BYTES ARE STORED IN COMPUTER
      IBYTE = 4
      IF (ITEST.EQ.(((52*256+51)*256+50)*256+49)) IBYTE = 1
      ICOUNT = 0
      I = 0
      J = 0
C     START OF DECODING
   1  CONTINUE
        I = I + 1
        ACHAR(IBYTE) = IBUF(I)
C       IF A CARRIAGE OR LINE FEED IGNORE, IF A SPACE THEN END OF DATA
        IF (ACHAR(IBYTE) .EQ. CRETN) GO TO 1 
        IF (ACHAR(IBYTE) .EQ. LFEED) GO TO 1 
        IF (ACHAR(IBYTE) .EQ. ASPACE) GO TO 5
        ICOUNT = ICOUNT + 1
C       STRIP OFF ANY HIGHER ORDER BITS
        CALL INTAND(INN,127,K)
C       GET NUMBER REPRESENTION OF INPUT CHARACTER
        INN = ICHAR(K)
C       GET SIGN BIT
        CALL INTAND(INN,ISIGN,JSIGN)
C       GET CONTINUATION BIT (IF ANY)
        CALL INTAND(INN,IOFLOW,JOFLOW)
C       REMOVE BITS WE DONT WANT
        CALL INTAND(INN,MASK1,ITEMP)
   2    CONTINUE
          IF(JOFLOW.EQ.0) GO TO 4
C         THERE IS ANOTHER BYTE IN THIS SAMPLE
          ITEMP = ITEMP * 32
   3        CONTINUE
            I = I + 1
            ACHAR(IBYTE) = IBUF(I)
          IF (ACHAR(IBYTE) .EQ. CRETN) GO TO 3 
          IF (ACHAR(IBYTE) .EQ. LFEED) GO TO 3 
          ICOUNT = ICOUNT + 1
C         STRIP OFF ANY HIGHER ORDER BITS
          CALL INTAND(INN,127,K)
          INN = ICHAR(K)
C         GET CONTINUATION BIT (IF ANY)
          CALL INTAND(INN,IOFLOW,JOFLOW)
          CALL INTAND(INN,MASK2,K)
          ITEMP = ITEMP + K
        GO TO 2
   4    CONTINUE
        IF (JSIGN.NE.0) ITEMP = -ITEMP
        J = J +1
        IF (J .GT. IMAX) GO TO 5
        IOUT(J) = ITEMP
      IF(ICOUNT.LT.LB) GO TO 1
   5  CONTINUE
      LOUT = J
      IF (J .GT. IMAX) THEN
         LOUT = IMAX
         IERROR = -1
      ENDIF
      RETURN
      END
C**********************************************************************
C                                                                     *
C      SUBROUTINE INTAND(I1,I2,I3)                                    *
C                                                                     *
C      SUBROUTINE BITWISE "ANDS" THE INTEGERS IN I1 AND I2            *
C      AND RETURNS THE RESULT IN INTEGER I3                           *
C      FOR EXAMPLE                                                    *
C      IF THE 32 BITS IN I1 ARE 11001100 1110001110 11110000 11111111 *
C      AND IN I2 ARE            10101010 1100110011 00110011 00110011 *
C      I3 WILL BECOME           10001000 1100000010 00110000 00110011 *
C                                                                     *
C      NOTE "AND" IS NOT STANDARD FORTRAN 77 FUNCTION SO THIS IS A    *
C      MACHINE DEPENDANT SUBROUTINE                                   *
C**********************************************************************
C
       SUBROUTINE INTAND(I1,I2,I3) 
       INTEGER    I1,I2,I3
C##SUN## REMOVE COMMENT FROM NEXT LINE FOR SUN COMPUTERS
c      I3 = AND(I1,I2)
C##DEC## REMOVE COMMENT FROM NEXT LINE FOR PDP AND VAX COMPUTERS and HP's
cwf       I3 = JIAND(I1,I2)
C##IBM## REMOVE COMMENT FROM NEXT LINE FOR IBM PC'S (I THINK)
       I3 = IAND(I1,I2)
       RETURN
       END
C************************************************************************

c ----- END OF gse20.f -----
