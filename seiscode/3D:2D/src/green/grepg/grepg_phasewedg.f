c this is <grepg_phasewedg.f>
c------------------------------------------------------------------------------
cS
c
c Copyright (c) 1983-2001 by the California Institute of Technology
c Copyright (c) 1999 by Thomas Forbriger (IfG Stuttgart)
c
c This is a modfied copy of code written by Tim Pearson for the PGPLOT library
c For the license conditions see
c http://www.astro.caltech.edu/~tjp/pgplot
c or README.PGPLOT.copyright.notice
c
c plot color wedge
c
c ----
c
c Just copied from TJP-Code 
c 
c NOTICE: meanings of I ang G flags have changed here:
c I:  plot wedge for full color phase and amplitude plot by using the HUE
c     value to represent the phase angle
c G:  use selfmade color map to represent angle values from 0. to 360. deg.
c
c The main purpose of this modification of the original subroutine is to:
c 1. provide the full color phase angle wedge
c 2. to create a wedge with axis labels suitable for the range from 0. to
c   360. (i.e. stepwidth of major tick marks is 45.)
c
c REVISIONS and CHANGES
c    14/10/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
C*PGWEDG -- annotate an image plot with a wedge
C%void cpgwedg(const char *side, float disp, float width, \
C% float fg, float bg, const char *label);
C+
c
      subroutine grepg_phasewedg(SIDE, DISP, WIDTH, FG, BG, LABEL,bgw)
      CHARACTER *(*) SIDE,LABEL
      REAL DISP, WIDTH, FG, BG
      logical bgw
C
C Plot an annotated grey-scale or color wedge parallel to a given axis
C of the the current viewport. This routine is designed to provide a
C brightness/color scale for an image drawn with PGIMAG or PGGRAY.
C The wedge will be drawn with the transfer function set by PGSITF
C and using the color index range set by PGSCIR.
C
C Arguments:
C  SIDE   (input)  : The first character must be one of the characters
C                    'B', 'L', 'T', or 'R' signifying the Bottom, Left,
C                    Top, or Right edge of the viewport.
C                    The second character should be 'I' to use PGIMAG
C                    to draw the wedge, or 'G' to use PGGRAY.
C  DISP   (input)  : the displacement of the wedge from the specified
C                    edge of the viewport, measured outwards from the
C                    viewport in units of the character height. Use a
C                    negative value to write inside the viewport, a
C                    positive value to write outside.
C  WIDTH  (input)  : The total width of the wedge including annotation,
C                    in units of the character height.
C  FG     (input)  : The value which is to appear with shade
C                    1 ("foreground"). Use the values of FG and BG
C                    that were supplied to PGGRAY or PGIMAG.
C  BG     (input)  : the value which is to appear with shade
C                    0 ("background").
C  LABEL  (input)  : Optional units label. If no label is required
C                    use ' '.
C--
C  15-Oct-1992: New routine (MCS)
C   2-Aug-1995: no longer needs common (TJP).
C-----------------------------------------------------------------------
cE
      LOGICAL PGNOTO
C                                        Temporary window coord storage.
      REAL WXA,WXB,WYA,WYB, XA,XB,YA,YB
C                                        Viewport coords of wedge.
      REAL VXA,VXB,VYA,VYB
C                          Original and anotation character heights.
      REAL OLDCH, NEWCH
C                          Size of unit character height (NDC units).
      REAL NDCSIZ
C                          True if wedge plotted horizontally.
      LOGICAL HORIZ
C                          Use PGIMAG (T) or PGGRAY (F).
      LOGICAL IMAGE
C                          Symbolic version of SIDE.
      INTEGER NSIDE,BOT,TOP,LFT,RGT
      PARAMETER (BOT=1,TOP=2,LFT=3,RGT=4)
      INTEGER I
      REAL WEDWID, WDGINC, VWIDTH, VDISP, XCH, YCH, LABWID, FG1, BG1
C                          Set the fraction of WIDTH used for anotation.
      REAL TXTFRC
      PARAMETER (TXTFRC=0.6)
C                          Char separation between numbers and LABEL.
      REAL TXTSEP
      PARAMETER (TXTSEP=2.2)
C                          Array to draw wedge in.
      INTEGER WDGPIX
      PARAMETER (WDGPIX=300)
      REAL WDGARR(WDGPIX)
      REAL WDamp(WDGPIX)
C                          Define the coordinate-mapping function.
      REAL TR(6)
      SAVE TR
      DATA TR /0.0,1.0,0.0,0.0,0.0,1.0/
C-----------------------------------------------------------------------
      IF(PGNOTO('PGWEDG')) RETURN
C
C Get a numeric version of SIDE.
C
      IF(SIDE(1:1).EQ.'B' .OR. SIDE(1:1).EQ.'b') THEN
        NSIDE = BOT
        HORIZ = .TRUE.
      ELSE IF(SIDE(1:1).EQ.'T' .OR. SIDE(1:1).EQ.'t') THEN
        NSIDE = TOP
        HORIZ = .TRUE.
      ELSE IF(SIDE(1:1).EQ.'L' .OR. SIDE(1:1).EQ.'l') THEN
        NSIDE = LFT
        HORIZ = .FALSE.
      ELSE IF(SIDE(1:1).EQ.'R' .OR. SIDE(1:1).EQ.'r') THEN
        NSIDE = RGT
        HORIZ = .FALSE.
      ELSE
        CALL GRWARN('Invalid "SIDE" argument in PGWEDG.')
        RETURN
      END IF
C
C Determine which routine to use.
C
      IF (LEN(SIDE).LT.2) THEN
         IMAGE = .FALSE.
      ELSE IF(SIDE(2:2).EQ.'I' .OR. SIDE(2:2).EQ.'i') THEN
         IMAGE = .TRUE.
      ELSE IF(SIDE(2:2).EQ.'G' .OR. SIDE(2:2).EQ.'g') THEN
         IMAGE = .FALSE.
      ELSE
         CALL GRWARN('Invalid "SIDE" argument in PGWEDG.')
      END IF
C
      CALL PGBBUF
C
C Store the current world and viewport coords and the character height.
C
      CALL PGQWIN(WXA, WXB, WYA, WYB)
      CALL PGQVP(0, XA, XB, YA, YB)
      CALL PGQCH(OLDCH)
C
C Determine the unit character height in NDC coords.
C
      CALL PGSCH(1.0)
      CALL PGQCS(0, XCH, YCH)
      IF(HORIZ) THEN
        NDCSIZ = YCH
      ELSE
        NDCSIZ = XCH
      END IF
C
C Convert 'WIDTH' and 'DISP' into viewport units.
C
      VWIDTH = WIDTH * NDCSIZ * OLDCH
      VDISP  = DISP * NDCSIZ * OLDCH
C
C Determine the number of character heights required under the wedge.
C
      LABWID = TXTSEP
      IF(LABEL.NE.' ') LABWID = LABWID + 1.0
C
C Determine and set the character height required to fit the wedge
C anotation text within the area allowed for it.
C
      NEWCH = TXTFRC*VWIDTH / (LABWID*NDCSIZ)
      CALL PGSCH(NEWCH)
C
C Determine the width of the wedge part of the plot minus the anotation.
C (NDC units).
C
      WEDWID = VWIDTH * (1.0-TXTFRC)
C
C Use these to determine viewport coordinates for the wedge + annotation.
C
      VXA = XA
      VXB = XB
      VYA = YA
      VYB = YB
      IF(NSIDE.EQ.BOT) THEN
        VYB = YA - VDISP
        VYA = VYB - WEDWID
      ELSE IF(NSIDE.EQ.TOP) THEN
        VYA = YB + VDISP
        VYB = VYA + WEDWID
      ELSE IF(NSIDE.EQ.LFT) THEN
        VXB = XA - VDISP
        VXA = VXB - WEDWID
      ELSE IF(NSIDE.EQ.RGT) THEN
        VXA = XB + VDISP
        VXB = VXA + WEDWID
      END IF
C
C Set the viewport for the wedge.
C
      CALL PGSVP(VXA, VXB, VYA, VYB)
C
C Swap FG/BG if necessary to get axis direction right.
C
      FG1 = MAX(FG,BG)
      BG1 = MIN(FG,BG)
C
C Create a dummy wedge array to be plotted.
C
      WDGINC = (FG1-BG1)/(WDGPIX-1)
      DO 1 I=1,WDGPIX
        WDGARR(I) = BG1 + (I-1) * WDGINC
        wdamp(i)=1.
 1    CONTINUE
C
C Draw the wedge then change the world coordinates for labelling.
C
      IF (HORIZ) THEN
        CALL PGSWIN(1.0, REAL(WDGPIX), 0.9, 1.1)
        IF (IMAGE) THEN
          call grepg_phase(wdamp, WDGARR, WDGPIX,1, 1,
     &      WDGPIX, 1,1, 0.,1., TR, bgw)
c           CALL PGIMAG(WDGARR, WDGPIX,1, 1,WDGPIX, 1,1, FG,BG, TR)
        ELSE
           CALL PGIMAG(WDGARR, WDGPIX,1, 1,WDGPIX, 1,1, FG,BG, TR)
c           CALL PGGRAY(WDGARR, WDGPIX,1, 1,WDGPIX, 1,1, FG,BG, TR)
        END IF
        CALL PGSWIN(BG1,FG1,0.0,1.0)
      ELSE
        CALL PGSWIN(0.9, 1.1, 1.0, REAL(WDGPIX))
        IF (IMAGE) THEN
          call grepg_phase(wdamp, WDGARR, 1,WDGPIX, 1,1,
     &      1,WDGPIX, 0.,1., TR, bgw)
c           CALL PGIMAG(WDGARR, 1,WDGPIX, 1,1, 1,WDGPIX, FG,BG, TR)
        ELSE
           CALL PGIMAG(WDGARR, 1,WDGPIX, 1,1, 1,WDGPIX, FG,BG, TR)
c           CALL PGGRAY(WDGARR, 1,WDGPIX, 1,1, 1,WDGPIX, FG,BG, TR)
        END IF
        CALL PGSWIN(0.0, 1.0, BG1, FG1)
      ENDIF
C
C Draw a labelled frame around the wedge.
C
      IF(NSIDE.EQ.BOT) THEN
        CALL PGBOX('BCNST',45.0,9,'BC',0.0,0)
      ELSE IF(NSIDE.EQ.TOP) THEN
        CALL PGBOX('BCMST',45.0,9,'BC',0.0,0)
      ELSE IF(NSIDE.EQ.LFT) THEN
        CALL PGBOX('BC',0.0,0,'BCNST',45.0,9)
      ELSE IF(NSIDE.EQ.RGT) THEN
        CALL PGBOX('BC',0.0,0,'BCMST',45.0,9)
      ENDIF
C
C Write the units label.
C
      IF(LABEL.NE.' ') THEN
        CALL PGMTXT(SIDE,TXTSEP,1.0,1.0,LABEL)
      END IF
C
C Reset the original viewport and world coordinates.
C
      CALL PGSVP(XA,XB,YA,YB)
      CALL PGSWIN(WXA,WXB,WYA,WYB)
      CALL PGSCH(OLDCH)
      CALL PGEBUF
      RETURN
      END
c
c ----- END OF grepg_phasewedg.f -----
