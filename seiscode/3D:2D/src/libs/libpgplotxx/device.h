/*! \file device.h
 * \brief PGPLOT device class (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/03/2015
 * 
 * PGPLOT device class (prototypes)
 * 
 * Copyright (c) 2015 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 17/03/2015   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_PGPLOTDEVICE_H_VERSION

#define TF_PGPLOTDEVICE_H_VERSION \
  "TF_DEVICE_H   V1.0   (17-03-2015)"

#include <pgplotxx/basicdevice.h>

namespace pgplot {

/*! \brief Interface provided through device.h
 *
 * \defgroup device_h Interface provided through device.h
 */
/*@{*/

/*! \brief the standard device class
 *
 * This class includes the full pgplot functionality and does not
 * care about interactive or not. It derives directly from \c basic_device
 * and exports the cursor functions.
 *
 * \sa basic_device
 * \sa pgplotxxdemo.cc
 */
class device: public basic_device {
  public:
    //! create a new device
    device(const char* devName): basic_device(devName) { }
    /*! \name interface to PGPLOT cursor routines
     * 
     * Reimplementation of cursor functions to make them public.
     */
    //@{
    /*! \brief read cursor position, with anchor.
     *
     *
     * - If mode=0, the anchor point is ignored and the routine behaves
     *   like PGCURS.
     * - If mode=1, a straight line is drawn joining the anchor point 
     *   and the cursor position.
     * - If mode=2, a hollow rectangle is extended as the cursor is moved,
     *   with one vertex at the anchor point and the opposite vertex at the
     *   current cursor position; the edges of the rectangle are horizontal
     *   and vertical.
     * - If mode=3, two horizontal lines are extended across the width of
     *   the display, one drawn through the anchor point and the other
     *   through the moving cursor position. This could be used to select
     *   a Y-axis range when one end of the range is known.
     * - If mode=4, two vertical lines are extended over the height of
     *   the display, one drawn through the anchor point and the other
     *   through the moving cursor position. This could be used to select an
     *   X-axis range when one end of the range is known.
     * - If mode=5, a horizontal line is extended through the cursor
     *   position over the width of the display. This could be used to select
     *   an X-axis value such as the start of an X-axis range. The anchor point
     *   is ignored.
     * - If mode=6, a vertical line is extended through the cursor
     *   position over the height of the display. This could be used to select
     *   a Y-axis value such as the start of a Y-axis range. The anchor point
     *   is ignored.
     * - If mode=7, a cross-hair, centered on the cursor, is extended over
     *   the width and height of the display. The anchor point is ignored.
     *
     * \return
     *             1 if the call was successful; 0 if the device
     *             has no cursor or some other error occurs.
     * \param mode (input): display mode (0, 1, ..7: see above).
     * \param posn (input): if posn=1, PGBAND attempts to place the cursor
     *             at point (x,y); if posn=0, it leaves the cursor
     *             at its current position. (On some devices this
     *             request may be ignored.)
     * \param xref (input): the world x-coordinate of the anchor point.
     * \param yref (input): the world y-coordinate of the anchor point.
     * \param x (in/out): the world x-coordinate of the cursor.
     * \param y (in/out): the world y-coordinate of the cursor.
     * \param ch (output): the character typed by the user; if the device has
     *             no cursor or if some other error occurs, the value CHAR(0)
     *             [ASCII NUL character] is returned.
     */
    int band(int mode, int posn, float xref, float yref, float *x, float *y,
             char *ch)
      { return(basic_device::band(mode, posn, xref, yref, x, y, ch)); }
    int band(const int& mode, const int& posn, 
             const float& xref, const float& yref, float& x, float& y,
             char& ch)
    { return(this->band(mode, posn, xref, yref, &x, &y, &ch)); }
    /*! \brief read cursor position.
     * \return
     *         1 if the call was successful;
     *         0 if the device has no cursor or some other error occurs.
     * \param x (in/out): the world x-coordinate of the cursor.
     * \param y (in/out): the world y-coordinate of the cursor.
     * \param ch (output): the character typed by the user; if the device has
     *                     no cursor or if some other error occurs, the value
     *                     CHAR(0) [ASCII NUL character] is returned.
     */
    int curs(float *x, float *y, char *ch)
      { return(basic_device::curs(x, y, ch)); }
    int curs(float& x, float& y, char& ch)
    { return(this->curs(&x, &y, &ch)); }
    basic_device& lcur(int maxpt, int *npt, float *x, float *y)
      { basic_device::lcur(maxpt, npt, x, y); return(*this); }
    basic_device& ncur(int maxpt, int *npt, float *x, float *y, int symbol)
      { basic_device::ncur(maxpt, npt, x, y, symbol); return(*this); }
    basic_device& olin(int maxpt, int *npt, float *x, float *y, int symbol)
      { basic_device::olin(maxpt, npt, x, y, symbol); return(*this); }
    //@}
}; // class device

/*@}*/

} // namespace pgplot

#endif // TF_PGPLOTDEVICE_H_VERSION (includeguard)

/* ----- END OF device.h ----- */
