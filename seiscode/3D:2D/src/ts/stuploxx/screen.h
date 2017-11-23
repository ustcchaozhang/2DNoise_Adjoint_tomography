/*! \file screen.h
 * \brief all stuff to handle a full plot screen (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/01/2008
 * 
 * all stuff to handle a full plot screen (prototypes)
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
 * REVISIONS and CHANGES 
 *  - 28/01/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef STUPLO_SCREEN_H_VERSION

#define STUPLO_SCREEN_H_VERSION \
  "STUPLO_SCREEN_H   V1.0   "

#include"utilitystructures.h"
#include"panel.h"
#include"scaling.h"
#include"windowpanel.h"

namespace stuplo {

  /*----------------------------------------------------------------------*/

  /*! \brief Screen base class
   *
   * A screen knows to to draw a specific layout on a PGPLOT device and how to
   * handle interactive cursor requests. This is the largest plotting
   * framework. All plotting in stuploxx is done by calling member functions
   * of a class derived from screen. This is done through a common interface
   * that provides member functions plot and cursor. Data itself is passed to
   * the class thorugh the constructor.
   *
   * Scaling is set upon construction of the instance of the class. Sclaing
   * values are stored in a handle of type THScaling. This ensures
   * synchronization of scaling values in each unit of the program. Since the
   * handle instance is created prior to the screen instance and since the
   * scaling handle is passed to the screen, it scaling values will remain
   * even is the screen is deleted. This is necessary in cases where data is
   * reread and a second screen instance is created. In this case the screen
   * instance will receive a scaling handle with preset value. The screen
   * classes have to handle these cases indicated by the member value
   * "initialized" in struct Scaling.
   */
  class Screen {
    public:
      //! receive data upon creation
      Screen(const PanelVector& pv,
             const THScaling& hs,
             const PGstyle& st): 
        Mpv(pv), Mscaling(hs), Mstyle(st) { }
      //! needs a virtual destructor
      virtual ~Screen() { }
      //! create a plot on a specific PGPLOT-device
      virtual void plot(pgplot::basic_device& dev) const =0;
      /*! \brief create a plot on a specific PGPLOT-device
       * \return true if plot should be redrawn
       */
      virtual bool cursor(pgplot::device& dev,
                          const char& curval,
                          float& x, float&y) =0;
    protected:
      //! keep my data here
      PanelVector Mpv;
      //! my handle to the scaling struct
      THScaling Mscaling;
      //! PGPLOT style options
      stuplo::PGstyle Mstyle;
  }; // class Screen

  /*======================================================================*/

  /*! \brief Chart Stepper plot mode
   *
   * For interface details see base class Screen
   */
  class ChartStepperScreen: public Screen {
    public:
      typedef Screen Tbase;
      //! receive data upon creation
      ChartStepperScreen(const PanelVector& pv, 
                         const THScaling& hs,
                         const PGstyle& st,
                         const bool& debug=false); 
      //! needs a virtual destructor
      virtual ~ChartStepperScreen() { }
      //! create a plot on a specific PGPLOT-device
      virtual void plot(pgplot::basic_device& dev) const;
      //! create a plot on a specific PGPLOT-device
      virtual bool cursor(pgplot::device& dev,
                          const char& curval,
                          float& x, float&y);
    private:
      //! initialize time axis scaling values
      void inittime();
      //! initialize scaling values
      void initscaling();
      //! adjust scaling values
      void adjustscaling();
      //! time axis panel
      mutable TimeAxisWindowPanel Mtimeaxis;
      //! title panel
      TextWindowPanel Mtitle;
      //! vector type for tiled panel for time series plots
      typedef std::vector<ChartStepWindowPanel> Twindowpanelvector;
      //! tiled panel for time series plots
      Twindowpanelvector Mwindowpanelvector;
      //! debug mode
      bool Mdebug;
      //! viewport for all graph panels
      pgplot::Trect Mgraphvp;
  }; // class Screen

} // namespace stuplo

#endif // STUPLO_SCREEN_H_VERSION (includeguard)

/* ----- END OF screen.h ----- */
