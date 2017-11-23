/*! \file fngnuplotplot.cc
 * \brief create a gnuplot plot file (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 14/02/2012
 * 
 * create a gnuplot plot file (implementation)
 * 
 * Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * This file is part of the conv/many suite.
 *
 * The conv/many suite is free software; you can redistribute it and/or modify
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
 *  - 14/02/2012   V1.0   Thomas Forbriger
 *  - 02/05/2012   V1.1   plot gap and break indicators too
 *  - 08/01/2013   V1.2   report analysis periond in plot title
 * 
 * ============================================================================
 */
#define TF_FNGNUPLOTPLOT_CC_VERSION \
  "TF_FNGNUPLOTPLOT_CC   V1.2"

#include <gapfunctions.h>
#include <aff/functions/max.h>

void gnuplotplot(std::ostream& os,
                 const std::string& psname,
                 const CompletenessBins& cb,
                 const Tvecofgaps& vog)
{
  // find limits
  Tvecofgaps::const_iterator I=vog.begin();
  GapSeries::Tgapseries total(0,cb.nbins()-1);
  total=0;
  I=vog.begin();
  while (I!=vog.end())
  {
    GapSeries gs=seriesofmissingsamples(*I, cb);
    for (unsigned j=0; j<cb.nbins(); ++j)
    {
      total(j)+=gs.gapseries(j);
    }
    ++I;
  }
  int maxval=aff::func::max(total);
  bool gapsexist=(maxval>0);
  maxval = maxval > 1 ? maxval : 1;

  // are there gaps or breaks?
  unsigned int ntotalbreaks=0;
  for (Tvecofgaps::const_iterator I=vog.begin(); I!=vog.end(); ++I)
  {
    Gapsummary summary=I->summarize();
    ntotalbreaks += summary.nbreaks;
  } // for (Tvecofgaps::const_iterator I=vog.begin(); I!=vog.end(); ++I)
  bool breaksexist=(ntotalbreaks>0);

  // create header
  os << "set terminal postscript color solid\n";
  os << "set output \"" << psname << "\"\n";
  os << "set title \"data gaps in bins of "
    << cb.binsize().timestring() << "\\n"
    << "between "
    << cb.earliest().timestring().substr(4)
    << " and "
    << cb.latest().timestring().substr(4)
    << "\"\n";
  os << "set ylabel \"number of missing samples\"\n";
  os << "set timefmt \"%Y/%m/%d-%H:%M:%S\"\n";
  os << "set xdata time\n";
  libtime::TRelativeTime dur=cb.latest()-cb.earliest(); 
  if (dur > libtime::TRelativeTime(5))
  {
    os << "set format x \"%d.%m.%y\"\n";
  }
  else if (dur > libtime::TRelativeTime(1))
  {
    os << "set format x \"%d.%m.%y %H\"\n";
  }
  else
  {
    os << "set format x \"%d.%m.%y %H:%M\"\n";
  }
  os << "set boxwidth 0.9 relative\n";
  os << "set style fill solid 0.2 border -1\n";
  os << "set xtic rotate by -45 scale 0\n";
  os << "set grid\n";
  os << "set key outside enhanced horizontal\n";
  os << "set auto x\n";
  os << "set yrange [" << -0.05 * maxval << ":" << 1.05*maxval << "]\n";
  os << "plot ";

  bool printlabels=(cb.nbins()<20);

  // setup plot command
  I=vog.begin();
  while (I!=vog.end())
  {
    os << "\'-\' using 1:3 title \"" << I->ID.ID << "\" with boxes";
    if (printlabels)
    {
      os << ", \\\n";
      os << "  \'-\' using 1:3:2 with labels center offset 0,-0.8 notitle";
    }
    ++I;
    if (I!=vog.end() || breaksexist || gapsexist)
    {
      os << ", \\\n  ";
    }
    else
    {
      os << "\n";
    }
  }
  if (gapsexist)
  {
    os << "  \'-\' using 1:2 title \"gaps\" with points ps 2 pt 13 lt 1";
    if (breaksexist)
    {
      os << ", \\\n  ";
    }
    else
    {
      os << "\n";
    }
  }
  if (breaksexist)
  {
    os << "  \'-\' using 1:2 title \"breaks\" with points ps 1 pt 8 lt 3\n";
  }

  // setup data
  // ==========
  //
  // number of missing samples
  // -------------------------
  unsigned int nperstream=1;
  if (printlabels) { nperstream=2; }
  I=vog.begin();
  while (I!=vog.end())
  {
    GapSeries gs=seriesofmissingsamples(*I, cb);
    for (unsigned int i=0; i<nperstream; ++i)
    {
      for (unsigned int j=0; j<cb.nbins(); ++j)
      {
        libtime::TAbsoluteTime time=cb.bin(j)+cb.binsize()/2;
        os << time.hierarchicalstring()
          << " " << gs.gapseries(j) << " " << total(j) << "\n";
      }
      os << "e\n";
    }
    for (unsigned int j=0; j<cb.nbins(); ++j)
    {
      total(j)-=gs.gapseries(j);
    }
    ++I;
  }
  
  //
  // gap indicators
  // --------------
  if (gapsexist)
  {
    I=vog.begin();
    while (I!=vog.end())
    {
      GapSeries gs=seriesofmissingsamples(*I, cb);
      for (unsigned int j=0; j<cb.nbins(); ++j)
      {
        if (gs.gapseries(j)>0)
        {
          libtime::TAbsoluteTime time=cb.bin(j)+cb.binsize()/2;
          os << time.hierarchicalstring() << " 0\n";
        }
      }
      ++I;
    }
    os << "e\n";
  }
   
  //
  // gap break indicators
  // --------------------
  if (breaksexist)
  {
    I=vog.begin();
    while (I!=vog.end())
    {
      for (Gapsofstream::Tvecofgap::const_iterator J=I->gap.begin(); 
           J!=I->gap.end(); ++J)
      {
        if (J->isbreak())
        {
          libtime::TAbsoluteTime time=J->first;
          os << time.hierarchicalstring() << " 0\n";
        }
      }
      ++I;
    }
    os << "e\n";
  }
}

/* ----- END OF fngnuplotplot.cc ----- */
