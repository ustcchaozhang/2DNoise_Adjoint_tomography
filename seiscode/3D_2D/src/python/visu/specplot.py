#!/usr/bin/env python2
## @file specplot.py
# @brief Plot power spectras.
# 
# -----------------------------------------------------------------------------
# 
# $Id: specplot.py 616 2012-05-05 00:03:30Z uhcnl $
# @author Daniel Armbruster
# \date 08/02/2012
# 
# Purpose: Plot the power spectrum.
#
# ----
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# ----
# 
# Copyright (c) 2012 by Daniel Armbruster
# 
# REVISIONS and CHANGES 
# 08/02/2012    V0.1      Daniel Armbruster
# 04/05/2012    V0.2      Stable for TFSoftware.
# 12/05/2012    V0.3      Plot multiple datafiles; provide legend;
#                         provide setting legfontsize
# 23/05/2012    V0.4      renamed to specplot
# 15/08/2012    V0.4.1    verbosity correction
# 23/08/2012    V0.4.2    semilogx and semilogy options introduced.
# 
# =============================================================================
"""
Plot spectrum using python's matplotlib.
"""

import sys
import os
import getopt
import string
import matplotlib
import matplotlib.pyplot as plt

__version__ = "V0.4.2"
__subversion__ = "$Id: specplot.py 616 2012-05-05 00:03:30Z uhcnl $"
__license__ = "GPLv2"
__author__ = "Daniel Armbruster"
__copyright__ = "Copyright (c) 2012 by Daniel Armbruster"

# -----------------------------------------------------------------------------
class Error(Exception):

  def __init__(self, msg=""):
    self.msg = str(msg)

  def display(self):
    sys.stderr.write("specplot (ERROR): " + self.msg + "\n")

class Usage(Error):

  def display(self):
    usage_text = "Version: "+__version__+"\nLicense: "+__license__+ \
      "\n"+__subversion__+"\nAuthor: "+__author__+ """
 Usage: specplot [-v|--verbose] [--overwrite] [--oformat arg] [--outbase arg] 
                  [-t|--title arg] [--orientation arg] [--nologx] [--nology]
                  [--nologlog] [-g|--grid]
                  [--xlabel arg] [--ylabel arg] [--xlim val:val]
                  [--ylim val:val]
                  [--legend arg] [--legpos arg] [--legfontsize arg]
                  DATAFILE [DATAFILE [...]]
    or: specplot -h|--help"""
    if 0 != len(self.msg):
      sys.stderr.write("specplot: " + self.msg + "\n")
    sys.stderr.write(usage_text)
    if 0 != len(self.msg):
      sys.stderr.write("\n")

# -----------------------------------------------------------------------------
def help():
  """
  Printing helptext to stdout.
  """
  help_text = \
  """
-------------------------------------------------------------------------------
 -v|--verbose         Be verbose.
 -h|--help            Display this help.
 --overwrite          Overwrite output file if existing.
 --outbase arg        Basename of the output file. If not specified there will
                      be plotted an interactive plot.
 --oformat arg        Format of the output file.
                      Supported formats: emf, eps, jpeg, jpg, pdf, png,
                      ps (default), raw, rgba, svg, svgz, tif, tiff.

 Plot appearance:
 -t|--title arg       Title of the plot.
 --orientation arg    Orientation of the plot. Valid arguments are 'landscape'
                      and 'portrait' (default).
 -g|--grid            Enable grid.
 --xlabel arg         x-axis label
 --ylabel arg         y-axis label  
 --xlim val:val       Set fixed x-limits of the current axis.
 --ylim val:val       Set fixed y-limits of the current axis. 
 --nologx             Disable logscale of x-axis.
 --nology             Disable logscale of y-axis.
 --nologlog           Disable loglog scale of axis (same as '--nosemilogx' and
                      '--nosemilogy').
 --fit                Fit plot to page (only is saving plot to file).
 --legend arg         Specify legend arguments.
                      arg is a list of whitespace separated strings. If it
                      isn't desired to print the legend for a specific spectrum
                      just pass '--'. By default plotting a legend is enabled.
                      To disable a legend use the '--legpos' commandline
                      argument. Additionally the legend position can be
                      specified using the '--legpos' commandline argument.
 --legpos arg         Set legend position or disable legend.
                      Valid arguments are 'unset' to disable legend, 'best',
                      'upper right' (default), 'upper left', 'lower left',
                      'lower right', 'right', 'center left', 'center right',
                      'lower center', 'upper center' and 'center'.
 --legfontsize arg    Set the fontsize of legend text. (arg of int type)
                      
 DATAFILE(s)          File(s) which contain(s) the data (two columns).\n"""
  Usage().display()
  sys.stdout.write(help_text)

# -----------------------------------------------------------------------------
def main(argv=None):
  if argv is None:
    argv = sys.argv
  try:
    try:
      opts, args = getopt.getopt(argv[1:], "hvgt:", ["help", "verbose", \
          "overwrite", "title=", "orientation=", "outbase=", "oformat=", \
          "xlabel=", "ylabel=", "nologlog", "grid", "fit", "xlim=", "ylim=", \
          "legend=", "legpos=", "legfontsize=", "nologx", "nology"])
    except getopt.GetoptError as err:
      raise Usage(err.msg)
    # fetch arguments
    verbose = False
    overwrite = False
    plotTitle = ""
    orientation = "portrait"
    oformat = "ps"
    outbase = ""
    xlabel = ""
    ylabel = ""
    nologlog = False
    nologx = False
    nology = False
    grid = False
    fit = None
    xlim = []
    ylim = []
    legargs = args
    legpos = 'upper right'
    legfontsize = "8"

    for opt, arg in opts:
      if opt in ("-v", "--verbose"):
        verbose = True
      elif opt in ("-h", "--help"):
        help()
        sys.exit()
      elif opt in ("--overwrite"):
        overwrite = True
      elif opt in ("--oformat"):
        oformat = arg
      elif opt in ("--orientation"):
        orientation = arg
        if orientation not in ("portrait", "landscape"):
          raise Usage("Invalid 'orientation' argument.")
      elif opt in ("--outbase"):
        outbase = arg
      elif opt in ("-t", "--title"):
        plotTitle = arg
      elif opt in ("--xlabel"):
        xlabel = arg
      elif opt in ("--ylabel"):
        ylabel = arg
      elif opt in ("-g", "--grid"):
        grid = True
      elif opt in ("--nologlog"):
        nologlog = True
      elif opt in ("--nologx"):
        nologx = True
      elif opt in ("--nology"):
        nology = True
      elif opt in ("--fit"):
        fit = "tight"
      elif opt in ("--xlim"):
        try:
          xlim = [float(x) for x in arg.split(":")]
          if 2 != len(xlim):
            raise
        except:
          raise Usage("Invalid 'xlim' argument.")
      elif opt in ("--ylim"):
        try:
          ylim = [float(y) for y in arg.split(":")]
          if 2 != len(ylim):
            raise
        except:
          raise Usage("Invalid 'ylim' argument.")
      elif opt in ("--legend"):
        legargs = arg.split()
      elif opt in ("--legpos"):
        legpos = arg
      elif opt in ("--legfontsize"):
        try:
          legfontsize = int(arg)
        except:
          raise Usage("Invalid 'legfontsize' argument.")
      else:
        raise Usage("Unhandled option chosen.")

    if len(args) < 1:
      raise Usage("Invalid arguments.")

    if len(outbase) and os.access(outbase+"."+string.lower(oformat), os.F_OK) \
        and not overwrite:
      raise Usage(outbase+"."+string.lower(oformat)+" already exists.")

    # save data in 2D lists
    x_coords = []
    y_coords = []
    # read data from datafiles
    for datafile in args:
      if not os.access(datafile, os.F_OK):
        raise Error("Invalid path to file '{0}'.".format(datafile))
      if 0 == os.stat(datafile).st_size:
        raise Error("Given DATAFILE '{0}' is an empty file.".format(datafile))
      if verbose:
        sys.stdout.write( \
          "specplot: Reading file '{0}' ... \n".format(datafile))
      try:
        x = []
        y = []
        file = open(datafile, 'r')
        for line in file:
          col1,col2 = line.split()
          x.append(col1)
          y.append(col2)
      except IOError as err:
        raise Error( \
          "[Errno "+str(err.errno)+"] "+err.strerror+": "+err.filename)
      else:
        file.close()
      # check data consistency
      if len(x) == len(y):
        x_coords.append(x)
        y_coords.append(y)
      else:
        raise Error( \
          "Data not consistent in file '{0}' not consistent.".format(datafile))

    # generate plot
    if verbose:
      sys.stdout.write("specplot: Preparing plot ... \n")

    fig = plt.figure()
    ax = fig.add_subplot(111)
    # x and y limits
    if 2 == len(xlim):
      plt.xlim(xlim[0], xlim[1])
    if 2 == len(ylim):
      plt.ylim(ylim[0], ylim[1])
    # plot data
    legend_handlers = []
    for x, y in zip(x_coords, y_coords):
      legend_handlers.append(tuple(plt.plot(x, y)))
    # remove handlers where printing the legend isn't desired
    indices = [i for i, x in enumerate(legargs) if "--" == x]
    for i in indices:
      del legend_handlers[i]
      del legargs[i]

    plt.grid(grid)
    # using logscale if desired
    if nologx and nology:
      nologlog = True

    if nology and not nologlog:
      if verbose:
        sys.stdout.write("specplot: Using log scaling for x-axis ... \n")
      plt.semilogx()
    elif nologx and not nologlog:
      if verbose:
        sys.stdout.write("specplot: Using log scaling for y-axis ... \n")
      plt.semilogy()
    elif not nologlog:
      if verbose:
        sys.stdout.write("specplot: Using loglog scaling  ... \n")
      plt.loglog()
    else:
      if verbose:
        sys.stdout.write("specplot: Disable logscale ... \n")

    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.title(plotTitle)
    # legend specific
    if 'unset' != legpos:
      try:
        if verbose:
          sys.stdout.write("specplot: Plotting legend ...\n")
        ax.legend(legend_handlers, legargs, loc=legpos, \
          prop={"size":legfontsize})
      except:
        raise Error("While plotting legend.")

    if len(outbase):
      if verbose:
        sys.stdout.write( \
          "specplot: Saving plot to file '{0}' ... \n".format( \
          outbase+"."+string.lower(oformat))) 
      try:
        plt.savefig(outbase+"."+string.lower(oformat), \
          orientation=orientation, format=oformat, bbox_inches=fit)
      except ValueError as err:
        raise Usage(err.message)
    else:
      if verbose:
        sys.stdout.write("specplot: Generating interactive plot ... \n")
      plt.show()

  except Error as err:
    err.display()
    return 2
  except Usage as err:
    err.display()
    return 2

# -----------------------------------------------------------------------------
if __name__ == "__main__":
  sys.exit(main())

# ----- END OF specplot.py ----- 
