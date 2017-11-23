#!/bin/bash
# this is <DL1logger.sh>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2008, 2014 by Thomas Forbriger (BFO Schiltach) 
# 
# start DL1logger with specific path settings
#
# ----
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version. 
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
# ----
# 
# REVISIONS and CHANGES 
#    19/12/2008   V1.0   Thomas Forbriger
#    13/01/2009   V1.1   issue port settings
#    13/11/2009   V1.2   after hardware replacement: uses genuine serial port
#    24/03/2014   V1.3   adjust values for flocke
#    31/03/2014   V1.4   separate data and log path; remove bin type output
# 
# ============================================================================

LOGGERBINARY=$HOME/bin/linux/DL1logger

#DLDEVICE=/dev/ttyUSB0
DLDEVICE=/dev/ttyS0
PORTSETTINGS=1c00:4:dad:a30:3:1c:7f:15:4:0:1:0:11:13:1a:0:12:f:17:16:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0

DATADIR=$HOME/DL1/data

DATAPATH=$DATADIR/%T/%Y/%M/DL1data%Y%M%D.%T
ACTIVEPATH=$DATADIR/%T/DL1current.%T
DATATYPES=ascii

MEMORY=$DATADIR/DL1memory

echo stty -F $DLDEVICE $PORTSETTINGS | /bin/logger -i -t "DL1logger $0" 2>&1
stty -F $DLDEVICE $PORTSETTINGS | /bin/logger -i -t "DL1logger $0" 2>&1
stty -F $DLDEVICE -a | /bin/logger -i -t "DL1logger $0" 2>&1

$LOGGERBINARY -port $DLDEVICE -memory $MEMORY \
  -datapath $DATAPATH -activepath $ACTIVEPATH \
  -datatypes $DATATYPES -toleratewrongtime

# ----- END OF DL1logger.sh ----- 
