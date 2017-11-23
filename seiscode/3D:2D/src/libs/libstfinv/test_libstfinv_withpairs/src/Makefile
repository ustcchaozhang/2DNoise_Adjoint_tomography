# Makefile for Test_libstfinv
#
# Copyright (c) 2011 by Thomas Bohlen (KIT Karlsruhe) and Lisa Rehor (KIT Karlsruhe) 
#
# This file was copied from the DENISE code and adjusted for its use
# in this test programme.
#
# ----
# DENISE is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version. 
# 
# DENISE is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
# ----

#--------------------------------------------------------
# edit here:


CC=gcc

#LFLAGS=-lstfinv -lfourierxx -lfftw3 -laff -lstdc++ 
LFLAGS=-lstfinv -lfourierxx -lfftw3 -laff 


CFLAGS= -Wall

all:	Test_libstfinv
	mv Test_libstfinv ..

.c.o:
	$(CC) -c -I$(LOCINCLUDEDIR) $(CFLAGS) $<


FFT_Test= \
	read_parameters.c \
	Test_libstfinv.c  
	
FFT_Test_OBJ = $(FFT_Test:%.c=%.o)

Test_libstfinv:		$(FFT_Test_OBJ) 
	$(CC) $(CFLAGS) -I$(LOCINCLUDEDIR) -L$(LOCLIBDIR) $(FFT_Test_OBJ) -o Test_libstfinv $(LFLAGS)


clean:
	find . -name "*.o" -exec rm {} \; 
	find . -name "*.c%" -exec rm {} \;
	find . -name "*.bck" -exec rm {} \;
	rm -f ../Test_libstfinv
