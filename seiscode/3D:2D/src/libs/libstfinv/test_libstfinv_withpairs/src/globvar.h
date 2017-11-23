/*------------------------------------------------------------------------
 *  globvar.h 
 *
 *  Copyright (c) 2011 by Thomas Bohlen (KIT Karlsruhe) and Lisa Rehor (KIT Karlsruhe) 
 *
 *  This file was copied from the DENISE code and adjusted for its use
 *  in this test programme.
 *
 * ----
 * DENISE is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version. 
 * 
 * DENISE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *  ----------------------------------------------------------------------*/

/* definition of global variables used in the finite difference programs*/
/* generally, for the names of the global variables
   uppercase letters are used */

int ntr;
char FILE_OBS[STRING_SIZE], FILE_SYN[STRING_SIZE], FILE_CONV[STRING_SIZE], FILE_STF[STRING_SIZE], FILE_ADD_CONV[STRING_SIZE], FILE_ADD[STRING_SIZE];
char stfinv_par[STRING_SIZE]; 
