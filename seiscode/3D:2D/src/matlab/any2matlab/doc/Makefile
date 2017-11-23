# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2010 by Daniel Armbruster  
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
# Daniel Armbruster 2010/06/25
#
# REVISIONS and CHANGES 
# 25/06/2010	V1.0	Daniel Armbruster  
# 
# ============================================================================
#

.PHONY: all
all: help

flist: Makefile $(wildcard *.tex)
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: cleanall 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist

# 
###########################################
# Your settings.
############################################
MAIN = any2matlab
IMGDIR = ./pics/
PDFIMAGES = 
EPSIMAGES = 
TABLEDIR = ./tab/
TABLES = 
###############################################
# You don't need to change anything below here
###############################################
#Variablendefinition
LATEX = /usr/bin/latex
PDFLATEX = /usr/bin/pdflatex
ZIP =  /usr/bin/zip -r
PS2PDF = /usr/bin/ps2pdf
DVIPS = /usr/bin/dvips
DVIPSOPTS = 
CONVERT = /usr/bin/convert
TAR = tar -zcf
ECHO = echo
LATEXFLAGS = -interaction=nonstopmode
BIBTEX = bibtex
FEATURES = index abbr bib
DEL = rm -rf

TEXFILES = $(wildcard *.tex)

PICS = $(wildcard $(IMGDIR)/*)


###############################################
#**********************************************

.SUFFIXES:
.SUFFIXES: .pdf .tex .ps .dvi .png .gif .bmp .jpg .eps .fig .csv 
# latex viermal wegen Referenzen laufen lassen
.tex.pdf:
	$(PDFLATEX) $(LATEXFLAGS) $<
	$(PDFLATEX) $(LATEXFLAGS) $<
	$(PDFLATEX) $(LATEXFLAGS) $<
.tex.dvi:
	$(LATEX) $(LATEXFLAGS) $<
	$(LATEX) $(LATEXFLAGS) $<
	$(LATEX) $(LATEXFLAGS) $<
.dvi.ps:
	$(DVIPS) $(DVIPSOPTS) $<
.fig.eps:
	fig2dev -L eps $< $@
.gif.png:
	$(CONVERT) $< $@
.bmp.png:
	$(CONVERT) $< $@
.jpg.png:
	$(CONVERT) $< $@
.gif.eps:
	$(CONVERT) $< $@
.bmp.eps:
	$(CONVERT) $< $@
.jpg.eps:
	$(CONVERT) $< $@
.png.eps:
	$(CONVERT) $< $@
.eps.pdf:
	epstopdf $< 
.ps.pdf:
	ps2pdf $< $@
.eps.png:
	$(CONVERT) $< $@
.fig.pdf:
	fig2dev -L eps $< $@.eps
	epstopdf $@.eps -o=$@
	rm $@.eps
.csv.tex:
	cat $< | sed "s/;/\t\&\t/g"|sed 's/$$/\t\\\\/g'|sed 's/\"//g' > $@

#-------------------------
# pdflatex, latex
pdf: 
	rm -f $(MAIN).pdf
	make $(MAIN).pdf
pdfimages:
	cd $(IMGDIR); make -f ../[mM]akefile $(PDFIMAGES)
ps: epsimages
	rm -f $(MAIN).ps
	make $(MAIN).ps
	ps2pdf $(MAIN).ps 
dvi: epsimages 
	make $(MAIN).dvi
epsimages:
#	cd $(IMGDIR); make -f ../[mM]akefile $(EPSIMAGES)

#------------------------
# literatur-, index-, abkuerzungsverzeichnis
index: $(MAIN).ind $(MAIN).ilg
abbr: $(MAIN).nls
bib: $(MAIN).blg $(MAIN).bbl

# Vorbereitung
$(MAIN).idx $(MAIN).nlo $(MAIN).aux: $(MAIN).tex
	$(LATEX) $(LATEXFLAGS) $(MAIN).tex
				 
# Index/Stichwortverzeichnis (Regel index)
$(MAIN).ind $(MAIN).ilg: $(MAIN).tex $(MAIN).idx
	$(MAKEINDEX) $(MAIN).idx

# Abkuerzungsverzeichnis (Regel abbr)
$(MAIN).nls: $(MAIN).tex $(MAIN).nlo
	$(MAKEINDEX) $(MAIN).nlo -s nomencl.ist -o $(MAIN).nls

# Literaturverzeichnis (Regel bib)
$(MAIN).blg $(MAIN).bbl: $(MAIN).tex $(MAIN).bib $(MAIN).aux
	$(BIBTEX) $(MAIN)

#--------------------------
# archiv
ARCHIVFILES = $(TEXFILES) $(PICS) 
ARCHIVFILES += [mM]akefile

tararchiv: $(MAIN).tar.gz
$(MAIN).tar.gz: $(ARCHIVFILES)
	$(TAR) $(MAIN).tar.gz $(ARCHIVFILES)
ziparchiv: $(MAIN).zip
$(MAIN).zip: $(ARCHIVFILES)
	$(ZIP) $(MAIN).zip $(ARCHIVFILES)

#-------------------------
# show
pdfshow: pdf
	@evince $(MAIN).pdf &

dvishow: dvi
	@xdvi $(MAIN).pdf &

acroshow: pdf
	@acroread $(MAIN).pdf &

psshow: ps
	@gv $(MAIN).ps &

#------------------------
# table
table: 
	cd $(TABLEDIR); make -f ../[mM]akefile $(TABLES)

#------------------------
#clean
# Temporaere Dateien loeschen
clean: cleantemp

cleantemp:
	$(DEL) $(MAIN).aux $(MAIN).bbl $(MAIN).bit $(MAIN).blg $(MAIN).dvi $(MAIN).glo\
$(MAIN).idx $(MAIN).ilg $(MAIN).ilg $(MAIN).ind $(MAIN).lof $(MAIN).log $(MAIN).lot\
$(MAIN).nlo $(MAIN).nls $(MAIN).out $(MAIN).toc $(MAIN).snm $(MAIN).nav
							 
# Aux-Dateien loeschen (auch von anderen Dateien)
cleanaux:
	$(DEL) *.aux

cleanlatex:
	rm -f $(MAIN).toc $(MAIN).out $(MAIN).aux

cleanbackup:
	@rm -f *~ *.bak

cleanall: cleanbackup cleantemp

#-------------------------
# help
.PHONY: help
help:
	@$(ECHO) "************************************************************"
	@$(ECHO) "                 LATEX-makefile"
	@$(ECHO)	"author:    Daniel Armbruster"
	@$(ECHO) "email:     dani.armbruster@gmx.de"
	@$(ECHO) "created:   12.12.2009"
	@$(ECHO) "modified:  17.06.2016"
	@$(ECHO) "************************************************************"
	@$(ECHO) "------------------------------------------------------------"
	@$(ECHO) "Hilfe - fuer LATEX-makefile"
	@$(ECHO) "------------------------------------------------------------"
	@$(ECHO) "pdf:          Erstellen eines PDF-Dokumentes"
	@$(ECHO) "tararchiv:    Erstellen eines *.tar-Archivs"
	@$(ECHO) "ziparchiv:    Erstellen eines *.zip-Archivs"
	@$(ECHO) "pdfshow:      Anzeigen des PDF-Dokumentes mit xpdf"
	@$(ECHO) "acroshow:     Anzeigen des PDF-Dokumentes mit acroread"
	@$(ECHO) "psshow:       Anzeigen des PS-Dokumentes mit evince"
	@$(ECHO) "table:        Konverter fuer Tabellen -> *.csv nach *.tex"
	@$(ECHO) "cleanbackup:  Löschen der Backup-Dateien"	
	@$(ECHO) "cleantemp:    Löschen der temporären Dateien"	
	@$(ECHO) "cleanaux:     Löschen aller *.aux Dateien im Verzeichnis"	
	@$(ECHO) "cleanall:     Ausführen von cleantemp und cleanbackup"
	@$(ECHO) "help:         Hilfe"
	@$(ECHO) 
	@$(ECHO) "------------------------------------------------------------"
	@$(ECHO) "Info:"
	@$(ECHO) "------------------------------------------------------------"
	@$(ECHO) "Hauptdatei: $(MAIN).tex"
	@$(ECHO) "zuletzt verwendete Bilder: $(PDFIMAGES)" 
	@$(ECHO)	"zuletzt konvertierte Tabellen: $(TABLES)"
	@$(ECHO)
	@$(ECHO) "------------------------------------------------------------" 
	@$(ECHO) "Bemerkungen:"
	@$(ECHO) "------------------------------------------------------------"
	@$(ECHO) "1. Bilder-Dateiname ohne Endung in *.tex-file angeben."
	@$(ECHO) "2. Die Bilder-Dateien im gewünschten Format in die makefile"  
	@$(ECHO) "   eintragen."  
	@$(ECHO) "3. Die Tabellen muessen als Feldtrenner ein ';'-Zeichen und" 
	@$(ECHO) "   und als Texttrenner ein '\"'-Zeichen haben."
	@$(ECHO) 

# ----- END OF Makefile ----- 
