/*! \file thiesdl1file.cc
 * \brief handle a ThiesDL1 data file (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/09/2011
 * 
 * handle a ThiesDL1 data file (implementation)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 13/09/2011   V1.0   Thomas Forbriger
 *  - 31/10/2012   V1.1   distinguish warning and critical warning; the latter
 *                        for duplicate sample values being at variance
 *  - 07/11/2012   V1.2   add sample values for duplicate entries
 *  - 07/01/2015   V1.3   
 *                        modifications applied to File::put() function:
 *                        - accept duplicate time stamps as part of normal
 *                          operation; this is consistent with the behaviour
 *                          of DL1logger as implemented on 20/03/2014; it is
 *                          further consistent with the tested behaviour of
 *                          the ThiesDL1 logger (see note below).
 *                        - add sample values for duplicate time stamps
 *                        - tr-mode (input stream tolerates redundant samples)
 *                          now suppresses a notice-message which otherwise
 *                          would be output to the terminal in cases of
 *                          duplicate time stamps
 *  - 08/07/2016   V1.4   thof: 
 *                        - make correct use of new DATRW_report_assert
 *                        - make correct use of new DATRW_nonfatal_assert
 * 
 * ============================================================================
 * A statement of Volker König at ThiesClima regarding the duplicate entries
 * in the data files is added at the end of this source file.
 * ============================================================================
 * Since the statement by Volker König is quite unsatisfactory, Peter Duffner
 * and Thomas Forbriger on 07.11.2012 checked Thies DL1 recordings of several
 * month against manual readings taken directly at the display of the Thies
 * DL1 logger by Peter Duffner for cases where duplicate samples are present
 * in the recorded data. The result is, that the values provided on the data
 * loggers display are obtained, if data values for lines with duplicate time
 * value are added to form the final value for the respective minute.
 * ============================================================================
 */
#define DATRW_THIESDL1FILE_CC_VERSION \
  "DATRW_THIESDL1FILE_CC   V1.4"

#include <datrwxx/thiesdl1file.h>
#include <datrwxx/thiesdl1line.h>
#include <datrwxx/error.h>
#include <aff/seriesoperators.h>

namespace datrw {

  namespace thiesdl1 {

    const char* const precipitationID="WR1";

    const libtime::TRelativeTime dl1samplinginterval(0,0,1);

    void ExceptionRecordWindow::report() const {
      this->Exception::report();
      this->my_report();
    } // void ExceptionRecordWindow::report() const

    /*----------------------------------------------------------------------*/

    void ExceptionRecordWindow::my_report() const {
      std::cerr  << "  earliest in expected window: " <<
        this->Mearliest.timestring();
      std::cerr  << "  latest in expected window: " <<
        this->Mlatest.timestring();
      std::cerr  << "  DL1 data line: " << this->Mdataline;
    } // void ExceptionRecordWindow::my_report() const

    /*======================================================================*/

#define DL1_rcassert( C, M, E, L, N ) \
    if (!(C)) { throw( thiesdl1::ExceptionRecordWindow( M , __FILE__, \
                                                   __LINE__, #C, \
                                                   E, L, N)); }

    /*======================================================================*/

    void FileHeader::clear()
    {
      this->lines.lines.clear();
      this->expectedinitialdataline.clear();
      this->expectedfinaldataline.clear();
      this->initialdataline.clear();
      this->readsuccessfully=false;
    } // void FileHeader::clear()

    /*----------------------------------------------------------------------*/

    FileHeader readheader(std::istream & is)
    {
      FileHeader retval;
      retval.readsuccessfully=false;
      struct Found {
        Found(): 
          initial(false),
          final(false),
          earliest(false),
          latest(false),
          creation(false)
        { }
        bool initial, 
             final, 
             earliest, 
             latest, 
             creation;
      }; // Found
      Found found;
      std::string line;
      DATRW_assert(getline(is, line), "ERROR: reading file");
      while (line.substr(0,2) == "# ")
      {
        retval.lines.append(line.substr(2));
        if (line.substr(2,15) == "earliest date: ")
        {
          retval.earliestdate=
            libtime::TAbsoluteTime(line.substr(27,4)+":"+
                                   line.substr(24,2)+":"+
                                   line.substr(21,2)+":"+
                                   line.substr(32,15));
          // std::cout << retval.earliestdate.timestring() << std::endl;
          found.earliest=true;
        }
        else if (line.substr(2,15) == "latest date:   ")
        {
          retval.latestdate=
            libtime::TAbsoluteTime(line.substr(27,4)+":"+
                                   line.substr(24,2)+":"+
                                   line.substr(21,2)+":"+
                                   line.substr(32,15));
          // std::cout << retval.latestdate.timestring() << std::endl;
          found.latest=true;
        }
        else if (line.substr(2,15) == "creation date: ")
        {
          retval.creationdate=
            libtime::TAbsoluteTime(line.substr(27,4)+":"+
                                   line.substr(24,2)+":"+
                                   line.substr(21,2)+":"+
                                   line.substr(32,15));
          // std::cout << retval.creationdate.timestring() << std::endl;
          found.creation=true;
        }
        else if (line.substr(2,15) == "initial line:  ")
        {
          retval.expectedinitialdataline=line.substr(17);
          // std::cout << retval.expectedinitialdataline << std::endl;
          found.initial=true;
        }
        else if (line.substr(2,15) == "final line:    ")
        {
          retval.expectedfinaldataline=line.substr(17);
          // std::cout << retval.expectedfinaldataline << std::endl;
          found.final=true;
        }
        // read next line
        DATRW_assert(getline(is, line), "ERROR: reading file");
      } // while (line.substr(0,2) == "# ")
      DATRW_assert(found.initial &&
                   found.final &&
                   found.earliest &&
                   found.latest &&
                   found.creation,
                   "ERROR: could not extract full header data");
      DATRW_report_assert(line == retval.expectedinitialdataline,
                          "initial data line is not matsching"
                          "\n    expected: " 
                          << retval.expectedinitialdataline <<
                          "\n    found:    " 
                          << line);
      DATRW_assert(line == retval.expectedinitialdataline,
                   "ERROR: missing expected initial data line");
      DATRW_assert(retval.earliestdate<=retval.latestdate,
                   "ERROR: inconsistent date values in data header");
      DATRW_assert(retval.latestdate<=retval.creationdate,
                   "ERROR: inconsistent date values in data header");
      retval.initialdataline=line;
      retval.readsuccessfully=true;
      return(retval);
    } // FileHeader readheader(std::istream & is)

    /*----------------------------------------------------------------------*/

    unsigned int FileHeader::nsamples() const
    {
      DATRW_assert(this->readsuccessfully,
                   "FileHeader::nsamples: Data is not set ready");
      return((this->latestdate-this->earliestdate)/dl1samplinginterval);
    } // unsigned int FileHeader::nsamples() const

    /*----------------------------------------------------------------------*/

    sff::WID2 FileHeader::wid2line() const
    {
      DATRW_assert(this->readsuccessfully,
                   "FileHeader::wid2line: Data is not set ready");
      sff::WID2 retval;
      retval.date=this->earliestdate+(dl1samplinginterval/2);
      retval.dt=libtime::time2double(dl1samplinginterval);
      retval.nsamples=(this->nsamples());
      retval.station=expectedfinaldataline.substr(12,5);
      retval.instype=expectedfinaldataline.substr(21,5);
      retval.channel=precipitationID;
      return(retval);
    } // sff::WID2 FileHeader::wid2line() const

    /*======================================================================*/

    void File::readwithheader(std::istream& is)
    {
      Mheader=readheader(is);
      this->read(is, Mheader);
    } // void File::readwithheader(std::istream& is)

    /*----------------------------------------------------------------------*/

    void File::read(std::istream& is, const FileHeader& header)
    {
      Mheader=header;
      DATRW_assert(Mreadyforreading,
                   "ERROR: File container is not empty");
      Mreadsuccessfully=false;
      Mreadyforreading=false;
      libtime::TRelativeTime fileduration=Mheader.latestdate-
        Mheader.earliestdate;
      DATRW_nonfatal_assert(!Mbetolerantagainstwrongtime,
                            fileduration<libtime::TRelativeTime(5),
                            "Duration of file is larger than 5 days!\n"
                            "earlist sample: "
                            << Mheader.earliestdate.timestring()
                            << "\n"
                            << "latest sample: "
                            << Mheader.latestdate.timestring()
                            << "\n");
      // prepare sample container
      Mnsamples=Mheader.nsamples();
      Miseries=Tiseries(Mnsamples); 
      Miseries=0; 
      Mfilled=aff::Series<bool>(Mnsamples); 
      Mfilled=false; 
      // read samples
      bool hot=true;
      while (hot && is.good())
      {
        std::string line;
        DATRW_assert(getline(is, line), "ERROR: reading file");
        if (line==Mheader.expectedfinaldataline)
        {
          hot=false;
        }
        else
        {
          DataLine dataline(line);
          this->put(line);
        }
      } // while (hot && is.good())
      DATRW_assert(!hot, "missed expected last line of data file");
      Mreadsuccessfully=true;
    } // void File::read(std::istream& is)

    /*----------------------------------------------------------------------*/

    void File::clear()
    {
      Mreadyforreading=true;
      Mfoundunexpecteddatatime=false;
      Mheader.clear();
      Mnsamples=0;
      Mreadsuccessfully=false;
      Mtracefree.lines.clear();
      Miseries=0;
      Mfilled=false;
    } // void File::clear()

    /*----------------------------------------------------------------------*/

    bool File::isproperlyfilled(const bool& throwerrors) const
    {
      if (throwerrors)
      {
        DATRW_assert(!Mreadyforreading,
                     "No data was read into File structure");
        DATRW_assert(Mheader.readsuccessfully,
                     "Header data not read successfully");
        DATRW_assert(Mreadsuccessfully,
                     "Data not read successfully");
      }
      return((!Mreadyforreading)
             &&(Mreadsuccessfully)&&(Mheader.readsuccessfully));
    } // bool File::isproperlyfilled(const bool& throwerrors=false) const

    /*----------------------------------------------------------------------*/

    void File::put(const DataLine& line)
    {
      std::ostringstream oss;
      // check the time value against the expected time range 
      if (!Mbetolerantagainstwrongtime)
      {
        DL1_rcassert ((line.time()>=this->Mheader.earliestdate)
                      && (line.time()<=this->Mheader.latestdate),
                      "sample does not fit in expected window",
                      this->Mheader.earliestdate, this->Mheader.latestdate,
                      line.line());
      }
      else
      {
        if ((line.time()<this->Mheader.earliestdate)
            || (line.time()>this->Mheader.latestdate))
        {
          std::cerr << "sample does not fit in expected window";
          std::cerr << "  earliest in expected window: " <<
            this->Mheader.earliestdate.timestring();
          std::cerr  << "  latest in expected window: " <<
            this->Mheader.latestdate.timestring();
          std::cerr  << "  DL1 data line: " << line.line();
          oss.clear();
          oss.str("");
          oss << "ERROR: data line with weird time: " << line.line();
          Mtracefree.append(oss.str());
          Mfoundunexpecteddatatime=true;
        }
      }
      // evaluate value and fill my record of samples
      // this uses libtime nfit, which provides safe rounding
      unsigned int i=(line.time()-
                      this->Mheader.earliestdate)/dl1samplinginterval;
      if (!this->Mbetolerantagainstwrongtime)
      {
        DL1_rcassert (((i>=0) && (i<Mnsamples)),
                      "sample index out of range",
                      this->Mheader.earliestdate,
                      this->Mheader.latestdate, 
                      line.line());
      }
      if (i>=0 && i<Mnsamples)
      {
        if (Mfilled(i))
        {
          oss.clear();
          oss.str("");
          oss << "NOTICE: duplicate sample time (index " 
            << i << "): " << line.line();
          if (!this->Mbetolerantagainstredundant)
          {
            std::cerr << oss.str() << std::endl;
          }
          Mtracefree.append(oss.str());
        }
        Mfilled(i)=true;
        Miseries(i) += line.counts();
      }
      else
      {
        oss.clear();
        oss.str("");
        oss << "ERROR: sample index " << i << " out of range: " << line.line();
        std::cerr << oss.str() << std::endl;
        Mtracefree.append(oss.str());
      }
    } // void File::put(const DataLine& line)

    /*----------------------------------------------------------------------*/

    int File::nsamples() const
    {
      return(this->Mheader.nsamples());
    } // int File::nsamples() const

    /*----------------------------------------------------------------------*/

    sff::WID2 File::wid2line() const
    {
      DATRW_assert(this->isproperlyfilled(),
                   "Data is not set ready");
      return(Mheader.wid2line());
    } // sff::WID2 File::wid2line() const

    /*----------------------------------------------------------------------*/

    //! return data block of values
    Tfseries File::fseries() const
    {
      Tfseries retval(Mnsamples);
      retval.copyin(Miseries);
      retval *= DataLine::gain;
      return(retval);
    } // Tdseries File::fseries() const

    /*----------------------------------------------------------------------*/

    //! return data block of values
    Tdseries File::dseries() const
    {
      Tdseries retval(Mnsamples);
      retval.copyin(Miseries);
      retval *= DataLine::gain;
      return(retval);
    } // Tdseries File::dseries() const

    /*----------------------------------------------------------------------*/

    //! return data block of counts
    Tiseries File::iseries() const
    {
      return(Miseries);
    } // Tiseries File::iseries() const

  } // namespace thiesdl1

} // namespace datrw

/*======================================================================*/
/*
 * E-Mail from Volker König at ThiesClima regarding duplicate entries in the
 * loggers data files.
 * -------------------------------------------------------------------------

From Volker.Koenig@thiesclima.com Wed Nov  7 11:34:00 2012
Date: Wed, 7 Nov 2012 11:33:48 +0100
From: "Koenig, Volker" <Volker.Koenig@thiesclima.com>
To: "Forbriger, Thomas (GPI)" <thomas.forbriger@kit.edu>
Subject: AW: AW: doppelte Einträge Datenerfassung Thies DL1/N V1.10a

Sehr geehrter Herr Forbriger,

Aufgrund der Auftragsnummer können wir sicher sein, dass der Datalogger mit der neuesten Software arbeitet.

Der Datalogger legt jeweils um 24:00 einen zusätzlichen Status-Datensatz mit 0.0 ab. Dies entspricht damit
der normalen Funktion.

Die 8 anderen Werte mit gleichen (doppelten) Zeitstempel können wir im Moment nicht eindeutig erklären.
Eine Ursache könnte sein, dass die interne Uhr des Loggers zu den Messzeitpunkten von außern neu gestellt wurde
und dadurch zwei Messwerte mit gleichem Zeitstempel erfasst wurden.
Eine andere Möglichkeit könnte ein Timing Problem sein. Das wirklich knapp zum Anfang und Ende einer Minute ein
Meßwert erfasst wird. Ein solches Timing Problem ist jedoch bisher nicht bekannt und läßt sich wahrscheinlich
nur schwer nachvollziehen, wenn es wirklich nur so selten auftritt.
Sind wir mit einer der beiden Ursachen auf der richtigen Spur, sollten die beiden Messwerte addiert werden
um den richtigen Summenwert zu erhalten.

Wenn Sie uns den Datalogger zusenden, könnten wir diesen in unserem Haus überprüfen bzw. testen. Dies kann jedoch
unter Umständen einen längeren Zeitraum erfordern.

Für Rückfragen stehen wir jederzeit zur Verfügung.

Mit freundlichen Grüßen

............................................
Adolf Thies GmbH & Co.KG
Meteorologie - Umweltmesstechnik
Vertrieb Süd

Volker König

Tel.: +49 (0) 551 79001 125
Fax:  +49 (0) 551 79001 64

Volker.Koenig@thiesclima.com
www.thiesclima.com

ADOLF THIES GMBH & CO. KG   Hauptstr. 76   37083 Göttingen
Registergericht Göttingen HRA 2488   Geschäftsführer: Wolfgang Behrens



-----Ursprüngliche Nachricht-----
Von: Thomas Forbriger (GPI, BFO) [mailto:Thomas.Forbriger@kit.edu]
Gesendet: Dienstag, 6. November 2012 13:37
An: Forbriger, Thomas (GPI)
Cc: Koenig, Volker; Peter Duffner
Betreff: Re: AW: doppelte Einträge Datenerfassung Thies DL1/N V1.10a

Sehr geehrter Herr König,

uns ist gerade aufgefallen, dass wir den Datenlogger separat bestellt haben und dieser garnicht auf der Rechnung steht, die ich Ihnen geschickt habe. Die Rechnung für den Datenlogger ist vom 19.7.2007. Die Auftragsnummer war
AB0702712 und die Rechnungsnummer RE0704090. Ich hoffe, dass Sie damit die Spur aufnehmen können.

Beste Grüße,
Thomas Forbriger

On Mon, 5 Nov 2012, Forbriger, Thomas (GPI) wrote:

> Sehr geehrter Herr König,
>
> im Anhang sende ich Ihnen zwei Fotos (Aufkleber Datenlogger und Rechnung).
> Ich hoffe, dass Ihnen eine der darauf angegebenen Nummern weiterhilft.
> Falls Sie die Seriennummer daraus erschließen können, wäre ich für
> eine Mitteilung dankbar, damit ich sie in unserem Stationsbuch vermerken kann.
>
> Beste Grüße,
> Thomas Forbriger
>
> On Fri, 2 Nov 2012, Koenig, Volker wrote:
>
>> Sehr geehrter Herr Forbriger,
>>
>> können Sie mit noch die Auftragsnummer oder Seriennummer nennen, mit
>> denen der Logger geliefert wurde.
>>
>> Wissen Sie außerdem ob in den letzten Jahren bei diesem Logger ein
>> Software-Update durchgeführt wurde.
>>
>> Für Rückfragen stehen wir jederzeit zur Verfügung.
>>
>> Mit freundlichen Grüßen
>>
>> ............................................
>> Adolf Thies GmbH & Co.KG
>> Meteorologie - Umweltmesstechnik
>> Vertrieb Süd
>>
>> Volker König
>>
>> Tel.: +49 (0) 551 79001 125
>> Fax:  +49 (0) 551 79001 64
>>
>> Volker.Koenig@thiesclima.com
>> www.thiesclima.com
>>
>> ADOLF THIES GMBH & CO. KG   Hauptstr. 76   37083 Göttingen
>> Registergericht Göttingen HRA 2488   Geschäftsführer: Wolfgang Behrens
>>
>>
>> -----Ursprüngliche Nachricht-----
>> Von: Thomas Forbriger (GPI, BFO) [mailto:Thomas.Forbriger@kit.edu]
>> Gesendet: Freitag, 2. November 2012 11:58
>> An: Koenig, Volker
>> Cc: Peter Duffner; Rudolf Widmer-Schnidrig
>> Betreff: doppelte Einträge Datenerfassung Thies DL1/N V1.10a
>>
>> Sehr geehrter Herr König,
>>
>> wie eben telefonische besprochen, sende ich Ihnen ein paar Beispiele zum geschilderten Problem.
>>
>> Ich will kurz nochmal die Situation schildern. Seit 2008 betreiben wir einen Regenmesser Ihrer Firma zusammen mit einem DL1/N Datenlogger. Der Datenlogger wird von einem PC aus über serielle Schnittstelle ausgelesen.
>> Auf diese Weise erstellen wir Dateien, die jeweils die Messdaten eines Tages enthalten. Solche Dateien schicke ich im Anhang mit.
>>
>> Beim Verarbeiten der Daten ist schon immer aufgefallen, dass immer wieder doppelte Einträge in den Dateien vorhanden sind. Häufig ist das für den 24:00 Uhr Eintrag der Fall. Ich hänge Ihnen ein Protokoll einer Datenauswertung im Anhang an (Datei dataextract.log). Dort sehen Sie die Meldungen doppelter Einträge, in diesem Fall auch mal um 18:25 am 21.5.2012.
>> Wir haben diese Einträge immer ignoriert, weil wir davon ausgegangen sind, dass einfach die gleiche Datenzeile zweimal gesendet wurde.
>>
>> Bei der Analyse der letzten 2,5 Jahre sind allerdings acht Einträge aufgefallen, in denen zwei unterschiedliche Regenwerte zur selben Minute gemeldet werden (siehe Datei critical.log). Das wirft jetzt die konkrete Frage auf, wie der korrekte Messwert für Minuten lautet, für die ein doppelter Eintrag ausgegeben wird.
>>
>> Sollen wir die erste Zeile verwenden?
>> Sollen wir den jeweils größeren Messwert verwenden?
>> Sollen wir die Summe der beiden Messwerte verwenden?
>>
>> Die betroffenen Tages-Dateien hänge ich ebenfalls an diese E-Mail an.
>>
>> Ich hoffe, dass Sie anhand der Software im Datenlogger das Problem nachvollziehen können und uns mitteilen können, wie mit den doppelten Einträgen korrekt verfahren werden soll.
>>
>> Mit freundlichen Grüßen,
>> Thomas Forbriger
>>
>> --
>> | Dr. Thomas Forbriger      e-mail: Thomas.Forbriger@kit.edu
>> | Observatorium Schiltach (BFO), Heubach 206, D-77709 Wolfach,
>> | Germany,
>> | Tel.: ++49 (0)7836/2151, Fax.: ++49 (0)7836/955240
>> | http://www.rz.uni-karlsruhe.de/~bi77
*/

/* ----- END OF thiesdl1file.cc ----- */
