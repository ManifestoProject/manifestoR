Notizen
=======

1 In welchem Format werden die Dokumente intern gehandlet?

    * ManifestoDocument S3 class definieren, die ...
      * ... erbt von PlainTextDocument, TextDocument --> content(), meta() implementieren
      * ... eine struct ist aus [data.frame mit text= code= vorhaelt], meta=[meta object aus tm]
    
    * zu Copurs konvertieren:
      * ManifestoSource: funktioniert aehnlich wie VectorSource, aber mit $reader, der die Elemente nicht nach PlainTextDocument coerced
      * zwei reader providen:
	* readManifesto.csv: macht aus Dateinamen/filehandles die ManifestoDocument-Objekte
	* readManifesto.df: Standard, laesst Document-Objekte unveraendert
    
    * Fuer Corpus convenience-Funktionen schreiben
      * factor.bycode
    
2 Wie sieht das DB Schema aus?

    * noch offen, haengt von 1 ab
    * vermutlich: pro Dokument: textstring; dann Annotationen: QS von characterpos characterpos + Code
    
3 Wie wird die Textdatenbank mit dem Core Dataset verknuepft?

    * PartyCode+electiondate bilden ID fuer document (collection?)
    * Flag fuer primary falls mehrere Dokumente
    * cooles Konzept: sources in tm. eigene schreiben fuer Manifesto-Datensatz? + wrapper-Methode?
    
3 Wie sieht der Server aus, der auf die DB zugreifen laesst?

    * API keys werden erzeugt mit Anmeldung und bei jedem Zugriff mitgeschickt
    * Unklar: gibt es im R package eine Funktion um den API key fuer registrierte Nutzer_innen zu holen?
    * wird ueber ein get request aufgerufen -> drei Aktionen moeglich: getCoreDataset und getOriginalDocument und getCodedDocument
    * Unklar: Wie wird hier damit umgegangen, dass es potenziell mehrere Dokumente pro Case gibt?
    
4 Wie kommen die Dokumente in die DB?

    * quasi-manuell, python Script bauen, dass einzelne .csv Dateien eincheckt
    * Wird die DB versioniert?
    

    
    