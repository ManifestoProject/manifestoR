Notizen
=======

1 In welchem Format werden die Dokumente intern gehandlet?

    * noch offen
    * anschauen: wie machen das R text tools, tm, Ken Benoit: quanteda
    * CRAN View
    * von Nico: Weblicht TCF, TEI, DTAE, CMDI
    
2 Wie sieht das DB Schema aus?

    * noch offen, haengt von 1 ab
    * vermutlich: pro Dokument: textstring; dann Annotationen: QS von characterpos characterpos + Code
    
3 Wie wird die Textdatenbank mit dem Core Dataset verknuepft?

    * PartyCode+electiondate bilden ID fuer document (collection?)
    * Flag fuer primary falls mehrere Dokumente
    
3 Wie sieht der Server aus, der auf die DB zugreifen laesst?

    * API keys werden erzeugt mit Anmeldung und bei jedem Zugriff mitgeschickt
    * Unklar: gibt es im R package eine Funktion um den API key fuer registrierte Nutzer_innen zu holen?
    * wird ueber ein get request aufgerufen -> drei Aktionen moeglich: getCoreDataset und getOriginalDocument und getCodedDocument
    * Unklar: Wie wird hier damit umgegangen, dass es potenziell mehrere Dokumente pro Case gibt?
    
4 Wie kommen die Dokumente in die DB?

    * quasi-manuell, python Script bauen, dass einzelne .csv Dateien eincheckt
    * Wird die DB versioniert?
    

    
    