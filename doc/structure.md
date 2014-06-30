Notizen
=======

1 In welchem Format werden die Dokumente intern gehandlet?

    * wie machen das R text tools, tm, Ken Benoit: quanteda
      * quanteda benutzt tm nicht, hat sich ein eigenes corpus Format gebaut...
      * genauso R Text Tools und Will Lowe's austin
    * von Nico: Weblicht TCF, TEI, DTAE, CMDI
    
    * tm Corpus verbreitet und aktuell maintained;
      * metadata pro Dokument moeglich --> core dataset?
      * cooles Konzept: sources in tm. eigene schreiben fuer Manifesto-Datensatz? + wrapper-Methode?
      * cooles Konzept: Transformations: eigene schreiben, nutzbar machen. : codes, complete-sentences, ...
    
    * noch unklar: Wie werden die Codes/Annotationen gehalten?
      * aktuelle Idee: in Meta-Data: data.frame mit begin=char, end=char, code=factor, type=?
      * **Problem:** Wenn Transformations applied werden, (stemming, stopword Entfernung, ...), stimmen die char Positionen nicht mehr
      * Moeglichkeit 1: eigenes Corpus-Format mit Annotationen in tm einbetten, transformationen dafuer ueberschreiben -- unbefriedigend
      * Moeglichkeit 2: eigenes Corpus-Format (als data.frame) mit Uebersetzungsmoeglichkeit in tm, wenn alles fertig ist --> sinnvoll? 
      * Moeglichkeit 3: wir stellen schon transformierte, aber annotierte Texte bereit --> ineffizient und bloed
      * Moeglichkeit 4: Code als <>-Tags in den Text einfuegen --> wie gehen die in Term-Document-Matrices ein? Auswahl? --> unpraktisch und potentiell instabil
    
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
    

    
    