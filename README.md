# Lernmonitor · Musikalische Entwicklung und ihre Messbarkeit

Shiny-App, in der Studierende des Seminars ihre Lernprotokolle (Instrument,
Cup Stacking, 10-Finger-Schreiben, Clapping Music) und ihre Ergebnisse in
Tests und Fragebögen ansehen. Anmeldung mit der Matrikelnummer, die auch in
den Protokollen als ID dient.

## Starten

```r
install.packages(c("shiny", "bslib", "ggplot2"))
shiny::runApp()
```

## Daten

| Umgebungsvariable | Bedeutung |
|---|---|
| `MONITOR_DATA_ROOT` | Ordner, in dem `<gruppe>_protocol/output/results` liegt. Standard: `/srv/shiny-server` |
| `MONITOR_TEST_DIRS` | Ergebnisverzeichnisse der Tests und Fragebögen, getrennt durch `;` |
| `MONITOR_ADMIN_PASSWORD` | Passwort der Dozentenansicht. Ohne Variable ist sie gesperrt |

- Die Daten werden beim Seitenaufruf und bei jedem Klick auf „Anzeigen" neu
  eingelesen.
- Protokolle: nur abgeschlossene Sitzungen (`complete = TRUE`), pro Person
  und Tag die letzte. Jede Person zählt in der Gruppe, in der sie die meisten
  Protokolle hat.
- Tests: Die ID muss dieselbe Matrikelnummer sein. Alle Messungen bleiben
  erhalten; als Datum gilt der Beginn der Sitzung.
- Gibt es weder Protokolle noch Testergebnisse, laufen Demodaten (IDs
  `1001`–`1024`).
- Ergebnisdateien gehören nicht ins Repository (`data/`, `output/` sind
  gitignored).

## Seiten

| Seite | Inhalt |
|---|---|
| Übersicht | Zahl der Protokolle, erstes und letztes Datum, alle Testergebnisse mit Datum |
| Lernverlauf | Kurven je nach Gruppe, dazu der Gruppenmittelwert pro Kalenderwoche |
| Übezeit | Minuten pro Woche, dazu der Gruppenmittelwert |
| Univariat | Eigener Wert im Histogramm der übrigen Personen |
| Bivariat | Streudiagramm zweier Variablen mit eigenem Punkt |
| Dozent*in | Rücklauf pro Woche, alle Personen, Ansicht einer Person öffnen |

## Verrechnung

- Instrument: wie im MLP. Dimension = Mittel der beantworteten Kern-Items,
  L3 auf 1–5, L1 minus 3 (−2 … +2).
- Cup Stacking: Zeit (Sekunden) und Fehler. 10-Finger-Schreiben: Wörter pro
  Minute, Fehler halb-richtig und falsch.
- Clapping Music: absolute Bewertungen 1–10; relative Bewertungen minus 3
  (−2 … +2), gemittelt.
- Die Automatisierungs-Items und der Kommentar werden nicht angezeigt.
- Hauptgröße für Vergleiche in der Gruppe: Instrument = Mittel der drei
  L3-Dimensionen, Stacking = Zeit, Typing = Wörter pro Minute, Clapping =
  Mittel der drei absoluten Bewertungen.

## Anonymität

- Gruppenmittel erscheinen erst ab 3 Personen pro Kalenderwoche
  (`min_n_group_mean` in `app.R`).
- Verteilungen erscheinen erst ab 5 weiteren Personen mit einem Wert
  (`min_n_distribution`). In der Dozentenansicht gilt diese Grenze nicht.

## Neuen Test ergänzen

Eine Zeile in `test_registry` (`R/test_data.R`): Modul und Feld in der
psychTestR-Ergebnisdatei sowie die Beschriftung. Der Wert erscheint dann in
der Übersicht und in Univariat/Bivariat.

## Aufbau

- `app.R`: Oberfläche und Server
- `R/protocol_data.R`: Protokolle einlesen, Gruppenmittel, Demodaten
- `R/test_data.R`: Tests einlesen, Personentabelle, Variablen der Vergleiche
- `R/plots.R`: Diagramme und UHH-Farben
