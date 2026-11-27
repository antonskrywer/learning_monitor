# TOM-Ergebnisse aus "Learning by Listening" (WiSe 25/26) in das
# Vorjahresformat des Lernmonitors umwandeln (Spalten p_id, test, time,
# value, run).
#
# Eingabe: psyquest-Export mit den Spalten id (Matrikelnummer),
#          session.complete, session.time_started, TOM.Incremental, TOM.Entity
# Ausgabe: CSV, die der Monitor über MONITOR_LEGACY_FILES liest
#          (auf dem Server: /home/shiny/monitor_data/tom_wise2526.csv)
#
# Aufruf:
#   Rscript tools/convert_tom_wise2526.R <mindset_results.csv> <ausgabe.csv>
#
# Die Ausgabe enthält Matrikelnummern und gehört nicht ins Repository.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("Aufruf: Rscript convert_tom_wise2526.R <eingabe.csv> <ausgabe.csv>")

d <- read.csv(args[1], stringsAsFactors = FALSE)
d$id <- toupper(trimws(as.character(d$id)))

# Nur abgeschlossene Sitzungen mit Matrikelnummer (Ziffern), keine Testläufe
d <- d[d$session.complete %in% c(TRUE, "TRUE") & grepl("^[0-9]{5,}$", d$id), ]

long <- do.call(rbind, lapply(c(Incremental = "TOM.Incremental", Entity = "TOM.Entity"),
                              function(col) {
  data.frame(p_id = d$id, test = paste0("TOM_", sub("TOM.", "", col, fixed = TRUE)),
             time = d$session.time_started, value = round(as.numeric(d[[col]]), 4),
             run = 1, stringsAsFactors = FALSE)
}))
long <- long[!is.na(long$value), ]

write.csv(long, args[2], row.names = FALSE, fileEncoding = "UTF-8")
message(sprintf("%d Personen, %d Zeilen -> %s", length(unique(long$p_id)), nrow(long), args[2]))
