# ============================================================
# Tests und Fragebögen einlesen, Personentabelle für die Vergleiche
# ============================================================

# Ein Eintrag pro angezeigtem Wert: Modul und Feld in der psychTestR-
# Ergebnisdatei. Ein neuer Test oder Fragebogen ist eine neue Zeile.
test_registry <- data.frame(
  id     = c("GMS_General", "TOM_Incremental", "TOM_Entity", "MIQ_ability", "JAJ_ability"),
  module = c("GMS", "TOM", "TOM", "MIQ", "JAJ"),
  field  = c("General", "Incremental", "Entity", "ability", "ability"),
  label  = c("Gold-MSI: Allgemeine Musikalität (Skalenmittelwert)",
             "Theory of Musicality: Incremental (Skalenmittelwert)",
             "Theory of Musicality: Entity (Skalenmittelwert)",
             "Intelligenz (MIQ): Fähigkeitswert",
             "Arbeitsgedächtnis (JAJ): Fähigkeitswert"),
  stringsAsFactors = FALSE
)

empty_tests <- function() {
  data.frame(p_id = character(), test = character(), time = as.POSIXct(character()),
             value = numeric(), date = as.Date(character()), stringsAsFactors = FALSE)
}

# Eine Ergebnisdatei -> eine Zeile pro vorhandenem Wert der Registry.
read_test_file <- function(file) {
  res <- tryCatch(as.list(readRDS(file)), error = function(e) NULL)
  session <- res[["session"]]
  p_id <- normalize_id(session$p_id)
  if (is.null(session) || length(p_id) != 1 || is.na(p_id) || p_id == "") return(NULL)
  rows <- lapply(seq_len(nrow(test_registry)), function(i) {
    value <- protocol_number(res[[test_registry$module[i]]][[test_registry$field[i]]])
    if (is.na(value)) return(NULL)
    data.frame(p_id = p_id, test = test_registry$id[i],
               time = as.POSIXct(session$time_started), value = value,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Alle Messungen aus den Ergebnisverzeichnissen. psychTestR speichert eine
# Sitzung mehrfach (Zwischenstände), deshalb zählt jeder Wert pro Sitzung
# (Person, Test, Startzeit) nur einmal.
read_tests <- function(dirs) {
  files <- unlist(lapply(dirs, list.files, pattern = "\\.rds$", full.names = TRUE))
  rows <- Filter(Negate(is.null), lapply(files, read_test_file))
  if (length(rows) == 0) return(empty_tests())
  finish_test_data(do.call(rbind, rows))
}

finish_test_data <- function(data) {
  data$date <- as.Date(format(data$time, "%Y-%m-%d"))
  data <- data[order(data$p_id, data$test, data$time), ]
  data <- data[!duplicated(data[c("p_id", "test", "time")], fromLast = TRUE), ]
  rownames(data) <- NULL
  data
}

# Demodaten: die meisten Personen haben jeden Test einmal, einige den
# Arbeitsgedächtnistest zweimal.
simulate_test_data <- function(ids, start = as.Date("2026-10-21"), seed = 2) {
  set.seed(seed)
  draw <- c(
    GMS_General     = function() round(min(7, max(1, rnorm(1, 4.6, 0.9))), 2),
    TOM_Incremental = function() round(min(5, max(1, rnorm(1, 3.8, 0.6))), 2),
    TOM_Entity      = function() round(min(5, max(1, rnorm(1, 2.4, 0.7))), 2),
    MIQ_ability     = function() round(rnorm(1, 0.2, 1), 2),
    JAJ_ability     = function() round(rnorm(1, 0, 1), 2)
  )
  rows <- list()
  add <- function(id, test, day) {
    rows[[length(rows) + 1]] <<- data.frame(
      p_id = id, test = test, time = as.POSIXct(start + day) + 16 * 3600,
      value = draw[[test]](), stringsAsFactors = FALSE
    )
  }
  for (id in ids) {
    for (test in names(draw)) if (runif(1) < 0.85) add(id, test, match(test, names(draw)) * 7)
    if (runif(1) < 0.25) add(id, "JAJ_ability", 49)
  }
  finish_test_data(do.call(rbind, rows))
}

# ------------------------------------------------------------
# Vergleiche (Univariat, Bivariat)
# ------------------------------------------------------------

# Wählbare Variablen. scope "group": nur innerhalb der eigenen Gruppe
# vergleichbar, weil jede Gruppe eine andere Hauptgröße hat.
monitor_variables <- rbind(
  data.frame(id = test_registry$id, label = test_registry$label, scope = "seminar",
             stringsAsFactors = FALSE),
  data.frame(
    id    = c("practice_mean", "n_protocols", "main_last", "main_change"),
    label = c("Übezeit pro Woche (Mittel, Minuten)",
              "Zahl der Protokolle",
              "Lernstand im letzten Protokoll (nur eigene Gruppe)",
              "Veränderung vom ersten zum letzten Protokoll (nur eigene Gruppe)"),
    scope = c("seminar", "seminar", "group", "group"),
    stringsAsFactors = FALSE
  )
)

# Eine Zeile pro Person: Kennwerte der Protokolle und der jeweils letzte Wert
# jedes Tests. Personen ohne Protokoll haben keine Gruppe.
person_table <- function(protocols, tests) {
  persons <- person_summary(protocols)
  latest <- tests[!duplicated(tests[c("p_id", "test")], fromLast = TRUE), ]
  for (test in test_registry$id) {
    values <- latest[latest$test == test, c("p_id", "value")]
    names(values)[2] <- test
    persons <- merge(persons, values, by = "p_id", all = TRUE)
  }
  persons$n_protocols[is.na(persons$n_protocols)] <- 0
  persons$n_tests <- rowSums(!is.na(persons[test_registry$id]))
  persons
}

# Personen mit Werten in allen gewählten Variablen; own markiert die
# ausgewählte Person. Bei einer Gruppen-Variable nur die eigene Gruppe.
comparison_data <- function(persons, id, vars) {
  if (any(monitor_variables$scope[match(vars, monitor_variables$id)] == "group")) {
    group <- persons$group[persons$p_id == id]
    persons <- persons[persons$group %in% group, ]
  }
  out <- persons[stats::complete.cases(persons[vars]), c("p_id", "group", vars)]
  out$own <- out$p_id == id
  out
}

# Achsenbeschriftung; Gruppen-Variablen nennen die Hauptgröße der Gruppe.
variable_label <- function(var, group) {
  if (var == "main_last" && !is.na(group)) {
    paste("Letztes Protokoll:", group_main_labels[[group]])
  } else if (var == "main_change" && !is.na(group)) {
    paste("Veränderung:", group_main_labels[[group]])
  } else {
    monitor_variables$label[monitor_variables$id == var]
  }
}
