# ============================================================
# Tests und Fragebögen einlesen, Personentabelle für die Vergleiche
# ============================================================

# Ein Eintrag pro angezeigtem Wert: Modul und Feld in der psychTestR-
# Ergebnisdatei. Ein neuer Test oder Fragebogen ist eine neue Zeile.
test_registry <- data.frame(
  id     = c("GMS_General", "TOM_Incremental", "TOM_Entity", "MIQ_ability", "JAJ_ability",
             "BDS_score", "RAT_ability", "BAT_ability"),
  module = c("GMS", "TOM", "TOM", "MIQ", "JAJ", "BDS", "RAT", "BAT"),
  field  = c("General", "Incremental", "Entity", "ability", "ability", "score",
             "ability", "ability"),
  label  = c("Gold-MSI: Allgemeine Musikalität (Skalenmittelwert)",
             "Theory of Musicality: Incremental (Skalenmittelwert)",
             "Theory of Musicality: Entity (Skalenmittelwert)",
             "Intelligenz (MIQ): Fähigkeitswert",
             "Arbeitsgedächtnis (JAJ): Fähigkeitswert",
             "Ziffernfolgen rückwärts (BDS): Anteil richtig",
             "Rhythmuswahrnehmung (RAT): Fähigkeitswert",
             "Beat-Wahrnehmung (BAT): Fähigkeitswert"),
  stringsAsFactors = FALSE
)

# Werte, die in einer Sitzung zweimal erhoben werden (Anfang und Ende). Für
# sie gibt es in den Vergleichen Anfang, Ende und Veränderung statt des
# letzten Werts. Die Batterie speichert den selbst angegebenen Durchgang als
# <Modul>_RUN$run; die Übersicht zeigt ihn, Anfang/Ende ergeben sich aber aus
# der Uhrzeit (siehe paired_values()).
paired_tests <- c("TOM_Incremental", "TOM_Entity")
paired_parts <- c(pre  = "Anfang der Sitzung",
                  post = "Ende der Sitzung",
                  diff = "Veränderung (Ende minus Anfang)")

empty_tests <- function() {
  data.frame(p_id = character(), test = character(), time = as.POSIXct(character()),
             value = numeric(), run = numeric(), date = as.Date(character()),
             stringsAsFactors = FALSE)
}

# Eine Ergebnisdatei -> eine Zeile pro vorhandenem Wert der Registry.
read_test_file <- function(file) {
  res <- tryCatch(as.list(readRDS(file)), error = function(e) NULL)
  session <- res[["session"]]
  p_id <- normalize_id(session$p_id)
  if (is.null(session) || length(p_id) != 1 || is.na(p_id) || p_id == "") return(NULL)
  rows <- lapply(seq_len(nrow(test_registry)), function(i) {
    module <- test_registry$module[i]
    value <- protocol_number(res[[module]][[test_registry$field[i]]])
    if (is.na(value)) return(NULL)
    data.frame(p_id = p_id, test = test_registry$id[i],
               time = as.POSIXct(session$time_started), value = value,
               run = protocol_number(res[[paste0(module, "_RUN")]][["run"]]),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Alle Messungen aus den Ergebnisverzeichnissen. psychTestR speichert eine
# Sitzung mehrfach (Zwischenstände), deshalb zählt jeder Wert pro Sitzung
# (Person, Test, Startzeit) nur einmal. dirs darf Platzhalter (*) enthalten;
# Dateien ohne Wert aus der Registry (z.B. Protokolle) werden übergangen.
# legacy: CSV-Dateien mit Messungen aus früheren Semestern (z.B. TOM aus dem
# WiSe 25/26), Spalten p_id, test, time, value, optional run; test wie in
# test_registry$id. Platzhalter (*) erlaubt.
read_tests <- function(dirs, legacy = character()) {
  dirs <- unique(unlist(lapply(dirs[nzchar(dirs)], Sys.glob)))
  files <- unlist(lapply(dirs, list.files, pattern = "\\.rds$", full.names = TRUE))
  rows <- Filter(Negate(is.null), lapply(files, read_test_file))
  # Frühere Messungen nur von Personen, die auch jetzt einen Test gemacht
  # haben; sonst stünden Teilnehmende früherer Seminare in der Personenliste.
  current_ids <- unique(unlist(lapply(rows, `[[`, "p_id")))
  legacy_files <- unique(unlist(lapply(legacy[nzchar(legacy)], Sys.glob)))
  for (file in legacy_files) {
    old <- read_legacy_file(file)
    if (!is.null(old)) rows <- c(rows, list(old[old$p_id %in% current_ids, ]))
  }
  if (length(rows) == 0) return(empty_tests())
  finish_test_data(do.call(rbind, rows))
}

read_legacy_file <- function(file) {
  data <- tryCatch(utils::read.csv(file, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
                   error = function(e) NULL)
  if (is.null(data) || !all(c("p_id", "test", "time", "value") %in% names(data))) return(NULL)
  if (!"run" %in% names(data)) data$run <- NA_real_
  data <- data.frame(p_id = normalize_id(data$p_id), test = data$test,
                     time = as.POSIXct(data$time), value = as.numeric(data$value),
                     run = as.numeric(data$run), stringsAsFactors = FALSE)
  data[data$test %in% test_registry$id & !is.na(data$value) & nzchar(data$p_id), ]
}

finish_test_data <- function(data) {
  data$date <- as.Date(format(data$time, "%Y-%m-%d"))
  data <- data[order(data$p_id, data$test, data$time), ]
  data <- data[!duplicated(data[c("p_id", "test", "time")], fromLast = TRUE), ]
  rownames(data) <- NULL
  data
}

# Demodaten: die meisten Personen haben jeden Test einmal, einige den
# Arbeitsgedächtnistest zweimal. Den TOM füllen alle zweimal in derselben
# Sitzung aus (Anfang, Ende), einige zusätzlich ein Jahr früher.
simulate_test_data <- function(ids, start = as.Date("2026-10-21"), seed = 2) {
  set.seed(seed)
  draw <- c(
    GMS_General     = function() round(min(7, max(1, rnorm(1, 4.6, 0.9))), 2),
    TOM_Incremental = function() round(min(5, max(1, rnorm(1, 3.8, 0.6))), 2),
    TOM_Entity      = function() round(min(5, max(1, rnorm(1, 2.4, 0.7))), 2),
    MIQ_ability     = function() round(rnorm(1, 0.2, 1), 2),
    JAJ_ability     = function() round(rnorm(1, 0, 1), 2),
    BDS_score       = function() round(min(1, max(0, rnorm(1, 0.6, 0.15))), 2),
    RAT_ability     = function() round(rnorm(1, 0, 1), 2),
    BAT_ability     = function() round(rnorm(1, 0, 1), 2)
  )
  rows <- list()
  add <- function(id, test, day, hour = 16, run = NA_real_, shift = 0) {
    rows[[length(rows) + 1]] <<- data.frame(
      p_id = id, test = test, time = as.POSIXct(start + day) + hour * 3600,
      value = draw[[test]]() + shift, run = run, stringsAsFactors = FALSE
    )
  }
  for (id in ids) {
    for (test in setdiff(names(draw), paired_tests)) {
      if (runif(1) < 0.85) add(id, test, match(test, names(draw)) * 7)
    }
    if (runif(1) < 0.25) add(id, "JAJ_ability", 49)
    last_year <- runif(1) < 0.3
    for (test in paired_tests) {
      if (last_year) add(id, test, -365, run = 1)
      add(id, test, 14, hour = 16, run = 1 + last_year)
      add(id, test, 14, hour = 17.5, run = 2 + last_year,
          shift = if (test == "TOM_Incremental") 0.3 else -0.2)
    }
  }
  finish_test_data(do.call(rbind, rows))
}

# ------------------------------------------------------------
# Vergleiche (Univariat, Bivariat)
# ------------------------------------------------------------

# Wählbare Variablen. scope "group": nur innerhalb der eigenen Gruppe
# vergleichbar, weil jede Gruppe eine andere Hauptgröße hat.
# Paarige Werte erscheinen als Anfang, Ende und Veränderung.
test_variables <- do.call(rbind, lapply(seq_len(nrow(test_registry)), function(i) {
  id <- test_registry$id[i]
  label <- test_registry$label[i]
  if (id %in% paired_tests) {
    data.frame(id = paste0(id, "_", names(paired_parts)),
               label = paste0(sub(" \\(.*\\)$", "", label), ", ", paired_parts),
               scope = "seminar", stringsAsFactors = FALSE)
  } else {
    data.frame(id = id, label = label, scope = "seminar", stringsAsFactors = FALSE)
  }
}))

monitor_variables <- rbind(
  test_variables,
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

# Anfang, Ende und Veränderung der paarigen Werte. Maßgeblich ist der Tag der
# ersten Messung ab semester_start; frühere Messungen (z.B. aus dem Vorjahr)
# zählen nicht. Anfang = erste Messung an diesem Tag, Ende = letzte, falls es
# mindestens zwei gibt.
paired_values <- function(tests, semester_start) {
  ids <- unique(tests$p_id)
  out <- data.frame(p_id = ids, stringsAsFactors = FALSE)
  for (test in paired_tests) {
    pre <- post <- rep(NA_real_, length(ids))
    for (k in seq_along(ids)) {
      t <- tests[tests$p_id == ids[k] & tests$test == test & tests$date >= semester_start, ]
      if (nrow(t) == 0) next
      t <- t[order(t$time), ]
      t <- t[t$date == t$date[1], ]
      pre[k] <- t$value[1]
      if (nrow(t) > 1) post[k] <- t$value[nrow(t)]
    }
    out[[paste0(test, "_pre")]]  <- pre
    out[[paste0(test, "_post")]] <- post
    out[[paste0(test, "_diff")]] <- post - pre
  }
  out
}

# Eine Zeile pro Person: Kennwerte der Protokolle, der jeweils letzte Wert
# jedes Tests und die paarigen Werte. Personen ohne Protokoll haben keine
# Gruppe.
person_table <- function(protocols, tests, semester_start) {
  persons <- person_summary(protocols)
  latest <- tests[!duplicated(tests[c("p_id", "test")], fromLast = TRUE), ]
  for (test in test_registry$id) {
    values <- latest[latest$test == test, c("p_id", "value")]
    names(values)[2] <- test
    persons <- merge(persons, values, by = "p_id", all = TRUE)
  }
  persons <- merge(persons, paired_values(tests, semester_start), by = "p_id", all = TRUE)
  persons$n_protocols[is.na(persons$n_protocols)] <- 0
  persons$n_tests <- rowSums(!is.na(persons[test_registry$id]))
  persons
}

# Personen mit Werten in allen gewählten Variablen; own markiert die
# ausgewählte Person (id = NULL: niemand ausgewählt). Bei einer
# Gruppen-Variable nur die eigene Gruppe, ohne Person also niemand.
comparison_data <- function(persons, id, vars) {
  if (is.null(id)) id <- NA_character_
  if (any(is_group_variable(vars))) {
    group <- persons$group[persons$p_id %in% id]
    persons <- persons[persons$group %in% group, ]
  }
  out <- persons[stats::complete.cases(persons[vars]), c("p_id", "group", vars)]
  out$own <- !is.na(id) & out$p_id == id
  out
}

is_group_variable <- function(vars) {
  monitor_variables$scope[match(vars, monitor_variables$id)] == "group"
}

# Alle seminarweiten Variablen in Langform (Variable, Wert, eigene Person)
# für die Histogramm-Übersicht der Dozentenansicht. Nur Variablen mit Werten;
# der Name enthält die Zahl der Personen.
all_variables_long <- function(persons, id) {
  vars <- monitor_variables$id[monitor_variables$scope == "seminar"]
  rows <- lapply(vars, function(var) {
    keep <- !is.na(persons[[var]])
    if (!any(keep)) return(NULL)
    data.frame(variable = var, value = persons[[var]][keep],
               own = !is.null(id) & persons$p_id[keep] %in% id, stringsAsFactors = FALSE)
  })
  long <- do.call(rbind, rows)
  if (is.null(long)) return(NULL)
  n <- table(long$variable)
  labels <- sprintf("%s (n = %d)", monitor_variables$label[match(names(n), monitor_variables$id)],
                    as.integer(n))
  long$variable <- factor(long$variable, levels = intersect(vars, names(n)),
                          labels = labels[match(intersect(vars, names(n)), names(n))])
  long
}

# Teilnahme pro Test/Fragebogen ab semester_start: Personen und Messungen.
# Bei paarigen Werten zusätzlich die Personen mit Anfang und Ende.
participation_table <- function(tests, persons, semester_start) {
  tests <- tests[tests$date >= semester_start, ]
  out <- data.frame(
    test   = test_registry$label,
    n_persons = vapply(test_registry$id, function(t) length(unique(tests$p_id[tests$test == t])), integer(1)),
    n_measurements = vapply(test_registry$id, function(t) sum(tests$test == t), integer(1)),
    n_paired = vapply(test_registry$id, function(t) {
      if (t %in% paired_tests) sum(!is.na(persons[[paste0(t, "_diff")]])) else NA_integer_
    }, integer(1)),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
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
