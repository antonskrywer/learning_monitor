# ============================================================
# Lernprotokolle einlesen und aufbereiten
# ============================================================
# Quelle: psychTestR-Ergebnisdateien (.rds) der vier Protokoll-Apps
# (instrumental_protocol, stacking_protocol, typing_protocol,
# clapping_protocol). Eine Zeile pro abgeschlossener Sitzung.

protocol_groups <- c(
  instrumental = "Instrument",
  stacking     = "Cup Stacking",
  typing       = "10-Finger-Schreiben",
  clapping     = "Clapping Music"
)

# Kern-Items pro Dimension wie im MLP. Gemittelt wird über die Items, die in
# der Sitzung beantwortet wurden (Kurzskala: je ein Item pro Dimension).
mlp_dimensions <- list(
  general    = c("0003", "0004"),
  technique  = c("0005", "0006", "0007", "0009", "0010", "0011"),
  expression = c("0000", "0001", "0002")
)

# Felder, die unverändert aus der Ergebnisdatei übernommen werden
protocol_fields <- list(
  stacking = c("time_sec", "errors"),
  typing   = c("words_per_min", "errors_half", "errors_wrong"),
  clapping = c("abs_overall", "abs_individual", "abs_sync")
)
# Vergleich zur letzten Session (1-5), wird auf -2 ... +2 zentriert
clapping_relative <- c("rel_overall", "rel_individual", "rel_sync")

measure_columns <- c(
  paste0("L3_", names(mlp_dimensions)), paste0("L1_", names(mlp_dimensions)),
  unlist(protocol_fields, use.names = FALSE), clapping_relative
)

# Diagramme im Reiter "Lernverlauf" pro Gruppe. type "level": eine Kurve pro
# Größe; type "change": Vergleich zum letzten Protokoll, zentriert bei 0.
group_panels <- list(
  instrumental = list(
    list(type = "level", title = "Entwicklung der Leistung",
         note = "Deine Einschätzung gemessen am Stück: 1 = niedrigste, 5 = höchste Stufe.",
         vars = c(L3_general = "Generelle Qualität", L3_technique = "Technik",
                  L3_expression = "Expressivität"),
         limits = c(1, 5)),
    list(type = "change", title = "Individuelle Entwicklung",
         note = "Vergleich mit der letzten Woche: grün = besser, grau = gleich, rot = schlechter.",
         vars = c(L1_general = "Generelle Qualität", L1_technique = "Technik",
                  L1_expression = "Expressivität"),
         caption = "Mittel aus genereller Qualität, Technik und Expressivität")
  ),
  stacking = list(
    list(type = "level", title = "Zeit und Fehler",
         note = "Zeit für den Cycle und Zahl der umgefallenen Cups pro Protokoll.",
         vars = c(time_sec = "Zeit für den Cycle (Sekunden)", errors = "Fehler"),
         limits = NULL)
  ),
  typing = list(
    list(type = "level", title = "Geschwindigkeit und Fehler",
         note = "Ergebnis des Schreibtests pro Protokoll.",
         vars = c(words_per_min = "Wörter pro Minute",
                  errors_half = "Fehler: halb-richtig", errors_wrong = "Fehler: falsch"),
         limits = NULL)
  ),
  clapping = list(
    list(type = "level", title = "Bewertung der Performance",
         note = "Absolute Bewertung: 1 = niedrigste, 10 = höchste Stufe.",
         vars = c(abs_overall = "Gesamteindruck", abs_individual = "Einzelperformance",
                  abs_sync = "Synchronisation"),
         limits = c(1, 10)),
    list(type = "change", title = "Individuelle Entwicklung",
         note = "Vergleich mit der letzten Session: grün = besser, grau = gleich, rot = schlechter.",
         vars = c(rel_overall = "Gesamteindruck", rel_individual = "Einzelperformance",
                  rel_sync = "Synchronisation"),
         caption = "Mittel aus Gesamteindruck, Einzelperformance und Synchronisation")
  )
)

# Hauptgröße pro Gruppe für die Vergleiche innerhalb der Gruppe
group_main_labels <- c(
  instrumental = "Bewertung (Mittel der drei Dimensionen, 1–5)",
  stacking     = "Zeit für den Cycle (Sekunden)",
  typing       = "Wörter pro Minute",
  clapping     = "Bewertung (Mittel der drei Aspekte, 1–10)"
)

# Antwort -> Zahl; Zahlenfelder können als Text mit Komma gespeichert sein.
protocol_number <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  suppressWarnings(as.numeric(sub(",", ".", trimws(as.character(x[[1]])), fixed = TRUE)))
}

# Matrikelnummern werden ohne Rücksicht auf Groß-/Kleinschreibung verglichen.
normalize_id <- function(x) toupper(trimws(as.character(x)))

protocol_row <- function(p_id, group, time, practice, values) {
  row <- data.frame(p_id = p_id, group = group, time = time,
                    practice_week_min = practice, stringsAsFactors = FALSE)
  for (col in measure_columns) {
    row[[col]] <- if (col %in% names(values)) values[[col]] else NA_real_
  }
  row
}

# Mittelwert einer MLP-Dimension auf einer Ebene ("L1", "L3"), NA wenn kein
# Item beantwortet wurde.
mlp_dimension_mean <- function(module, item_ids, level) {
  values <- vapply(paste0("TMLP_", item_ids, "_", level),
                   function(key) protocol_number(module[[key]]), numeric(1))
  if (all(is.na(values))) NA_real_ else mean(values, na.rm = TRUE)
}

protocol_values <- function(module, group) {
  values <- list()
  if (group == "instrumental") {
    for (dim in names(mlp_dimensions)) {
      values[[paste0("L3_", dim)]] <- mlp_dimension_mean(module, mlp_dimensions[[dim]], "L3")
      # L1 (1-5) zentriert: 0 = gleich wie in der letzten Woche
      values[[paste0("L1_", dim)]] <- mlp_dimension_mean(module, mlp_dimensions[[dim]], "L1") - 3
    }
  } else {
    for (field in protocol_fields[[group]]) values[[field]] <- protocol_number(module[[field]])
    if (group == "clapping") {
      for (field in clapping_relative) values[[field]] <- protocol_number(module[[field]]) - 3
    }
  }
  values
}

# Eine Ergebnisdatei -> eine Zeile. Unvollständige oder fremde Dateien: NULL.
read_protocol_session <- function(file, group) {
  res <- tryCatch(as.list(readRDS(file)), error = function(e) NULL)
  module <- res[[paste0(group, "_protocol")]]
  session <- res[["session"]]
  if (is.null(module) || is.null(session) || !isTRUE(session$complete)) return(NULL)
  p_id <- normalize_id(session$p_id)
  if (length(p_id) != 1 || is.na(p_id) || p_id == "") return(NULL)
  protocol_row(p_id, group, as.POSIXct(session$time_started),
               protocol_number(module$practice_week_min), protocol_values(module, group))
}

# Alle abgeschlossenen Sitzungen der vier Protokolle unter
# <root>/<gruppe>_protocol/output/results.
read_protocols <- function(root) {
  rows <- list()
  for (group in names(protocol_groups)) {
    dir <- file.path(root, paste0(group, "_protocol"), "output", "results")
    files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
    rows <- c(rows, lapply(files, read_protocol_session, group = group))
  }
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(empty_protocols())
  finish_protocol_data(do.call(rbind, rows))
}

empty_protocols <- function() {
  finish_protocol_data(protocol_row("", "instrumental", Sys.time(), NA_real_, list())[0, ])
}

# Datum, Kalenderwoche (Montag) und Hauptgröße ergänzen; pro Person und Tag
# nur die letzte Sitzung behalten; jede Person zählt nur in ihrer Gruppe.
finish_protocol_data <- function(data) {
  data$date <- as.Date(format(data$time, "%Y-%m-%d"))
  data$week <- data$date - (as.integer(format(data$date, "%u")) - 1)
  data$main <- protocol_main(data)
  if (nrow(data) == 0) return(data)
  data <- data[order(data$p_id, data$time), ]
  data <- data[!duplicated(data[c("p_id", "group", "date")], fromLast = TRUE), ]
  # Gruppe einer Person = Gruppe mit den meisten Protokollen
  counts <- table(data$p_id, data$group)
  main_group <- colnames(counts)[max.col(counts, ties.method = "first")]
  names(main_group) <- rownames(counts)
  data <- data[data$group == main_group[data$p_id], ]
  rownames(data) <- NULL
  data
}

protocol_main <- function(data) {
  row_mean <- function(cols) {
    out <- rowMeans(data[cols], na.rm = TRUE)
    out[is.nan(out)] <- NA_real_
    out
  }
  main <- rep(NA_real_, nrow(data))
  pick <- function(group, values) main[data$group == group] <<- values[data$group == group]
  pick("instrumental", row_mean(paste0("L3_", names(mlp_dimensions))))
  pick("stacking", data$time_sec)
  pick("typing", data$words_per_min)
  pick("clapping", row_mean(protocol_fields$clapping))
  main
}

# Breites Format -> langes Format: eine Zeile pro Sitzung und Größe.
# vars: benannter Vektor Spalte = Beschriftung.
protocol_long <- function(data, vars) {
  rows <- lapply(names(vars), function(col) {
    data.frame(p_id = data$p_id, date = data$date, week = data$week,
               variable = rep(vars[[col]], nrow(data)), value = data[[col]],
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$variable <- factor(out$variable, levels = unname(vars))
  out[!is.na(out$value), ]
}

# Gruppenmittel pro Kalenderwoche und Größe. Pro Person zählt der letzte Wert
# der Woche; Wochen mit weniger als min_n Personen entfallen. Der Mittelwert
# wird in der Wochenmitte (Donnerstag) eingezeichnet.
group_mean_long <- function(data, vars, min_n) {
  long <- protocol_long(data, vars)
  if (nrow(long) == 0) return(long[c("date", "variable", "value")])
  long <- long[order(long$date), ]
  long <- long[!duplicated(long[c("p_id", "week", "variable")], fromLast = TRUE), ]
  out <- stats::aggregate(value ~ week + variable, data = long,
                          FUN = function(v) if (length(v) >= min_n) mean(v) else NA_real_)
  out <- out[!is.na(out$value), ]
  out$date <- out$week + 3
  out[c("date", "variable", "value")]
}

# Kennwerte pro Person: Gruppe, Zahl der Protokolle, mittlere Übezeit,
# Hauptgröße im letzten Protokoll und ihre Veränderung seit dem ersten.
person_summary <- function(data) {
  rows <- lapply(split(data, data$p_id), function(d) {
    main <- d$main[!is.na(d$main)]
    data.frame(
      p_id          = d$p_id[1],
      group         = d$group[1],
      n_protocols   = nrow(d),
      first_date    = min(d$date),
      last_date     = max(d$date),
      practice_mean = if (all(is.na(d$practice_week_min))) NA_real_
                      else mean(d$practice_week_min, na.rm = TRUE),
      main_last     = if (length(main) > 0) main[length(main)] else NA_real_,
      main_change   = if (length(main) > 1) main[length(main)] - main[1] else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0) {
    return(data.frame(p_id = character(), group = character(), n_protocols = integer(),
                      first_date = as.Date(character()), last_date = as.Date(character()),
                      practice_mean = numeric(), main_last = numeric(),
                      main_change = numeric(), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# Demodaten: 6 Personen pro Gruppe (IDs 1001-1024), wöchentliche Protokolle
# mit einfachen Lernkurven.
# ------------------------------------------------------------
simulate_protocol_data <- function(n_weeks = 8, start = as.Date("2026-10-19"), seed = 1) {
  set.seed(seed)
  clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)
  rating <- function(skill, previous, cols_level, cols_change, hi) {
    values <- as.list(clamp(round(skill + rnorm(3, 0, 0.3 * hi / 5)), 1, hi))
    names(values) <- cols_level
    if (!is.null(previous)) {
      change <- clamp(round((skill - previous) * 10 / hi + rnorm(3, 0, 0.4)), -2, 2)
      values[cols_change] <- as.list(change)
    }
    values
  }

  rows <- list()
  id <- 1000
  for (group in names(protocol_groups)) {
    for (person in 1:6) {
      id <- id + 1
      practice <- runif(1, 40, 180)
      talent <- rnorm(1)
      skill <- if (group == "clapping") runif(3, 3, 5) else runif(3, 2, 3)
      previous <- NULL
      for (week in seq_len(n_weeks)) {
        if (week > 1 && runif(1) < 0.12) next                  # Woche ausgelassen
        values <- switch(group,
          instrumental = {
            skill <- clamp(skill + rnorm(3, 0.25, 0.15), 1, 5)
            rating(skill, previous, paste0("L3_", names(mlp_dimensions)),
                   paste0("L1_", names(mlp_dimensions)), 5)
          },
          clapping = {
            skill <- clamp(skill + rnorm(3, 0.5, 0.3), 1, 10)
            rating(skill, previous, protocol_fields$clapping, clapping_relative, 10)
          },
          stacking = list(
            time_sec = round(max(8, 12 + (40 - 4 * talent) * exp(-0.2 * week) + rnorm(1, 0, 2)), 1),
            errors   = rpois(1, 4 * exp(-0.2 * week))
          ),
          typing = list(
            words_per_min = round(max(5, 16 + 2 * talent + 3 * week + rnorm(1, 0, 2))),
            errors_half   = rpois(1, 3 * exp(-0.1 * week)),
            errors_wrong  = rpois(1, 6 * exp(-0.15 * week))
          )
        )
        time <- as.POSIXct(start + 7 * (week - 1) + sample(0:4, 1)) + 15 * 3600
        rows[[length(rows) + 1]] <- protocol_row(
          as.character(id), group, time, round(clamp(practice + rnorm(1, 0, 25), 0, 400)), values
        )
        previous <- skill
      }
    }
  }
  finish_protocol_data(do.call(rbind, rows))
}
