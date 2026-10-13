# ============================================================
# Diagramme des Monitors (ggplot2)
# ============================================================

# UHH Corporate-Design-Farben
uhh_rot       <- "#E2001A"
uhh_blau      <- "#0271BB"
uhh_steingrau <- "#3B515B"
uhh_hellrot   <- "#F08288"
uhh_hellblau  <- "#80B8DB"
uhh_hellgrau  <- "#B2BABD"
# Grün gehört nicht zur UHH-Palette, nur für "besser" im Individuums-Verlauf
monitor_gruen <- "#2E9E5B"

series_colours   <- c("Eigene Werte" = uhh_blau, "Mittelwert der Gruppe" = uhh_hellgrau)
series_linetypes <- c("Eigene Werte" = "solid", "Mittelwert der Gruppe" = "dashed")
progress_colours <- c("besser" = monitor_gruen, "gleich" = uhh_hellgrau, "schlechter" = uhh_rot)
group_colours    <- c("Instrument" = uhh_blau, "Cup Stacking" = uhh_rot,
                      "10-Finger-Schreiben" = uhh_steingrau, "Clapping Music" = uhh_hellblau)
group_mean_caption <- "Gestrichelt: Mittelwert deiner Gruppe pro Kalenderwoche"

theme_monitor <- function() {
  ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      legend.position    = "bottom",
      legend.title       = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      text               = ggplot2::element_text(colour = uhh_steingrau),
      axis.text          = ggplot2::element_text(colour = uhh_steingrau),
      plot.caption       = ggplot2::element_text(hjust = 0),
      strip.text         = ggplot2::element_text(face = "bold", hjust = 0, colour = uhh_steingrau)
    )
}

scale_x_monitor <- function() ggplot2::scale_x_date(date_labels = "%d.%m.", name = NULL)

empty_plot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message, size = 6, colour = uhh_steingrau) +
    ggplot2::theme_void()
}

format_value <- function(x) format(round(x, 2), decimal.mark = ",", nsmall = 0)

# --- Lernverlauf: Größen im Zeitverlauf ---------------------
# Ein Feld pro Größe; eigene Werte als Linie, Gruppenmittel gestrichelt.
# own, group: date, variable, value. limits = NULL: freie y-Achse ab 0.
plot_level <- function(own, group, limits = NULL) {
  if (nrow(own) == 0) return(empty_plot("Noch keine Werte vorhanden."))
  long <- rbind(
    data.frame(own[c("date", "variable", "value")], series = "Eigene Werte"),
    if (nrow(group) > 0) data.frame(group, series = "Mittelwert der Gruppe")
  )
  long$series <- factor(long$series, levels = rev(names(series_colours)))
  p <- ggplot2::ggplot(long, ggplot2::aes(date, value, colour = series,
                                          linetype = series, group = series)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_colour_manual(values = series_colours) +
    ggplot2::scale_linetype_manual(values = series_linetypes) +
    ggplot2::guides(colour = ggplot2::guide_legend(reverse = TRUE),
                    linetype = ggplot2::guide_legend(reverse = TRUE)) +
    scale_x_monitor() +
    ggplot2::labs(y = NULL, caption = if (nrow(group) > 0) group_mean_caption) +
    theme_monitor()
  if (is.null(limits)) {
    p + ggplot2::facet_wrap(~variable, ncol = 1, scales = "free_y") +
      ggplot2::expand_limits(y = 0)
  } else {
    p + ggplot2::facet_wrap(~variable, ncol = 1) +
      ggplot2::scale_y_continuous(limits = limits, breaks = seq(limits[1], limits[2]))
  }
}

# --- Lernverlauf: Vergleich zum letzten Protokoll -----------
# y = Veränderung seit dem letzten Protokoll (Mittel der Größen), zentriert
# bei 0. Farbe nach Lage zur Nulllinie: darüber grün, darauf grau, darunter rot.

progress_state <- function(value) {
  factor(ifelse(value > 0, "besser", ifelse(value < 0, "schlechter", "gleich")),
         levels = names(progress_colours))
}

# Zerlegt die Linie in Abschnitte zwischen zwei Protokollen. Kreuzt ein
# Abschnitt die Nulllinie, wird er dort geteilt, damit die Farbe genau an der
# Nulllinie wechselt.
progress_segments <- function(long) {
  segment <- function(x0, y0, x1, y1) {
    data.frame(x = x0, y = y0, xend = x1, yend = y1, state = progress_state((y0 + y1) / 2))
  }
  long <- long[order(long$date), ]
  x <- as.numeric(long$date)
  y <- long$value
  out <- list()
  for (i in seq_len(nrow(long) - 1)) {
    if (y[i] * y[i + 1] < 0) {
      x_zero <- x[i] + (x[i + 1] - x[i]) * y[i] / (y[i] - y[i + 1])
      out[[length(out) + 1]] <- segment(x[i], y[i], x_zero, 0)
      out[[length(out) + 1]] <- segment(x_zero, 0, x[i + 1], y[i + 1])
    } else {
      out[[length(out) + 1]] <- segment(x[i], y[i], x[i + 1], y[i + 1])
    }
  }
  if (length(out) == 0) return(NULL)
  out <- do.call(rbind, out)
  out$x <- as.Date(out$x)
  out$xend <- as.Date(out$xend)
  out
}

plot_change <- function(own, group, caption) {
  if (nrow(own) == 0) {
    return(empty_plot("Noch keine Vergleichswerte: Der Vergleich wird erst ab\ndem zweiten Protokoll erhoben."))
  }
  own <- stats::aggregate(value ~ date, data = own, FUN = mean)
  own$state <- progress_state(own$value)
  segments <- progress_segments(own)

  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, colour = uhh_steingrau, linewidth = 0.6)
  if (nrow(group) > 0) {
    group <- stats::aggregate(value ~ date, data = group, FUN = mean)
    p <- p +
      ggplot2::geom_line(data = group, ggplot2::aes(date, value), colour = uhh_hellgrau,
                         linewidth = 1, linetype = "dashed") +
      ggplot2::geom_point(data = group, ggplot2::aes(date, value), colour = uhh_hellgrau, size = 2.4)
    caption <- paste0(caption, "\n", group_mean_caption)
  }
  if (!is.null(segments)) {
    p <- p + ggplot2::geom_segment(
      data = segments,
      ggplot2::aes(x, y, xend = xend, yend = yend, colour = state),
      linewidth = 1.3, lineend = "round", show.legend = TRUE
    )
  }
  p +
    ggplot2::geom_point(data = own, ggplot2::aes(date, value, colour = state),
                        size = 3.4, show.legend = TRUE) +
    ggplot2::scale_colour_manual(values = progress_colours, drop = FALSE) +
    ggplot2::scale_y_continuous(
      limits = c(-2, 2), breaks = -2:2,
      labels = c("−2\ndeutlich schlechter", "−1", "0\ngleich", "+1", "+2\ndeutlich besser"),
      name = "Lernfortschritt"
    ) +
    scale_x_monitor() +
    ggplot2::labs(caption = caption) +
    theme_monitor()
}

# --- Übezeit ------------------------------------------------
# Ein Balken pro Protokoll (Minuten in der Woche), Gruppenmittel gestrichelt.
plot_practice <- function(own, group) {
  own <- own[!is.na(own$value), ]
  if (nrow(own) == 0) return(empty_plot("Noch keine Übezeiten vorhanden."))
  p <- ggplot2::ggplot(own, ggplot2::aes(date, value)) +
    ggplot2::geom_col(fill = uhh_blau, width = 4)
  if (nrow(group) > 0) {
    p <- p +
      ggplot2::geom_line(data = group, colour = uhh_steingrau, linewidth = 1, linetype = "dashed") +
      ggplot2::geom_point(data = group, colour = uhh_steingrau, size = 2.6)
  }
  p + scale_x_monitor() +
    ggplot2::labs(y = "Minuten pro Woche", caption = if (nrow(group) > 0) group_mean_caption) +
    theme_monitor() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

# --- Univariat ----------------------------------------------
# Verteilung der übrigen Personen, eigener Wert als rote Linie.
plot_histogram <- function(others, own_value, label) {
  ggplot2::ggplot(data.frame(value = others), ggplot2::aes(value)) +
    ggplot2::geom_histogram(bins = 12, fill = uhh_hellblau, colour = "white") +
    ggplot2::geom_vline(xintercept = own_value, colour = uhh_rot, linewidth = 1.6) +
    ggplot2::scale_y_continuous(breaks = function(l) unique(floor(pretty(l))), name = "Personen") +
    ggplot2::labs(x = label,
                  caption = sprintf("Rote Linie: eigener Wert (%s). Balken: übrige Personen (n = %d).",
                                    format_value(own_value), length(others))) +
    theme_monitor()
}

# --- Bivariat -----------------------------------------------
# data: x, y, own. Übrige Personen hellblau, eigener Punkt rot.
plot_scatter <- function(data, x_label, y_label) {
  r <- suppressWarnings(stats::cor(data$x, data$y))
  ggplot2::ggplot(data[!data$own, ], ggplot2::aes(x, y)) +
    ggplot2::geom_point(colour = uhh_hellblau, size = 3.4) +
    ggplot2::geom_point(data = data[data$own, ], colour = uhh_rot, size = 5.5) +
    ggplot2::labs(x = x_label, y = y_label,
                  caption = sprintf("Roter Punkt: eigener Wert. Korrelation r = %s (n = %d).",
                                    format_value(r), nrow(data))) +
    theme_monitor()
}

# --- Rücklauf (Dozentenansicht) -----------------------------
# Zahl der Protokolle pro Kalenderwoche und Gruppe.
plot_response <- function(protocols) {
  if (nrow(protocols) == 0) return(empty_plot("Noch keine Protokolle vorhanden."))
  counts <- stats::aggregate(n ~ week + group, FUN = sum,
                             data = data.frame(week = protocols$week, n = 1,
                                               group = protocol_groups[protocols$group]))
  counts$group <- factor(counts$group, levels = unname(protocol_groups))
  ggplot2::ggplot(counts, ggplot2::aes(week, n, fill = group)) +
    ggplot2::geom_col(width = 5) +
    ggplot2::scale_fill_manual(values = group_colours) +
    ggplot2::scale_x_date(date_labels = "%d.%m.", name = "Woche ab") +
    ggplot2::scale_y_continuous(breaks = function(l) unique(floor(pretty(l))), name = "Protokolle") +
    theme_monitor() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}
