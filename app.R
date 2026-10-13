# ============================================================
# Musikalische Entwicklung und ihre Messbarkeit · Lernmonitor
# Shiny App  |  Anton Schreiber, Universität Hamburg (2026)
# ============================================================
# Zeigt Studierenden des Seminars ihre Lernprotokolle (Instrument, Cup
# Stacking, 10-Finger-Schreiben, Clapping Music) und ihre Ergebnisse in den
# Tests und Fragebögen.
# Voraussetzungen: install.packages(c("shiny", "bslib", "ggplot2"))
# Starten:         shiny::runApp()
#
# Umgebungsvariablen:
#   MONITOR_DATA_ROOT       Ordner mit <gruppe>_protocol/output/results
#                           (Standard: /srv/shiny-server)
#   MONITOR_TEST_DIRS       Ergebnisverzeichnisse der Tests, getrennt durch ";"
#   MONITOR_ADMIN_PASSWORD  Passwort der Dozentenansicht (ohne: gesperrt)
# Gibt es weder Protokolle noch Testergebnisse, laufen Demodaten.
# Hilfsfunktionen in R/ werden von Shiny automatisch geladen.
# ============================================================

library(shiny)
library(bslib)

data_root      <- Sys.getenv("MONITOR_DATA_ROOT", unset = "/srv/shiny-server")
test_dirs      <- strsplit(Sys.getenv("MONITOR_TEST_DIRS"), ";", fixed = TRUE)[[1]]
admin_password <- Sys.getenv("MONITOR_ADMIN_PASSWORD")

# Schutz der Anonymität: Gruppenmittel erst ab so vielen Personen pro Woche,
# Verteilungen erst ab so vielen weiteren Personen mit einem Wert.
min_n_group_mean   <- 3
min_n_distribution <- 5

load_monitor_data <- function() {
  protocols <- read_protocols(data_root)
  tests <- read_tests(test_dirs)
  demo <- nrow(protocols) == 0 && nrow(tests) == 0
  if (demo) {
    protocols <- simulate_protocol_data()
    tests <- simulate_test_data(unique(protocols$p_id))
  }
  list(protocols = protocols, tests = tests,
       persons = person_table(protocols, tests), demo = demo)
}

variable_choices <- setNames(monitor_variables$id, monitor_variables$label)

# ── UI ──────────────────────────────────────────────────────
ui <- page_navbar(
  title = span(
    class = "monitor-title",
    img(src = "uhh-logo.svg", alt = "Universität Hamburg"),
    "Musikalische Entwicklung und ihre Messbarkeit"
  ),
  window_title = "Musikalische Entwicklung und ihre Messbarkeit",
  theme = bs_theme(version = 5, primary = uhh_blau, secondary = uhh_steingrau,
                   danger = uhh_rot, "body-color" = uhh_steingrau),
  fillable = FALSE,
  header = tags$head(tags$style(HTML(paste0("
    .navbar { background: #fff !important; border-bottom: 3px solid ", uhh_blau, "; }
    .monitor-title { display: flex; align-items: center; gap: 16px;
                     font-weight: 700; color: ", uhh_steingrau, "; }
    .monitor-title img { height: 44px; }
    .navbar .nav-link { color: ", uhh_steingrau, "; }
    .navbar .nav-link.active { color: ", uhh_blau, " !important; font-weight: 700; }
    .demo-note { background: #fff; border: 1px solid ", uhh_hellblau, ";
                 border-left: 5px solid ", uhh_blau, "; color: ", uhh_steingrau, "; }
    .curve-panel { margin-bottom: 2.5rem; }
  ")))),
  sidebar = sidebar(
    width = 280,
    textInput("student_id", "Matrikelnummer", placeholder = "ID aus den Protokollen"),
    actionButton("login", "Anzeigen", class = "btn-primary"),
    uiOutput("login_message"),
    uiOutput("demo_note")
  ),

  nav_panel(
    "Übersicht",
    uiOutput("overview")
  ),
  nav_panel(
    "Lernverlauf",
    h3("Lernverlauf"),
    uiOutput("curve_panels")
  ),
  nav_panel(
    "Übezeit",
    h3("Übezeit"),
    p("Gesamte Übezeit in der Woche vor dem Protokoll (Minuten)."),
    plotOutput("practice_plot", height = "420px")
  ),
  nav_panel(
    "Univariat",
    h3("Dein Wert im Seminar"),
    selectInput("uni_var", "Variable", choices = variable_choices, width = "100%"),
    plotOutput("uni_plot", height = "440px")
  ),
  nav_panel(
    "Bivariat",
    h3("Zusammenhang zweier Variablen"),
    layout_columns(
      selectInput("bi_x", "x-Achse", choices = variable_choices, width = "100%"),
      selectInput("bi_y", "y-Achse", choices = variable_choices,
                  selected = "practice_mean", width = "100%")
    ),
    plotOutput("bi_plot", height = "480px")
  ),
  nav_spacer(),
  nav_panel(
    "Dozent*in",
    h3("Dozentenansicht"),
    uiOutput("lecturer_ui")
  )
)

# ── Server ──────────────────────────────────────────────────
server <- function(input, output, session) {
  # Daten werden beim Seitenaufruf und bei jedem "Anzeigen" neu eingelesen
  monitor   <- reactiveVal(load_monitor_data())
  active_id <- reactiveVal(NULL)
  lecturer  <- reactiveVal(FALSE)
  wrong_password <- reactiveVal(FALSE)

  observeEvent(input$login, {
    monitor(load_monitor_data())
    active_id(normalize_id(input$student_id))
  })

  known_id <- reactive(!is.null(active_id()) && active_id() %in% monitor()$persons$p_id)

  need_person <- function() {
    validate(need(known_id(),
                  "Bitte links die Matrikelnummer eingeben und auf „Anzeigen“ klicken."))
  }

  person <- reactive({
    need_person()
    monitor()$persons[monitor()$persons$p_id == active_id(), ]
  })
  own_protocols <- reactive({
    need_person()
    monitor()$protocols[monitor()$protocols$p_id == active_id(), ]
  })
  group_protocols <- reactive({
    monitor()$protocols[monitor()$protocols$group %in% person()$group, ]
  })
  need_protocols <- function() {
    validate(need(nrow(own_protocols()) > 0, "Es liegen noch keine Lernprotokolle vor."))
  }

  output$login_message <- renderUI({
    if (!is.null(active_id()) && !known_id()) {
      div(class = "text-danger small", "Zu dieser Matrikelnummer liegen keine Daten vor.")
    }
  })

  output$demo_note <- renderUI({
    if (monitor()$demo) {
      div(class = "alert demo-note small mt-3",
          strong("Demodaten."), " Es wurden keine Ergebnisse gefunden.",
          " Beispiel-IDs: 1001 (Instrument), 1007 (Cup Stacking),",
          " 1013 (10-Finger-Schreiben), 1019 (Clapping Music).")
    }
  })

  # --- Übersicht ---
  output$overview <- renderUI({
    group <- person()$group
    date_or_dash <- function(date) if (is.na(date)) "–" else format(date, "%d.%m.%Y")
    box <- function(title, value) value_box(title, value, theme = "primary")
    tagList(
      h3(paste0("Matrikelnummer ", active_id(),
                if (!is.na(group)) paste0(" · Gruppe ", protocol_groups[[group]]))),
      layout_columns(
        box("Ausgefüllte Protokolle", person()$n_protocols),
        box("Erstes Protokoll", date_or_dash(person()$first_date)),
        box("Letztes Protokoll", date_or_dash(person()$last_date))
      ),
      h4("Tests und Fragebögen", class = "mt-4"),
      tableOutput("test_table")
    )
  })

  output$test_table <- renderTable({
    req(known_id())
    tests <- monitor()$tests[monitor()$tests$p_id == active_id(), ]
    validate(need(nrow(tests) > 0, "Es liegen noch keine Ergebnisse aus Tests oder Fragebögen vor."))
    tests <- tests[order(tests$time), ]
    data.frame(
      "Test / Fragebogen" = test_registry$label[match(tests$test, test_registry$id)],
      "Wert"              = format_value(tests$value),
      "Datum"             = format(tests$date, "%d.%m.%Y"),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, align = "lrl")

  # --- Lernverlauf ---
  panels <- reactive({
    need_protocols()
    group_panels[[person()$group]]
  })

  output$curve_panels <- renderUI({
    lapply(seq_along(panels()), function(i) {
      panel <- panels()[[i]]
      height <- if (panel$type == "level") 90 + 230 * length(panel$vars) else 440
      div(class = "curve-panel", h4(panel$title), p(panel$note),
          plotOutput(paste0("curve_", i), height = paste0(height, "px")))
    })
  })

  # Keine Gruppe hat mehr als zwei Diagramme
  lapply(1:2, function(i) {
    output[[paste0("curve_", i)]] <- renderPlot({
      req(length(panels()) >= i)
      panel <- panels()[[i]]
      own <- protocol_long(own_protocols(), panel$vars)
      group <- group_mean_long(group_protocols(), panel$vars, min_n_group_mean)
      if (panel$type == "level") plot_level(own, group, panel$limits)
      else plot_change(own, group, panel$caption)
    }, res = 96)
  })

  # --- Übezeit ---
  output$practice_plot <- renderPlot({
    need_protocols()
    vars <- c(practice_week_min = "Übezeit")
    plot_practice(protocol_long(own_protocols(), vars),
                  group_mean_long(group_protocols(), vars, min_n_group_mean)[c("date", "value")])
  }, res = 96)

  # --- Univariat und Bivariat ---
  # Die Dozentenansicht zeigt Verteilungen auch bei wenigen Personen.
  min_others <- reactive(if (lecturer()) 1 else min_n_distribution)

  compare <- function(vars) {
    data <- comparison_data(monitor()$persons, active_id(), vars)
    validate(need(any(data$own), "Zu dieser Auswahl liegt für dich noch kein Wert vor."))
    validate(need(sum(!data$own) >= min_others(), sprintf(
      "Der Vergleich erscheint, sobald Werte von mindestens %d weiteren Personen vorliegen.",
      min_n_distribution)))
    data
  }

  output$uni_plot <- renderPlot({
    need_person()
    data <- compare(input$uni_var)
    values <- data[[input$uni_var]]
    plot_histogram(values[!data$own], values[data$own],
                   variable_label(input$uni_var, person()$group))
  }, res = 96)

  output$bi_plot <- renderPlot({
    need_person()
    validate(need(input$bi_x != input$bi_y, "Bitte zwei verschiedene Variablen wählen."))
    data <- compare(c(input$bi_x, input$bi_y))
    plot_scatter(data.frame(x = data[[input$bi_x]], y = data[[input$bi_y]], own = data$own),
                 variable_label(input$bi_x, person()$group),
                 variable_label(input$bi_y, person()$group))
  }, res = 96)

  # --- Dozentenansicht ---
  observeEvent(input$lecturer_login, {
    lecturer(nzchar(admin_password) && identical(input$lecturer_password, admin_password))
    wrong_password(!lecturer())
    if (lecturer()) monitor(load_monitor_data())
  })

  observeEvent(input$lecturer_show, active_id(input$lecturer_person))

  output$lecturer_ui <- renderUI({
    if (!nzchar(admin_password)) {
      return(p("Die Dozentenansicht ist nicht eingerichtet",
               "(Umgebungsvariable MONITOR_ADMIN_PASSWORD fehlt)."))
    }
    if (!lecturer()) {
      return(tagList(
        passwordInput("lecturer_password", "Passwort"),
        actionButton("lecturer_login", "Anmelden", class = "btn-primary"),
        if (wrong_password()) div(class = "text-danger small mt-2", "Falsches Passwort.")
      ))
    }
    persons <- monitor()$persons
    tagList(
      p("Eine Person auswählen, um ihre Ansicht in den anderen Reitern zu öffnen."),
      layout_columns(
        col_widths = c(6, 3),
        selectInput("lecturer_person", NULL, choices = persons$p_id, width = "100%"),
        actionButton("lecturer_show", "Person ansehen", class = "btn-primary")
      ),
      h4("Rücklauf pro Woche", class = "mt-4"),
      plotOutput("response_plot", height = "380px"),
      h4("Alle Personen", class = "mt-4"),
      tableOutput("person_table")
    )
  })

  output$response_plot <- renderPlot({
    req(lecturer())
    plot_response(monitor()$protocols)
  }, res = 96)

  output$person_table <- renderTable({
    req(lecturer())
    persons <- monitor()$persons
    persons <- persons[order(persons$group, persons$p_id), ]
    data.frame(
      "Matrikelnummer"     = persons$p_id,
      "Gruppe"             = unname(protocol_groups[persons$group]),
      "Protokolle"         = persons$n_protocols,
      "Letztes Protokoll"  = format(persons$last_date, "%d.%m.%Y"),
      "Übezeit (Mittel)"   = round(persons$practice_mean),
      "Tests / Fragebögen" = persons$n_tests,
      check.names = FALSE
    )
  }, digits = 0, na = "–", striped = TRUE, hover = TRUE)
}

shinyApp(ui, server)
