# Combined Grade Estimator - Shiny App (one app, several courses)
#
# Students can pick their course from the dropdown, or you can give each class
# a direct link that opens on its course:
#   .../grade_estimator/?course=wakao
#   .../grade_estimator/?course=govt2305
#   .../grade_estimator/?course=econ
#
# To add or change a course, edit the `courses` list below.

library(shiny)

# ---- Course settings -------------------------------------------------------
courses <- list(

  wakao = list(
    name = "Prof. Wakao",
    bg = "#f5f7fa", accent = "#2f6aa3", dark = "#1f4e79",
    components = list(
      list(id = "exams", label = "Exams (best 3 of 4)", weight = 60,
           heading = "Exams — 60% (lowest of 4 is dropped)",
           parts = paste0("exam", 1:4), part_labels = paste("Exam", 1:4),
           drop_lowest = TRUE),
      list(id = "tutoring", label = "Tutoring Center Assignment", weight = 5),
      list(id = "paper",    label = "Position Paper",             weight = 15),
      list(id = "homework", label = "Homework (average)",         weight = 20)
    )
  ),

  govt2305 = list(
    name = "GOVT-2305 (Skipworth)",
    bg = "#e6f2ee", accent = "#2e8b6e", dark = "#1d5c4a",
    components = list(
      list(id = "paper",   label = "Position Paper & Class Discussion Assignment", weight = 40),
      list(id = "quizzes", label = "Quizzes (average)", weight = 30),
      list(id = "midterm", label = "Midterm Oral Exam", weight = 10),
      list(id = "final",   label = "Final Oral Exam",   weight = 20)
    )
  ),

  econ = list(
    name = "ECON-2301/2302 (Li)",
    bg = "#e8f0fa", accent = "#3567b5", dark = "#1f3f73",
    components = list(
      list(id = "exam1",       label = "Quiz/Exam 1",           weight = 20),
      list(id = "exam2",       label = "Quiz/Exam 2",           weight = 20),
      list(id = "exam3",       label = "Quiz/Exam 3",           weight = 20),
      list(id = "assignments", label = "Assignments (average)", weight = 35),
      list(id = "discussion",  label = "Discussion",            weight = 5)
    )
  )
)

# ---- Helpers ---------------------------------------------------------------
letter_grade <- function(p) {
  if (is.na(p)) return("-")
  if (p >= 90) "A" else if (p >= 80) "B" else if (p >= 70) "C" else if (p >= 60) "D" else "F"
}

grade_input <- function(id, label) {
  numericInput(id, label, value = NA, min = 0, max = 100, step = 0.1)
}

clean <- function(x) if (is.null(x) || is.na(x)) NA else max(0, min(100, x))

all_input_ids <- function(key) {
  ids <- unlist(lapply(courses[[key]]$components, function(comp)
    if (!is.null(comp$parts)) comp$parts else comp$id))
  paste0(key, "_", ids)
}

# ---- UI --------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .card { background: #fff; border-radius: 10px; padding: 20px; margin-bottom: 20px;
            box-shadow: 0 1px 4px rgba(0,0,0,.08); border-top: 4px solid #2f6aa3; }
    .final { font-size: 56px; font-weight: 700; line-height: 1; }
    .letter { font-size: 28px; font-weight: 600; color: #555; }
    .note { color: #666; font-size: 13px; }
  "))),
  uiOutput("theme"),
  titlePanel("Course Grade Estimator"),
  fluidRow(
    column(5,
      div(class = "card",
        selectInput("course", "Course",
                    choices = setNames(names(courses), sapply(courses, `[[`, "name"))),
        hr(),
        uiOutput("inputs"),
        actionButton("reset", "Clear all", icon = icon("eraser")),
        p(class = "note", br(),
          "Enter each grade as a percentage (0–100). Leave a box blank if that grade isn't available yet.")
      )
    ),
    column(7,
      div(class = "card",
        h4("Estimated Final Grade"),
        uiOutput("final_display"),
        p(class = "note", textOutput("basis_note"))
      ),
      div(class = "card",
        h4("Breakdown"),
        tableOutput("breakdown")
      )
    )
  )
)

# ---- Server ----------------------------------------------------------------
server <- function(input, output, session) {

  # Open on the course named in the link, e.g. ?course=econ
  observe({
    q <- parseQueryString(session$clientData$url_search)
    if (!is.null(q$course) && q$course %in% names(courses))
      updateSelectInput(session, "course", selected = q$course)
  })

  cfg <- reactive({
    req(input$course %in% names(courses))
    courses[[input$course]]
  })

  output$theme <- renderUI({
    c <- cfg()
    tags$style(HTML(sprintf(
      "body { background: %s; } h2 { color: %s; } .card { border-top-color: %s; }
       .final { color: %s; } .btn-default { border-color: %s; color: %s; }",
      c$bg, c$dark, c$accent, c$dark, c$accent, c$dark)))
  })

  output$inputs <- renderUI({
    c <- cfg(); key <- input$course
    tagList(lapply(c$components, function(comp) {
      if (!is.null(comp$parts)) {
        tagList(
          h5(strong(comp$heading)),
          fluidRow(lapply(seq_along(comp$parts), function(i)
            column(6, grade_input(paste0(key, "_", comp$parts[i]),
                                  paste0(comp$part_labels[i], " (%)"))))),
          hr()
        )
      } else {
        grade_input(paste0(key, "_", comp$id),
                    sprintf("%s (%%) — %d%%", comp$label, comp$weight))
      }
    }))
  })

  observeEvent(input$reset, {
    for (id in all_input_ids(input$course)) updateNumericInput(session, id, value = NA)
  })

  results <- reactive({
    c <- cfg(); key <- input$course
    get <- function(id) clean(input[[paste0(key, "_", id)]])

    rows <- lapply(c$components, function(comp) {
      if (!is.null(comp$parts)) {
        v <- sapply(comp$parts, get)
        entered <- sum(!is.na(v))
        dropped <- NA
        if (isTRUE(comp$drop_lowest) && entered == length(v)) {
          d <- which.min(v)
          dropped <- comp$part_labels[d]
          s <- mean(v[-d])
        } else if (entered > 0) {
          s <- mean(v, na.rm = TRUE)
        } else s <- NA
        list(label = comp$label, weight = comp$weight, score = s,
             drop = isTRUE(comp$drop_lowest), dropped = dropped,
             entered = entered, total = length(v))
      } else {
        list(label = comp$label, weight = comp$weight, score = get(comp$id),
             drop = FALSE, dropped = NA, entered = NA, total = NA)
      }
    })

    scores  <- sapply(rows, `[[`, "score")
    weights <- sapply(rows, `[[`, "weight")
    have    <- !is.na(scores)
    final   <- if (any(have)) sum(scores[have] * weights[have]) / sum(weights[have]) else NA
    list(rows = rows, scores = scores, weights = weights, have = have, final = final)
  })

  output$final_display <- renderUI({
    f <- results()$final
    if (is.na(f)) {
      div(class = "final", style = "color:#bbb;", "—")
    } else {
      tagList(
        div(class = "final", sprintf("%.2f%%", f)),
        div(class = "letter", paste("Letter grade:", letter_grade(f)))
      )
    }
  })

  output$basis_note <- renderText({
    r <- results()
    if (!any(r$have)) return("Enter at least one grade to see an estimate.")
    counted <- sum(r$weights[r$have])
    msg <- if (counted < 100)
      sprintf("Based on the %d%% of the course you've entered so far (assumes you keep the same performance on the rest).", counted)
    else "Based on all course components."
    for (row in r$rows) {
      if (row$drop) {
        if (!is.na(row$dropped))
          msg <- paste0(msg, " ", row$dropped, " (your lowest) was dropped.")
        else if (row$entered > 0)
          msg <- paste0(msg, " The lowest exam will be dropped once all ", row$total, " are entered.")
      }
    }
    msg
  })

  output$breakdown <- renderTable({
    r <- results()
    data.frame(
      Category = sapply(r$rows, `[[`, "label"),
      Weight = paste0(r$weights, "%"),
      Score = ifelse(r$have, sprintf("%.2f%%", r$scores), "—"),
      `Points Earned` = ifelse(r$have,
                               sprintf("%.2f / %d", r$scores * r$weights / 100, r$weights), "—"),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, width = "100%")
}

shinyApp(ui, server)
