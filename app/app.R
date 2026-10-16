# Combined Grade Estimator - Shiny App (one app, several courses)
#
# Students pick their professor, then their course, or you can give each class
# a direct link that opens on its course:
#   https://swakao1.github.io/grade-estimator/?course=wakao
#   https://swakao1.github.io/grade-estimator/?course=govt2305
#   https://swakao1.github.io/grade-estimator/?course=econ
#   https://swakao1.github.io/grade-estimator/?course=span1411
#   https://swakao1.github.io/grade-estimator/?course=hist1301
#   https://swakao1.github.io/grade-estimator/?course=hist1302
#
# To add or change a course, edit the `courses` list below.

library(shiny)

# ---- Course settings -------------------------------------------------------
courses <- list(

  wakao = list(
    prof = "Wakao", course = "GOVT-2305/2306",
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
    prof = "Skipworth", course = "GOVT-2305",
    bg = "#e6f2ee", accent = "#2e8b6e", dark = "#1d5c4a",
    components = list(
      list(id = "paper",   label = "Position Paper & Class Discussion Assignment", weight = 40),
      list(id = "quizzes", label = "Quizzes (average)", weight = 30),
      list(id = "midterm", label = "Midterm Oral Exam", weight = 10),
      list(id = "final",   label = "Final Oral Exam",   weight = 20)
    )
  ),

  econ = list(
    prof = "Li", course = "ECON-2301/2302",
    bg = "#e8f0fa", accent = "#3567b5", dark = "#1f3f73",
    components = list(
      list(id = "exam1",       label = "Quiz/Exam 1",           weight = 20),
      list(id = "exam2",       label = "Quiz/Exam 2",           weight = 20),
      list(id = "exam3",       label = "Quiz/Exam 3",           weight = 20),
      list(id = "assignments", label = "Assignments (average)", weight = 35),
      list(id = "discussion",  label = "Discussion",            weight = 5)
    )
  ),

  span1411 = list(
    prof = "Naranjo", course = "SPAN-1411",
    bg = "#fbf1e6", accent = "#c8692b", dark = "#8a4318",
    components = list(
      list(id = "quizzes",     label = "Chapter Quizzes",  weight = 25),
      list(id = "discussions", label = "Discussions",      weight = 25),
      list(id = "exams",       label = "Chapter Exams",    weight = 20),
      list(id = "final",       label = "Final Exam",       weight = 10),
      list(id = "project",     label = "Cultural Project", weight = 20)
    )
  ),

  hist1301 = list(
    prof = "Schafman", course = "HIST-1301",
    bg = "#f4eef7", accent = "#7a4f9e", dark = "#4e2f6b",
    components = list(
      list(id = "attendance", label = "Attendance",                       weight = 10),
      list(id = "quizzes",    label = "Quizzes",                          weight = 5),
      list(id = "discussion", label = "Discussion",                       weight = 5),
      list(id = "responses",  label = "Written Primary Source Responses", weight = 5),
      list(id = "oral",       label = "Oral Source Presentation",         weight = 5),
      list(id = "writing",    label = "Writing Assignment",               weight = 20),
      list(id = "midterm",    label = "Midterm Exam",                     weight = 25),
      list(id = "final",      label = "Final Exam",                       weight = 25)
    )
  ),

  hist1302 = list(
    prof = "Schafman", course = "HIST-1302",
    bg = "#f8ecee", accent = "#a8434f", dark = "#702832",
    components = list(
      list(id = "attendance", label = "Attendance",                              weight = 10),
      list(id = "quizzes",    label = "Quizzes",                                 weight = 5),
      list(id = "discussion", label = "Discussion",                              weight = 5),
      list(id = "responses",  label = "Written Primary Source Responses",        weight = 5),
      list(id = "oral",       label = "Oral Source Presentation",                weight = 5),
      list(id = "writing1",   label = "Writing Assignment 1",                    weight = 10),
      list(id = "spr",        label = "Social and Personal Responsibility Paper", weight = 10),
      list(id = "midterm",    label = "Midterm Exam",                            weight = 25),
      list(id = "final",      label = "Final Exam",                              weight = 25)
    )
  )
)

# ---- Helpers ---------------------------------------------------------------
letter_grade <- function(p) {
  if (is.na(p)) return("-")
  p <- round(p, 2)  # match the percentage shown on screen
  if (p >= 90) "A" else if (p >= 80) "B" else if (p >= 70) "C" else if (p >= 60) "D" else "F"
}

grade_input <- function(id, label) {
  numericInput(id, label, value = NA, min = 0, max = 150, step = 0.1)
}

# Scores above 100 (extra credit) are kept; only negatives are corrected.
clean <- function(x) if (is.null(x) || is.na(x)) NA else max(0, x)

profs <- sort(unique(sapply(courses, `[[`, "prof")))

# Course choices for one professor: labels are course numbers, values are keys
course_choices <- function(prof) {
  keys <- names(courses)[sapply(courses, `[[`, "prof") == prof]
  setNames(keys, sapply(courses[keys], `[[`, "course"))
}

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
        selectInput("prof", "Professor", choices = profs),
        selectInput("course", "Course", choices = NULL),
        hr(),
        uiOutput("inputs"),
        actionButton("reset", "Clear all", icon = icon("eraser")),
        p(class = "note", br(),
          "Enter each grade as a percentage (scores above 100 for extra credit are fine). ",
          "Leave a box blank only if that grade isn't available yet. ",
          "If you missed an exam or assignment, enter 0.")
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
  link_course <- reactiveVal(NULL)
  observeEvent(session$clientData$url_search, {
    q <- parseQueryString(session$clientData$url_search)
    if (!is.null(q$course) && q$course %in% names(courses)) {
      link_course(q$course)
      updateSelectInput(session, "prof", selected = courses[[q$course]]$prof)
    }
  }, once = TRUE)

  # When the professor changes, show only that professor's courses
  observeEvent(input$prof, {
    ch <- course_choices(input$prof)
    sel <- ch[1]
    if (isTRUE(link_course() %in% ch)) {  # use the linked course once, then forget it
      sel <- link_course()
      link_course(NULL)
    }
    updateSelectInput(session, "course", choices = ch, selected = sel)
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
          msg <- paste0(msg, " The lowest exam will be dropped once all ", row$total,
                        " are entered. If you missed an exam, enter 0 for it so the estimate is accurate.")
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
