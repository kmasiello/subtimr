library(shiny)
library(bslib)

source("R/helpers.R")

ui <- page_fillable(
  title = "Subtimr",
  theme = bs_theme(version = 5, primary = "#2e7d32"),
  navset_card_underline(
    title = "Subtimr \u2014 Substitution Tracker",

    nav_panel(
      "Game",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Clock"),
          div(
            style = "text-align:center;",
            h5(textOutput("half_label", inline = TRUE), class = "text-muted mb-0"),
            h1(textOutput("clock_display", inline = TRUE), style = "font-size: 3.5rem; font-weight: 700;"),
            div(textOutput("total_time_display", inline = TRUE), class = "text-muted")
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            actionButton("start_clock", "Start", class = "btn-success w-100"),
            actionButton("pause_clock", "Pause", class = "btn-warning w-100"),
            actionButton("reset_clock", "Reset", class = "btn-outline-danger w-100")
          ),
          uiOutput("end_half_ui"),
          hr(),
          actionButton("new_game", "New Game\u2026", class = "btn-outline-primary w-100"),
          downloadButton("download_summary", "Download Time Summary (CSV)", class = "btn-outline-secondary w-100 mt-2"),
          hr(),
          uiOutput("add_to_game_ui")
        ),
        card(
          card_header("Substitute"),
          layout_columns(
            col_widths = c(6, 6),
            div(
              h5("On field"),
              uiOutput("on_field_table"),
              uiOutput("sub_out_ui")
            ),
            div(
              h5("Bench"),
              uiOutput("bench_table"),
              uiOutput("sub_in_ui")
            )
          ),
          div(
            class = "mt-2",
            actionButton("do_sub", "Substitute", class = "btn-primary"),
            textOutput("sub_validation", inline = TRUE)
          )
        )
      )
    ),

    nav_panel(
      "Roster",
      card(
        card_header("Manage roster"),
        p("This roster is saved to disk and persists across games."),
        layout_columns(
          col_widths = c(3, 6, 3),
          textInput("new_number", "Number", placeholder = "e.g. 7"),
          textInput("new_name", "Name", placeholder = "e.g. Alex Kim"),
          actionButton("add_player", "Add player", class = "btn-primary mt-4")
        ),
        tableOutput("roster_table"),
        uiOutput("remove_player_ui"),
        actionButton("remove_player", "Remove selected", class = "btn-outline-danger")
      )
    )
  )
)

server <- function(input, output, session) {

  # ---- Persisted state, loaded once when the app/session starts ----
  game_state <- reactiveValues(
    players = local({ gs <- load_game_state(); gs$players }),
    clock   = local({ gs <- load_game_state(); gs$clock })
  )
  roster <- reactiveVal(load_roster())

  current_half <- reactiveVal(local({ gs <- load_game_state(); gs$current_half }))
  completed_halves_seconds <- reactiveVal(local({ gs <- load_game_state(); gs$completed_halves_seconds }))

  # ---- Momentary row highlighting after a substitution ----
  highlight_ids <- reactiveVal(character(0))
  highlight_expire <- reactiveVal(NULL)

  # Self-scheduling: reruns every 1.5s while there's an active highlight,
  # clearing it once its expiry has passed, then goes dormant again.
  observe({
    ids <- highlight_ids()
    if (length(ids) == 0) return(invisible())
    invalidateLater(1500)
    if (!is.null(isolate(highlight_expire())) && Sys.time() >= isolate(highlight_expire())) {
      highlight_ids(character(0))
    }
  })

  persist <- function() {
    save_game_state(list(
      players = game_state$players, clock = game_state$clock,
      current_half = current_half(), completed_halves_seconds = completed_halves_seconds()
    ))
  }

  # Ticks once a second purely to redraw displays; never mutates state.
  tick <- reactiveTimer(1000)

  # ---------------- Roster management ----------------
  observeEvent(input$add_player, {
    validate(need(nzchar(trimws(input$new_name)), "Enter a name"))
    r <- roster()
    r <- rbind(r, data.frame(
      id = new_id(), number = trimws(input$new_number), name = trimws(input$new_name),
      stringsAsFactors = FALSE
    ))
    roster(r)
    save_roster(r)
    updateTextInput(session, "new_number", value = "")
    updateTextInput(session, "new_name", value = "")
  })

  output$roster_table <- renderTable({
    r <- roster()
    if (nrow(r) == 0) return(data.frame(Number = character(0), Name = character(0)))
    data.frame(Number = r$number, Name = r$name)
  })

  output$remove_player_ui <- renderUI({
    r <- roster()
    if (nrow(r) == 0) return(NULL)
    choices <- setNames(r$id, paste0(r$number, " \u2014 ", r$name))
    selectInput("remove_player_id", NULL, choices = choices)
  })

  observeEvent(input$remove_player, {
    req(input$remove_player_id)
    r <- roster()
    r <- r[r$id != input$remove_player_id, , drop = FALSE]
    roster(r)
    save_roster(r)
  })

  # ---------------- New game setup ----------------
  observeEvent(input$new_game, {
    r <- roster()
    validate_have_roster <- nrow(r) > 0
    if (!validate_have_roster) {
      showModal(modalDialog("Add players to the roster first.", easyClose = TRUE))
      return()
    }
    showModal(modalDialog(
      title = "Start a new game",
      checkboxGroupInput(
        "today_players", "Who's starting on the field? Everyone else on the roster starts on the bench.",
        choices = setNames(r$id, paste0(r$number, " \u2014 ", r$name)),
        selected = character(0)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_new_game", "Start game", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$confirm_new_game, {
    r <- roster()
    starters <- input$today_players  # NULL is fine -- everyone starts on the bench
    game_state$players <- data.frame(
      id = r$id, number = r$number, name = r$name,
      on_field = r$id %in% starters,
      seconds_played = 0,
      entered_at = ifelse(r$id %in% starters, 0, NA_real_),
      seconds_prior_halves = 0,
      stringsAsFactors = FALSE
    )
    game_state$clock <- empty_clock()
    current_half(1)
    completed_halves_seconds(0)
    persist()
    removeModal()
  })

  # Bring in any roster player not yet part of today's game, as a bench
  # substitute (e.g. someone who wasn't in the starting lineup).
  output$add_to_game_ui <- renderUI({
    r <- roster()
    players <- game_state$players
    eligible <- r[!(r$id %in% players$id), , drop = FALSE]
    if (nrow(r) == 0) return(NULL)
    if (nrow(eligible) == 0) return(p("All roster players are already in today's game."))
    tagList(
      selectInput(
        "add_to_game_id", "Add a substitute to the bench",
        choices = setNames(eligible$id, paste0(eligible$number, " \u2014 ", eligible$name))
      ),
      actionButton("add_to_game", "Add to bench", class = "btn-outline-primary w-100")
    )
  })

  observeEvent(input$add_to_game, {
    req(input$add_to_game_id)
    r <- roster()
    sel <- r[r$id == input$add_to_game_id, , drop = FALSE]
    req(nrow(sel) == 1)
    p <- game_state$players
    p <- rbind(p, data.frame(
      id = sel$id, number = sel$number, name = sel$name,
      on_field = FALSE, seconds_played = 0, entered_at = NA_real_,
      seconds_prior_halves = 0,
      stringsAsFactors = FALSE
    ))
    game_state$players <- p
    persist()
  })

  # ---------------- Clock controls ----------------
  observeEvent(input$start_clock, {
    cl <- game_state$clock
    if (!isTRUE(cl$running)) {
      cl$running <- TRUE
      cl$started_at <- Sys.time()
      game_state$clock <- cl
      persist()
    }
  })

  observeEvent(input$pause_clock, {
    cl <- game_state$clock
    if (isTRUE(cl$running)) {
      cl$accumulated <- current_game_seconds(cl)
      cl$running <- FALSE
      cl$started_at <- as.POSIXct(NA)
      game_state$clock <- cl
      persist()
    }
  })

  observeEvent(input$reset_clock, {
    showModal(modalDialog(
      title = "Reset game?",
      "This clears the clock and every player's playing time for the whole game (both halves), and starts over at Half 1.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Reset", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_reset, {
    p <- game_state$players
    if (nrow(p) > 0) {
      p$seconds_played <- 0
      p$entered_at <- ifelse(p$on_field, 0, NA_real_)
      p$seconds_prior_halves <- 0
    }
    game_state$players <- p
    game_state$clock <- empty_clock()
    current_half(1)
    completed_halves_seconds(0)
    persist()
    removeModal()
  })

  # ---- End of Half 1 -> Start of Half 2 ----
  # Pauses the clock, rolls each player's current-half time into their
  # running total, then resets the (half) clock to zero. Who's on
  # field/bench carries over unchanged unless the coach subs before/after.
  output$end_half_ui <- renderUI({
    if (current_half() >= 2) return(NULL)
    div(class = "mt-2", actionButton("end_half", "End Half 1, Start Half 2", class = "btn-outline-primary w-100"))
  })

  observeEvent(input$end_half, {
    cl <- game_state$clock
    if (isTRUE(cl$running)) {
      cl$accumulated <- current_game_seconds(cl)
      cl$running <- FALSE
      cl$started_at <- as.POSIXct(NA)
    }
    p <- game_state$players
    if (nrow(p) > 0) {
      p$seconds_prior_halves <- p$seconds_prior_halves + player_seconds(p, cl)
      p$seconds_played <- 0
      p$entered_at <- ifelse(p$on_field, 0, NA_real_)
    }
    completed_halves_seconds(completed_halves_seconds() + current_game_seconds(cl))
    game_state$players <- p
    game_state$clock <- empty_clock()
    current_half(2)
    persist()
  })

  output$half_label <- renderText({
    sprintf("Half %d", current_half())
  })

  output$clock_display <- renderText({
    tick()
    format_time(current_game_seconds(game_state$clock))
  })

  output$total_time_display <- renderText({
    tick()
    sprintf("Total: %s", format_time(completed_halves_seconds() + current_game_seconds(game_state$clock)))
  })

  # ---------------- Live tables (on field / bench) ----------------
  # Built by hand (rather than renderTable) so that recently-subbed rows can
  # be tagged with a Bootstrap contextual class for the momentary highlight.
  render_time_table <- function(df, highlight_ids) {
    if (nrow(df) == 0) return(p(class = "text-muted", "None"))
    tags$table(
      class = "table table-sm table-hover mb-0",
      tags$thead(tags$tr(tags$th("#"), tags$th("Name"), tags$th("Half"), tags$th("Total"))),
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            class = if (df$id[i] %in% highlight_ids) "table-success" else NULL,
            tags$td(df$number[i]), tags$td(df$name[i]), tags$td(df$half[i]), tags$td(df$total[i])
          )
        })
      )
    )
  }

  output$on_field_table <- renderUI({
    tick()
    p <- game_state$players
    onf <- p[p$on_field, , drop = FALSE]
    if (nrow(onf) == 0) return(render_time_table(onf, character(0)))
    half_secs <- player_seconds(onf, game_state$clock)
    total_secs <- player_total_seconds(onf, game_state$clock)
    # Longest total time on field first, ties broken alphabetically by name.
    ord <- order(-total_secs, onf$name)
    df <- data.frame(
      id = onf$id, number = onf$number, name = onf$name,
      half = format_time(half_secs), total = format_time(total_secs),
      stringsAsFactors = FALSE
    )[ord, , drop = FALSE]
    render_time_table(df, highlight_ids())
  })

  output$bench_table <- renderUI({
    tick()
    p <- game_state$players
    b <- p[!p$on_field, , drop = FALSE]
    if (nrow(b) == 0) return(render_time_table(b, character(0)))
    half_secs <- player_seconds(b, game_state$clock)
    total_secs <- player_total_seconds(b, game_state$clock)
    ord <- order(b$name)
    df <- data.frame(
      id = b$id, number = b$number, name = b$name,
      half = format_time(half_secs), total = format_time(total_secs),
      stringsAsFactors = FALSE
    )[ord, , drop = FALSE]
    render_time_table(df, highlight_ids())
  })

  # Selection checkboxes only regenerate when on-field membership changes,
  # not every second, so a coach's in-progress selection is never wiped
  # out mid-tick.
  output$sub_out_ui <- renderUI({
    players <- game_state$players
    onf <- players[players$on_field, , drop = FALSE]
    if (nrow(onf) == 0) return(p("No one on the field yet."))
    checkboxGroupInput(
      "sub_out", "Select to sub OUT",
      choices = setNames(onf$id, paste0(onf$number, " \u2014 ", onf$name))
    )
  })

  output$sub_in_ui <- renderUI({
    players <- game_state$players
    b <- players[!players$on_field, , drop = FALSE]
    if (nrow(b) == 0) return(p("No one on the bench."))
    checkboxGroupInput(
      "sub_in", "Select to sub IN",
      choices = setNames(b$id, paste0(b$number, " \u2014 ", b$name))
    )
  })

  output$sub_validation <- renderText({
    n_out <- length(input$sub_out)
    n_in <- length(input$sub_in)
    if (n_out == 0 && n_in == 0) "Select bench players to bring on (no need to sub anyone out first)." else ""
  })

  observeEvent(input$do_sub, {
    n_out <- length(input$sub_out)
    n_in <- length(input$sub_in)
    validate(need(n_out > 0 || n_in > 0, "Select at least one player to move."))

    p <- game_state$players
    now <- current_game_seconds(game_state$clock)

    # OUT and IN are handled independently -- you don't need a matching
    # count on each side, so this also covers picking the initial lineup
    # (only IN selections, nobody to sub out yet).
    out_idx <- p$id %in% input$sub_out
    p$seconds_played[out_idx] <- p$seconds_played[out_idx] + (now - p$entered_at[out_idx])
    p$on_field[out_idx] <- FALSE
    p$entered_at[out_idx] <- NA_real_

    in_idx <- p$id %in% input$sub_in
    p$on_field[in_idx] <- TRUE
    p$entered_at[in_idx] <- now

    game_state$players <- p
    persist()

    # Confirmation: toast + momentary highlight on the affected rows.
    out_names <- p$name[out_idx]
    in_names <- p$name[in_idx]
    msg_parts <- c(
      if (length(in_names) > 0) sprintf("In: %s", paste(in_names, collapse = ", ")),
      if (length(out_names) > 0) sprintf("Out: %s", paste(out_names, collapse = ", "))
    )
    showNotification(paste(msg_parts, collapse = " \u2014 "), type = "message", duration = 3)
    highlight_expire(Sys.time() + 1.5)
    highlight_ids(union(input$sub_out, input$sub_in))

    updateCheckboxGroupInput(session, "sub_out", selected = character(0))
    updateCheckboxGroupInput(session, "sub_in", selected = character(0))
  })

  # ---------------- Export ----------------
  output$download_summary <- downloadHandler(
    filename = function() sprintf("subtimr_%s.csv", format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(file) {
      p <- game_state$players
      half_secs <- if (nrow(p) > 0) player_seconds(p, game_state$clock) else numeric(0)
      total_secs <- if (nrow(p) > 0) player_total_seconds(p, game_state$clock) else numeric(0)
      out <- data.frame(
        Number = p$number, Name = p$name,
        `Current Half Seconds` = round(half_secs), `Current Half Time` = format_time(half_secs),
        `Total Seconds` = round(total_secs), `Total Time` = format_time(total_secs),
        check.names = FALSE
      )
      utils::write.csv(out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
