library(shiny)
library(bslib)
library(jsonlite)
library(DBI)

source("R/helpers.R")
source("R/duckdb_helpers.R")

ui <- page_fillable(
  title = "Subtimr",
  theme = bs_theme(
    version = 5,
    preset = "bootstrap",
    primary = "#2e7d32",
    secondary = "#558b2f",
    success = "#66bb6a",
    bg = "#fafafa",
    fg = "#212121",
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins", wght = c(500, 600, 700))
  ),
  navset_card_underline(
    title = tags$span(
      icon("stopwatch", class = "me-2"),
      "Subtimr",
      style = "font-weight: 600;"
    ),

    nav_panel(
      "Game",
      layout_columns(
        col_widths = c(4, 8),

        # Left sidebar - Clock and controls
        card(
          class = "shadow-sm",
          card_header(
            class = "bg-primary text-white fw-semibold",
            icon("clock", class = "me-2"),
            "Game Clock"
          ),
          card_body(
            div(
              class = "text-center py-3",
              uiOutput("clock_status_badge"),
              tags$div(
                class = "badge bg-secondary fs-6 mb-2",
                textOutput("half_label", inline = TRUE)
              ),
              div(
                uiOutput("clock_display_styled")
              ),
              div(
                class = "text-muted mt-2 fs-6",
                icon("hourglass-half", class = "me-1"),
                textOutput("total_time_display", inline = TRUE)
              )
            ),
            div(
              class = "d-grid gap-2 mt-4",
              div(
                class = "btn-group",
                actionButton(
                  "start_clock",
                  tags$span(icon("play"), " Start"),
                  class = "btn-success"
                ),
                actionButton(
                  "pause_clock",
                  tags$span(icon("pause"), " Pause"),
                  class = "btn-warning"
                ),
                actionButton(
                  "reset_clock",
                  tags$span(icon("rotate-left"), " Reset"),
                  class = "btn-outline-danger"
                )
              )
            ),
            uiOutput("end_half_ui"),

            tags$hr(class = "my-3"),
            # Score tracker
            div(
              class = "card bg-light",
              div(
                class = "card-body py-3",
                tags$h6("Score", class = "text-center mb-3 fw-semibold"),
                div(
                  class = "d-flex justify-content-center align-items-center gap-3",
                  div(
                    class = "text-center",
                    tags$label("Us", class = "small text-muted d-block mb-1"),
                    div(
                      style = "font-size: 2.5rem; font-weight: 700; color: #2e7d32;",
                      textOutput("score_us", inline = TRUE)
                    ),
                    div(
                      class = "btn-group btn-group-sm mt-2",
                      actionButton(
                        "score_us_minus",
                        icon("minus"),
                        class = "btn-outline-secondary"
                      ),
                      actionButton(
                        "score_us_plus",
                        icon("plus"),
                        class = "btn-outline-success"
                      )
                    )
                  ),
                  tags$span("-", style = "font-size: 2rem; color: #999;"),
                  div(
                    class = "text-center",
                    tags$label("Them", class = "small text-muted d-block mb-1"),
                    div(
                      style = "font-size: 2.5rem; font-weight: 700; color: #666;",
                      textOutput("score_them", inline = TRUE)
                    ),
                    div(
                      class = "btn-group btn-group-sm mt-2",
                      actionButton(
                        "score_them_minus",
                        icon("minus"),
                        class = "btn-outline-secondary"
                      ),
                      actionButton(
                        "score_them_plus",
                        icon("plus"),
                        class = "btn-outline-danger"
                      )
                    )
                  )
                )
              )
            )
          ),
          card_footer(
            class = "bg-light",
            div(
              class = "d-grid gap-2",
              actionButton(
                "new_game",
                tags$span(icon("circle-plus"), " New Game"),
                class = "btn-primary"
              ),
              downloadButton(
                "download_summary",
                "Download Summary (CSV)",
                class = "btn-outline-secondary"
              ),
              tags$hr(class = "my-2"),
              uiOutput("add_to_game_ui")
            )
          )
        ),

        # Right panel - Substitution interface
        card(
          class = "shadow-sm",
          card_header(
            class = "bg-success text-white fw-semibold",
            icon("people-arrows", class = "me-2"),
            "Substitutions"
          ),
          card_body(
            layout_columns(
              col_widths = c(6, 6),

              # On field column
              div(
                class = "border-end pe-3",
                div(
                  class = "d-flex align-items-center mb-3",
                  icon(
                    "person-running",
                    class = "text-success me-2",
                    style = "font-size: 1.2rem;"
                  ),
                  tags$h5(
                    textOutput("on_field_header", inline = TRUE),
                    class = "mb-0 fw-semibold"
                  )
                ),
                div(
                  class = "mb-3",
                  style = "max-height: 400px; overflow-y: auto;",
                  uiOutput("on_field_table")
                ),
                div(
                  class = "card bg-light border-0",
                  div(
                    class = "card-body py-2",
                    tags$small(class = "text-muted fw-semibold", "SUB OUT:"),
                    uiOutput("sub_out_ui")
                  )
                )
              ),

              # Bench column
              div(
                class = "ps-3",
                div(
                  class = "d-flex align-items-center mb-3",
                  icon(
                    "chair",
                    class = "text-secondary me-2",
                    style = "font-size: 1.2rem;"
                  ),
                  tags$h5(
                    textOutput("bench_header", inline = TRUE),
                    class = "mb-0 fw-semibold"
                  )
                ),
                div(
                  class = "mb-3",
                  style = "max-height: 400px; overflow-y: auto;",
                  uiOutput("bench_table")
                ),
                div(
                  class = "card bg-light border-0",
                  div(
                    class = "card-body py-2",
                    tags$small(class = "text-success fw-semibold", "SUB IN:"),
                    uiOutput("sub_in_ui")
                  )
                )
              )
            )
          ),
          card_footer(
            class = "bg-light",
            div(
              class = "d-flex align-items-center gap-2",
              actionButton(
                "do_sub",
                tags$span(icon("arrow-right-arrow-left"), " Make Substitution"),
                class = "btn-success btn-lg flex-grow-1"
              )
            ),
            div(
              class = "text-muted small mt-2",
              textOutput("sub_validation", inline = TRUE)
            )
          )
        )
      )
    ),

    nav_panel(
      "Notes",
      card(
        class = "shadow-sm",
        card_header(
          class = "bg-warning text-dark fw-semibold",
          icon("clipboard", class = "me-2"),
          "Game Notes"
        ),
        card_body(
          div(
            class = "alert alert-info mb-4",
            icon("circle-info", class = "me-2"),
            "Notes are saved per game and included in the CSV export."
          ),
          uiOutput("notes_list")
        )
      )
    ),

    nav_panel(
      "Roster",
      card(
        class = "shadow-sm",
        card_header(
          class = "bg-primary text-white fw-semibold",
          icon("users", class = "me-2"),
          "Manage Roster"
        ),
        card_body(
          div(
            class = "alert alert-info mb-4",
            icon("circle-info", class = "me-2"),
            "This roster is saved to disk and persists across games."
          ),
          div(
            class = "card bg-light mb-4",
            div(
              class = "card-body",
              tags$h6("Add New Player", class = "mb-3 fw-semibold"),
              layout_columns(
                col_widths = c(3, 6, 3),
                textInput("new_number", "Number", placeholder = "#"),
                textInput("new_name", "Name", placeholder = "Name"),
                selectInput(
                  "new_position",
                  "Position",
                  choices = c("Striker", "Center", "Defender"),
                  selected = "Center"
                )
              ),
              layout_columns(
                col_widths = c(12),
                div(
                  actionButton(
                    "add_player",
                    tags$span(icon("user-plus"), " Add Player"),
                    class = "btn-primary w-100"
                  )
                )
              )
            )
          ),
          tags$h6("Current Roster", class = "mb-3 fw-semibold"),
          div(
            style = "max-height: 500px; overflow-y: auto;",
            tableOutput("roster_table")
          ),
          tags$hr(),
          div(
            class = "card bg-light border-info",
            div(
              class = "card-body",
              tags$h6("Edit Player", class = "mb-3 fw-semibold text-info"),
              layout_columns(
                col_widths = c(9, 3),
                uiOutput("edit_player_ui"),
                div(
                  style = "padding-top: 1.8rem;",
                  actionButton(
                    "edit_player_btn",
                    tags$span(icon("pen-to-square"), " Edit"),
                    class = "btn-outline-info w-100"
                  )
                )
              )
            )
          ),
          tags$hr(),
          div(
            class = "card bg-light border-danger",
            div(
              class = "card-body",
              tags$h6("Remove Player", class = "mb-3 fw-semibold text-danger"),
              layout_columns(
                col_widths = c(9, 3),
                uiOutput("remove_player_ui"),
                div(
                  style = "padding-top: 1.8rem;",
                  actionButton(
                    "remove_player",
                    tags$span(icon("user-minus"), " Remove"),
                    class = "btn-outline-danger w-100"
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Load initial state once
  initial_state <- load_game_state()

  # ---- Persisted state ----
  game_state <- reactiveValues(
    players = initial_state$players,
    clock = initial_state$clock,
    score_us = initial_state$score_us %||% 0,
    score_them = initial_state$score_them %||% 0,
    notes = initial_state$notes %||% data.frame(
      player_id = character(0),
      note = character(0),
      stringsAsFactors = FALSE
    )
  )

  roster <- reactiveVal(load_roster())
  current_half <- reactiveVal(initial_state$current_half)
  completed_halves_seconds <- reactiveVal(initial_state$completed_halves_seconds)
  highlight_ids <- reactiveVal(character(0))
  highlight_expire <- reactiveVal(NULL)

  # Highlight animation: fade after 1.5s
  observe({
    ids <- highlight_ids()
    if (length(ids) == 0) return(invisible())
    invalidateLater(1500)
    if (
      !is.null(isolate(highlight_expire())) &&
        Sys.time() >= isolate(highlight_expire())
    ) {
      highlight_ids(character(0))
    }
  })

  # Persist all game state to disk/cloud
  persist <- function() {
    save_game_state(list(
      players = game_state$players,
      clock = game_state$clock,
      current_half = current_half(),
      completed_halves_seconds = completed_halves_seconds(),
      score_us = game_state$score_us,
      score_them = game_state$score_them,
      notes = game_state$notes
    ))
  }

  tick <- reactiveTimer(1000)

  # ---- Roster management ----
  observeEvent(input$add_player, {
    if (!nzchar(trimws(input$new_name))) {
      showNotification("Please enter a player name", type = "warning", duration = 2)
      return()
    }

    r <- roster()
    r <- rbind(
      r,
      data.frame(
        id = new_id(),
        number = trimws(input$new_number),
        name = trimws(input$new_name),
        position = input$new_position,
        stringsAsFactors = FALSE
      )
    )
    roster(r)
    save_roster(r)
    updateTextInput(session, "new_number", value = "")
    updateTextInput(session, "new_name", value = "")
    showNotification("Player added successfully!", type = "message", duration = 2)
  })

  output$roster_table <- renderTable(
    {
      r <- roster()
      if (nrow(r) == 0) {
        return(data.frame(Number = character(0), Name = character(0), Position = character(0)))
      }
      r <- r[order(r$name), , drop = FALSE]
      data.frame(Number = r$number, Name = r$name, Position = r$position)
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
  )

  output$edit_player_ui <- renderUI({
    r <- roster()
    if (nrow(r) == 0) {
      return(p(class = "text-muted", "No players in roster"))
    }
    choices <- setNames(r$id, paste0("#", r$number, " — ", r$name))
    selectInput("edit_player_id", NULL, choices = choices)
  })

  observeEvent(input$edit_player_btn, {
    req(input$edit_player_id)
    r <- roster()
    player <- r[r$id == input$edit_player_id, , drop = FALSE]
    req(nrow(player) == 1)

    showModal(modalDialog(
      title = sprintf("Edit Player #%s", player$number[1]),
      layout_columns(
        col_widths = c(3, 6, 3),
        textInput(
          "edit_number",
          "Number",
          value = player$number[1],
          placeholder = "#"
        ),
        textInput(
          "edit_name",
          "Name",
          value = player$name[1],
          placeholder = "Name"
        ),
        selectInput(
          "edit_position",
          "Position",
          choices = c("Striker", "Center", "Defender"),
          selected = player$position[1]
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_edit_player", "Save Changes", class = "btn-primary")
      )
    ))
    session$userData$editing_player_id <- input$edit_player_id
  })

  observeEvent(input$confirm_edit_player, {
    player_id <- session$userData$editing_player_id
    req(player_id)

    if (!nzchar(trimws(input$edit_name))) {
      showNotification("Please enter a player name", type = "warning", duration = 2)
      return()
    }

    r <- roster()
    player_idx <- which(r$id == player_id)
    req(length(player_idx) == 1)

    r$number[player_idx] <- trimws(input$edit_number)
    r$name[player_idx] <- trimws(input$edit_name)
    r$position[player_idx] <- input$edit_position

    roster(r)
    save_roster(r)
    removeModal()
    showNotification("Player updated successfully!", type = "message", duration = 2)
  })

  output$remove_player_ui <- renderUI({
    r <- roster()
    if (nrow(r) == 0) {
      return(p(class = "text-muted", "No players in roster"))
    }
    choices <- setNames(r$id, paste0("#", r$number, " — ", r$name))
    selectInput("remove_player_id", NULL, choices = choices)
  })

  observeEvent(input$remove_player, {
    req(input$remove_player_id)
    r <- roster()
    r <- r[r$id != input$remove_player_id, , drop = FALSE]
    roster(r)
    save_roster(r)
    showNotification("Player removed", type = "warning", duration = 2)
  })

  # ---- New game setup ----
  observeEvent(input$new_game, {
    r <- roster()
    if (nrow(r) == 0) {
      showModal(modalDialog("Add players to the roster first.", easyClose = TRUE))
      return()
    }

    positions <- c("Striker", "Center", "Defender")
    checkbox_ui <- lapply(positions, function(pos) {
      pos_players <- r[r$position == pos, , drop = FALSE]
      if (nrow(pos_players) == 0) return(NULL)
      pos_players <- pos_players[order(pos_players$name), , drop = FALSE]
      tagList(
        tags$div(
          class = "fw-bold text-primary mt-3 mb-2",
          style = "font-size: 1.1rem;",
          pos
        ),
        checkboxGroupInput(
          paste0("today_players_", tolower(pos)),
          NULL,
          choices = setNames(
            pos_players$id,
            paste0("#", pos_players$number, " — ", pos_players$name)
          ),
          selected = character(0)
        )
      )
    })
    showModal(modalDialog(
      title = "Start a new game",
      tags$p("Who's starting on the field? Everyone else on the roster starts on the bench."),
      tagList(checkbox_ui),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_new_game", "Start game", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$confirm_new_game, {
    r <- roster()
    starters <- c(
      input$today_players_striker,
      input$today_players_center,
      input$today_players_defender
    )

    game_state$players <- data.frame(
      id = r$id,
      number = r$number,
      name = r$name,
      position = r$position,
      on_field = r$id %in% starters,
      seconds_played = 0,
      entered_at = ifelse(r$id %in% starters, 0, NA_real_),
      seconds_prior_halves = 0,
      stringsAsFactors = FALSE
    )
    game_state$clock <- empty_clock()
    game_state$score_us <- 0
    game_state$score_them <- 0
    game_state$notes <- data.frame(
      player_id = character(0),
      note = character(0),
      stringsAsFactors = FALSE
    )
    current_half(1)
    completed_halves_seconds(0)
    persist()
    removeModal()
    showNotification("New game started!", type = "message", duration = 2)
  })

  output$add_to_game_ui <- renderUI({
    r <- roster()
    players <- game_state$players
    eligible <- r[!(r$id %in% players$id), , drop = FALSE]
    if (nrow(r) == 0 || nrow(eligible) == 0) {
      return(p(
        class = "text-muted small",
        if (nrow(r) == 0) "No roster players." else "All roster players are in the game."
      ))
    }
    tagList(
      selectInput(
        "add_to_game_id",
        "Add a player to the game",
        choices = setNames(
          eligible$id,
          paste0("#", eligible$number, " — ", eligible$name, " (", eligible$position, ")")
        )
      ),
      actionButton("add_to_game", "Add to bench", class = "btn-outline-primary w-100")
    )
  })

  observeEvent(input$add_to_game, {
    req(input$add_to_game_id)
    r <- roster()
    sel <- r[r$id == input$add_to_game_id, , drop = FALSE]
    req(nrow(sel) == 1)

    # Backwards compatibility
    p <- game_state$players
    if (nrow(p) > 0 && !"position" %in% names(p)) {
      p$position <- "Center"
    }

    p <- rbind(
      p,
      data.frame(
        id = sel$id,
        number = sel$number,
        name = sel$name,
        position = sel$position,
        on_field = FALSE,
        seconds_played = 0,
        entered_at = NA_real_,
        seconds_prior_halves = 0,
        stringsAsFactors = FALSE
      )
    )
    game_state$players <- p
    persist()
    showNotification("Player added to bench", type = "message", duration = 2)
  })

  # ---- Clock controls ----
  observeEvent(input$start_clock, {
    game_state$clock <- start_clock(game_state$clock)
    persist()
  })

  observeEvent(input$pause_clock, {
    game_state$clock <- pause_clock(game_state$clock)
    persist()
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
    game_state$score_us <- 0
    game_state$score_them <- 0
    current_half(1)
    completed_halves_seconds(0)
    persist()
    removeModal()
    showNotification("Game reset", type = "warning", duration = 2)
  })

  # ---- Half transitions ----
  output$end_half_ui <- renderUI({
    if (current_half() >= 2) return(NULL)
    div(
      class = "mt-3",
      actionButton(
        "end_half",
        tags$span(icon("forward"), " End Half 1, Start Half 2"),
        class = "btn-outline-primary w-100"
      )
    )
  })

  observeEvent(input$end_half, {
    cl <- game_state$clock
    if (isTRUE(cl$running)) {
      cl <- pause_clock(cl)
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
    showNotification("Half 2 started!", type = "message", duration = 2)
  })

  output$half_label <- renderText(sprintf("Half %d", current_half()))

  output$clock_status_badge <- renderUI({
    tick()
    is_running <- isTRUE(game_state$clock$running)
    div(
      class = sprintf("badge fs-6 mb-2 %s", if (is_running) "bg-success" else "bg-danger"),
      if (is_running) {
        tags$span(icon("circle"), " RUNNING")
      } else {
        tags$span(icon("circle-pause"), " PAUSED")
      }
    )
  })

  output$clock_display_styled <- renderUI({
    tick()
    is_running <- isTRUE(game_state$clock$running)
    color <- if (is_running) "#2e7d32" else "#999999"
    div(
      format_time(current_game_seconds(game_state$clock)),
      style = sprintf(
        "font-size: 4rem; font-weight: 700; color: %s; line-height: 1; font-family: 'Courier New', monospace;",
        color
      )
    )
  })

  output$total_time_display <- renderText({
    tick()
    sprintf(
      "Total: %s",
      format_time(completed_halves_seconds() + current_game_seconds(game_state$clock))
    )
  })

  # ---- Player displays (consolidated) ----
  output$on_field_header <- renderText({
    sprintf("On field (%d)", sum(game_state$players$on_field))
  })

  output$bench_header <- renderText({
    sprintf("Bench (%d)", sum(!game_state$players$on_field))
  })

  output$on_field_table <- renderUI({
    tick()
    render_player_table(
      game_state$players,
      game_state$clock,
      on_field = TRUE,
      highlight_ids()
    )
  })

  output$bench_table <- renderUI({
    tick()
    render_player_table(
      game_state$players,
      game_state$clock,
      on_field = FALSE,
      highlight_ids()
    )
  })

  # ---- Substitution UI (consolidated) ----
  output$sub_out_ui <- renderUI({
    build_sub_ui(game_state$players, on_field = TRUE)
  })

  output$sub_in_ui <- renderUI({
    build_sub_ui(game_state$players, on_field = FALSE)
  })

  output$sub_validation <- renderText({
    n_out <- length(input$sub_out)
    n_in <- length(input$sub_in)
    if (n_out == 0 && n_in == 0) "Select players to substitute" else ""
  })

  observeEvent(input$do_sub, {
    # Collect substitutions from all positions
    sub_out <- c(
      input$sub_out_striker,
      input$sub_out_center,
      input$sub_out_defender
    )
    sub_out <- sub_out[!is.null(sub_out)]

    sub_in <- c(
      input$sub_in_striker,
      input$sub_in_center,
      input$sub_in_defender
    )
    sub_in <- sub_in[!is.null(sub_in)]

    if (length(sub_out) == 0 && length(sub_in) == 0) {
      showNotification(
        "Select at least one player to substitute",
        type = "warning",
        duration = 2
      )
      return()
    }

    p <- game_state$players
    now <- current_game_seconds(game_state$clock)
    out_idx <- p$id %in% sub_out

    # Update seconds played for players coming out
    if (any(out_idx)) {
      valid_entered <- out_idx & !is.na(p$entered_at)
      if (any(valid_entered)) {
        p$seconds_played[valid_entered] <- p$seconds_played[valid_entered] +
          (now - p$entered_at[valid_entered])
      }
    }

    p$on_field[out_idx] <- FALSE
    p$entered_at[out_idx] <- NA_real_

    in_idx <- p$id %in% sub_in
    p$on_field[in_idx] <- TRUE
    p$entered_at[in_idx] <- now

    game_state$players <- p
    persist()

    out_names <- p$name[out_idx]
    in_names <- p$name[in_idx]
    msg_parts <- c(
      if (length(in_names) > 0) sprintf("In: %s", paste(in_names, collapse = ", ")),
      if (length(out_names) > 0) sprintf("Out: %s", paste(out_names, collapse = ", "))
    )
    showNotification(
      HTML(paste(msg_parts, collapse = "<br>")),
      type = "message",
      duration = 3
    )
    highlight_expire(Sys.time() + 1.5)
    highlight_ids(union(sub_out, sub_in))

    # Clear all position-based checkboxes
    for (pos in c("striker", "center", "defender")) {
      updateCheckboxGroupInput(session, paste0("sub_out_", pos), selected = character(0))
      updateCheckboxGroupInput(session, paste0("sub_in_", pos), selected = character(0))
    }
  })

  # ---- Notes functionality ----
  observeEvent(input$show_note, {
    player_id <- input$show_note
    p <- game_state$players
    player_info <- p[p$id == player_id, , drop = FALSE]
    req(nrow(player_info) == 1)

    current_note <- get_player_note(player_id, game_state$notes)

    showModal(modalDialog(
      title = sprintf(
        "Notes for #%s — %s",
        player_info$number,
        player_info$name
      ),
      textAreaInput(
        "note_text",
        NULL,
        value = current_note,
        rows = 5,
        placeholder = "Enter notes about this player's performance..."
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_note", "Save Note", class = "btn-primary")
      )
    ))
    session$userData$current_note_player_id <- player_id
  })

  observeEvent(input$save_note, {
    player_id <- session$userData$current_note_player_id
    req(player_id)

    notes <- game_state$notes
    notes <- notes[notes$player_id != player_id, , drop = FALSE]

    if (nzchar(trimws(input$note_text))) {
      notes <- rbind(
        notes,
        data.frame(
          player_id = player_id,
          note = trimws(input$note_text),
          stringsAsFactors = FALSE
        )
      )
    }

    game_state$notes <- notes
    persist()
    removeModal()
    showNotification("Note saved", type = "message", duration = 2)
  })

  output$notes_list <- renderUI({
    p <- game_state$players
    notes <- game_state$notes

    if (nrow(p) == 0) {
      return(p(class = "text-muted", "No players in the game yet."))
    }

    player_notes <- merge(
      p,
      notes,
      by.x = "id",
      by.y = "player_id",
      all.x = TRUE
    )
    player_notes <- player_notes[order(player_notes$name), , drop = FALSE]

    tagList(
      lapply(seq_len(nrow(player_notes)), function(i) {
        has_note <- !is.na(player_notes$note[i]) && nzchar(player_notes$note[i])
        div(
          class = "card mb-3",
          div(
            class = "card-body",
            div(
              class = "d-flex justify-content-between align-items-start",
              tags$h6(
                class = "mb-2",
                sprintf("#%s — %s", player_notes$number[i], player_notes$name[i])
              ),
              actionButton(
                paste0("edit_note_", player_notes$id[i]),
                icon("pen-to-square"),
                class = "btn btn-sm btn-outline-secondary",
                onclick = sprintf(
                  "Shiny.setInputValue('show_note', '%s', {priority: 'event'});",
                  player_notes$id[i]
                )
              )
            ),
            if (has_note) {
              div(
                class = "text-muted small mt-2",
                style = "white-space: pre-wrap;",
                player_notes$note[i]
              )
            } else {
              p(class = "text-muted small fst-italic mb-0", "No notes yet")
            }
          )
        )
      })
    )
  })

  # ---- Score tracking ----
  output$score_us <- renderText(as.character(game_state$score_us))
  output$score_them <- renderText(as.character(game_state$score_them))

  observeEvent(input$score_us_plus, {
    game_state$score_us <- game_state$score_us + 1
    persist()
  })

  observeEvent(input$score_us_minus, {
    if (game_state$score_us > 0) {
      game_state$score_us <- game_state$score_us - 1
      persist()
    }
  })

  observeEvent(input$score_them_plus, {
    game_state$score_them <- game_state$score_them + 1
    persist()
  })

  observeEvent(input$score_them_minus, {
    if (game_state$score_them > 0) {
      game_state$score_them <- game_state$score_them - 1
      persist()
    }
  })

  # ---- Export ----
  output$download_summary <- downloadHandler(
    filename = function() {
      sprintf("subtimr_%s_%d-%d.csv",
              format(Sys.time(), "%Y%m%d_%H%M"),
              game_state$score_us,
              game_state$score_them)
    },
    content = function(file) {
      p <- game_state$players
      notes <- game_state$notes

      # Calculate H1 and H2 times
      h1_secs <- if (nrow(p) > 0) {
        if (current_half() == 1) {
          player_seconds(p, game_state$clock)
        } else {
          p$seconds_prior_halves
        }
      } else {
        numeric(0)
      }

      h2_secs <- if (nrow(p) > 0) {
        if (current_half() == 2) {
          player_seconds(p, game_state$clock)
        } else {
          rep(0, nrow(p))
        }
      } else {
        numeric(0)
      }

      total_secs <- if (nrow(p) > 0) {
        player_total_seconds(p, game_state$clock)
      } else {
        numeric(0)
      }

      out <- data.frame(
        Number = p$number,
        Name = p$name,
        Position = p$position,
        `H1 Seconds` = round(h1_secs),
        `H1 Time` = format_time(h1_secs),
        `H2 Seconds` = round(h2_secs),
        `H2 Time` = format_time(h2_secs),
        `Total Seconds` = round(total_secs),
        `Total Time` = format_time(total_secs),
        check.names = FALSE
      )

      # Merge with notes
      if (nrow(notes) > 0) {
        out$id <- p$id
        out <- merge(
          out,
          notes,
          by.x = "id",
          by.y = "player_id",
          all.x = TRUE,
          sort = FALSE
        )
        out$id <- NULL
        if ("note" %in% names(out)) {
          names(out)[names(out) == "note"] <- "Notes"
          out$Notes[is.na(out$Notes)] <- ""
        } else {
          out$Notes <- ""
        }
      } else {
        out$Notes <- ""
      }

      utils::write.csv(out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
