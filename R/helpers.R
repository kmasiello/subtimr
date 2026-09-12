# Persistence + timing helpers for subtimr
#
# Design notes:
# - Roster (players who *could* play) is stored in data/roster.csv and
#   persists across games/app restarts.
# - The state of the *current* game (who's on field/bench, seconds played,
#   clock) is stored in data/game_state.rds and is re-saved after every
#   mutating action. On startup the app reloads this file, so a phone
#   screen lock, browser refresh, or R session restart cannot lose data.
# - The clock never depends on a running timer for its *value* -- only for
#   redrawing the UI. The true elapsed time is always derived from
#   wall-clock timestamps (clock_started_at) plus an accumulated total, so
#   there is no drift and no loss even if the app is closed while the
#   clock is running.

roster_path <- file.path("data", "roster.csv")
roster_sync_path <- file.path("data", "roster_sync.rds")
game_state_path <- file.path("data", "game_state.rds")
game_state_sync_path <- file.path("data", "game_state_sync.rds")

empty_roster <- function() {
  data.frame(
    id = character(0),
    number = character(0),
    name = character(0),
    position = character(0),
    stringsAsFactors = FALSE
  )
}

load_roster <- function() {
  # Try cloud first, fallback to local
  roster <- tryCatch(
    {
      load_roster_cloud()
    },
    error = function(e) {
      if (file.exists(roster_path)) {
        utils::read.csv(
          roster_path,
          stringsAsFactors = FALSE,
          colClasses = "character"
        )
      } else {
        empty_roster()
      }
    }
  )
  
  # Migrate missing position column for backwards compatibility
  if (!"position" %in% names(roster)) {
    roster$position <- rep("Center", nrow(roster))
  }
  
  roster
}

save_roster <- function(roster) {
  # Save to both cloud and local for redundancy
  save_roster_cloud(roster)
  utils::write.csv(roster, roster_path, row.names = FALSE)
}

new_id <- function() {
  format(as.numeric(Sys.time()) * 1000, scientific = FALSE, digits = 15)
}

empty_players <- function() {
  data.frame(
    id = character(0),
    number = character(0),
    name = character(0),
    position = character(0),
    on_field = logical(0),
    seconds_played = numeric(0),
    entered_at = numeric(0),
    seconds_prior_halves = numeric(0),
    stringsAsFactors = FALSE
  )
}

# The clock always represents the *current half only*; it resets to zero
# when a new half starts. Seconds from completed halves are rolled into
# `seconds_prior_halves` (per player) and `completed_halves_seconds`
# (game-wide, for the total-time display).
empty_clock <- function() {
  list(running = FALSE, started_at = as.POSIXct(NA), accumulated = 0)
}

empty_game_state <- function() {
  list(
    players = empty_players(),
    clock = empty_clock(),
    current_half = 1,
    completed_halves_seconds = 0,
    label = NULL
  )
}

# Fill in defaults for any fields missing from a game state saved by an
# older version of the app, so old save files don't break on load.
#
# Deliberately not modifyList(defaults, gs) here: data.frames are lists,
# so modifyList would recurse into gs$players and try to merge it
# column-by-column against the (0-row) default players table, which
# errors when row counts differ.
migrate_game_state <- function(gs) {
  defaults <- empty_game_state()
  for (field in setdiff(names(defaults), "players")) {
    if (is.null(gs[[field]])) gs[[field]] <- defaults[[field]]
  }
  if (is.null(gs$players)) {
    gs$players <- defaults$players
  }
  if (!"seconds_prior_halves" %in% names(gs$players)) {
    gs$players$seconds_prior_halves <- 0
  }
  if (!"position" %in% names(gs$players)) {
    gs$players$position <- rep("Center", nrow(gs$players))
  }
  gs
}

load_game_state <- function() {
  # Try cloud first, fallback to local
  tryCatch(
    {
      load_game_state_cloud()
    },
    error = function(e) {
      if (file.exists(game_state_path)) {
        migrate_game_state(tryCatch(
          readRDS(game_state_path),
          error = function(e) empty_game_state()
        ))
      } else {
        empty_game_state()
      }
    }
  )
}

save_game_state <- function(state) {
  # Save to both cloud and local for redundancy
  save_game_state_cloud(state)
  saveRDS(state, game_state_path)
}

# Current total elapsed game time, in seconds, given a clock list.
current_game_seconds <- function(clock) {
  extra <- if (isTRUE(clock$running)) {
    as.numeric(difftime(Sys.time(), clock$started_at, units = "secs"))
  } else {
    0
  }
  clock$accumulated + extra
}

# Seconds a given player has played *in the current half*, accounting for
# time on field right now.
player_seconds <- function(players, clock) {
  now <- current_game_seconds(clock)
  ifelse(
    players$on_field,
    players$seconds_played + (now - players$entered_at),
    players$seconds_played
  )
}

# Total seconds played across the whole game (completed halves + current half).
player_total_seconds <- function(players, clock) {
  players$seconds_prior_halves + player_seconds(players, clock)
}

format_time <- function(secs) {
  secs <- as.integer(round(pmax(secs, 0)))
  h <- secs %/% 3600
  m <- (secs %% 3600) %/% 60
  s <- secs %% 60
  ifelse(
    h > 0,
    sprintf("%d:%02d:%02d", h, m, s),
    sprintf("%02d:%02d", m, s)
  )
}

# ---- Clock mutation helpers ----
# Consolidate clock state changes to reduce repetition
start_clock <- function(clock) {
  if (!isTRUE(clock$running)) {
    clock$running <- TRUE
    clock$started_at <- Sys.time()
  }
  clock
}

pause_clock <- function(clock) {
  if (isTRUE(clock$running)) {
    clock$accumulated <- current_game_seconds(clock)
    clock$running <- FALSE
    clock$started_at <- as.POSIXct(NA)
  }
  clock
}

# ---- Note lookup helper ----
get_player_note <- function(player_id, notes_df) {
  note <- notes_df[notes_df$player_id == player_id, "note"]
  if (length(note) == 0) "" else note
}

# ---- UI building helpers ----
# Render a player table for on-field or bench display
# on_field: boolean, if TRUE show on-field players, if FALSE show bench
render_player_table <- function(players, clock, on_field, highlight_ids) {
  p <- players[players$on_field == on_field, , drop = FALSE]
  
  if (nrow(p) == 0) {
    return(tags$p(class = "text-muted", "None"))
  }
  
  half_secs <- player_seconds(p, clock)
  total_secs <- player_total_seconds(p, clock)
  
  # On field: sort by total time (descending), then name
  # Bench: sort by name
  if (on_field) {
    ord <- order(-total_secs, p$name)
  } else {
    ord <- order(p$name)
  }
  
  df <- data.frame(
    id = p$id,
    number = p$number,
    name = p$name,
    half = format_time(half_secs),
    total = format_time(total_secs),
    stringsAsFactors = FALSE
  )[ord, , drop = FALSE]
  
  tags$table(
    class = "table table-sm table-hover mb-0",
    tags$thead(tags$tr(
      tags$th("#"),
      tags$th("Name"),
      tags$th("Half"),
      tags$th("Total"),
      tags$th("")
    )),
    tags$tbody(
      lapply(seq_len(nrow(df)), function(i) {
        tags$tr(
          class = if (df$id[i] %in% highlight_ids) "table-success" else NULL,
          tags$td(df$number[i]),
          tags$td(df$name[i]),
          tags$td(df$half[i]),
          tags$td(df$total[i]),
          tags$td(
            actionButton(
              paste0("note_", df$id[i]),
              icon("clipboard"),
              class = "btn btn-sm btn-outline-secondary",
              onclick = sprintf(
                "Shiny.setInputValue('show_note', '%s', {priority: 'event'});",
                df$id[i]
              )
            )
          )
        )
      })
    )
  )
}

# Build substitution UI (checkboxes by position) for on-field or bench
build_sub_ui <- function(players, on_field) {
  p <- players[players$on_field == on_field, , drop = FALSE]
  
  if (nrow(p) == 0) {
    return(tags$p(class = "text-muted small", 
                  if (on_field) "No one on the field yet." else "No one on the bench."))
  }
  
  # Add position column for backwards compatibility
  if (!"position" %in% names(p)) {
    p$position <- "Center"
  }
  
  positions <- c("Striker", "Center", "Defender")
  pos_type <- if (on_field) "out" else "in"
  
  ui_elements <- lapply(positions, function(pos) {
    pos_players <- p[p$position == pos, , drop = FALSE]
    if (nrow(pos_players) == 0) {
      return(NULL)
    }
    
    tagList(
      tags$div(
        class = sprintf("fw-bold small text-muted mt-2 mb-1"),
        pos
      ),
      checkboxGroupInput(
        paste0("sub_", pos_type, "_", tolower(pos)),
        NULL,
        choices = setNames(
          pos_players$id,
          paste0("#", pos_players$number, " — ", pos_players$name)
        )
      )
    )
  })
  
  if (all(sapply(ui_elements, is.null))) {
    return(tags$p(class = "text-muted small",
                  if (on_field) "No one on the field yet." else "No one on the bench."))
  }
  
  tagList(ui_elements)
}
