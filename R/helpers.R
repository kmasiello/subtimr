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
game_state_path <- file.path("data", "game_state.rds")

empty_roster <- function() {
  data.frame(
    id = character(0), number = character(0), name = character(0),
    stringsAsFactors = FALSE
  )
}

load_roster <- function() {
  if (file.exists(roster_path)) {
    utils::read.csv(roster_path, stringsAsFactors = FALSE, colClasses = "character")
  } else {
    empty_roster()
  }
}

save_roster <- function(roster) {
  utils::write.csv(roster, roster_path, row.names = FALSE)
}

new_id <- function() {
  format(as.numeric(Sys.time()) * 1000, scientific = FALSE, digits = 15)
}

empty_players <- function() {
  data.frame(
    id = character(0), number = character(0), name = character(0),
    on_field = logical(0), seconds_played = numeric(0),
    entered_at = numeric(0), seconds_prior_halves = numeric(0),
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
    players = empty_players(), clock = empty_clock(),
    current_half = 1, completed_halves_seconds = 0, label = NULL
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
  if (is.null(gs$players)) gs$players <- defaults$players
  if (!"seconds_prior_halves" %in% names(gs$players)) {
    gs$players$seconds_prior_halves <- 0
  }
  gs
}

load_game_state <- function() {
  if (file.exists(game_state_path)) {
    migrate_game_state(tryCatch(readRDS(game_state_path), error = function(e) empty_game_state()))
  } else {
    empty_game_state()
  }
}

save_game_state <- function(state) {
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
