# DuckDB Cloud persistence for subtimr
#
# Uses MotherDuck (https://motherduck.com/) for cloud storage.
# Game state persists across app restarts, timeouts, and device changes.
#
# Setup:
#   1. Create a MotherDuck account and get your token
#   2. Set environment variable: Sys.setenv(MOTHERDUCK_TOKEN = "your_token")
#   3. Or set in Connect Cloud: add to app environment
#
# The schema stores one row per game session, updated on each mutation.

library(duckdb)
library(base64enc)

# Initialize connection to DuckDB Cloud (MotherDuck)
init_duckdb <- function() {
  token <- Sys.getenv("MOTHERDUCK_TOKEN", "")

  # For local dev: use local DuckDB if no token
  if (token == "") {
    conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  } else {
    # Connect to MotherDuck with error handling
    conn <- tryCatch(
      {
        conn <- DBI::dbConnect(
          duckdb::duckdb(),
          sprintf("md:?motherduck_token=%s", token)
        )
        # Test the connection immediately
        DBI::dbGetQuery(conn, "SELECT 1")
        conn
      },
      error = function(e) {
        warning(
          "MotherDuck connection failed, falling back to local: ",
          e$message
        )
        DBI::dbConnect(duckdb::duckdb(), ":memory:")
      }
    )
  }

  # Create tables if they don't exist
  tryCatch(
    {
      DBI::dbExecute(
        conn,
        "
      CREATE TABLE IF NOT EXISTS game_state (
        session_id VARCHAR PRIMARY KEY,
        game_state_json VARCHAR,
        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      );
    "
      )

      DBI::dbExecute(
        conn,
        "
      CREATE TABLE IF NOT EXISTS roster (
        session_id VARCHAR PRIMARY KEY,
        roster_json VARCHAR,
        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      );
    "
      )
    },
    error = function(e) {
      warning("Failed to create tables: ", e$message)
    }
  )

  conn
}

# Get or create a stable session ID for this app instance
get_session_id <- function() {
  # In a real deployment, this would be per-user or per-app-instance
  # For now, use a fixed ID (all users share state within a deployed app)
  "subtimr_session"
}

# Save game state to DuckDB Cloud
save_game_state_cloud <- function(state) {
  tryCatch(
    {
      conn <- init_duckdb()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)

      session_id <- get_session_id()

      # Use base64 encoding of RDS for perfect fidelity
      state_rds <- serialize(state, NULL)
      state_b64 <- base64enc::base64encode(state_rds)

      # Get current timestamp as a value
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

      # Upsert (replace if exists)
      DBI::dbExecute(
        conn,
        "INSERT INTO game_state (session_id, game_state_json, updated_at) 
      VALUES (?, ?, ?)
      ON CONFLICT(session_id) DO UPDATE SET 
         game_state_json = EXCLUDED.game_state_json,
        updated_at = EXCLUDED.updated_at",
        params = list(session_id, state_b64, now)
      )

      invisible(TRUE)
    },
    error = function(e) {
      warning("Failed to save game state to cloud: ", e$message)
      # Fallback: save locally so data isn't lost
      saveRDS(state, game_state_path)
      invisible(FALSE)
    }
  )
}

# Load game state from DuckDB Cloud
load_game_state_cloud <- function() {
  tryCatch(
    {
      conn <- init_duckdb()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)

      session_id <- get_session_id()

      result <- DBI::dbGetQuery(
        conn,
        "SELECT game_state_json FROM game_state WHERE session_id = ?",
        params = list(session_id)
      )

      if (nrow(result) == 0) {
        # No state in cloud; try local fallback (DIRECT, not through load_game_state())
        if (file.exists(game_state_path)) {
          return(migrate_game_state(tryCatch(
            readRDS(game_state_path),
            error = function(e) empty_game_state()
          )))
        } else {
          return(empty_game_state())
        }
      }

      state_b64 <- result$game_state_json[1]
      state_rds <- base64enc::base64decode(state_b64)
      state <- unserialize(state_rds)
      migrate_game_state(state)
    },
    error = function(e) {
      warning("Failed to load game state from cloud: ", e$message)
      # Fallback to local (DIRECT, not through load_game_state())
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

# Save roster to DuckDB Cloud
save_roster_cloud <- function(roster) {
  tryCatch(
    {
      conn <- init_duckdb()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)

      session_id <- get_session_id()

      # Use base64 encoding of RDS for perfect fidelity
      roster_rds <- serialize(roster, NULL)
      roster_b64 <- base64enc::base64encode(roster_rds)

      # Get current timestamp as a value
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

      DBI::dbExecute(
        conn,
        "INSERT INTO roster (session_id, roster_json, updated_at) 
       VALUES (?, ?, ?)
       ON CONFLICT(session_id) DO UPDATE SET 
         roster_json = EXCLUDED.roster_json,
         updated_at = EXCLUDED.updated_at",
        params = list(session_id, roster_b64, now)
      )

      invisible(TRUE)
    },
    error = function(e) {
      warning("Failed to save roster to cloud: ", e$message)
      # Fallback: save locally (DIRECT, not through save_roster())
      utils::write.csv(roster, roster_path, row.names = FALSE)
      invisible(FALSE)
    }
  )
}

# Load roster from DuckDB Cloud
load_roster_cloud <- function() {
  tryCatch(
    {
      conn <- init_duckdb()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)

      session_id <- get_session_id()

      result <- DBI::dbGetQuery(
        conn,
        "SELECT roster_json FROM roster WHERE session_id = ?",
        params = list(session_id)
      )

      if (nrow(result) == 0) {
        # No roster in cloud; try local fallback (DIRECT, not through load_roster())
        if (file.exists(roster_path)) {
          return(utils::read.csv(
            roster_path,
            stringsAsFactors = FALSE,
            colClasses = "character"
          ))
        } else {
          return(empty_roster())
        }
      }

      roster_b64 <- result$roster_json[1]
      roster_rds <- base64enc::base64decode(roster_b64)
      roster <- unserialize(roster_rds)
      roster
    },
    error = function(e) {
      warning("Failed to load roster from cloud: ", e$message)
      # Fallback to local (DIRECT, not through load_roster())
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
}
