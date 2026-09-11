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
        test_conn <- DBI::dbConnect(
          duckdb::duckdb(),
          sprintf("md:?motherduck_token=%s", token)
        )
        # Test the connection immediately
        DBI::dbGetQuery(test_conn, "SELECT 1")
        test_conn
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
#
# ⚠️ KNOWN LIMITATION -- intentional for now, not a bug:
# This returns a single fixed ID, so EVERY browser/device that connects
# to a deployed instance of this app reads and writes the *same* roster
# and game state row in MotherDuck. There is no per-user or per-team
# isolation: two different coaches/teams pointed at the same deployment
# will see and overwrite each other's data (last write wins).
#
# This is acceptable ONLY for the single-team use case this app was
# built for (one coach, multiple personal devices, one game at a time).
# Do NOT deploy this app for multiple independent teams/users without
# first changing this to a per-user or per-game key -- see
# STATE_PERSISTENCE_LIMITATIONS.md for options (e.g. session$id,
# an authenticated user id, or a URL-based game id).
get_session_id <- function() {
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
      state_b64 <- base64enc::base64encode(state_rds, mode = "raw")

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

      # Mark cloud save as successful. This is a separate sidecar file
      # (not game_state_path itself) because save_game_state() always
      # writes the raw, unwrapped state to game_state_path right after
      # calling this function -- wrapping it here would just get
      # overwritten and the marker lost.
      saveRDS(list(cloud_saved_at = now), game_state_sync_path)
      invisible(TRUE)
    },
    error = function(e) {
      warning("Failed to save game state to cloud: ", e$message)
      # Mark that the cloud write failed, so the next load knows to
      # prefer the local copy over a possibly-stale cloud row. The
      # local copy itself is written by save_game_state() regardless.
      saveRDS(list(cloud_saved_at = NA), game_state_sync_path)
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
        "SELECT game_state_json, updated_at FROM game_state WHERE session_id = ?",
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

      # If the last cloud save attempt failed, the sync marker records
      # cloud_saved_at = NA -- in that case prefer the local copy rather
      # than the (possibly stale) cloud row, since we know the cloud
      # write was never acknowledged.
      if (file.exists(game_state_sync_path)) {
        sync_info <- tryCatch(readRDS(game_state_sync_path), error = function(e) NULL)
        if (!is.null(sync_info) && is.na(sync_info$cloud_saved_at) && file.exists(game_state_path)) {
          local_state <- tryCatch(readRDS(game_state_path), error = function(e) NULL)
          if (!is.null(local_state)) {
            return(migrate_game_state(local_state))
          }
        }
      }

      state_b64 <- result$game_state_json[1]
      state_rds <- base64enc::base64decode(state_b64, output = "raw")
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
      roster_b64 <- base64enc::base64encode(roster_rds, mode = "raw")

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

      # Mark cloud save as successful (sidecar file, since attributes
      # on a data frame don't survive a write.csv/read.csv round trip)
      saveRDS(list(cloud_saved_at = now), roster_sync_path)
      invisible(TRUE)
    },
    error = function(e) {
      warning("Failed to save roster to cloud: ", e$message)
      # Fallback: mark that the cloud write failed, so the next load
      # knows to prefer the local copy over a possibly-stale cloud row
      saveRDS(list(cloud_saved_at = NA), roster_sync_path)
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
        "SELECT roster_json, updated_at FROM roster WHERE session_id = ?",
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

      # If the last cloud save attempt failed, the sync marker records
      # cloud_saved_at = NA -- in that case prefer the local copy rather
      # than the (possibly stale) cloud row, since we know the cloud
      # write was never acknowledged.
      if (file.exists(roster_sync_path)) {
        sync_info <- tryCatch(readRDS(roster_sync_path), error = function(e) NULL)
        if (!is.null(sync_info) && is.na(sync_info$cloud_saved_at) && file.exists(roster_path)) {
          local_roster <- tryCatch(
            utils::read.csv(
              roster_path,
              stringsAsFactors = FALSE,
              colClasses = "character"
            ),
            error = function(e) NULL
          )
          if (!is.null(local_roster)) {
            return(local_roster)
          }
        }
      }

      roster_b64 <- result$roster_json[1]
      roster_rds <- base64enc::base64decode(roster_b64, output = "raw")
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
