library(shiny)
library(ggplot2)
library(dplyr)
library(hoopR)
library(plotly)
library(tidyr)

# =========================================================
# FIXED SEASON LIST
# =========================================================

SEASONS <- c(
  "2025-26","2024-25","2023-24","2022-23","2021-22","2020-21",
  "2019-20","2018-19","2017-18","2016-17","2015-16","2014-15",
  "2013-14","2012-13","2011-12","2010-11","2009-10","2008-09"
)

default_season <- "2023-24"

# =========================================================
# TEAM META
# =========================================================

TEAM_META <- data.frame(
  team = c(
    "ATL","BOS","BKN","CHA","CHI","CLE","DAL","DEN","DET","GSW",
    "HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NOP","NYK",
    "OKC","ORL","PHI","PHX","POR","SAC","SAS","TOR","UTA","WAS"
  ),
  conference = c(
    "East","East","East","East","East","East","West","West","East","West",
    "West","East","West","West","West","East","East","West","West","East",
    "West","East","East","West","West","West","West","East","West","East"
  ),
  division = c(
    "Southeast","Atlantic","Atlantic","Southeast","Central","Central",
    "Southwest","Northwest","Central","Pacific","Southwest","Central",
    "Pacific","Pacific","Southwest","Southeast","Central","Northwest",
    "Southwest","Atlantic","Northwest","Southeast","Atlantic","Pacific",
    "Northwest","Pacific","Southwest","Atlantic","Northwest","Southeast"
  ),
  stringsAsFactors = FALSE
)

# =========================================================
# STAT CONFIG
# =========================================================

volume_stat_bases <- c("pts", "ast", "reb", "stl", "blk", "fg3m", "min")
percentage_stat_bases <- c("fg_pct", "fg3_pct", "efg_pct", "ts_pct")

profile_stats <- c(
  "pts", "ast", "reb", "stl", "blk",
  "fg3m", "fg_pct", "fg3_pct", "efg_pct", "ts_pct"
)

radar_profile_stats <- c("pts", "ast", "reb", "stl", "blk")

selector_stat_choices <- c(
  "Points" = "pts",
  "Assists" = "ast",
  "Rebounds" = "reb",
  "Steals" = "stl",
  "Blocks" = "blk",
  "3PM" = "fg3m",
  "FG%" = "fg_pct",
  "3PT%" = "fg3_pct",
  "EFG%" = "efg_pct",
  "TS%" = "ts_pct",
  "Minutes" = "min"
)

matchup_box_stat_choices <- c(
  "Points" = "pts",
  "Rebounds" = "reb",
  "Assists" = "ast",
  "3PM" = "fg3m"
)

matchup_shooting_stat_choices <- c(
  "FG%" = "fg_pct",
  "3PT%" = "fg3_pct",
  "EFG%" = "efg_pct",
  "TS%" = "ts_pct"
)

TEAM_COLORS <- c(
  ATL = "#E03A3E",
  BOS = "#007A33",
  BKN = "#000000",
  CHA = "#00B5E2",
  CHI = "#CE1141",
  CLE = "#6F263D",
  DAL = "#00538C",
  DEN = "#0E2240",
  DET = "#C8102E",
  GSW = "#FFC72C",
  HOU = "#CE1141",
  IND = "#FDBB30",
  LAC = "#C8102E",
  LAL = "#552583",
  MEM = "#5D76A9",
  MIA = "#98002E",
  MIL = "#00471B",
  MIN = "#0C2340",
  NOP = "#0C2340",
  NYK = "#F58426",
  OKC = "#007AC1",
  ORL = "#0077C0",
  PHI = "#006BB6",
  PHX = "#1D1160",
  POR = "#E03A3E",
  SAC = "#5A2D81",
  SAS = "#C4CED4",
  TOR = "#CE1141",
  UTA = "#5A2D81",
  WAS = "#002B5C"
)

neutral_player1_color <- "#F2F2F2"
neutral_player2_color <- "#FF8C69"

# =========================================================
# COLOUR HELPERS
# =========================================================

blend_hex <- function(col1, col2, amount = 0.5) {
  rgb1 <- grDevices::col2rgb(col1) / 255
  rgb2 <- grDevices::col2rgb(col2) / 255
  out <- rgb1 * (1 - amount) + rgb2 * amount
  grDevices::rgb(out[1], out[2], out[3])
}

relative_luminance <- function(col) {
  rgb <- grDevices::col2rgb(col) / 255
  sum(c(0.2126, 0.7152, 0.0722) * rgb)
}

adjust_color_for_dark_bg <- function(col) {
  out <- col
  lum <- relative_luminance(out)
  
  if (lum < 0.18) {
    out <- blend_hex(out, "#FFFFFF", 0.62)
  } else if (lum < 0.30) {
    out <- blend_hex(out, "#FFFFFF", 0.40)
  }
  
  out
}

get_team_color <- function(team_abbr, fallback = "#D9D9D9") {
  if (is.null(team_abbr) || length(team_abbr) == 0 || is.na(team_abbr)) return(fallback)
  if (team_abbr %in% names(TEAM_COLORS)) {
    return(adjust_color_for_dark_bg(unname(TEAM_COLORS[[team_abbr]])))
  }
  fallback
}

# =========================================================
# LABEL / FORMAT HELPERS
# =========================================================

is_pct_stat <- function(stat_base) {
  stat_base %in% percentage_stat_bases
}

stat_plain_label <- function(stat_base) {
  switch(
    stat_base,
    "pts" = "Points",
    "ast" = "Assists",
    "reb" = "Rebounds",
    "stl" = "Steals",
    "blk" = "Blocks",
    "fg3m" = "3PM",
    "fg_pct" = "FG%",
    "fg3_pct" = "3PT%",
    "efg_pct" = "EFG%",
    "ts_pct" = "TS%",
    "min" = "Minutes",
    stat_base
  )
}

stat_col_for_mode <- function(stat_base, mode) {
  paste0(stat_base, "_", mode)
}

stat_label_for_mode <- function(stat_base, mode) {
  if (is_pct_stat(stat_base)) {
    return(stat_plain_label(stat_base))
  }
  
  suffix <- switch(
    mode,
    "tot" = "season total",
    "avg" = "season average",
    "p36" = "per 36"
  )
  
  paste(stat_plain_label(stat_base), suffix)
}

format_stat_value <- function(value, stat_base) {
  out <- rep(NA_character_, length(value))
  ok <- !is.na(value)
  
  if (is_pct_stat(stat_base)) {
    out[ok] <- sprintf("%.1f%%", value[ok])
  } else {
    out[ok] <- sprintf("%.2f", value[ok])
  }
  
  out
}

format_matchup_value <- function(value, stat_base) {
  out <- rep(NA_character_, length(value))
  ok <- !is.na(value)
  
  if (is_pct_stat(stat_base)) {
    out[ok] <- sprintf("%.1f%%", value[ok])
  } else {
    out[ok] <- sprintf("%.1f", value[ok])
  }
  
  out
}

winner_first_result <- function(team1, pts1, team2, pts2, winner, loser) {
  winner_score <- ifelse(winner == team1, pts1, pts2)
  loser_score  <- ifelse(winner == team1, pts2, pts1)
  paste(winner, "beat", loser, paste0(winner_score, "-", loser_score))
}

# =========================================================
# CLICKABLE PLAYER HELPERS
# =========================================================

player_toggle_link <- function(player_name, label = NULL, active = FALSE) {
  if (is.null(label)) label <- player_name
  esc <- gsub("'", "\\\\'", player_name, fixed = TRUE)
  
  tags$a(
    href = "#",
    onclick = sprintf(
      "Shiny.setInputValue('toggle_saved_player', '%s', {priority:'event'}); return false;",
      esc
    ),
    style = paste0(
      "display:inline-block;",
      "margin-right:8px;",
      "margin-bottom:6px;",
      "text-decoration:none;",
      "cursor:pointer;",
      "font-weight:600;",
      if (active) {
        "background:#355CDE;color:white;padding:3px 8px;border-radius:6px;"
      } else {
        "color:#355CDE;"
      }
    ),
    label
  )
}

player_set_link <- function(input_id, player_name, label) {
  esc <- gsub("'", "\\\\'", player_name, fixed = TRUE)
  
  tags$a(
    href = "#",
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority:'event'}); return false;",
      input_id, esc
    ),
    style = paste0(
      "display:inline-block;",
      "margin-left:6px;",
      "padding:2px 6px;",
      "border-radius:5px;",
      "background:#efefef;",
      "color:#222;",
      "font-size:11px;",
      "text-decoration:none;"
    ),
    label
  )
}

radar_polygon_area <- function(r_vals) {
  n <- length(r_vals)
  if (n < 3) return(0)
  area <- 0.5 * sin(2 * pi / n) * sum(r_vals * c(r_vals[-1], r_vals[1]))
  area
}

# =========================================================
# ROBUST HELPERS
# =========================================================

extract_table_by_required_cols <- function(x, required_cols_upper) {
  if (is.data.frame(x)) {
    nms <- toupper(names(x))
    if (all(required_cols_upper %in% nms)) return(x)
  }
  
  if (is.list(x)) {
    nms <- names(x)
    
    if (!is.null(nms)) {
      if (all(c("headers", "rowSet") %in% nms)) {
        df <- as.data.frame(x$rowSet, stringsAsFactors = FALSE)
        colnames(df) <- x$headers
        if (all(required_cols_upper %in% toupper(names(df)))) return(df)
      }
      
      if (all(c("headers", "rowset") %in% nms)) {
        df <- as.data.frame(x$rowset, stringsAsFactors = FALSE)
        colnames(df) <- x$headers
        if (all(required_cols_upper %in% toupper(names(df)))) return(df)
      }
    }
    
    for (item in x) {
      out <- tryCatch(
        extract_table_by_required_cols(item, required_cols_upper),
        error = function(e) NULL
      )
      if (!is.null(out) && is.data.frame(out) && nrow(out) >= 0) {
        out_names <- toupper(names(out))
        if (all(required_cols_upper %in% out_names)) return(out)
      }
    }
  }
  
  NULL
}

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

add_derived_player_stats <- function(df) {
  df$fg_pct  <- ifelse(df$fga > 0, 100 * df$fgm / df$fga, 0)
  df$fg3_pct <- ifelse(df$fg3a > 0, 100 * df$fg3m / df$fg3a, 0)
  df$efg_pct <- ifelse(df$fga > 0, 100 * (df$fgm + 0.5 * df$fg3m) / df$fga, 0)
  df$ts_pct  <- ifelse((df$fga + 0.44 * df$fta) > 0, 100 * df$pts / (2 * (df$fga + 0.44 * df$fta)), 0)
  
  for (st in volume_stat_bases) {
    df[[paste0(st, "_tot")]] <- df[[st]]
    df[[paste0(st, "_avg")]] <- ifelse(df$gp > 0, df[[st]] / df$gp, 0)
    
    if (st == "min") {
      df[[paste0(st, "_p36")]] <- 36
    } else {
      df[[paste0(st, "_p36")]] <- ifelse(df$min > 0, df[[st]] / df$min * 36, 0)
    }
  }
  
  for (st in percentage_stat_bases) {
    df[[paste0(st, "_tot")]] <- df[[st]]
    df[[paste0(st, "_avg")]] <- df[[st]]
    df[[paste0(st, "_p36")]] <- df[[st]]
  }
  
  df
}

add_derived_team_shooting <- function(df) {
  df$fg_pct  <- ifelse(df$fga > 0, 100 * df$fgm / df$fga, 0)
  df$fg3_pct <- ifelse(df$fg3a > 0, 100 * df$fg3m / df$fg3a, 0)
  df$efg_pct <- ifelse(df$fga > 0, 100 * (df$fgm + 0.5 * df$fg3m) / df$fga, 0)
  df$ts_pct  <- ifelse((df$fga + 0.44 * df$fta) > 0, 100 * df$pts / (2 * (df$fga + 0.44 * df$fta)), 0)
  df
}

make_team_badge <- function(team_abbr) {
  col <- get_team_color(team_abbr, "#D9D9D9")
  tags$span(
    style = paste0(
      "display:inline-block;",
      "padding:6px 10px;",
      "margin-right:8px;",
      "margin-bottom:4px;",
      "border-radius:16px;",
      "background:", col, ";",
      "color:black;",
      "font-weight:700;"
    ),
    team_abbr
  )
}

compute_percentile_vs_pool <- function(pool_values, target_value) {
  round(mean(pool_values <= target_value, na.rm = TRUE) * 100, 1)
}

build_risc_summary <- function(percentiles) {
  n <- length(percentiles)
  max_area <- radar_polygon_area(rep(100, n))
  raw_area <- radar_polygon_area(percentiles)
  score <- round(100 * raw_area / max_area, 1)
  avg_pct <- round(mean(percentiles, na.rm = TRUE), 1)
  balance <- round(100 - sd(percentiles, na.rm = TRUE), 1)
  list(score = score, area = raw_area, avg_pct = avg_pct, balance = balance)
}

plotly_png_config <- function(filename_base = "plot") {
  list(
    displaylogo = FALSE,
    doubleClick = "reset+autosize",
    toImageButtonOptions = list(
      format = "png",
      filename = filename_base,
      height = 900,
      width = 1400,
      scale = 2
    )
  )
}

# =========================================================
# LOAD ONE SEASON
# =========================================================

load_season_stats <- function(season_string) {
  stats_raw <- nba_leaguedashplayerstats(
    season = season_string,
    per_mode_detailed = "Totals"
  )
  
  required_upper <- c(
    "PLAYER_NAME","TEAM_ABBREVIATION","PTS","AST","REB","STL","BLK","MIN","GP",
    "FG3M","FGM","FGA","FG3A","FTM","FTA"
  )
  
  stats <- extract_table_by_required_cols(stats_raw, required_upper)
  
  if (is.null(stats) || !is.data.frame(stats) || nrow(stats) == 0) {
    stop(paste("Could not extract NBA stats from API for", season_string))
  }
  
  names(stats) <- toupper(names(stats))
  
  if (!"AGE" %in% names(stats)) stats$AGE <- NA
  if (!"PLAYER_ID" %in% names(stats)) stats$PLAYER_ID <- NA
  
  keep_cols <- c(
    "PLAYER_ID","PLAYER_NAME","TEAM_ABBREVIATION","PTS","AST","REB","STL","BLK",
    "MIN","GP","AGE","FG3M","FGM","FGA","FG3A","FTM","FTA"
  )
  
  missing_cols <- setdiff(keep_cols[c(-1, -11)], names(stats))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns for", season_string, ":", paste(missing_cols, collapse = ", ")))
  }
  
  stats <- stats[, keep_cols, drop = FALSE]
  
  colnames(stats) <- c(
    "player_id","player","team","pts","ast","reb","stl","blk",
    "min","gp","age","fg3m","fgm","fga","fg3a","ftm","fta"
  )
  
  stats$player_id <- as.character(stats$player_id)
  
  num_cols <- c("pts","ast","reb","stl","blk","min","gp","age","fg3m","fgm","fga","fg3a","ftm","fta")
  for (nm in num_cols) stats[[nm]] <- safe_numeric(stats[[nm]])
  
  stats <- stats[complete.cases(stats[, c("player","team","pts","ast","reb","stl","blk","min","gp")]), ]
  stats <- stats[stats$min > 0 & stats$gp > 0, ]
  
  if (nrow(stats) == 0) {
    stop(paste("No usable player rows remained after cleaning for", season_string))
  }
  
  stats <- add_derived_player_stats(stats)
  stats$season <- season_string
  
  stats
}

# =========================================================
# PLAYER CAREER DATA
# =========================================================

career_cache <- new.env(parent = emptyenv())

load_player_career <- function(player_id) {
  raw <- nba_playercareerstats(player_id = player_id)
  
  if (!is.list(raw) || is.null(raw$SeasonTotalsRegularSeason)) {
    stop("Could not retrieve player career data.")
  }
  
  df <- raw$SeasonTotalsRegularSeason
  names(df) <- toupper(names(df))
  
  needed <- c(
    "SEASON_ID","TEAM_ABBREVIATION","GP","MIN","PTS","AST","REB","STL","BLK",
    "FG3M","FGM","FGA","FG3A","FTM","FTA"
  )
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) {
    stop(paste("Career data missing:", paste(miss, collapse = ", ")))
  }
  
  df <- df[, needed, drop = FALSE]
  colnames(df) <- c(
    "season","team","gp","min","pts","ast","reb","stl","blk",
    "fg3m","fgm","fga","fg3a","ftm","fta"
  )
  
  num_cols <- c("gp","min","pts","ast","reb","stl","blk","fg3m","fgm","fga","fg3a","ftm","fta")
  for (nm in num_cols) df[[nm]] <- safe_numeric(df[[nm]])
  
  df <- df[df$gp > 0, , drop = FALSE]
  df <- df[!is.na(df$season), , drop = FALSE]
  
  df <- add_derived_player_stats(df)
  
  valid_levels <- rev(SEASONS)
  df <- df[df$season %in% valid_levels, , drop = FALSE]
  df$season <- factor(df$season, levels = valid_levels, ordered = TRUE)
  df <- df[order(df$season), , drop = FALSE]
  df$season <- as.character(df$season)
  
  df
}

get_player_career_data <- function(player_id) {
  key <- as.character(player_id)
  if (exists(key, envir = career_cache, inherits = FALSE)) {
    return(career_cache[[key]])
  }
  out <- load_player_career(player_id)
  career_cache[[key]] <- out
  out
}

# =========================================================
# TEAM MATCHUP DATA
# =========================================================

team_games_cache <- new.env(parent = emptyenv())

load_team_games <- function(season_string) {
  raw <- nba_leaguegamelog(
    season = season_string,
    player_or_team = "T",
    season_type = "Regular Season"
  )
  
  required_upper <- c(
    "GAME_ID","TEAM_ABBREVIATION","GAME_DATE","MATCHUP","WL",
    "PTS","REB","AST","FG3M","FGM","FGA","FG3A","FTM","FTA"
  )
  df <- extract_table_by_required_cols(raw, required_upper)
  
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    stop(paste("Could not load team game log for", season_string))
  }
  
  names(df) <- toupper(names(df))
  df <- df[, c(
    "GAME_ID","TEAM_ABBREVIATION","GAME_DATE","MATCHUP","WL",
    "PTS","REB","AST","FG3M","FGM","FGA","FG3A","FTM","FTA"
  ), drop = FALSE]
  
  colnames(df) <- c(
    "game_id","team","game_date","matchup","wl",
    "pts","reb","ast","fg3m","fgm","fga","fg3a","ftm","fta"
  )
  
  num_cols <- c("pts","reb","ast","fg3m","fgm","fga","fg3a","ftm","fta")
  for (nm in num_cols) df[[nm]] <- safe_numeric(df[[nm]])
  
  df$game_date <- as.Date(df$game_date)
  df$opponent <- sub("^.*(?:vs\\.|@)\\s*", "", df$matchup)
  df$location <- ifelse(grepl("vs\\.", df$matchup), "Home", "Away")
  
  df <- add_derived_team_shooting(df)
  
  paired <- df %>%
    group_by(game_id) %>%
    arrange(team, .by_group = TRUE) %>%
    mutate(row_n = row_number()) %>%
    ungroup()
  
  paired2 <- paired %>%
    select(
      game_id, team, game_date, matchup, wl,
      pts, reb, ast, fg3m, fg_pct, fg3_pct, efg_pct, ts_pct,
      opponent, location, row_n
    ) %>%
    tidyr::pivot_wider(
      names_from = row_n,
      values_from = c(
        team, game_date, matchup, wl,
        pts, reb, ast, fg3m, fg_pct, fg3_pct, efg_pct, ts_pct,
        opponent, location
      ),
      names_sep = ""
    )
  
  paired2 <- paired2[complete.cases(paired2[, c("team1","team2","pts1","pts2")]), , drop = FALSE]
  
  paired2$winner <- ifelse(paired2$pts1 > paired2$pts2, paired2$team1, paired2$team2)
  paired2$loser  <- ifelse(paired2$pts1 > paired2$pts2, paired2$team2, paired2$team1)
  paired2$margin <- abs(paired2$pts1 - paired2$pts2)
  
  paired2
}

get_team_games <- function(season_string) {
  if (exists(season_string, envir = team_games_cache, inherits = FALSE)) {
    return(team_games_cache[[season_string]])
  }
  out <- load_team_games(season_string)
  team_games_cache[[season_string]] <- out
  out
}

# =========================================================
# CACHE
# =========================================================

season_cache <- new.env(parent = emptyenv())

get_season_data <- function(season_string) {
  if (exists(season_string, envir = season_cache, inherits = FALSE)) {
    return(season_cache[[season_string]])
  }
  
  df <- load_season_stats(season_string)
  season_cache[[season_string]] <- df
  df
}

# =========================================================
# UI
# =========================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .small-note {
        color: #777;
        font-size: 12px;
        line-height: 1.45;
      }
      .saved-top5-card {
        background:#101010;
        border:1px solid #2b2b2b;
        border-radius:10px;
        padding:10px 12px;
        margin-bottom:8px;
        color:white;
      }
      .saved-top5-rank {
        display:inline-block;
        min-width:26px;
        font-weight:800;
        color:#CFCFCF;
      }
      .saved-top5-meta {
        color:#BFBFBF;
        font-size:12px;
      }
    "))
  ),
  
  titlePanel("NBA Player Analytics Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Season", choices = SEASONS, selected = default_season),
      selectInput("conference_filter", "Conference filter", choices = c("All", "East", "West"), selected = "All", multiple = FALSE),
      selectInput("division_filter", "Division filter", choices = c("All", sort(unique(TEAM_META$division))), selected = "All", multiple = TRUE),
      selectInput("team_filter", "Team filter", choices = NULL, multiple = TRUE),
      uiOutput("matchup_pair_ui"),
      checkboxInput("use_team_colors", "Use team colours", value = TRUE),
      
      conditionalPanel(
        condition = "input.main_tabs != 'Team Matchups'",
        sliderInput("age_filter", "Age filter", min = 18, max = 45, value = c(18, 45), step = 1),
        sliderInput("minutes_filter", "Minutes filter", min = 0, max = 3000, value = c(200, 3000), step = 50),
        selectInput("player1", "Player 1", choices = NULL),
        selectInput("player2", "Player 2 (optional)", choices = NULL),
        fluidRow(
          column(6, actionButton("save_player1", "Save Player 1", width = "100%")),
          column(6, actionButton("save_player2", "Save Player 2", width = "100%"))
        ),
        br(),
        uiOutput("saved_players_bar"),
        selectInput(
          "mode", "Stat Mode",
          choices = c("Season total" = "tot", "Season average" = "avg", "Per 36" = "p36"),
          selected = "avg"
        ),
        selectInput("xstat_base", "X Axis", choices = selector_stat_choices, selected = "pts"),
        selectInput("ystat_base", "Y-Axis/Key Stat", choices = selector_stat_choices, selected = "stl")
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'Team Matchups'",
        hr(),
        checkboxGroupInput(
          "matchup_box_stats",
          "Team matchup box-score stats",
          choices = matchup_box_stat_choices,
          selected = c("pts", "reb", "ast")
        ),
        checkboxGroupInput(
          "matchup_shooting_stats",
          "Team matchup shooting stats",
          choices = matchup_shooting_stat_choices,
          selected = character(0)
        )
      )
    ),
    
    mainPanel(
      uiOutput("season_message"),
      conditionalPanel(
        condition = "output.data_ready == false",
        tags$div(style = "padding: 20px; font-size: 18px;", "Loading NBA data...")
      ),
      conditionalPanel(
        condition = "output.data_ready == true",
        tagList(
          tabsetPanel(
            id = "main_tabs",
            tabPanel("Scatter Comparison", plotlyOutput("statPlot", height = "760px")),
            tabPanel(
              "Radar Chart",
              fluidRow(
                column(8, plotlyOutput("radarPlot", height = "760px")),
                column(4, br(), uiOutput("impactScores"))
              ),
              br(),
              fluidRow(
                column(12, tags$h4("Top 5 RISC players in current filter"), uiOutput("top_risc_ui"))
              )
            ),
            tabPanel(
              "Distribution Chart",
              plotlyOutput("distPlot", height = "650px"),
              br(),
              tags$h4("Players in hovered/clicked bin"),
              uiOutput("distPlayers")
            ),
            tabPanel("Career Progression", plotlyOutput("careerPlot", height = "760px")),
            tabPanel(
              "Team Matchups",
              plotlyOutput("matchupPlot", height = "760px"),
              br(),
              tableOutput("matchupTable")
            )
          ),
          br(),
          conditionalPanel(
            condition = "input.main_tabs != 'Team Matchups'",
            fluidRow(
              column(width = 6, h4("Player comparison"), tableOutput("comparison_table")),
              column(width = 6, h4("Most similar players to Player 1"), uiOutput("similarity_table_ui"))
            )
          )
        )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {
  
  loaded_stats <- reactiveVal(NULL)
  load_error <- reactiveVal(NULL)
  saved_players <- reactiveVal(character(0))
  
  output$data_ready <- reactive({
    !is.null(loaded_stats()) && is.null(load_error())
  })
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)
  
  toggle_saved <- function(player_name) {
    if (is.null(player_name) || !nzchar(player_name)) return(NULL)
    cur <- saved_players()
    if (player_name %in% cur) {
      saved_players(setdiff(cur, player_name))
    } else {
      saved_players(c(cur, player_name))
    }
  }
  
  add_saved <- function(player_name) {
    if (is.null(player_name) || !nzchar(player_name) || identical(player_name, "None")) return(NULL)
    cur <- saved_players()
    if (!(player_name %in% cur)) {
      saved_players(c(cur, player_name))
    }
  }
  
  observeEvent(input$toggle_saved_player, {
    toggle_saved(input$toggle_saved_player)
  }, ignoreInit = TRUE)
  
  observeEvent(input$save_player1, { add_saved(input$player1) })
  observeEvent(input$save_player2, { add_saved(input$player2) })
  
  observeEvent(input$set_player1_from_saved, {
    req(input$set_player1_from_saved)
    updateSelectInput(session, "player1", selected = input$set_player1_from_saved)
  }, ignoreInit = TRUE)
  
  observeEvent(input$set_player2_from_saved, {
    req(input$set_player2_from_saved)
    updateSelectInput(session, "player2", selected = input$set_player2_from_saved)
  }, ignoreInit = TRUE)
  
  selected_matchup_teams <- reactive({
    valid_choices <- available_team_choices()
    teams <- input$team_filter
    teams <- teams[teams %in% valid_choices]
    if (is.null(teams) || length(teams) < 2) return(NULL)
    teams[1:2]
  })
  
  output$matchup_pair_ui <- renderUI({
    teams <- input$team_filter
    
    if (is.null(teams) || length(teams) < 2) {
      return(
        tags$details(
          style = "margin-top:6px; margin-bottom:12px;",
          tags$summary(style = "cursor:pointer; font-weight:600;", "Team matchup pair"),
          tags$div(
            style = "margin-top:8px; color:#666;",
            "Select at least 2 teams above. The first 2 selected teams are used in Team Matchups."
          )
        )
      )
    }
    
    matchup_teams <- teams[1:2]
    extra_n <- max(length(teams) - 2, 0)
    
    tags$details(
      style = "margin-top:6px; margin-bottom:12px;",
      tags$summary(style = "cursor:pointer; font-weight:600;", "Team matchup pair (first 2 selected teams)"),
      tags$div(
        style = "margin-top:8px;",
        make_team_badge(matchup_teams[1]),
        make_team_badge(matchup_teams[2]),
        if (extra_n > 0) tags$span(style = "color:#666; font-size:12px;", paste0("+", extra_n, " more selected"))
      )
    )
  })
  
  output$saved_players_bar <- renderUI({
    cur <- saved_players()
    
    if (length(cur) == 0) {
      return(
        tags$div(
          style = "margin-top:6px; margin-bottom:12px;",
          tags$strong("Saved Players"),
          tags$div(style = "margin-top:6px; color:#666;", "Click players on charts/lists to save them here.")
        )
      )
    }
    
    tags$div(
      style = "margin-top:8px; margin-bottom:12px;",
      tags$strong("Saved Players"),
      tags$div(
        style = "margin-top:8px;",
        lapply(cur, function(p) {
          tags$div(
            style = paste0(
              "display:flex;align-items:center;gap:6px;",
              "padding:6px 8px;margin-bottom:6px;border-radius:8px;",
              "background:#f3f3f3;border:1px solid #ddd;"
            ),
            tags$span(style = "font-weight:700; flex:1;", p),
            player_set_link("set_player1_from_saved", p, "P1"),
            player_set_link("set_player2_from_saved", p, "P2"),
            player_toggle_link(p, "Remove", active = FALSE)
          )
        })
      )
    )
  })
  
  observeEvent(TRUE, {
    result <- tryCatch(get_season_data(default_season), error = function(e) e)
    if (inherits(result, "error")) {
      load_error(result$message)
    } else {
      loaded_stats(result)
      load_error(NULL)
    }
  }, once = TRUE)
  
  observeEvent(input$season, {
    result <- tryCatch(get_season_data(input$season), error = function(e) e)
    if (inherits(result, "error")) {
      load_error(result$message)
    } else {
      loaded_stats(result)
      load_error(NULL)
    }
  }, ignoreInit = TRUE)
  
  observe({
    req(loaded_stats())
    
    conf_sel <- input$conference_filter
    division_pool <- TEAM_META
    
    if (!is.null(conf_sel) && conf_sel != "All") {
      division_pool <- division_pool[division_pool$conference == conf_sel, , drop = FALSE]
    }
    
    valid_divisions <- sort(unique(division_pool$division))
    current_divs <- input$division_filter
    
    if (is.null(current_divs) || length(current_divs) == 0) current_divs <- "All"
    current_divs <- current_divs[current_divs %in% c("All", valid_divisions)]
    if (length(current_divs) == 0) current_divs <- "All"
    if (length(current_divs) > 1 && "All" %in% current_divs) {
      current_divs <- setdiff(current_divs, "All")
    }
    
    updateSelectInput(
      session,
      "division_filter",
      choices = c("All", valid_divisions),
      selected = current_divs
    )
  })
  
  observeEvent(input$division_filter, {
    divs <- input$division_filter
    if (is.null(divs)) return(NULL)
    
    if (length(divs) > 1 && "All" %in% divs) {
      updateSelectInput(session, "division_filter", selected = setdiff(divs, "All"))
    }
    
    if (length(divs) == 0) {
      updateSelectInput(session, "division_filter", selected = "All")
    }
  }, ignoreInit = TRUE)
  
  available_team_choices <- reactive({
    req(loaded_stats())
    df <- loaded_stats()
    teams <- sort(unique(df$team))
    
    meta <- TEAM_META[match(teams, TEAM_META$team), , drop = FALSE]
    keep <- rep(TRUE, length(teams))
    
    conf_sel <- input$conference_filter
    div_sel  <- input$division_filter
    
    if (!is.null(conf_sel) && !identical(conf_sel, "All")) {
      keep <- keep & meta$conference %in% conf_sel
    }
    
    if (!is.null(div_sel) && length(div_sel) > 0 && !("All" %in% div_sel)) {
      keep <- keep & meta$division %in% div_sel
    }
    
    sort(teams[keep])
  })
  
  observe({
    req(loaded_stats())
    df <- loaded_stats()
    
    teams <- available_team_choices()
    current_teams <- input$team_filter
    selected_teams <- current_teams[current_teams %in% teams]
    
    updateSelectInput(session, "team_filter", choices = teams, selected = selected_teams)
    
    if (all(is.na(df$age))) {
      updateSliderInput(session, "age_filter", min = 18, max = 45, value = c(18, 45))
    } else {
      age_min <- floor(min(df$age, na.rm = TRUE))
      age_max <- ceiling(max(df$age, na.rm = TRUE))
      current_age <- input$age_filter
      if (is.null(current_age) || length(current_age) != 2) current_age <- c(age_min, age_max)
      current_age[1] <- max(age_min, current_age[1])
      current_age[2] <- min(age_max, current_age[2])
      
      updateSliderInput(session, "age_filter", min = age_min, max = age_max, value = current_age)
    }
    
    min_min <- floor(min(df$min, na.rm = TRUE))
    max_min <- ceiling(max(df$min, na.rm = TRUE))
    current_minutes <- input$minutes_filter
    if (is.null(current_minutes) || length(current_minutes) != 2) current_minutes <- c(min_min, max_min)
    current_minutes[1] <- max(min_min, current_minutes[1])
    current_minutes[2] <- min(max_min, current_minutes[2])
    
    updateSliderInput(session, "minutes_filter", min = min_min, max = max_min, value = current_minutes)
  })
  
  filtered_stats <- reactive({
    req(loaded_stats())
    df <- loaded_stats()
    
    if (!is.null(input$minutes_filter) && length(input$minutes_filter) == 2) {
      df <- df[df$min >= input$minutes_filter[1] & df$min <= input$minutes_filter[2], , drop = FALSE]
    }
    
    team_pool <- available_team_choices()
    
    if (!is.null(input$team_filter) && length(input$team_filter) > 0) {
      chosen <- input$team_filter[input$team_filter %in% team_pool]
      df <- df[df$team %in% chosen, , drop = FALSE]
    } else {
      df <- df[df$team %in% team_pool, , drop = FALSE]
    }
    
    if (!all(is.na(df$age)) && !is.null(input$age_filter) && length(input$age_filter) == 2) {
      df <- df[df$age >= input$age_filter[1] & df$age <= input$age_filter[2], , drop = FALSE]
    }
    
    df
  })
  
  observe({
    req(filtered_stats())
    df <- filtered_stats()
    players <- sort(unique(df$player))
    
    default_player1 <- if ("Kris Dunn" %in% players) "Kris Dunn" else if (length(players) > 0) players[1] else character(0)
    current_p1 <- input$player1
    current_p2 <- input$player2
    
    selected_p1 <- if (!is.null(current_p1) && current_p1 %in% players) current_p1 else default_player1
    player2_choices <- c("None", players)
    selected_p2 <- if (!is.null(current_p2) && (current_p2 %in% player2_choices)) current_p2 else "None"
    
    updateSelectInput(session, "player1", choices = players, selected = selected_p1)
    updateSelectInput(session, "player2", choices = player2_choices, selected = selected_p2)
  })
  
  output$season_message <- renderUI({
    if (!is.null(load_error())) {
      div(style = "color: #b00020; font-weight: bold; margin-bottom: 12px;", paste("Season could not be loaded:", load_error()))
    } else {
      NULL
    }
  })
  
  output$comparison_table <- renderTable({
    req(filtered_stats())
    req(input$player1)
    
    df <- filtered_stats()
    mode <- input$mode
    
    player1_df <- df[df$player == input$player1, , drop = FALSE]
    validate(need(nrow(player1_df) == 1, "Player 1 unavailable under current filters."))
    
    use_player2 <- !is.null(input$player2) && input$player2 != "None"
    player2_df <- if (use_player2) df[df$player == input$player2, , drop = FALSE] else df[0, , drop = FALSE]
    
    make_row <- function(metric, p1, p2 = NA) {
      data.frame(Metric = as.character(metric), Player1 = as.character(p1), Player2 = as.character(p2), stringsAsFactors = FALSE)
    }
    
    rows <- list(
      make_row("Team", player1_df$team[1], if (nrow(player2_df) == 1) player2_df$team[1] else NA),
      make_row("Age", if (!is.na(player1_df$age[1])) round(player1_df$age[1], 0) else NA, if (nrow(player2_df) == 1 && !is.na(player2_df$age[1])) round(player2_df$age[1], 0) else NA),
      make_row("Minutes", round(player1_df$min[1], 0), if (nrow(player2_df) == 1) round(player2_df$min[1], 0) else NA)
    )
    
    for (st in profile_stats) {
      col_name <- stat_col_for_mode(st, mode)
      rows[[length(rows) + 1]] <- make_row(
        stat_label_for_mode(st, mode),
        format_stat_value(player1_df[[col_name]][1], st),
        if (nrow(player2_df) == 1) format_stat_value(player2_df[[col_name]][1], st) else NA
      )
    }
    
    out <- bind_rows(rows)
    colnames(out) <- c("Metric", input$player1, if (use_player2) input$player2 else "Player 2")
    out
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s")
  
  output$similarity_table_ui <- renderUI({
    req(filtered_stats())
    req(input$player1)
    
    df <- filtered_stats()
    mode <- input$mode
    
    player1_df <- df[df$player == input$player1, , drop = FALSE]
    validate(need(nrow(player1_df) == 1, "Player 1 unavailable under current filters."))
    
    sim_cols <- vapply(profile_stats, function(st) stat_col_for_mode(st, mode), character(1))
    
    sim_df <- df[, c("player", "team", sim_cols), drop = FALSE]
    sim_df <- sim_df[complete.cases(sim_df), , drop = FALSE]
    
    target <- as.numeric(player1_df[1, sim_cols, drop = TRUE])
    
    scale_vec <- apply(sim_df[, sim_cols, drop = FALSE], 2, sd, na.rm = TRUE)
    scale_vec[scale_vec == 0 | is.na(scale_vec)] <- 1
    
    distances <- apply(sim_df[, sim_cols, drop = FALSE], 1, function(row) {
      sqrt(sum(((as.numeric(row) - target) / scale_vec)^2))
    })
    
    sim_df$distance <- distances
    sim_df <- sim_df[sim_df$player != input$player1, , drop = FALSE]
    sim_df <- sim_df[order(sim_df$distance), , drop = FALSE]
    sim_df <- head(sim_df, 8)
    
    cur_saved <- saved_players()
    
    tags$div(
      tags$p(
        class = "small-note",
        HTML(
          paste0(
            "<b>How similarity is calculated:</b> standardized Euclidean distance across Points, Assists, Rebounds, ",
            "Steals, Blocks, 3PM, FG%, 3PT%, EFG% and TS% in the currently selected stat mode. ",
            "<b>Lower is better.</b> A score of <b>0</b> would mean two players are statistically identical across these profile stats. ",
            "Smaller values therefore represent a closer overall match."
          )
        )
      ),
      tags$table(
        class = "table table-striped table-hover",
        tags$thead(tags$tr(tags$th("Player"), tags$th("Team"), tags$th("SimilarityScore"))),
        tags$tbody(
          lapply(seq_len(nrow(sim_df)), function(i) {
            tags$tr(
              tags$td(player_toggle_link(sim_df$player[i], sim_df$player[i], sim_df$player[i] %in% cur_saved)),
              tags$td(sim_df$team[i]),
              tags$td(round(sim_df$distance[i], 3))
            )
          })
        )
      )
    )
  })
  
  output$statPlot <- renderPlotly({
    req(filtered_stats())
    req(input$player1)
    
    df <- filtered_stats()
    validate(need(nrow(df) > 0, "No players match the current filters."))
    
    mode <- input$mode
    xstat <- stat_col_for_mode(input$xstat_base, mode)
    ystat <- stat_col_for_mode(input$ystat_base, mode)
    
    player1_df <- df[df$player == input$player1, , drop = FALSE]
    validate(need(nrow(player1_df) == 1, "Player 1 is not available under current filters."))
    
    use_player2 <- !is.null(input$player2) && input$player2 != "None"
    player2_df <- if (use_player2) df[df$player == input$player2, , drop = FALSE] else df[0, , drop = FALSE]
    
    p1_color <- if (isTRUE(input$use_team_colors)) get_team_color(player1_df$team[1], neutral_player1_color) else neutral_player1_color
    p2_color <- if (nrow(player2_df) == 1 && isTRUE(input$use_team_colors)) get_team_color(player2_df$team[1], neutral_player2_color) else neutral_player2_color
    
    if (isTRUE(input$use_team_colors)) {
      df$point_color <- vapply(df$team, get_team_color, character(1), fallback = "#A8A8A8")
      df$point_alpha <- 0.82
    } else {
      df$point_color <- "grey70"
      df$point_alpha <- 0.55
    }
    
    df$hover_text <- paste0(
      "<b>", df$player, "</b><br>",
      "Team: ", df$team,
      ifelse(is.na(df$age), "", paste0("<br>Age: ", sprintf("%.0f", df$age))),
      "<br>Season: ", df$season,
      "<br>", stat_label_for_mode(input$xstat_base, mode), ": ", format_stat_value(df[[xstat]], input$xstat_base),
      "<br>", stat_label_for_mode(input$ystat_base, mode), ": ", format_stat_value(df[[ystat]], input$ystat_base),
      "<br><br>Click point to save/remove player"
    )
    
    x_max <- max(df[[xstat]], na.rm = TRUE)
    y_max <- max(df[[ystat]], na.rm = TRUE)
    
    top_y <- df %>%
      arrange(desc(.data[[ystat]]), desc(.data[[xstat]])) %>%
      slice_head(n = 3)
    
    top_x <- df %>%
      arrange(desc(.data[[xstat]]), desc(.data[[ystat]])) %>%
      slice_head(n = 3)
    
    top_right <- df %>%
      mutate(
        x_norm = ifelse(x_max > 0, .data[[xstat]] / x_max, 0),
        y_norm = ifelse(y_max > 0, .data[[ystat]] / y_max, 0),
        top_right_score = x_norm + y_norm
      ) %>%
      arrange(desc(top_right_score), desc(.data[[xstat]]), desc(.data[[ystat]])) %>%
      slice_head(n = 1)
    
    excluded_players <- c(input$player1, if (use_player2) input$player2 else NULL)
    
    label_df <- bind_rows(top_y, top_x, top_right) %>%
      distinct(player, .keep_all = TRUE) %>%
      filter(!(player %in% excluded_players))
    
    lm_fit <- stats::lm(df[[ystat]] ~ df[[xstat]])
    line_x <- seq(min(df[[xstat]], na.rm = TRUE), max(df[[xstat]], na.rm = TRUE), length.out = 200)
    line_df <- data.frame(
      xline = line_x,
      yline = coef(lm_fit)[1] + coef(lm_fit)[2] * line_x
    )
    
    p <- ggplot(df, aes_string(x = xstat, y = ystat)) +
      geom_point(
        aes(fill = point_color, alpha = point_alpha, text = hover_text, key = player),
        size = 3.5,
        shape = 21,
        color = "grey10",
        stroke = 0.25,
        show.legend = FALSE
      ) +
      scale_fill_identity() +
      scale_alpha_identity() +
      geom_line(
        data = line_df,
        aes(x = xline, y = yline),
        inherit.aes = FALSE,
        color = "deepskyblue2",
        linewidth = 1.4
      ) +
      geom_text(
        data = label_df,
        aes(label = player),
        color = "grey95",
        size = 3.5,
        fontface = "bold",
        vjust = -0.8,
        inherit.aes = FALSE,
        x = label_df[[xstat]],
        y = label_df[[ystat]]
      ) +
      labs(
        title = paste(input$player1, "vs the NBA"),
        x = stat_label_for_mode(input$xstat_base, mode),
        y = stat_label_for_mode(input$ystat_base, mode)
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA),
        panel.grid.major = element_line(color = "grey20"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 22, face = "bold")
      )
    
    g <- ggplotly(p, tooltip = "text", source = "stat_src")
    
    g <- g %>%
      add_markers(
        data = player1_df,
        x = ~.data[[xstat]],
        y = ~.data[[ystat]],
        inherit = FALSE,
        marker = list(size = 28, color = p1_color, opacity = 0.22, line = list(width = 0)),
        customdata = ~player,
        hoverinfo = "skip",
        showlegend = FALSE
      ) %>%
      add_markers(
        data = player1_df,
        x = ~.data[[xstat]],
        y = ~.data[[ystat]],
        inherit = FALSE,
        marker = list(size = 14, color = p1_color, line = list(width = 2.5, color = "white")),
        customdata = ~player,
        text = ~paste0(
          "<b>", player, "</b><br>",
          "Team: ", team,
          ifelse(is.na(age), "", paste0("<br>Age: ", sprintf("%.0f", age))),
          "<br>Season: ", season,
          "<br>", stat_label_for_mode(input$xstat_base, mode), ": ", format_stat_value(.data[[xstat]], input$xstat_base),
          "<br>", stat_label_for_mode(input$ystat_base, mode), ": ", format_stat_value(.data[[ystat]], input$ystat_base),
          "<br><br>Click point to save/remove player"
        ),
        hovertemplate = "%{text}<extra></extra>",
        showlegend = FALSE
      )
    
    if (nrow(player2_df) == 1 && input$player2 != input$player1) {
      g <- g %>%
        add_markers(
          data = player2_df,
          x = ~.data[[xstat]],
          y = ~.data[[ystat]],
          inherit = FALSE,
          marker = list(size = 26, color = p2_color, opacity = 0.22, line = list(width = 0)),
          customdata = ~player,
          hoverinfo = "skip",
          showlegend = FALSE
        ) %>%
        add_markers(
          data = player2_df,
          x = ~.data[[xstat]],
          y = ~.data[[ystat]],
          inherit = FALSE,
          marker = list(size = 13, color = p2_color, line = list(width = 2.5, color = "white")),
          customdata = ~player,
          text = ~paste0(
            "<b>", player, "</b><br>",
            "Team: ", team,
            ifelse(is.na(age), "", paste0("<br>Age: ", sprintf("%.0f", age))),
            "<br>Season: ", season,
            "<br>", stat_label_for_mode(input$xstat_base, mode), ": ", format_stat_value(.data[[xstat]], input$xstat_base),
            "<br>", stat_label_for_mode(input$ystat_base, mode), ": ", format_stat_value(.data[[ystat]], input$ystat_base),
            "<br><br>Click point to save/remove player"
          ),
          hovertemplate = "%{text}<extra></extra>",
          showlegend = FALSE
        )
    }
    
    p1_x <- player1_df[[xstat]][1]
    p1_y <- player1_df[[ystat]][1]
    
    p1_ay <- -38
    p2_ay <- -38
    p1_ax <- 0
    p2_ax <- 0
    
    annotations_list <- list()
    
    if (nrow(player2_df) == 1 && input$player2 != input$player1) {
      p2_x <- player2_df[[xstat]][1]
      p2_y <- player2_df[[ystat]][1]
      x_span <- diff(range(df[[xstat]], na.rm = TRUE))
      y_span <- diff(range(df[[ystat]], na.rm = TRUE))
      if (is.na(x_span) || x_span == 0) x_span <- 1
      if (is.na(y_span) || y_span == 0) y_span <- 1
      
      close_x <- abs(p1_x - p2_x) < 0.08 * x_span
      close_y <- abs(p1_y - p2_y) < 0.08 * y_span
      
      if (close_x && close_y) {
        p1_ax <- -55
        p2_ax <- 55
        p1_ay <- -30
        p2_ay <- -60
      }
    }
    
    annotations_list[[1]] <- list(
      x = p1_x, y = p1_y,
      text = paste0("<b>", player1_df$player[1], "</b>"),
      showarrow = TRUE,
      arrowhead = 2,
      arrowsize = 1,
      arrowwidth = 1.4,
      arrowcolor = p1_color,
      ax = p1_ax,
      ay = p1_ay,
      font = list(color = "white", size = 15),
      bgcolor = "rgba(0,0,0,0.82)",
      bordercolor = p1_color,
      borderwidth = 1.5,
      borderpad = 4
    )
    
    if (nrow(player2_df) == 1 && input$player2 != input$player1) {
      annotations_list[[length(annotations_list) + 1]] <- list(
        x = p2_x, y = p2_y,
        text = paste0("<b>", player2_df$player[1], "</b>"),
        showarrow = TRUE,
        arrowhead = 2,
        arrowsize = 1,
        arrowwidth = 1.4,
        arrowcolor = p2_color,
        ax = p2_ax,
        ay = p2_ay,
        font = list(color = "white", size = 15),
        bgcolor = "rgba(0,0,0,0.82)",
        bordercolor = p2_color,
        borderwidth = 1.5,
        borderpad = 4
      )
    }
    
    g %>%
      layout(
        annotations = annotations_list,
        paper_bgcolor = "black",
        plot_bgcolor = "black",
        font = list(color = "white"),
        margin = list(t = 80, l = 60, r = 20, b = 60)
      ) %>%
      config(
        displaylogo = plotly_png_config("scatter_comparison")$displaylogo,
        doubleClick = plotly_png_config("scatter_comparison")$doubleClick,
        toImageButtonOptions = plotly_png_config("scatter_comparison")$toImageButtonOptions
      )
  })
  
  observeEvent(event_data("plotly_click", source = "stat_src"), {
    evt <- event_data("plotly_click", source = "stat_src")
    if (is.null(evt) || nrow(evt) == 0) return(NULL)
    
    player_name <- NULL
    
    if ("customdata" %in% names(evt) && !is.null(evt$customdata[1]) && !is.na(evt$customdata[1])) {
      player_name <- as.character(evt$customdata[1])
    } else if ("key" %in% names(evt) && !is.null(evt$key[1]) && !is.na(evt$key[1])) {
      player_name <- as.character(evt$key[1])
    }
    
    if (!is.null(player_name) && nzchar(player_name)) {
      toggle_saved(player_name)
    }
  }, ignoreInit = TRUE)
  
  radar_values <- reactive({
    req(filtered_stats())
    req(input$player1)
    
    df <- filtered_stats()
    mode <- input$mode
    
    player1_df <- df[df$player == input$player1, , drop = FALSE]
    validate(need(nrow(player1_df) == 1, "Player 1 unavailable under current filters."))
    
    use_player2 <- !is.null(input$player2) && input$player2 != "None"
    player2_df <- if (use_player2) df[df$player == input$player2, , drop = FALSE] else df[0, , drop = FALSE]
    
    stat_cols <- vapply(radar_profile_stats, function(st) stat_col_for_mode(st, mode), character(1))
    stat_labels <- vapply(radar_profile_stats, stat_plain_label, character(1))
    
    p1_vals <- vapply(seq_along(radar_profile_stats), function(i) {
      col <- stat_cols[i]
      compute_percentile_vs_pool(df[[col]], player1_df[[col]][1])
    }, numeric(1))
    
    p2_vals <- NULL
    if (nrow(player2_df) == 1 && input$player2 != input$player1) {
      p2_vals <- vapply(seq_along(radar_profile_stats), function(i) {
        col <- stat_cols[i]
        compute_percentile_vs_pool(df[[col]], player2_df[[col]][1])
      }, numeric(1))
    }
    
    p1_risc <- build_risc_summary(p1_vals)
    p2_risc <- if (!is.null(p2_vals)) build_risc_summary(p2_vals) else NULL
    
    p1_best_idx <- which.max(p1_vals)
    p1_low_idx  <- which.min(p1_vals)
    
    p2_best_idx <- if (!is.null(p2_vals)) which.max(p2_vals) else NA
    p2_low_idx  <- if (!is.null(p2_vals)) which.min(p2_vals) else NA
    
    p1_share <- NA
    p2_share <- NA
    if (!is.null(p2_risc)) {
      total_area <- p1_risc$area + p2_risc$area
      p1_share <- if (total_area > 0) round(100 * p1_risc$area / total_area, 1) else 50
      p2_share <- if (total_area > 0) round(100 * p2_risc$area / total_area, 1) else 50
    }
    
    list(
      labels = stat_labels,
      p1_vals = p1_vals,
      p2_vals = p2_vals,
      p1_team = player1_df$team[1],
      p2_team = if (nrow(player2_df) == 1) player2_df$team[1] else NA,
      p1_risc = p1_risc,
      p2_risc = p2_risc,
      p1_best = paste0(stat_labels[p1_best_idx], " (", p1_vals[p1_best_idx], "th pct)"),
      p1_low = paste0(stat_labels[p1_low_idx], " (", p1_vals[p1_low_idx], "th pct)"),
      p2_best = if (!is.null(p2_vals)) paste0(stat_labels[p2_best_idx], " (", p2_vals[p2_best_idx], "th pct)") else NA,
      p2_low = if (!is.null(p2_vals)) paste0(stat_labels[p2_low_idx], " (", p2_vals[p2_low_idx], "th pct)") else NA,
      p1_share = p1_share,
      p2_share = p2_share
    )
  })
  
  risc_rankings <- reactive({
    req(filtered_stats())
    
    df <- filtered_stats()
    mode <- input$mode
    
    validate(need(nrow(df) > 0, "No players available for RISC ranking."))
    
    stat_cols <- vapply(radar_profile_stats, function(st) stat_col_for_mode(st, mode), character(1))
    rank_df <- df[, c("player", "team", stat_cols), drop = FALSE]
    rank_df <- rank_df[complete.cases(rank_df), , drop = FALSE]
    
    validate(need(nrow(rank_df) > 0, "No complete rows available for RISC ranking."))
    
    pct_mat <- sapply(seq_along(stat_cols), function(i) {
      col <- stat_cols[i]
      vals <- rank_df[[col]]
      sapply(vals, function(v) compute_percentile_vs_pool(vals, v))
    })
    
    if (is.vector(pct_mat)) pct_mat <- matrix(pct_mat, ncol = length(stat_cols))
    
    risc_scores <- apply(pct_mat, 1, function(x) build_risc_summary(as.numeric(x))$score)
    avg_pct <- apply(pct_mat, 1, function(x) round(mean(as.numeric(x), na.rm = TRUE), 1))
    best_idx <- apply(pct_mat, 1, which.max)
    
    out <- data.frame(
      player = rank_df$player,
      team = rank_df$team,
      risc = round(risc_scores, 1),
      avg_pct = avg_pct,
      best_category = vapply(best_idx, function(i) stat_plain_label(radar_profile_stats[i]), character(1)),
      stringsAsFactors = FALSE
    )
    
    out %>%
      arrange(desc(risc), desc(avg_pct), player) %>%
      slice_head(n = 5)
  })
  
  output$top_risc_ui <- renderUI({
    req(risc_rankings())
    top5 <- risc_rankings()
    cur_saved <- saved_players()
    
    tags$div(
      lapply(seq_len(nrow(top5)), function(i) {
        p <- top5$player[i]
        tags$div(
          class = "saved-top5-card",
          tags$div(
            tags$span(class = "saved-top5-rank", paste0("#", i)),
            player_toggle_link(p, label = p, active = p %in% cur_saved)
          ),
          tags$div(
            class = "saved-top5-meta",
            paste0(
              "Team: ", top5$team[i],
              " | RISC: ", top5$risc[i],
              " | Avg percentile: ", top5$avg_pct[i],
              " | Best category: ", top5$best_category[i]
            )
          )
        )
      })
    )
  })
  
  output$radarPlot <- renderPlotly({
    rv <- radar_values()
    
    p1_color <- if (isTRUE(input$use_team_colors)) get_team_color(rv$p1_team, neutral_player1_color) else neutral_player1_color
    p2_color <- if (!is.null(rv$p2_vals) && isTRUE(input$use_team_colors)) get_team_color(rv$p2_team, neutral_player2_color) else neutral_player2_color
    
    theta_closed <- c(rv$labels, rv$labels[1])
    p1_closed <- c(rv$p1_vals, rv$p1_vals[1])
    
    plot_obj <- plot_ly(type = "scatterpolar", mode = "lines+markers")
    
    plot_obj <- plot_obj %>%
      add_trace(
        r = p1_closed,
        theta = theta_closed,
        fill = "toself",
        name = input$player1,
        line = list(color = p1_color, width = 4),
        marker = list(color = p1_color, size = 8),
        fillcolor = grDevices::adjustcolor(p1_color, alpha.f = 0.22),
        connectgaps = TRUE,
        hovertemplate = paste0("<b>", input$player1, "</b><br>%{theta}: %{r:.1f} percentile<extra></extra>"),
        showlegend = FALSE
      )
    
    if (!is.null(rv$p2_vals)) {
      p2_closed <- c(rv$p2_vals, rv$p2_vals[1])
      
      plot_obj <- plot_obj %>%
        add_trace(
          r = p2_closed,
          theta = theta_closed,
          fill = "toself",
          name = input$player2,
          line = list(color = p2_color, width = 4),
          marker = list(color = p2_color, size = 8),
          fillcolor = grDevices::adjustcolor(p2_color, alpha.f = 0.18),
          connectgaps = TRUE,
          hovertemplate = paste0("<b>", input$player2, "</b><br>%{theta}: %{r:.1f} percentile<extra></extra>"),
          showlegend = FALSE
        )
    }
    
    ann <- list(
      list(
        x = 0.04, y = -0.08, xref = "paper", yref = "paper",
        xanchor = "left",
        text = paste0("<b>RISC</b><br>", input$player1, "<br>", rv$p1_risc$score),
        showarrow = FALSE,
        align = "center",
        font = list(color = "white", size = 13),
        bgcolor = "rgba(0,0,0,0.82)",
        bordercolor = p1_color,
        borderwidth = 2,
        borderpad = 7
      )
    )
    
    if (!is.null(rv$p2_vals)) {
      ann[[length(ann) + 1]] <- list(
        x = 0.96, y = -0.08, xref = "paper", yref = "paper",
        xanchor = "right",
        text = paste0("<b>RISC</b><br>", input$player2, "<br>", rv$p2_risc$score),
        showarrow = FALSE,
        align = "center",
        font = list(color = "white", size = 13),
        bgcolor = "rgba(0,0,0,0.82)",
        bordercolor = p2_color,
        borderwidth = 2,
        borderpad = 7
      )
    }
    
    plot_obj %>%
      layout(
        title = list(text = "Player Comparison", font = list(color = "white", size = 24), x = 0.5, y = 0.98),
        paper_bgcolor = "black",
        plot_bgcolor = "black",
        font = list(color = "white"),
        polar = list(
          bgcolor = "black",
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100),
            tickvals = c(20, 40, 60, 80, 100),
            ticktext = c("20", "40", "60", "80", "100"),
            tickfont = list(color = "white", size = 11),
            gridcolor = "rgba(255,255,255,0.15)",
            linecolor = "rgba(255,255,255,0.25)"
          ),
          angularaxis = list(
            tickfont = list(color = "white", size = 16),
            gridcolor = "rgba(255,255,255,0.15)",
            linecolor = "rgba(255,255,255,0.25)"
          )
        ),
        showlegend = FALSE,
        annotations = ann,
        margin = list(t = 90, l = 60, r = 60, b = 170)
      ) %>%
      config(
        displaylogo = plotly_png_config("radar_chart")$displaylogo,
        doubleClick = plotly_png_config("radar_chart")$doubleClick,
        toImageButtonOptions = plotly_png_config("radar_chart")$toImageButtonOptions
      )
  })
  
  output$impactScores <- renderUI({
    rv <- radar_values()
    
    box_style <- function(title, score, color, share = NULL, best_txt = NULL, low_txt = NULL, avg_txt = NULL, bal_txt = NULL) {
      tags$div(
        style = paste0(
          "background:#111;color:white;border:1px solid #333;",
          "border-left:6px solid ", color, ";",
          "border-radius:10px;padding:14px;margin-bottom:12px;"
        ),
        tags$div(style = "font-size:18px;font-weight:700;", title),
        tags$div(style = "font-size:30px;font-weight:800;margin-top:4px;", paste0(score, " RISC")),
        if (!is.null(share)) tags$div(style = "margin-top:6px;color:#bdbdbd;", share),
        if (!is.null(avg_txt)) tags$div(style = "margin-top:8px;", HTML(paste0("<b>Average percentile:</b> ", avg_txt))),
        if (!is.null(best_txt)) tags$div(style = "margin-top:4px;", HTML(paste0("<b>Best axis:</b> ", best_txt))),
        if (!is.null(low_txt)) tags$div(style = "margin-top:4px;", HTML(paste0("<b>Lowest axis:</b> ", low_txt))),
        if (!is.null(bal_txt)) tags$div(style = "margin-top:4px;", HTML(paste0("<b>Balance score:</b> ", bal_txt)))
      )
    }
    
    p1_color <- if (isTRUE(input$use_team_colors)) get_team_color(rv$p1_team, neutral_player1_color) else neutral_player1_color
    
    if (!is.null(rv$p2_vals)) {
      p2_color <- if (isTRUE(input$use_team_colors)) get_team_color(rv$p2_team, neutral_player2_color) else neutral_player2_color
      
      tags$div(
        tags$h4("RISC Breakdown"),
        tags$p(
          class = "small-note",
          HTML(
            "<b>RISC</b> stands for <b>Relative Impact Score Composite</b>. It uses the radar polygon area created by percentile scores across Points, Assists, Rebounds, Steals and Blocks. Higher is better. A larger polygon means a stronger all-round statistical impact relative to the currently filtered player pool."
          )
        ),
        box_style(
          input$player1, rv$p1_risc$score, p1_color,
          paste0("Share of combined RISC area: ", rv$p1_share, "%"),
          rv$p1_best, rv$p1_low,
          paste0(rv$p1_risc$avg_pct, "th percentile"),
          rv$p1_risc$balance
        ),
        box_style(
          input$player2, rv$p2_risc$score, p2_color,
          paste0("Share of combined RISC area: ", rv$p2_share, "%"),
          rv$p2_best, rv$p2_low,
          paste0(rv$p2_risc$avg_pct, "th percentile"),
          rv$p2_risc$balance
        ),
        tags$div(
          class = "small-note",
          tags$b("Interpretation: "),
          "90+ is elite across this five-stat profile, 70-89 is very strong, 50-69 is above average, and lower scores usually indicate a more specialised or limited box-score impact profile."
        )
      )
    } else {
      tags$div(
        tags$h4("RISC Breakdown"),
        tags$p(
          class = "small-note",
          HTML(
            "<b>RISC</b> stands for <b>Relative Impact Score Composite</b>. It uses the radar polygon area created by percentile scores across Points, Assists, Rebounds, Steals and Blocks. Higher is better. A larger polygon means a stronger all-round statistical impact relative to the currently filtered player pool."
          )
        ),
        box_style(
          input$player1, rv$p1_risc$score, p1_color,
          NULL, rv$p1_best, rv$p1_low,
          paste0(rv$p1_risc$avg_pct, "th percentile"),
          rv$p1_risc$balance
        ),
        tags$div(
          class = "small-note",
          tags$b("Interpretation: "),
          "90+ is elite across this five-stat profile, 70-89 is very strong, 50-69 is above average, and lower scores usually indicate a more specialised or limited box-score impact profile."
        )
      )
    }
  })
  
  dist_bin_data <- reactive({
    req(filtered_stats())
    
    df <- filtered_stats()
    mode <- input$mode
    stat_base <- input$ystat_base
    dist_col <- stat_col_for_mode(stat_base, mode)
    
    validate(need(dist_col %in% names(df), "Selected key stat is unavailable."))
    
    vals <- df[[dist_col]]
    vals <- vals[is.finite(vals)]
    
    validate(need(length(vals) > 1, "Not enough data for a distribution chart."))
    
    h <- hist(vals, breaks = 24, plot = FALSE)
    
    bin_df <- data.frame(
      bin_id = seq_along(h$counts),
      xmin = head(h$breaks, -1),
      xmax = tail(h$breaks, -1),
      count = h$counts,
      midpoint = (head(h$breaks, -1) + tail(h$breaks, -1)) / 2,
      stringsAsFactors = FALSE
    )
    
    player_bins <- df %>%
      filter(is.finite(.data[[dist_col]])) %>%
      mutate(bin_id = findInterval(.data[[dist_col]], h$breaks, all.inside = TRUE)) %>%
      group_by(bin_id) %>%
      summarise(
        players = list(sort(player)),
        sample_players = paste(head(sort(player), 6), collapse = ", "),
        .groups = "drop"
      )
    
    left_join(bin_df, player_bins, by = "bin_id")
  })
  
  output$distPlot <- renderPlotly({
    req(filtered_stats())
    req(input$player1)
    
    df <- filtered_stats()
    mode <- input$mode
    
    stat_base <- input$ystat_base
    dist_col <- stat_col_for_mode(stat_base, mode)
    dist_label <- stat_label_for_mode(stat_base, mode)
    
    validate(need(dist_col %in% names(df), "Selected key stat is unavailable."))
    
    player1_df <- df[df$player == input$player1, , drop = FALSE]
    validate(need(nrow(player1_df) == 1, "Player 1 unavailable under current filters."))
    
    use_player2 <- !is.null(input$player2) && input$player2 != "None"
    player2_df <- if (use_player2) df[df$player == input$player2, , drop = FALSE] else df[0, , drop = FALSE]
    
    p1_color <- if (isTRUE(input$use_team_colors)) get_team_color(player1_df$team[1], neutral_player1_color) else neutral_player1_color
    p2_color <- if (nrow(player2_df) == 1 && isTRUE(input$use_team_colors)) get_team_color(player2_df$team[1], neutral_player2_color) else neutral_player2_color
    
    bins <- dist_bin_data()
    
    hover_text <- paste0(
      "<b>", dist_label, "</b><br>",
      "Range: ", sprintf("%.2f", bins$xmin), " to ", sprintf("%.2f", bins$xmax),
      "<br>Players in bin: ", bins$count,
      ifelse(is.na(bins$sample_players) | bins$sample_players == "", "", paste0("<br>Examples: ", bins$sample_players)),
      "<extra></extra>"
    )
    
    g <- plot_ly(
      data = bins,
      x = ~midpoint,
      y = ~count,
      type = "bar",
      source = "dist_source",
      customdata = ~bin_id,
      hovertemplate = hover_text,
      marker = list(
        color = "rgba(92, 150, 223, 0.85)",
        line = list(color = "rgba(255,255,255,0.12)", width = 1)
      )
    )
    
    shapes_list <- list(
      list(
        type = "line",
        x0 = player1_df[[dist_col]][1],
        x1 = player1_df[[dist_col]][1],
        y0 = 0,
        y1 = 1,
        yref = "paper",
        line = list(color = p1_color, width = 2.5)
      )
    )
    
    annotations_list <- list(
      list(
        x = player1_df[[dist_col]][1],
        y = 0.97,
        yref = "paper",
        text = paste0("<b>", input$player1, "</b>"),
        showarrow = FALSE,
        xanchor = "center",
        font = list(color = p1_color, size = 15),
        bgcolor = "rgba(0,0,0,0.78)",
        bordercolor = p1_color,
        borderwidth = 1.5,
        borderpad = 4
      )
    )
    
    if (nrow(player2_df) == 1 && input$player2 != input$player1) {
      shapes_list[[length(shapes_list) + 1]] <- list(
        type = "line",
        x0 = player2_df[[dist_col]][1],
        x1 = player2_df[[dist_col]][1],
        y0 = 0,
        y1 = 1,
        yref = "paper",
        line = list(color = p2_color, width = 2.5)
      )
      
      annotations_list[[length(annotations_list) + 1]] <- list(
        x = player2_df[[dist_col]][1],
        y = 0.89,
        yref = "paper",
        text = paste0("<b>", input$player2, "</b>"),
        showarrow = FALSE,
        xanchor = "center",
        font = list(color = p2_color, size = 14),
        bgcolor = "rgba(0,0,0,0.78)",
        bordercolor = p2_color,
        borderwidth = 1.5,
        borderpad = 4
      )
    }
    
    g %>%
      layout(
        title = list(text = paste("Distribution of", dist_label), font = list(color = "white", size = 22)),
        xaxis = list(title = dist_label, color = "white", gridcolor = "rgba(255,255,255,0.08)", zeroline = FALSE),
        yaxis = list(title = "Count", color = "white", gridcolor = "rgba(255,255,255,0.08)", zeroline = FALSE),
        paper_bgcolor = "black",
        plot_bgcolor = "black",
        font = list(color = "white"),
        bargap = 0.08,
        annotations = annotations_list,
        shapes = shapes_list,
        margin = list(t = 80, l = 60, r = 20, b = 60)
      ) %>%
      config(
        displaylogo = plotly_png_config("distribution_chart")$displaylogo,
        doubleClick = plotly_png_config("distribution_chart")$doubleClick,
        toImageButtonOptions = plotly_png_config("distribution_chart")$toImageButtonOptions
      )
  })
  
  output$distPlayers <- renderUI({
    bins <- dist_bin_data()
    
    hover_evt <- event_data("plotly_hover", source = "dist_source")
    click_evt <- event_data("plotly_click", source = "dist_source")
    
    chosen_bin <- NULL
    
    if (!is.null(click_evt) && "customdata" %in% names(click_evt)) {
      chosen_bin <- click_evt$customdata[1]
    } else if (!is.null(hover_evt) && "customdata" %in% names(hover_evt)) {
      chosen_bin <- hover_evt$customdata[1]
    }
    
    if (is.null(chosen_bin)) {
      return(tags$div("Hover over or click a histogram bar to see the players in that bin."))
    }
    
    row <- bins[bins$bin_id == chosen_bin, , drop = FALSE]
    
    if (nrow(row) == 0 || length(row$players[[1]]) == 0) {
      return(tags$div("No players found in that bin."))
    }
    
    cur_saved <- saved_players()
    
    tags$div(
      tags$div(
        style = "margin-bottom:10px;font-family:monospace;",
        paste0("Range: ", round(row$xmin[1], 2), " to ", round(row$xmax[1], 2))
      ),
      tags$div(
        lapply(row$players[[1]], function(p) {
          player_toggle_link(p, p, p %in% cur_saved)
        })
      )
    )
  })
  
  output$careerPlot <- renderPlotly({
    req(filtered_stats())
    req(input$player1)
    
    df <- filtered_stats()
    player1_df <- df[df$player == input$player1, , drop = FALSE]
    validate(need(nrow(player1_df) == 1, "Player 1 unavailable under current filters."))
    
    player_id <- player1_df$player_id[1]
    validate(need(!is.na(player_id) && nzchar(player_id), "No player ID available for career progression."))
    
    career_df <- tryCatch(get_player_career_data(player_id), error = function(e) NULL)
    validate(need(!is.null(career_df) && nrow(career_df) > 0, "Career progression data could not be loaded."))
    
    mode <- input$mode
    stat_base <- input$ystat_base
    ycol <- stat_col_for_mode(stat_base, mode)
    ylabel <- stat_label_for_mode(stat_base, mode)
    
    validate(need(ycol %in% names(career_df), "Selected key stat is unavailable in career data."))
    
    career_df <- career_df %>%
      filter(!is.na(.data[[ycol]]), season %in% rev(SEASONS)) %>%
      mutate(season = factor(season, levels = rev(SEASONS), ordered = TRUE)) %>%
      arrange(season)
    
    validate(need(nrow(career_df) > 0, "No valid career progression values available."))
    
    if (isTRUE(input$use_team_colors)) {
      marker_cols <- vapply(career_df$team, get_team_color, character(1), fallback = neutral_player1_color)
    } else {
      marker_cols <- rep(neutral_player1_color, nrow(career_df))
    }
    
    career_df$hover_text <- paste0(
      "<b>", input$player1, "</b><br>",
      "Season: ", as.character(career_df$season),
      "<br>Team: ", career_df$team,
      "<br>Games: ", career_df$gp,
      "<br>", ylabel, ": ", format_stat_value(career_df[[ycol]], stat_base)
    )
    
    plot_ly(
      data = career_df,
      x = ~season,
      y = ~.data[[ycol]],
      type = "scatter",
      mode = "lines+markers+text",
      text = ~as.character(career_df$season),
      textposition = "top center",
      textfont = list(color = "white", size = 12),
      line = list(color = "grey75", width = 3),
      marker = list(size = 10, color = marker_cols, line = list(color = "grey10", width = 1)),
      hovertext = ~hover_text,
      hoverinfo = "text",
      name = input$player1
    ) %>%
      layout(
        title = list(text = paste(input$player1, "-", ylabel, "career progression"), font = list(color = "white", size = 20)),
        paper_bgcolor = "black",
        plot_bgcolor = "black",
        font = list(color = "white"),
        xaxis = list(
          title = "Season",
          type = "category",
          categoryorder = "array",
          categoryarray = as.character(career_df$season),
          tickangle = -45,
          automargin = TRUE,
          color = "white",
          gridcolor = "grey20"
        ),
        yaxis = list(
          title = ylabel,
          automargin = TRUE,
          color = "white",
          gridcolor = "grey20"
        ),
        margin = list(t = 85, l = 70, r = 20, b = 130),
        showlegend = FALSE
      ) %>%
      config(
        displaylogo = plotly_png_config("career_progression")$displaylogo,
        doubleClick = plotly_png_config("career_progression")$doubleClick,
        toImageButtonOptions = plotly_png_config("career_progression")$toImageButtonOptions
      )
  })
  
  matchup_data <- reactive({
    req(input$season)
    teams <- selected_matchup_teams()
    validate(need(!is.null(teams) && length(teams) == 2, "Select at least two teams in Team filter. The first two selected teams drive Team Matchups."))
    
    t1 <- teams[1]
    t2 <- teams[2]
    
    games <- get_team_games(input$season)
    
    out <- games %>%
      filter((team1 == t1 & team2 == t2) | (team1 == t2 & team2 == t1)) %>%
      arrange(game_date1)
    
    validate(need(nrow(out) > 0, "No head-to-head games found for the first two selected teams in this season."))
    
    is_t1_first <- out$team1 == t1
    
    std <- out %>%
      transmute(
        game_date = game_date1,
        team1 = t1,
        points1 = ifelse(is_t1_first, pts1, pts2),
        rebounds1 = ifelse(is_t1_first, reb1, reb2),
        assists1 = ifelse(is_t1_first, ast1, ast2),
        fg3m1 = ifelse(is_t1_first, fg3m1, fg3m2),
        fg_pct1 = ifelse(is_t1_first, fg_pct1, fg_pct2),
        fg3_pct1 = ifelse(is_t1_first, fg3_pct1, fg3_pct2),
        efg_pct1 = ifelse(is_t1_first, efg_pct1, efg_pct2),
        ts_pct1 = ifelse(is_t1_first, ts_pct1, ts_pct2),
        team2 = t2,
        points2 = ifelse(is_t1_first, pts2, pts1),
        rebounds2 = ifelse(is_t1_first, reb2, reb1),
        assists2 = ifelse(is_t1_first, ast2, ast1),
        fg3m2 = ifelse(is_t1_first, fg3m2, fg3m1),
        fg_pct2 = ifelse(is_t1_first, fg_pct2, fg_pct1),
        fg3_pct2 = ifelse(is_t1_first, fg3_pct2, fg3_pct1),
        efg_pct2 = ifelse(is_t1_first, efg_pct2, efg_pct1),
        ts_pct2 = ifelse(is_t1_first, ts_pct2, ts_pct1),
        winner = winner,
        loser = loser,
        margin = margin
      )
    
    std$result <- winner_first_result(std$team1, std$points1, std$team2, std$points2, std$winner, std$loser)
    std
  })
  
  output$matchupPlot <- renderPlotly({
    req(matchup_data())
    
    teams <- selected_matchup_teams()
    validate(need(!is.null(teams) && length(teams) == 2, "Select at least two teams in Team filter."))
    
    df <- matchup_data()
    t1 <- teams[1]
    t2 <- teams[2]
    
    selected_stats <- c(input$matchup_box_stats, input$matchup_shooting_stats)
    validate(need(length(selected_stats) >= 1, "Select at least one matchup stat."))
    
    c1 <- get_team_color(t1, neutral_player1_color)
    c2 <- get_team_color(t2, neutral_player2_color)
    
    make_subplot <- function(st) {
      col1 <- switch(
        st,
        "pts" = "points1",
        "reb" = "rebounds1",
        "ast" = "assists1",
        "fg3m" = "fg3m1",
        "fg_pct" = "fg_pct1",
        "fg3_pct" = "fg3_pct1",
        "efg_pct" = "efg_pct1",
        "ts_pct" = "ts_pct1"
      )
      
      col2 <- switch(
        st,
        "pts" = "points2",
        "reb" = "rebounds2",
        "ast" = "assists2",
        "fg3m" = "fg3m2",
        "fg_pct" = "fg_pct2",
        "fg3_pct" = "fg3_pct2",
        "efg_pct" = "efg_pct2",
        "ts_pct" = "ts_pct2"
      )
      
      stat_lab <- stat_plain_label(st)
      
      hover1 <- paste0(
        "<b>", t1, "</b><br>",
        "Date: ", df$game_date, "<br>",
        "Stat: ", stat_lab, "<br>",
        "Value: ", format_matchup_value(df[[col1]], st),
        "<br>Result: ", df$result,
        "<br>Winner: ", df$winner
      )
      
      hover2 <- paste0(
        "<b>", t2, "</b><br>",
        "Date: ", df$game_date, "<br>",
        "Stat: ", stat_lab, "<br>",
        "Value: ", format_matchup_value(df[[col2]], st),
        "<br>Result: ", df$result,
        "<br>Winner: ", df$winner
      )
      
      winner_annotations <- lapply(seq_len(nrow(df)), function(i) {
        ymax_i <- max(df[[col1]][i], df[[col2]][i], na.rm = TRUE)
        ypad <- 0.06 * diff(range(c(df[[col1]], df[[col2]]), na.rm = TRUE))
        if (!is.finite(ypad) || ypad == 0) ypad <- 0.5
        list(
          x = df$game_date[i],
          y = ymax_i + ypad,
          text = paste0("Winner: ", df$winner[i]),
          showarrow = FALSE,
          font = list(color = "white", size = 10),
          bgcolor = "rgba(0,0,0,0.65)",
          bordercolor = "grey40",
          borderwidth = 1
        )
      })
      
      plot_ly() %>%
        add_lines(
          x = df$game_date,
          y = df[[col1]],
          name = t1,
          line = list(color = c1, width = 3),
          marker = list(color = c1, size = 8),
          mode = "lines+markers+text",
          text = ifelse(df$winner == t1, "W", ""),
          textposition = "top center",
          hovertext = hover1,
          hoverinfo = "text",
          showlegend = FALSE
        ) %>%
        add_lines(
          x = df$game_date,
          y = df[[col2]],
          name = t2,
          line = list(color = c2, width = 3, dash = "dash"),
          marker = list(color = c2, size = 8),
          mode = "lines+markers+text",
          text = ifelse(df$winner == t2, "W", ""),
          textposition = "top center",
          hovertext = hover2,
          hoverinfo = "text",
          showlegend = FALSE
        ) %>%
        layout(
          title = list(text = stat_lab, font = list(color = "white", size = 16), x = 0.5),
          paper_bgcolor = "black",
          plot_bgcolor = "black",
          font = list(color = "white"),
          xaxis = list(title = "", color = "white", gridcolor = "grey20", tickangle = -35),
          yaxis = list(title = "", color = "white", gridcolor = "grey20", zerolinecolor = "grey30"),
          margin = list(t = 70, l = 45, r = 15, b = 60),
          annotations = winner_annotations
        )
    }
    
    plot_list <- lapply(selected_stats, make_subplot)
    ncols <- min(2, length(plot_list))
    nrows <- ceiling(length(plot_list) / ncols)
    
    subplot(plot_list, nrows = nrows, margin = 0.04, shareX = FALSE, shareY = FALSE, titleX = FALSE, titleY = FALSE) %>%
      layout(
        title = list(text = paste(t1, "vs", t2, "- head-to-head matchup stats"), font = list(color = "white", size = 20)),
        paper_bgcolor = "black",
        plot_bgcolor = "black",
        font = list(color = "white"),
        legend = list(orientation = "h", x = 0, y = 1.08, font = list(color = "white")),
        margin = list(t = 85, l = 60, r = 20, b = 40),
        annotations = list(
          list(
            x = 0.02, y = 1.12, xref = "paper", yref = "paper",
            text = paste0("<span style='color:", c1, ";font-weight:700;'>", t1, "</span> &nbsp;&nbsp; ",
                          "<span style='color:", c2, ";font-weight:700;'>", t2, "</span>"),
            showarrow = FALSE,
            xanchor = "left"
          )
        )
      ) %>%
      config(
        displaylogo = plotly_png_config("team_matchups")$displaylogo,
        doubleClick = plotly_png_config("team_matchups")$doubleClick,
        toImageButtonOptions = plotly_png_config("team_matchups")$toImageButtonOptions
      )
  })
  
  output$matchupTable <- renderTable({
    req(matchup_data())
    df <- matchup_data()
    
    data.frame(
      Date = as.character(df$game_date),
      Team1 = df$team1,
      Points1 = round(df$points1, 1),
      Rebounds1 = round(df$rebounds1, 1),
      Assists1 = round(df$assists1, 1),
      `3PM1` = round(df$fg3m1, 1),
      `FG%1` = sprintf("%.1f%%", df$fg_pct1),
      `3PT%1` = sprintf("%.1f%%", df$fg3_pct1),
      `EFG%1` = sprintf("%.1f%%", df$efg_pct1),
      `TS%1` = sprintf("%.1f%%", df$ts_pct1),
      Team2 = df$team2,
      Points2 = round(df$points2, 1),
      Rebounds2 = round(df$rebounds2, 1),
      Assists2 = round(df$assists2, 1),
      `3PM2` = round(df$fg3m2, 1),
      `FG%2` = sprintf("%.1f%%", df$fg_pct2),
      `3PT%2` = sprintf("%.1f%%", df$fg3_pct2),
      `EFG%2` = sprintf("%.1f%%", df$efg_pct2),
      `TS%2` = sprintf("%.1f%%", df$ts_pct2),
      Winner = df$winner,
      Loser = df$loser,
      Margin = df$margin,
      Result = df$result,
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s")
}

# =========================================================
## RUN APP
# =========================================================

shinyApp(ui, server)
