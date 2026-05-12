# ============================================================
# Run this script ONCE locally to download and cache all NBA data
# It will create a /data folder with one .rds file per season
# Run it in RStudio: open this file, Ctrl+A, then Ctrl+Enter
# ============================================================

library(hoopR)

SEASONS <- c(
  "2025-26","2024-25","2023-24","2022-23","2021-22","2020-21",
  "2019-20","2018-19","2017-18","2016-17","2015-16","2014-15",
  "2013-14","2012-13","2011-12","2010-11","2009-10","2008-09"
)

# Create data folder if it doesn't exist
if (!dir.exists("data")) dir.create("data")

for (season in SEASONS) {
  cat("Downloading", season, "...\n")
  result <- tryCatch({
    nba_leaguedashplayerstats(
      season = season,
      per_mode_detailed = "Totals"
    )
  }, error = function(e) {
    cat("  FAILED:", e$message, "\n")
    NULL
  })

  if (!is.null(result)) {
    saveRDS(result, file = paste0("data/", season, ".rds"))
    cat("  Saved", season, "\n")
  }
}

cat("\nDone! All available seasons saved to /data folder.\n")
