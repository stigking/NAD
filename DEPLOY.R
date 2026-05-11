# ============================================================
# NAD Shiny App - Full Setup & Deploy Script
# Run this entire script in RStudio Console (Ctrl+A, then Enter)
# Only needs to be run ONCE
# ============================================================

# --- Step 1: Install required packages if missing ---
pkgs <- c("renv", "rsconnect", "devtools")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

# --- Step 2: Make sure hoopR is installed (GitHub package) ---
if (!requireNamespace("hoopR", quietly = TRUE)) {
  devtools::install_github("sportsdataverse/hoopR")
}

# --- Step 3: Set working directory to project folder ---
setwd("~/OneDrive/Desktop/Project Apps")

# --- Step 4: Initialise renv and snapshot dependencies ---
renv::init(bare = FALSE)
renv::snapshot(prompt = FALSE)

# --- Step 5: Connect to Posit Connect Cloud ---
# Go to https://connect.posit.cloud → sign up → Publishing → Connect RStudio
# Copy the rsconnect::setAccountInfo(...) command they give you and paste it here:

# rsconnect::setAccountInfo(name="YOUR_USERNAME", token="YOUR_TOKEN", secret="YOUR_SECRET")

# --- Step 6: Deploy the app ---
rsconnect::deployApp(
  appDir   = ".",
  appName  = "NAD",
  appTitle = "NBA Analytics Dashboard",
  launch.browser = TRUE
)

# After deploy, go to your app URL on connect.posit.cloud
# → Settings → Access → set to "Anyone" to make it public
