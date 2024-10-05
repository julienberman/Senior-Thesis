# Install and load required packages

packages <- c(
  "car",
  "caret",
  "CVXR",
  "formula.tools",
  "GGally",
  "ggplot2",
  "ggrepel",
  "glmnet",
  "gridExtra",
  "leaflet",
  "lubridate",
  "maps",
  "modelsummary",
  "plotly",
  "readxl",
  "sf",
  "shiny",
  "stargazer",
  "tidyverse",
  "usmap",
  "zoo"
)

# Check if each package is installed, install it if necessary
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Load all packages (this is to ensure they're loaded after installation)
lapply(packages, library, character.only = TRUE)

# Print session info for logging
sessionInfo()

# Constants
REQUIRED_COLUMNS <- c(
  "race_id", "year", "race", "race_type", "primary",
  "statedistrict", "name", "party", "status", "unopposed",
  "votes", "totalvotes", "win", "share_male", "share_white",
  "share_black", "share_hisp"
)

VALID_RACE_TYPES <- c("gen", "pri", "run", "spe")
VALID_PARTIES <- c("DEM", "REP", "IND", "LIB", "GRN", "OTH")

# Use message() for Scons logging
log_info <- function(msg) {
  message(sprintf("[INFO] %s: %s", format(Sys.time()), msg))
}

log_warn <- function(msg) {
  message(sprintf("[WARNING] %s: %s", format(Sys.time()), msg))
}

log_error <- function(msg) {
  stop(sprintf("[ERROR] %s: %s", format(Sys.time()), msg))
}