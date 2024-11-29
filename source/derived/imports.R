# Install and load required packages

packages <- c(
  "broom",
  "car",
  "caret",
  "corrplot",
  "CVXR",
  "fastDummies",
  "fixest",
  "formula.tools",
  "GGally",
  "ggplot2",
  "ggrepel",
  "glmnet",
  "gridExtra",
  "gt",
  "haven",
  "insight",
  "janitor",
  "jsonlite",
  "leaflet",
  "lubridate",
  "maps",
  "modelsummary",
  "plotly",
  "readxl",
  "reshape2",
  "scales",
  "sf",
  "shiny",
  "stargazer",
  "stringr",
  "tidyverse",
  "texreg",
  "usmap",
  "wru",
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