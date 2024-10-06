#' Validate raw data structure and content
#' @param df Dataframe to validate
validate_raw_data <- function(df) {
  # Check required columns
  missing_cols <- setdiff(REQUIRED_COLUMNS, names(df))
  if (length(missing_cols) > 0) {
    log_error(sprintf("Missing required columns: %s", 
                      paste(missing_cols, collapse = ", ")))
  }
  
  # Basic data quality checks
  problems <- df %>%
    filter(
      year < 1900 | year > year(today()),
      !race_type %in% VALID_RACE_TYPES,
      !party %in% VALID_PARTIES & !is.na(party),
      votes > totalvotes & !is.na(votes) & !is.na(totalvotes),
      (votes == totalvotes & !unopposed & !is.na(votes)),
      (share_male < 0 | share_male > 1) & !is.na(share_male),
      (share_white < 0 | share_white > 1) & !is.na(share_white),
      (share_black < 0 | share_black > 1) & !is.na(share_black),
      (share_hisp < 0 | share_hisp > 1) & !is.na(share_hisp)
    )
  
  if (nrow(problems) > 0) {
    log_warn(sprintf("Found %d rows with data quality issues", nrow(problems)))
    print(problems)
  }
}

#' Clean string values
#' @param x Character vector to clean
#' @return Cleaned character vector
clean_string <- function(x) {
  if (!is.character(x)) return(x)
  
  x %>%
    str_trim() %>%
    str_squish() %>%
    na_if("") %>%
    na_if("NA") %>%
    na_if("N/A")
}

#' Standardize candidate names
#' @param name Vector of candidate names
#' @return Standardized names
standardize_name <- function(name) {
  name %>%
    str_to_lower() %>%
    str_replace_all("[^[:alnum:]\\s]", "") %>%  # Remove special chars
    str_replace_all("\\s+", " ") %>%            # Normalize spaces
    str_trim()
}

#' Parse and standardize state-district combinations
#' @param sd Vector of state-district values
#' @return Standardized state-district values
standardize_state_district <- function(sd) {
  sd %>%
    str_to_upper() %>%
    str_replace_all("\\s+", "") %>%     # Remove all whitespace
    str_replace_all("^([A-Z]{2})-", "\\1") # Standardize state-district separator
}

#' Calculate vote shares
#' @param df Dataframe containing votes and totalvotes
#' @return Dataframe with added vote_share column
calculate_vote_shares <- function(df) {
  df %>%
    mutate(
      vote_share = case_when(
        unopposed ~ 1,
        !is.na(votes) & !is.na(totalvotes) & totalvotes > 0 ~ votes / totalvotes,
        TRUE ~ NA_real_
      )
    )
}

#' Identify suspicious voting patterns
#' @param df Dataframe to check
#' @return Dataframe with suspicious_pattern column
flag_suspicious_patterns <- function(df) {
  df %>%
    mutate(
      suspicious_pattern = case_when(
        votes > totalvotes ~ "votes_exceed_total",
        votes == totalvotes & !unopposed ~ "equal_votes_not_unopposed",
        is.na(votes) & !unopposed ~ "missing_votes_not_unopposed",
        votes == 0 & !is.na(totalvotes) & totalvotes > 0 ~ "zero_votes_with_total",
        TRUE ~ "ok"
      )
    )
}

#' Generate summary statistics for a dataset
#' @param df Dataframe to summarize
#' @return List of summary statistics
generate_summary_stats <- function(df) {
  list(
    total_rows = nrow(df),
    years_range = range(df$year, na.rm = TRUE),
    races = table(df$race),
    parties = table(df$party),
    unopposed = sum(df$unopposed, na.rm = TRUE),
    missing_votes = sum(is.na(df$votes)),
    suspicious_patterns = table(df$suspicious_pattern),
    avg_vote_share = mean(df$vote_share, na.rm = TRUE)
  )
}

#' Print summary statistics
#' @param stats List of summary statistics
print_summary_stats <- function(stats) {
  log_info("Dataset Summary:")
  log_info(sprintf("Total rows: %d", stats$total_rows))
  log_info(sprintf("Year range: %d to %d", stats$years_range[1], stats$years_range[2]))
  log_info(sprintf("Races: %s", paste(names(stats$races), stats$races, collapse = ", ")))
  log_info(sprintf("Unopposed races: %d", stats$unopposed))
  log_info(sprintf("Missing vote counts: %d", stats$missing_votes))
  log_info(sprintf("Average vote share: %.2f", stats$avg_vote_share))
  
  if (any(stats$suspicious_patterns != "ok")) {
    log_warn("Suspicious patterns found:")
    print(stats$suspicious_patterns[stats$suspicious_patterns != "ok"])
  }
}

#' Check for duplicate entries
#' @param df Dataframe to check
#' @return Dataframe of duplicate entries
find_duplicates <- function(df) {
  df %>%
    group_by(year, race, statedistrict, race_type, name) %>%
    filter(n() > 1) %>%
    arrange(year, race, statedistrict, race_type, name)
}

#' Validate numerical ranges
#' @param df Dataframe to validate
#' @return Boolean indicating if all checks passed
validate_numerical_ranges <- function(df) {
  all_valid <- TRUE
  
  # Check vote shares
  invalid_shares <- df %>%
    filter(
      (share_male < 0 | share_male > 1) & !is.na(share_male) |
        (share_white < 0 | share_white > 1) & !is.na(share_white) |
        (share_black < 0 | share_black > 1) & !is.na(share_black) |
        (share_hisp < 0 | share_hisp > 1) & !is.na(share_hisp)
    )
  
  if (nrow(invalid_shares) > 0) {
    log_warn(sprintf("Found %d rows with invalid demographic shares", 
                     nrow(invalid_shares)))
    all_valid <- FALSE
  }
  
  # Check vote counts
  invalid_votes <- df %>%
    filter(
      votes < 0 | totalvotes < 0 |
        votes > totalvotes
    )
  
  if (nrow(invalid_votes) > 0) {
    log_warn(sprintf("Found %d rows with invalid vote counts", 
                     nrow(invalid_votes)))
    all_valid <- FALSE
  }
  
  return(all_valid)
}