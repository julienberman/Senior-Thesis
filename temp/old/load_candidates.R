# dependencies
setwd("/Users/julienberman/Documents/0-Thesis")
source("source/lib/imports.R")

######################
######################
######################
######################
######################

# Process the candidates data, in order to get candidate ids
# From OpenSecret Campaign Finance data

clean_name <- function(name) {
  name <- gsub("\\s*\\([^)]*\\)", "", name)  # Remove (party)
  name <- gsub("\\s*\\[[^]]*\\]", "", name)  # Remove [party]
  
  # Remove common suffixes
  suffixes <- paste0("\\s+(Jr|Sr|II|III|Junior|Senior)\\.?", collapse = "|")
  
  name <- gsub(suffixes, "", name, ignore.case = TRUE)
  trimws(name)
}

process_candidates <- function(year, dir) {
  # set the path
  file_path <- paste0(dir, "/cands", year, ".txt")
  
  # Read the file
  df <- read.csv(file_path, 
                 sep = ",",
                 quote = "|",
                 header = FALSE,
                 col.names = c('year', 'fecid', 'cid', 'name_full', 'party', 'seat',
                               'curr_district', 'curr_cand', 'cycle_cand', 'ico_status',
                               'result', 'no_pacs'))
  
  df <- df %>% 
    select(-c(curr_district, curr_cand, ico_status, no_pacs)) %>% 
    filter(cycle_cand == "Y") %>% 
    mutate(
      name_full = clean_name(name_full),
      result = substring(result, 2),
      office = case_when(
        str_detect(seat, "PRES") ~ "President",
        str_detect(seat, "S[12]") ~ "Senate",
        str_detect(seat, "^[A-Z]{2}[0-9]{2}$") ~ "House",
        TRUE ~ NA_character_  # For any cases that don't match above patterns
      ),
      state_abbr = substr(seat, 0, 2)
    ) %>%
    # split name
    mutate(
      # First split the name into all parts
      name_parts = str_split(name_full, "\\s+"),
      # Extract components
      name_first = sapply(name_parts, function(x) x[1]),
      name_last = sapply(name_parts, function(x) x[length(x)]),
      # Middle name is everything in between (if it exists)
      name_middle = sapply(name_parts, function(x) {
        if(length(x) > 2) {
          paste(x[2:(length(x)-1)], collapse = " ")
        } else {
          NA_character_
        }
      }),
      .after = name_full
    ) %>%
    select(-name_parts) %>%  # Remove the temporary list column
    relocate(office, state_abbr, .before = name_full) %>% 
    select(-cycle_cand) %>% 
    # non-withdrawn candidates
    filter(result == "W" | result == "L")
  
  return(df)
}

df_00 <- process_candidates("00", "temp/data_raw")
df_02 <- process_candidates("02", "temp/data_raw")
df_04 <- process_candidates("04", "temp/data_raw")
df_06 <- process_candidates("06", "temp/data_raw")
df_08 <- process_candidates("08", "temp/data_raw")
df_10 <- process_candidates("10", "temp/data_raw")
df_12 <- process_candidates("12", "temp/data_raw")
df_14 <- process_candidates("14", "temp/data_raw")
df_16 <- process_candidates("16", "temp/data_raw")
df_18 <- process_candidates("18", "temp/data_raw")
df_20 <- process_candidates("20", "temp/data_raw")
df_22 <- process_candidates("22", "temp/data_raw")

df_cands <- bind_rows(df_00, df_02, df_04, df_06, df_08, df_10, df_12, df_14, df_16, df_18, df_20, df_22) %>% 
  # convert to lowercase
  mutate(
    office = tolower(office),
    name_full = tolower(name_full),
    name_first = tolower(name_first),
    name_last = tolower(name_last),
    name_middle = tolower(name_middle),
    state_abbr = tolower(state_abbr)
  )

# write
write_csv(df_cands, "temp/data_clean/candidates.csv", na = "")
