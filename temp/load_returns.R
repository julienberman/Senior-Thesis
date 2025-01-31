# dependencies
setwd("/Users/julienberman/Documents/0-Thesis")
source("source/lib/imports.R")

######################
######################
######################
######################
######################
# Process the election returns data
# From CQ Voting and Elections Collection

# read data
df_pres <- read_csv("temp/data_raw/cq_pres_county.csv")
df_sen <- read_csv("temp/data_raw/cq_sen_county.csv")
df_gov <- read_csv("temp/data_raw/cq_gov_county.csv")
df_codes <- read_csv("temp/data_raw/state_codes.csv")

view(df_sen)
# preprocessing
df_pres <- df_pres %>% 
  select(-RedistrictedDate) %>% 
  mutate(
    RepVotes = na_if(RepVotes, "N/A"),
    RepCandidate = na_if(RepCandidate, "N/A"),
    RepStatus = na_if(RepStatus, "N/A"),
    DemVotes = na_if(DemVotes, "N/A"),
    DemCandidate = na_if(DemCandidate, "N/A"),
    DemStatus = na_if(DemStatus, "N/A"),
    ThirdVotes = na_if(ThirdVotes, "N/A"),
    ThirdCandidate = na_if(ThirdCandidate, "N/A"),
    ThirdStatus = na_if(ThirdStatus, "N/A"),
    ThirdParty = na_if(ThirdParty, "N/A"),
    OtherVotes = na_if(OtherVotes, "N/A"),
    CensusPop = na_if(CensusPop, "N/A"),
    RepVotes = as.numeric(gsub(",", "", RepVotes)),
    DemVotes = as.numeric(gsub(",", "", DemVotes)),
    ThirdVotes = as.numeric(gsub(",", "", ThirdVotes)),
    OtherVotes = as.numeric(gsub(",", "", OtherVotes)),
    CensusPop = as.numeric(gsub(",", "", CensusPop))
  )
df_sen <- df_sen %>% mutate(
    RepVotes = na_if(RepVotes, "N/A"),
    RepCandidate = na_if(RepCandidate, "N/A"),
    RepStatus = na_if(RepStatus, "N/A"),
    DemVotes = na_if(DemVotes, "N/A"),
    DemCandidate = na_if(DemCandidate, "N/A"),
    DemStatus = na_if(DemStatus, "N/A"),
    ThirdVotes = na_if(ThirdVotes, "N/A"),
    ThirdCandidate = na_if(ThirdCandidate, "N/A"),
    ThirdStatus = na_if(ThirdStatus, "N/A"),
    ThirdParty = na_if(ThirdParty, "N/A"),
    OtherVotes = na_if(OtherVotes, "N/A"),
    CensusPop = na_if(CensusPop, "N/A"),
    RepVotes = as.numeric(gsub(",", "", RepVotes)),
    DemVotes = as.numeric(gsub(",", "", DemVotes)),
    ThirdVotes = as.numeric(gsub(",", "", ThirdVotes)),
    OtherVotes = as.numeric(gsub(",", "", OtherVotes)),
    CensusPop = as.numeric(gsub(",", "", CensusPop)),
    TotalVotes = rowSums(across(c(RepVotes, DemVotes, ThirdVotes, OtherVotes)), na.rm = TRUE),
    RepVotesTotalPercent = (RepVotes / TotalVotes) * 100,
    DemVotesTotalPercent = (DemVotes / TotalVotes) * 100
  )
df_gov <- df_gov %>% mutate(
    RepVotes = na_if(RepVotes, "N/A"),
    RepCandidate = na_if(RepCandidate, "N/A"),
    RepStatus = na_if(RepStatus, "N/A"),
    DemVotes = na_if(DemVotes, "N/A"),
    DemCandidate = na_if(DemCandidate, "N/A"),
    DemStatus = na_if(DemStatus, "N/A"),
    ThirdVotes = na_if(ThirdVotes, "N/A"),
    ThirdCandidate = na_if(ThirdCandidate, "N/A"),
    ThirdStatus = na_if(ThirdStatus, "N/A"),
    ThirdParty = na_if(ThirdParty, "N/A"),
    OtherVotes = na_if(OtherVotes, "N/A"),
    CensusPop = na_if(CensusPop, "N/A"),
    RepVotes = as.numeric(gsub(",", "", RepVotes)),
    DemVotes = as.numeric(gsub(",", "", DemVotes)),
    ThirdVotes = as.numeric(gsub(",", "", ThirdVotes)),
    OtherVotes = as.numeric(gsub(",", "", OtherVotes)),
    CensusPop = as.numeric(gsub(",", "", CensusPop)),
    TotalVotes = rowSums(across(c(RepVotes, DemVotes, ThirdVotes, OtherVotes)), na.rm = TRUE),
    RepVotesTotalPercent = (RepVotes / TotalVotes) * 100,
    DemVotesTotalPercent = (DemVotes / TotalVotes) * 100
  )

# concatenate
df_long <- bind_rows(df_pres, df_sen, df_gov) %>% 
  rename(
   RepShare = RepVotesTotalPercent, 
   DemShare = DemVotesTotalPercent, 
   ThirdShare = ThirdVotesTotalPercent
  ) %>% 
  # pivot longer
  pivot_longer(
    cols = c(RepVotes, DemVotes, ThirdVotes, RepCandidate, DemCandidate, ThirdCandidate, RepStatus, DemStatus, ThirdStatus, RepShare, DemShare, ThirdShare),
    names_to = c("party", ".value"),
    names_pattern = "(Rep|Dem|Third)(Votes|Candidate|Status|Share)"
  ) %>%
  # merge in state codes
  left_join(df_codes, by = c("State" = "state")) %>% 
  relocate(state_abbr, state_num, .after = "State")

# cleaning
df_clean <- df_long %>% 
  mutate(
    # extract year
    year = as.numeric(substr(RaceDate, 0, 4)),
    party = case_when(
      party == "Rep" ~ "Republican",
      party == "Dem" ~ "Democrat",
      party == "Third" ~ "Third Party",
      TRUE ~ party
    ),
    PluralityParty = ifelse(PluralityParty != "R" & PluralityParty != "D", "T", PluralityParty),
    county_plurality = ifelse(PluralityParty == substr(party, 0, 1), 1, 0),
    county_share = Share / 100
    ) %>% 
  # remove useless columns
  select(-c(RaceDate, ThirdParty, OtherVotes, PluralityParty, PluralityVotes, RepVotesMajorPercent, DemVotesMajorPercent, OtherVotesTotalPercent, Share, RaceNotes, TitleNotes, OtherNotes)) %>% 
  # rename columns
  rename(
    office = Office,
    state = State,
    abbr = state_abbr,
    county = Area,
    county_pop = CensusPop,
    county_votes_cast = TotalVotes,
    county_votes_won = Votes,
    name_full = Candidate,
    status = Status
  ) %>% 
  # convert to lowercase
  mutate(across(where(is.character), tolower)) %>% 
  # order
  relocate(year, office, state, abbr, state_num, county, name_full, party, status, county_share, county_votes_won, county_votes_cast, county_plurality, county_pop) %>% 
  # drop n/a in important columns
  filter(!is.na(name_full) & !is.na(county_votes_won)) %>% 
  # drop bad third party candidates
  filter(!name_full == '"none of these candidates"' & !name_full == "write-in" & !name_full == "scattered write-ins") %>% 
  # drop federal absentee, etc.
  filter(!county == "federal absentee" & !county == "federal ballots" & !county == "votes not reported by county" & !county == "overseas vote")

# parse candidate names
parse_name <- function(name_string) {
  # If input is NA or empty, return all empty components
  if (is.na(name_string) || nchar(trimws(name_string)) == 0) {
    return(list(
      last_name = name_string,
      first_name = "",
      middle_name = "",
      suffix = ""
    ))
  }
  
  # Store original case version before converting to lowercase
  original_name <- trimws(name_string)
  name_string <- tolower(original_name)
  
  # Split into last name and remaining parts
  parts <- strsplit(name_string, ",\\s*")[[1]]
  if (length(parts) != 2) {
    # Instead of returning the potentially problematic lowercase version,
    # return the original case version
    return(list(
      last_name = original_name,
      first_name = "",
      middle_name = "",
      suffix = ""
    ))
  }
  
  # Use the original case for the parts that correspond to the lowercase split positions
  original_parts <- strsplit(original_name, ",\\s*")[[1]]
  last_name <- original_parts[1]
  remaining_part <- original_parts[2]
  
  # Split remaining part into words, maintaining original case
  name_parts <- unlist(strsplit(remaining_part, "\\s+"))
  
  # Initialize variables
  first_name <- ""
  middle_name <- ""
  suffix <- ""
  
  # Check for common suffixes
  common_suffixes <- c("jr", "jr.", "sr", "sr.", "ii", "iii", "iv")
  if (length(name_parts) > 0) {
    last_part <- name_parts[length(name_parts)]
    
    if (tolower(last_part) %in% common_suffixes) {
      suffix <- last_part
      name_parts <- name_parts[-length(name_parts)]
    }
    
    if (length(name_parts) > 0) {
      first_name <- name_parts[1]
      
      if (length(name_parts) > 1) {
        middle_name <- paste(name_parts[-1], collapse = " ")
      }
    }
  }
  
  return(list(
    last_name = last_name,
    first_name = first_name,
    middle_name = middle_name,
    suffix = suffix
  ))
}

# apply the name processing
df_parsed <- df_clean %>%
  mutate(
    parsed = lapply(name_full, parse_name),
    name_last = sapply(parsed, `[[`, "last_name"),
    name_first = sapply(parsed, `[[`, "first_name"),
    name_middle = sapply(parsed, `[[`, "middle_name"),
    name_suffix = sapply(parsed, `[[`, "suffix")
  ) %>%
  select(-parsed) %>% 
  relocate(name_first, name_last, name_middle, name_suffix, .after = name_full)

# write
write_csv(df_parsed, "temp/data_clean/returns.csv", na = "")

