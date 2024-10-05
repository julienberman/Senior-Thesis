# Load dataset
df <- read_csv("data/candidates-final.csv")
columns = c("race_id", "year", "race", "race_type", "primary", "statedistrict", "name", "party", "status", "unopposed", "votes", "totalvotes", "win", "share_male", "share_white", "share_black", "share_hisp")

df %>% 
  arrange(year, race, statedistrict, race_type) %>% 
  select(columns) %>% 
  filter(str_detect(name, "raskin")) %>% 
  View()

df %>% 
  arrange(year, race, statedistrict, race_type) %>% 
  select(columns) %>% 
  filter(votes == totalvotes) %>% 
  View()

df %>% 
  arrange(year, race, statedistrict, race_type) %>% 
  select(columns) %>% 
  filter(is.na(votes) & unopposed == FALSE) %>% 
  View()

df %>% 
  arrange(year, race, statedistrict, race_type) %>% 
  select(columns) %>% 
  filter(race == "Senate" & race_type == "gen" & str_detect(name, "brown")) %>% 
  View()

df %>% 
  arrange(year, race, statedistrict, race_type) %>% 
  select(columns) %>% 
  filter(is.na(share_white)) %>% 
  View()

colnames(df)