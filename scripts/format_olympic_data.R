# Description: Format the Olympic medal data sets into a single csv file. 

# 0. Set Up --------------------------------------------------------------------
# Load R packages 
library(dplyr)
library(tidyr)

# 1. Add hosting indicator -----------------------------------------------------
# Import the medal count 
# https://www.kaggle.com/datasets/josephcheng123456/olympic-historical-dataset-from-olympediaorg/discussion/342266#1903260
read.csv("data/raw-data/olympic2/Olympic_Games_Medal_Tally.csv") %>%
  filter(grepl(pattern = "Summer Olympics", x = edition)) %>% 
  filter(year <= 2023) %>% 
  mutate(country_noc = toupper(gsub(x = country_noc, replacement = "", pattern = " "))) %>%  
  select(edition_id, year, code = country_noc, total) -> 
  medal_tally

read.csv("data/raw-data/olympic2/Olympics_Games.csv") %>% 
  select(year, edition_id, code = country_noc) %>% 
  mutate(host = 1) -> 
  host_id

medal_tally %>% 
  full_join(host_id, by = join_by(edition_id, year, code)) %>%  
  mutate(host = ifelse(is.na(host), 0, host)) -> 
  host_info

write.csv(host_info, file = file.path("data/medal_count.csv"), row.names = FALSE)

# 2. Make Country Mapping -----------------------------------------------------

read.csv("data/raw-data/olympic2/Olympics_Country.csv") %>% 
  select(code = noc, country) ->
  country_code_mapping
  
write.csv(country_code_mapping, file = file.path("data/country_mapping.csv"), row.names = FALSE)







