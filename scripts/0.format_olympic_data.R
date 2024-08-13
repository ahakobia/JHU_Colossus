# Description: Format the Olympic medal data sets into a single csv file.

# 0. Set Up --------------------------------------------------------------------
# Load R packages
library(dplyr)
library(tidyr)

# 1. Add hosting indicator -----------------------------------------------------
# Import the medal count
# https://www.kaggle.com/datasets/josephcheng123456/olympic-historical-dataset-from-olympediaorg/discussion/342266#1903260
read.csv("data_kalyn/raw-data/olympic2/Olympic_Games_Medal_Tally.csv") %>%
    filter(grepl(pattern = "Summer Olympics", x = edition)) %>%
    mutate(country_noc = toupper(gsub(x = country_noc, replacement = "", pattern = " "))) %>%
    select(edition_id, year, code = country_noc, gold, silver, bronze, total) ->
    medal_tally

read.csv("data_kalyn/raw-data/olympic2/Olympics_Games.csv") %>%
    filter(grepl(pattern = "Summer Olympics", x = edition)) %>%
    select(year, edition_id, code = country_noc) %>%
    mutate(host = 1) ->
    host_id

medal_tally %>%
    full_join(host_id, by = join_by(edition_id, year, code)) %>%
    mutate(host = ifelse(is.na(host), 0, host)) %>%
    select(edition_id, year, code, total, host) ->
    host_info

write.csv(host_info, file = file.path("data_kalyn/medal_count.csv"), row.names = FALSE)

# 2. Make Country Mapping -----------------------------------------------------

read.csv("data_kalyn/raw-data/olympic2/Olympics_Country.csv") %>%
    select(code = noc, country) ->
    country_code_mapping

write.csv(country_code_mapping, file = file.path("data_kalyn/country_mapping.csv"), row.names = FALSE)

# 3. Misc Stats ---------------------------------------------------------------

# Load data Athlete results data table, this includes info for all athletes
# particpating in the games
read.csv("data_kalyn/raw-data/olympic2/Olympic_Athlete_Event_Results.csv") %>%
    filter(grepl(pattern = "Summer Olympics", x = edition)) %>%
    mutate(year = as.integer(substr(edition, 1, 4))) ->
    summer_olympic_athletes

# Count the number of athletes sent to compete in individual events. The
# challenge here is that some athletes compete in multiple events.
summer_olympic_athletes %>%
    filter(isTeamSport == "False") %>%
    select(edition_id, year, athlete_id, country_noc) %>%
    distinct() %>%
    group_by(edition_id, year, country_noc) %>%
    summarise(count = n()) %>%
    ungroup ->
    indiviudal_sport_events

# Count the number of team sports that qualified per country.
summer_olympic_athletes %>%
    filter(!isTeamSport == "False") %>%
    select(edition_id, year, country_noc, sport, event) %>%
    distinct() %>%
    group_by(edition_id, year, country_noc) %>%
    summarise(count = n()) ->
    team_sport_events

rbind(indiviudal_sport_events, team_sport_events) %>%
    group_by(edition_id, year, code = country_noc) %>%
    summarise(participants = sum(count)) ->
    participants_country

write.csv(participants_country, file = file.path("data_kalyn/country_athlete_count.csv"), row.names = FALSE)


# Figure out the total number of "athletes" sent to participate in each
# Olympic games.
participants_country %>%
    group_by(edition_id, year) %>%
    summarise(n_athlete = sum(participants)) ->
    participants_games

# Figure out the number of countries competing the in the games.
summer_olympic_athletes %>%
    select(edition_id, year, country_noc) %>%
    distinct() %>%
    group_by(edition_id) %>%
    summarise(n_country = n()) ->
    games_country_count


# Calculate the total number of medals awarded at the games
medal_tally %>%
    group_by(edition_id) %>%
    summarise(n_medal = sum(total)) %>%
    ungroup() ->
    games_medal_count

host_info %>%
    select(edition_id) %>%
    distinct() %>%
    left_join(participants_games, by = join_by(edition_id)) %>%
    na.omit %>%
    left_join(games_country_count, by = join_by(edition_id)) %>%
    left_join(games_medal_count, by = join_by(edition_id)) %>%
    na.omit ->
    games_stats

write.csv(games_stats, file = file.path("data_kalyn/games_stats.csv"), row.names = FALSE)


# 4. Calculate the portions ---------------------------------------------------

host_info %>%
    right_join(participants_country, by = join_by(edition_id, year, code)) %>%
    mutate(host = ifelse(is.na(host), 0, host)) %>%
    mutate(total = ifelse(is.na(total), 0, total)) %>%
    left_join(games_stats, by = join_by(edition_id, year)) %>%
    mutate(total = total/n_medal,
           participants = participants/n_athlete) %>%
    select(year, code, total, host, participants, n_country) ->
    medal_poportions

write.csv(medal_poportions, file = file.path("data_kalyn/medal_stats.csv"), row.names = FALSE)
