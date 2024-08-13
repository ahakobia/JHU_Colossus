# Description: Format the country name information associated with the UN
# Gender inequality index downlaoded from
# https://www.google.com/search?q=HDR23-24_Statistical_Annex_GII_Table&rlz=1C5CHFA_enUS1069US1069&oq=HDR23-24_Statistical_Annex_GII_Table&gs_lcrp=EgZjaHJvbWUyBggAEEUYOdIBBzI1MGowajmoAgCwAgE&sourceid=chrome&ie=UTF-8

# Load R packages
library(dplyr)
library(tidyr)

# Rename the UN country to be consistent with the IOC names.
data.frame("Country" = c("United Kingdom", "Korea (Republic of)", "Saudi Arabia", "Bahamas",
                         "China", "Iran (Islamic Republic of)", "Moldova (Republic of)",  "Viet Nam", "american samoa*", "Venezuela (Bolivarian Republic of)",
                         "Bolivia (Plurinational State of)",  "Eswatini (Kingdom of)", "Tanzania (United Republic of)",
                         "Gambia", "Guinea-Bissau",  "Congo (Democratic Republic of the)", "Korea (Republic of)", "Dominican Republic"),
           "IOC" = c("Great Britain", "Republic of Korea", "Kingdom of Saudi Arabia", "The Bahamas",
                     "People's Republic of China", "Islamic Republic of Iran", "Republic of Moldova",
                     "Vietnam", "American Samoa", "Venezuela", "Bolivia", "Eswatini", "United Republic of Tanzania",
                     "The Gambia", "Guinea Bissau", "Democratic Republic of the Congo", "Korea Team", "Dominican Republic")) ->
    UN_to_IOC


# Note that there was some editing done by hand before being able to import into
read.csv("data_kalyn/raw-data/HDR23-24_Statistical_Annex_GII_Table.csv") %>%
    select(Country, Value) %>%
    distinct() %>%
    full_join(UN_to_IOC, by = join_by(Country)) %>%
    mutate(Country = ifelse(!is.na(IOC), IOC, Country)) %>%
    select(-IOC) ->
    UN_gender_data1


# Save copies of the different names of Russia.
UN_gender_data1 %>%
    filter(Country == "Russian Federation") %>%
    pull(Value) ->
    Russia_Value

other_russian_names <- c("Unified Team",
                         "Russian Olympic Committee",
                         "Soviet Union",
                         "ROC")
russia <- data.frame(Country = other_russian_names,
                     Value = Russia_Value)

UN_gender_data1 %>%
    rbind(russia) ->
    UN_gender_data2


UN_gender_data2 %>%
    filter(Country == "Dominican Republic")

# For the Olympic countries/territories not reported in the UN
# use the regional values. Here create the mapping of the IOC Country
# and the UN index.
regional_values <- matrix(c("Andorra", "Europe and Central Asia",
                            "Aruba", "Latin America and the Caribbean",
                            "Dominica", "Latin America and the Caribbean",
                            "Bermuda", "Latin America and the Caribbean",
                            "British Virgin Islands", "Latin America and the Caribbean",
                            "Central African Republic", "Sub-Saharan Africa",
                            "Comoros", "Sub-Saharan Africa",
                            "Crete", "Europe and Central Asia",
                            "Democratic People's Republic of Korea", "East Asia and the Pacific",
                            "Djibouti", "Sub-Saharan Africa",
                            "East Germany", "Europe and Central Asia",
                            "Eritrea", "Sub-Saharan Africa",
                            "Grenada", "Latin America and the Caribbean",
                            "Hong Kong, China",  "East Asia and the Pacific",
                            "Individual Neutral Athletes", "Europe and Central Asia",
                            "Antigua and Barbuda", "Latin America and the Caribbean",
                            "Australasia", "World",
                            "Bohemia", "Europe and Central Asia",
                            "Cayman Islands", "Latin America and the Caribbean",
                            "Chinese Taipei",  "East Asia and the Pacific",
                            "Cook Islands", "East Asia and the Pacific",
                            "Czechoslovakia",  "East Asia and the Pacific",
                            "Democratic People's Republic of Korea", "East Asia and the Pacific",
                            "Yugoslavia", "Europe and Central Asia",
                            "West Indies Federation","Latin America and the Caribbean",
                            "West Germany", "Europe and Central Asia",
                            "Vanuatu", "East Asia and the Pacific",
                            "Unknown", "World",
                            "United States Virgin Islands", "Latin America and the Caribbean",
                            "United Arab Republic", "Arab States",
                            "Tuvalu", "East Asia and the Pacific",
                            "Turkmenistan", "Europe and Central Asia",
                            "South Yemen", "Sub-Saharan Africa",
                            "South Vietnam", "East Asia and the Pacific",
                            "South Sudan", "Sub-Saharan Africa",
                            "Solomon Islands",  "East Asia and the Pacific",
                            "Seychelles", "East Asia and the Pacific",
                            "Serbia and Montenegro", "Europe and Central Asia",
                            "São Tomé and Príncipe", "Sub-Saharan Africa",
                            "San Marino", "Europe and Central Asia",
                            "Samoa", "East Asia and the Pacific",
                            "Saint Vincent and the Grenadines", "Latin America and the Caribbean",
                            "Saint Kitts and Nevis", "Latin America and the Caribbean",
                            "Saar", "Europe and Central Asia",
                            "Rhodesia", "Sub-Saharan Africa",
                            "Refugee Olympic Team", "Sub-Saharan Africa",
                            "Puerto Rico", "Latin America and the Caribbean",
                            "Palestine", "Arab States",
                            "Palau", "East Asia and the Pacific",
                            "North Yemen", "Sub-Saharan Africa",
                            "North Borneo", "East Asia and the Pacific",
                            "Newfoundland", "World",
                            "Netherlands Antilles", "Latin America and the Caribbean",
                            "Nauru", "East Asia and the Pacific",
                            "Monaco", "Europe and Central Asia",
                            "Mixed team", "World",
                            "Marshall Islands", "East Asia and the Pacific",
                            "Malaya", "East Asia and the Pacific",
                            "Liechtenstein", "Europe and Central Asia",
                            "Kosovo", "Europe and Central Asia",
                            "Kiribati", "East Asia and the Pacific",
                            "Independent Olympic Athletes", "World",
                            "Guam", "East Asia and the Pacific",
                            "Federated States of Micronesia", "East Asia and the Pacific",
                            "Equatorial Guinea", "Sub-Saharan Africa",
                            "Australasia", "East Asia and the Pacific",
                            "Independent Olympic Athletes", "World",
                            "Unknown", "World",
                            "Newfoundland", "World",
                            "Turkey", "Arab States"), ncol = 2, byrow = TRUE)
colnames(regional_values) <- c("Country", "Region")
regional_values <- as.data.frame(regional_values)

# Import the regional values.
read.csv("data_kalyn/raw-data/HDR23-24_Statistical_Annex_GII_Table-Region.csv") ->
    regional_values_df

# Map the regional values to the countries that are missing the data values for.
regional_values %>%
    full_join(regional_values_df, by = join_by(Region)) %>%
    select(Country, Value) %>%
    na.omit %>%
    rbind(UN_gender_data2) %>%
    select(country = Country, value = Value) ->
    UN_gender_data3


# Map the UN files to the IOC codes.
mapping_file <- read.csv("data_kalyn/country_mapping.csv")

mapping_file %>%
    right_join(UN_gender_data3, by = join_by(country)) %>%
    select(code, gender_index = value) %>%
    distinct() ->
    gender_index_df

write.csv(gender_index_df, file = file.path("data_kalyn", "gender_index.csv"), row.names = FALSE)
