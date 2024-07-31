# Description: Format the GDP data to match the 

# 0. Script set up  ------------------------------------------------------------
# Load R packages 
library(dplyr)
library(tidyr)
library(readxl)
library(zoo)

# might delet latter
library(ggplot2)


# Function to interpolate and extrapolate
# Args 
#   year: column of df containing the year 
#   gdppc: column of the df containing the gdp values 
#   pop: column of the df containing population information
interpolate_extrapolate <- function(year, gdppc, pop) {
  # Create a zoo object for the data
  gdppc_z <- zoo(gdppc, year)
  pop_z <- zoo(pop, year)
  
  # Interpolate missing values
  interpolated_pop <- na.approx(pop_z, xout = year, rule = 2)
  interpolated_gdppc <- na.approx(gdppc_z, xout = year, rule = 2)
  
  # Convert back to data frame
  result <- data.frame(year = index(interpolated_pop),
                       pop = coredata(interpolated_pop), 
                       gdppc = coredata(interpolated_gdppc))
  return(result)
}

# Load the data frame that contains the mapping between the country name and the
# and the code, also read in the years of data to keep. 
mapping <- read.csv("data/country_mapping.csv")
YRS <- pull(read.csv("data/medal_count.csv"), year)

# The mapping data frames 
# To use with the regional GDP data frame, this df will update the country name 
# to match the notation used in the mapping file. 
to_IOC <- matrix(c("Bolivia (Plurinational State of)", "Bolivia", 
                   "China" , "People's Republic of China", 
                   "D.R. of the Congo", "Congo", 
                   "Czech Republic", "Czechia", 
                   "United Kingdom", "Great Britain", 
                   "Gambia", "The Gambia", 
                   "Guinea-Bissau", "Guinea Bissau", 
                   "China, Hong Kong SAR", "Hong Kong, China", 
                   "Iran (Islamic Republic of)", "Islamic Republic of Iran", 
                   "Lao People's DR", "Lao People's Democratic Republic", 
                   "TFYR of Macedonia", "North Macedonia", 
                   "D.P.R. of Korea", "Democratic People's Republic of Korea", 
                   "State of Palestine", "Palestine", 
                   "Saudi Arabia", "Kingdom of Saudi Arabia", 
                   "Sudan (Former)", "South Sudan", 
                   "Swaziland", "Eswatini", 
                   "U.R. of Tanzania: Mainland",  "United Republic of Tanzania", 
                   "Venezuela (Bolivarian Republic of)", "Venezuela", 
                   "Viet Nam", "Vietnam", 
                   "Former Yugoslavia", "Yugoslavia", 
                   "D.R. of the Congo", "Congo", 
                   "Republic of Korea", "Korea Team", 
                   "North Yemen", "Yemen", 
                   "Bolivia (Plurinational State of)", "Bolivia", 
                   "China", "People's Republic of China", 
                   "D.R. of the Congo", "Democratic Republic of the Congo", 
                   "Swaziland", "Eritrea", 
                   "ROC", "Russian Federation", 
                   "Soviet Union", "Russian Federation", 
                   "Russian Olympic Committee", "Russian Federation"), ncol = 2,  byrow = TRUE)

colnames(to_IOC) <- c("country", "IOC")
to_IOC <- as.data.frame(to_IOC)


# Countries to region mapping data frame, when there is insufficient information pertaining to the 
# the GDP values we will use the regional average. 
regional <- matrix(c( "American Samoa", "South and South East Asia", 
                      "Aruba", "Latin America", 
                      "Bermuda", "Latin America", 
                      "British Virgin Islands", "Latin America", 
                      "Chinese Taipei" , "South and South East Asia", 
                      "Federated States of Micronesia", "South and South East Asia", 
                      "Malaya", "South and South East Asia", 
                      "Netherlands Antilles", "Latin America", 
                      "Andorra", "Eastern Europe", 
                      "Antigua and Barbuda", "Latin America", 
                      "Aruba", "Latin America", 
                      "Australasia", "Western Offshoots", 
                      "Belize", "Latin America", 
                      "Bermuda", "Latin America", 
                      "Bhutan", "South and South East Asia", 
                      "Bohemia", "Eastern Europe", 
                      "British Virgin Islands", "Latin America", 
                      "Brunei Darussalam", "South and South East Asia", 
                      "Cayman Islands", "Latin America",
                      "Chinese Taipei", "South and South East Asia", 
                      "Cook Islands", "South and South East Asia", 
                      "Crete", "Western Europe", 
                      "East Germany", "Western Europe", 
                      "Fiji", "South and South East Asia", 
                      "Grenada", "Latin America",
                      "Guyana", "Latin America", 
                      "Guam", "South and South East Asia", 
                      "Kiribati", "South and South East Asia", 
                      "Kosovo", "Eastern Europe", 
                      "Liechtenstein", "Eastern Europe", 
                      "Maldives", "South and South East Asia", 
                      "Marshall Islands", "South and South East Asia", 
                      "Monaco", "Western Europe", 
                      "Nauru", "South and South East Asia", 
                      "Newfoundland", "Western Offshoots", 
                      "North Borneo", "South and South East Asia", 
                      "North Yemen", "Sub Saharan Africa", 
                      "Palau", "South and South East Asia", 
                      "Papua New Guinea", "South and South East Asia", 
                      "Korea", "South and South East Asia", 
                      "Rhodesia", "Sub Saharan Africa", 
                      "Saar", "Western Europe", 
                      "Saint Kitts and Nevis", "Latin America",
                      "Saint Vincent and the Grenadines", "Latin America",
                      "Samoa", "South and South East Asia", 
                      "San Marino", "Western Europe", 
                      "Serbia and Montenegro", "Western Europe", 
                      "Solomon Islands", "South and South East Asia", 
                      "Somalia", "Sub Saharan Africa", 
                      "South Vietnam", "South and South East Asia", 
                      "South Yemen", "Sub Saharan Africa", 
                      "Suriname", "Latin America",
                      "The Bahamas", "Latin America",
                      "Timor-Leste", "South and South East Asia", 
                      "Tonga", "South and South East Asia",
                      "Tuvalu", "South and South East Asia",
                      "United States Virgin Islands", "Latin America",
                      "Vanuatu", "South and South East Asia", 
                      "West Germany", "Western Europe", 
                      "West Indies Federation", "South and South East Asia"), ncol = 2,  byrow = TRUE)
colnames(regional) <- c("country", "region")
regional <- as.data.frame(regional)



# 1. Format Data ---------------------------------------------------------------

# Load the regional/country gdp values, gap fill for missing data here 
# using linear interpolation and constant extrapolation. 
read_excel("data/raw-data/mpd2023_web.xlsx", sheet =  "Full data") %>%
  select(country, year, gdppc, pop) %>% 
  filter(year >= 1800) %>% 
  group_by(country) %>% 
  do(interpolate_extrapolate(.$year, .$gdppc, .$pop)) %>% 
  ungroup() %>% 
  full_join(to_IOC, by = join_by(country), relationship = "many-to-many") %>% 
  mutate(country = ifelse(!is.na(IOC), IOC, country)) %>% 
  select(-IOC) %>% 
  filter(year >= 1850) %>%  
  full_join(mapping, by = join_by(country)) %>%  
  select(year, gdppc, code) -> 
  gdp_dataset_missingsome_countries


read_excel("data/raw-data/mpd2023_web.xlsx", sheet =  "Regional data") %>% 
  pivot_longer(names_to = "region", cols = -Year) %>% 
  rename(year = Year) %>% 
  mutate( gdppc = value, pop = 1) %>% 
  group_by(region) %>% 
  do(interpolate_extrapolate(.$year, .$gdppc, .$pop)) %>% 
  ungroup() -> 
  regional_gdp_interolpated

  
# Use the regional gdp data as an estimate of country gdp.
regional %>% 
  full_join(regional_gdp_interolpated, relationship = "many-to-many", by = join_by(region)) %>% 
  select(-region, -pop) %>% 
  full_join(mapping, by = join_by(country)) %>% 
  select(year, gdppc, code) -> 
  missing_gdp_values
  

rbind(gdp_dataset_missingsome_countries, 
           missing_gdp_values) %>% 
  filter(code %in%  mapping$code) %>%
  filter(year %in% YRS) -> 
  annual_GDP_values 


write.csv(annual_GDP_values, file.path("data", "annual_GDP.csv"), row.names = FALSE)

