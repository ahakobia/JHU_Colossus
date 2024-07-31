# Description: Create a file ranking the GDP status based on the UN, is is 
# a reflection of their status in 2022 would not vary with years. 

# Set Up 
library(dplyr)
library(tidyr)


# Load the data frame that contains the mapping between the country name and the
# and the code, also read in the years of data to keep. 
mapping <- read.csv("data/country_mapping.csv")


# The categories are they come from the HDR23-24_Statistical_Annex_GII_Table 
# VERY HIGH HUMAN DEVELOPMENT - 4 
# HIGH HUMAN DEVELOPMENT - 3 
# MEDIUM HUMAN DEVELOPMENT - 2 
# LOW HUMAN DEVELOPMENT - 1
# OTHER COUNTRIES OR TERRITORIES - 0 

cat_4 <- c("Switzerland",
           "Norway",
           "Iceland",
           "Hong Kong, China (SAR)",
           "Denmark",
           "Sweden",
           "Germany",
           "Ireland",
           "Singapore",
           "Australia",
           "Netherlands",
           "Belgium",
           "Finland",
           "Liechtenstein",
           "United Kingdom",
           "New Zealand",
           "United Arab Emirates",
           "Canada",
           "Korea (Republic of)",
           "Luxembourg",
           "United States",
           "Austria",
           "Slovenia",
           "Japan",
           "Israel",
           "Malta",
           "Spain",
           "France",
           "Cyprus",
           "Italy",
           "Estonia",
           "Czechia",
           "Greece",
           "Bahrain",
           "Andorra",
           "Poland",
           "Latvia",
           "Lithuania",
           "Croatia",
           "Qatar",
           "Saudi Arabia",
           "Portugal",
           "San Marino",
           "Chile",
           "Slovakia",
           "Türkiye",
           "Hungary",
           "Argentina",
           "Kuwait",
           "Montenegro",
           "Saint Kitts and Nevis",
           "Uruguay",
           "Romania",
           "Antigua and Barbuda",
           "Brunei Darussalam",
           "Russian Federation",
           "Bahamas",
           "Panama",
           "Oman",
           "Georgia",
           "Trinidad and Tobago",
           "Barbados",
           "Malaysia",
           "Costa Rica",
           "Serbia",
           "Thailand",
           "Kazakhstan",
           "Seychelles",
           "Belarus")


cat_3 <- c("Bulgaria", 
           "Palau", 
           "Mauritius", 
           "Grenada", 
           "Albania", 
           "China", 
           "Armenia", 
           "Mexico", 
           "Iran (Islamic Republic of)", 
           "Sri Lanka", 
           "Bosnia and Herzegovina", 
           "Saint Vincent and the Grenadines", 
           "Dominican Republic", 
           "Ecuador", 
           "North Macedonia", 
           "Cuba", 
           "Moldova (Republic of)", 
           "Maldives", 
           "Peru", 
           "Azerbaijan", 
           "Brazil", 
           "Colombia", 
           "Libya", 
           "Algeria", 
           "Turkmenistan", 
           "Guyana", 
           "Mongolia", 
           "Dominica", 
           "Tonga", 
           "Jordan", 
           "Ukraine", 
           "Tunisia", 
           "Marshall Islands", 
           "Paraguay", 
           "Fiji", 
           "Egypt", 
           "Uzbekistan", 
           "Viet Nam", 
           "Saint Lucia", 
           "Lebanon", 
           "South Africa", 
           "Palestine, State of", 
           "Indonesia", 
           "Philippines", 
           "Botswana", 
           "Jamaica", 
           "Samoa", 
           "Kyrgyzstan", 
           "Belize")


cat_2 <- c("Venezuela (Bolivarian Republic of)",
           "Bolivia (Plurinational State of)",
           "Morocco",
           "Nauru",
           "Gabon",
           "Suriname",
           "Bhutan",
           "Tajikistan",
           "El Salvador",
           "Iraq",
           "Bangladesh",
           "Nicaragua",
           "Cabo Verde",
           "Tuvalu",
           "Equatorial Guinea",
           "India",
           "Micronesia (Federated States of)",
           "Guatemala",
           "Kiribati",
           "Honduras",
           "Lao People's Democratic Republic", 
           "Vanuatu",
           "Sao Tome and Principe",
           "Eswatini (Kingdom of)",
           "Namibia",
           "Myanmar",
           "Ghana",
           "Kenya",
           "Nepal",
           "Cambodia",
           "Congo",
           "Angola",
           "Cameroon",
           "Comoros",
           "Zambia",
           "Papua New Guinea",
           "Timor-Leste",
           "Solomon Islands",
           "Syrian Arab Republic",
           "Haiti",
           "Uganda",
           "Zimbabwe", 
          "American Samoa")


cat_1 <- c("Nigeria",
           "Rwanda",
           "Togo",
           "Mauritania",
           "Pakistan",
           "Côte d'Ivoire",
           "Tanzania (United Republic of)",
           "Lesotho",
           "Senegal",
           "Sudan",
           "Djibouti",
           "Malawi",
           "Benin",
           "Gambia",
           "Eritrea",
           "Ethiopia",
           "Liberia",
           "Madagascar",
           "Guinea-Bissau",
           "Congo (Democratic Republic of the)",
           "Guinea",
           "Afghanistan",
           "Mozambique",
           "Sierra Leone",
           "Burkina Faso",
           "Yemen",
           "Burundi",
           "Mali",
           "Chad",
           "Niger",
           "Central African Republic",
           "South Sudan",
           "Somalia")

hdi_df <- as.data.frame(rbind(
  cbind(cat_4, 4), 
  cbind(cat_3, 3), 
  cbind(cat_2, 2), 
  cbind(cat_1, 1)))
colnames(hdi_df) <- c("country", "hdi")


# Rename the UN country to be consistent with the IOC names. 
data.frame("country" = c("United Kingdom", "Korea (Republic of)", "Saudi Arabia", "Bahamas", 
                         "China", "Iran (Islamic Republic of)", "Moldova (Republic of)",  "Viet Nam", "Venezuela (Bolivarian Republic of)", 
                         "Bolivia (Plurinational State of)",  "Eswatini (Kingdom of)", "Tanzania (United Republic of)", 
                         "Gambia", "Guinea-Bissau",  "Congo (Democratic Republic of the)", "Korea (Republic of)", "Dominican Republic", 
                         "Hong Kong, China (SAR)", "Palestine, State of", "Micronesia (Federated States of)"),
           "IOC" = c("Great Britain", "Republic of Korea", "Kingdom of Saudi Arabia", "The Bahamas", 
                     "People's Republic of China", "Islamic Republic of Iran", "Republic of Moldova", 
                     "Vietnam", "Venezuela", "Bolivia", "Eswatini", "United Republic of Tanzania", 
                     "The Gambia", "Guinea Bissau", "Democratic Republic of the Congo", "Korea Team", "Dominica", 
                     "Hong Kong, China", "Palestine", "Federated States of Micronesia")) -> 
  UN_to_IOC



hdi_df %>% 
  distinct() %>% 
  full_join(UN_to_IOC, by = join_by(country)) %>% 
  mutate(country = ifelse(!is.na(IOC), IOC, country)) %>%  
  select(-IOC) %>% 
  inner_join(mapping, by = "country") %>%  
  select(code, hdi) -> 
  out


write.csv(out, file = file.path("data/country_hdi.csv"), row.names = FALSE)




