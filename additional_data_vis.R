# Make some quick plots

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)

theme_set(theme_bw(base_size = 15))

# 1. Change in Times -----------------------------------------------------------

games_stats <- read.csv("data_kalyn/games_stats.csv")

games_stats %>%
    filter(edition_id == 1) ->
    inital_year


games_stats %>%
    mutate(athletes = n_athlete,
           country = n_country,
           medal = n_medal) ->
    to_plot

    ggplot(data = to_plot) +
        geom_point(aes(year, athletes, color = "Athlete"), linewidth = 1) +
        geom_point(aes(year, country, color = "Country"), linewidth = 1) +
        geom_point(aes(year, medal, color = "Medal"), linewidth = 1) +
        labs(y = "Raw Data", title = "Summer Olympics", x = NULL) +
        theme(legend.title = element_blank(),
              legend.position = c(0.12, 0.80))



games_stats %>%
    mutate(athletes = 100 * n_athlete/inital_year[["n_athlete"]],
           country = 100 * (n_country/inital_year[["n_country"]]),
           medal = 100 * (n_medal/inital_year[["n_medal"]])) ->
    to_plot

ggplot(data = to_plot) +
    geom_point(aes(year, athletes, color = "Athlete"), linewidth = 1) +
    geom_point(aes(year, country, color = "Country"), linewidth = 1) +
    geom_point(aes(year, medal, color = "Medal"), linewidth = 1) +
    labs(y = "Transformated Data", title = "Summer Olympics", x = NULL) +
    theme(legend.title = element_blank(),
          legend.position = c(0.13, 0.79))

# 2. Time Series ---------------------------------------------------------------

games_stats %>%
    select(year, value = n_medal) %>%
    mutate(variable = "medal count") %>%
    mutate(code = NA) ->
    medal_ts

read.csv("data_kalyn/annual_GDP.csv") %>%
    rename(value = gdppc) ->
    gdp_ts

to_plot <- rbind(medal_ts, gdp_ts)

medal_ts %>%
    ggplot(aes(year, value)) +
    geom_bar(stat = "identity") +
    labs(x = "Summer Olympic Games", y = "Medal Count")


gdp_ts %>%
    ggplot(aes(year, value, groupby = code)) +
    geom_line(alpha = 0.5) +
    labs(x = "Year",  y = "GDP 2015 USD$", title = "GDP per Capita per Country")

# 3. Demonstrating filling ---------------------------------------------------------------


read.csv("Final Dataset/before_interp.csv") ->
    before
read.csv("Final Dataset/after_filling.csv") ->
    after

before$na_flag <- FALSE
dist <- unique(before$code[which(is.na(before$expenditure))])

before %>%
    mutate(na_flag = if_else(code %in% dist, TRUE, FALSE))
before

ggplot() +
    geom_line(data = before, aes(year, expenditure,
                                 group = country, color = na_flag), alpha = 0.7) +
    theme(legend.position = "none") +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
    labs(y =  "Expenditure",
         x = "Year",
         title = "Country Sports Expenditure")


read.csv("Final Dataset/after_filling.csv")  ->
    after

after %>%
    ggplot() +
    geom_line(data = before, aes(year, expenditure,
                                 group = country), alpha = 0.7)


