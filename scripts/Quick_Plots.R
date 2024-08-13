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
    mutate(athletes = 100 * n_athlete/inital_year[["n_athlete"]],
           country = 100 * (n_country/inital_year[["n_country"]]),
           medal = 100 * (n_medal/inital_year[["n_medal"]])) ->
    to_plot

ggplot(data = to_plot) +
    geom_point(aes(year, athletes, color = "Athlete"), linewidth = 1) +
    geom_point(aes(year, country, color = "Country"), linewidth = 1) +
    geom_point(aes(year, medal, color = "Medal"), linewidth = 1) +
    labs(y = "Percent Change", title = "Summer Olympics", x = NULL) +
    theme(legend.title = element_blank(),
          legend.position = c(0.18, 0.75))


