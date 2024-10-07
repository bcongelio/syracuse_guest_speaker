library(tidyverse)
library(nflverse)
library(ggtext)
library(extrafont)
library(ggridges)

pbp <- nflreadr::load_pbp(2024)

teams <- nflreadr::load_teams(2024) |> 
  select(team_abbr, team_division)

team_airyards <- pbp |> 
  filter(complete_pass == 1 & !is.na(receiver) & !is.na(air_yards) & !is.na(down)) |> 
  group_by(posteam, air_yards) |> 
  summarize(
    total = n()) |> 
  filter(air_yards >= 0)

team_airyards <- team_airyards |> 
  left_join(teams, by = c("posteam" = "team_abbr"))

meansd <- function(x, ...) {
  mean <- mean(x)
  sd <- sd(x)
  c(mean - sd, mean, mean + sd)
}

ggplot(team_airyards, aes(x = air_yards, y = reorder(posteam, air_yards),
                          fill = posteam, color = posteam)) +
  geom_density_ridges(quantile_lines = TRUE,
                      quantile_fun = meansd) +
  nflplotR::scale_fill_nfl() +
  nflplotR::scale_color_nfl(type = "secondary") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  facet_wrap(~team_division, scales = "free_y") +
  labs(title = "Distribution of Air Yards on Completed Passes",
       subtitle = "2024 NFL Season | Weeks 1-5",
       caption = "Data: nflreadr  |  Chart: @BradCongelio",
       x = "Distribution of Air Yards",
       y = "") +
  theme(axis.text.y = element_nfl_logo(size = .75),
        strip.background = element_rect(fill = "#f7f7f7"),
        strip.text = element_text(family = "Roboto Condensed",
                                  face = "bold",
                                  size = 12.5)) +
  nfl_analytics_theme()
