library(tidyverse)
library(ggthemes)
library(ggimage)
library(nflfastR)
library(nflplotR)

pbp <- load_pbp(2020:2021)

master_20 <- pbp %>%
  filter(season == 2020 & season_type == "REG") %>%
  filter((!is.na(posteam) | !is.na(defteam)) & penalty == 0 &
           (pass == 1 | rush == 1))

master_21 <- pbp %>%
  filter(season == 2021) %>%
  filter((!is.na(posteam) | !is.na(defteam)) & penalty == 0 &
           (pass == 1 | rush == 1))

def_20 <- master_20 %>%
  group_by(defteam) %>%
  summarise(def_epa_20 = mean(epa, na.rm = TRUE))

epa_20 <- master_20 %>%
  group_by(posteam) %>%
  summarise(off_epa_20 = mean(epa, na.rm = TRUE)) %>%
  left_join(def_20, by = c("posteam" = "defteam")) %>%
  rename(team = "posteam")

def_21 <- master_21 %>%
  group_by(defteam) %>%
  summarise(def_epa_21 = mean(epa, na.rm = TRUE))

epa_21 <- master_21 %>%
  group_by(posteam) %>%
  summarise(off_epa_21 = mean(epa, na.rm = TRUE)) %>%
  left_join(def_21, by = c("posteam" = "defteam")) %>%
  rename(team = "posteam")

epa_comb_yrs <- merge(epa_20, epa_21, by = "team") %>%
  left_join(teams_colors_logos, by = c("team" = "team_abbr")) %>%
  mutate(team_color = case_when(team %in% "LA" ~ "#003594",
                                team %in% "LAC" ~ "#0080c6",
                                TRUE ~ as.character(team_color)),
         team_color2 = case_when(team %in% "LA" ~ "#ffa300",
                                 team %in% "LAC" ~ "#ffc20e",
                                 TRUE ~ as.character(team_color2)))

## Swapping primary and alternate colours for some teams. This will make for easier reading in the plot.

f <- function(data, rows, cols) {
  data[rows, cols] <- rev(data[rows, cols])
  data
}

epa_comb_yrs <- f(epa_comb_yrs, 10, c("team_color", "team_color2")) # DEN.
epa_comb_yrs <- f(epa_comb_yrs, 22, c("team_color", "team_color2")) # NE.
epa_comb_yrs <- f(epa_comb_yrs, 28, c("team_color", "team_color2")) # SEA.

### PLOT ###

ggplot(epa_comb_yrs) +
  geom_link(aes(x = off_epa_20, xend = off_epa_21, y = def_epa_20,
                yend = def_epa_21, colour = team_color, alpha = stat(index),
                size = stat(index)), show.legend = FALSE) +
  scale_colour_identity() +
  scale_y_reverse() +
  geom_nfl_logos(aes(x = off_epa_21, y = def_epa_21, team_abbr = team),
                 width = 0.04, alpha = 0.8) +
  theme_fivethirtyeight() +
  labs(title = "*Team Progression/Regression From <span style = 'color: red;'>2020</span> to <span style = 'color: red;'>2021</span>*",
       subtitle = "Tapered end represents 2020 | Logo represents 2021",
       x = "EPA per Play",
       y = "EPA Allowed per Play",
       caption = "**Data:** nflfastR | **Plot:** @BenHorsley89") +
  theme(plot.title = element_markdown(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.caption = element_markdown(size = 8),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

# Should your segments look jagged at first, select the AGG graphics device in Tools -> Global Options -> General -> Graphics -> Backend.

ggsave("NFL_Team_Tiers.png", width = 10, height = 10 / 1.7, dpi = 300)
