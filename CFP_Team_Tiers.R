library(tidyverse)
library(cfbfastR)
library(ggthemes)
library(ggimage)

pbp_20 <- load_cfb_pbp(seasons = 2020)
pbp_21 <- load_cfb_pbp(seasons = 2021)

fbs_teams <- cfbd_team_info(only_fbs = TRUE)
top25 <- c("Georgia", "Alabama", "Michigan State", "Oregon", "Ohio State",
           "Cincinnati", "Michigan", "Oklahoma", "Wake Forest", "Notre Dame",
           "Oklahoma State", "Baylor", "Auburn", "Texas A&M", "BYU",
           "Ole Miss", "Mississippi State", "Kentucky", "NC State",
           "Minnesota", "Wisconsin", "Iowa", "Fresno State",
           "San Diego State", "Pittsburgh")

team_info <- read.csv("College_Team_Info.csv", fileEncoding = "UTF-8-BOM")

master_20 <- pbp_20 %>%
  filter(is.na(penalty_detail) & (pass == 1 | rush == 1)) %>%
  filter(pos_team %in% fbs_teams$school &
           def_pos_team %in% fbs_teams$school)

master_21 <- pbp_21 %>%
  filter(is.na(penalty_detail) & (pass == 1 | rush == 1)) %>%
  filter(pos_team %in% fbs_teams$school &
           def_pos_team %in% fbs_teams$school)

def_20 <- master_20 %>%
  group_by(def_pos_team) %>%
  summarise(def_epa_20 = mean(EPA, na.rm = TRUE))

epa_20 <- master_20 %>%
  group_by(pos_team) %>%
  summarise(off_epa_20 = mean(EPA, na.rm = TRUE)) %>%
  left_join(def_20, by = c("pos_team" = "def_pos_team")) %>%
  rename(team = "pos_team")

def_21 <- master_21 %>%
  group_by(def_pos_team) %>%
  summarise(def_epa_21 = mean(EPA, na.rm = TRUE))

epa_21 <- master_21 %>%
  group_by(pos_team) %>%
  summarise(off_epa_21 = mean(EPA, na.rm = TRUE)) %>%
  left_join(def_21, by = c("pos_team" = "def_pos_team")) %>%
  rename(team = "pos_team")

top25_epa <- merge(epa_20, epa_21, by = "team") %>%
  filter(team %in% top25) %>%
  left_join(team_info, by = "team")

## Swapping primary and alternate colours for some teams. This will make for easier reading in the plot.

f <- function(data, rows, cols) {
  data[rows, cols] <- rev(data[rows, cols])
  data
}

top25_epa <- f(top25_epa, 3, c("color", "alt_color")) # Baylor.
top25_epa <- f(top25_epa, 8, c("color", "alt_color")) # Iowa.

### PLOT ###

ggplot(top25_epa) +
  geom_link(aes(x = off_epa_20, xend = off_epa_21, y = def_epa_20,
                yend = def_epa_21, colour = color, alpha = stat(index),
                size = stat(index)), show.legend = FALSE) +
  scale_colour_identity() +
  scale_y_reverse() +
  geom_image(aes(x = off_epa_21, y = def_epa_21, image = logo),
             asp = 1.7, size = 0.04) +
  theme_fivethirtyeight() +
  labs(title = "*Evolution of the Current <span style = 'color: red;'>CFP Top 25</span> From 2020 to 2021*",
       subtitle = "Week 10 Rankings | Tapered end represents 2020 | Logo represents 2021",
       x = "EPA per Play",
       y = "EPA Allowed per Play",
       caption = "**Data:** cfbfastR | **Plot:** @BenHorsley89") +
  theme(plot.title = element_markdown(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.caption = element_markdown(size = 8),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

# Should your segments look jagged at first, select the AGG graphics device in Tools -> Global Options -> General -> Graphics -> Backend.

ggsave("CFP_Team_Tiers.png", width = 10, height = 10 / 1.7, dpi = 300)
