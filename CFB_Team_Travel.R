library(tidyverse)
library(cfbfastR)
library(cfbplotR)
library(geosphere)
library(ggtext)
library(ggthemes)

teams <- cfbd_team_info(only_fbs = TRUE, year = 2021)
venues <- cfbd_venues()

conf <- c("ACC", "Big 12", "Big Ten", "SEC", "Pac-12", "FBS Independents")

venue_locs <- venues %>%
  rowwise() %>%
  mutate(venue_lat = location[[1]],
         venue_long = ifelse(length(location) == 2, location[[2]],
                                  NA)) %>%
  select(name, venue_lat, venue_long) %>%
  ungroup()

year <- c(2014:2021)
games <- data.frame(matrix(nrow = 0, ncol = 28))
colnames(games) <- colnames(cfbd_game_info(year = 2021))

for (i in year){
  
  x <- cfbd_game_info(year = i, season_type = "regular")
  games <- rbind(games, x)
  
}

home_teams <- games %>%
  select(game_id, season, week, venue, home_team, home_conference) %>%
  rename(school = home_team, conference = home_conference)

away_teams <- games %>%
  select(game_id, season, week, venue, away_team, away_conference) %>%
  rename(school = away_team, conference = away_conference)

bind_games <- bind_rows(home_teams, away_teams) %>%
  arrange(season, week, game_id) %>%
  filter(conference %in% conf)

team_locs <- teams %>%
  select(school, conference, latitude, longitude) %>%
  filter(school %in% bind_games$school) %>%
  select(-conference) %>%
  rename(school_lat = latitude,
         school_long = longitude)

venue_distinct <- venue_locs %>%
  filter(name %in% bind_games$venue) %>%
  subset(!duplicated(name))

game_locs <- bind_games %>%
  left_join(team_locs, by = "school") %>%
  left_join(venue_distinct, by = c("venue" = "name")) %>%
  relocate(venue_lat, .after = venue) %>%
  relocate(venue_long, .before = school) %>%
  mutate(venue_lat = case_when(venue == "Croke Park Stadium" ~ 53.45341,
                               TRUE ~ as.numeric(venue_lat)),
         venue_long = case_when(venue == "Croke Park Stadium" ~ -6.22368,
                               TRUE ~ as.numeric(venue_lat))) %>%
  mutate(travel_dist = ( distHaversine(cbind(venue_long, venue_lat),
                                     cbind(school_long, school_lat)) ) /
           1000) %>%
  mutate(travel_dist_miles = travel_dist * 0.62137)

travel <- game_locs %>%
  group_by(season, conference, school) %>%
  summarise(total_dist = sum(travel_dist),
            total_dist_miles = sum(travel_dist_miles)) %>%
  filter(total_dist == max(total_dist))

set.seed(70)
travel$jit <- runif(nrow(travel), -0.15, 0.15)

options(scipen = 999)

p <- ggplot(travel, aes(x = season, y = total_dist_miles)) +
  theme_fivethirtyeight() +
  scale_x_continuous(breaks = c(2014:2021)) +
  scale_y_continuous(limits = c(50000, 100000)) +
  geom_cfb_logos(aes(x = season + jit, team = school),
                 width = 0.03, alpha = 0.8) +
  labs(title = "*Which Schools <span style = 'color: red;'>Travel</span> the Furthest by <span style = 'color: red;'>Conference</span>?*",
       subtitle = "*CFP era | P5 + Independents | distHarvesine method | Regular season*",
       x = "Season",
       y = "Total Distance Travelled (mi)",
       caption = "**Data:** cfbfastR | **Plot:** @BenHorsley89") +
  theme(plot.title = element_markdown(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_markdown(size = 10, hjust = 0.5),
        axis.title = element_text(face = "bold"),
        plot.caption = element_markdown(size = 8),
        panel.grid.major.x = element_blank())

get_png <- function(filename) {
  
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
  
}

img <- get_png("Logo.png")

p +
  annotation_custom(img, xmin = 2014, xmax = 2015, ymin = 36000,
                    ymax = 46000) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1, 1, 1.4, 1), "lines"))

ggsave("CFB_Travel.png", width = 10, height = 10 / 1.7, dpi = 300)
