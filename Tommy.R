library(tidyverse)
library(ggimage)
library(ggtext)
library(ggthemes)
library(nflfastR)
library(nflplotR)

future::plan("multisession")
pbp <- load_pbp(2001:2021)

master <- pbp %>%
  filter(qb_dropback == 1 & penalty == 0)

qbs <- master %>%
  group_by(passer_player_name, posteam, season) %>%
  summarise(n_dropbacks = sum(qb_dropback),
            epa = mean(epa, na.rm = TRUE)) %>%
  filter(n_dropbacks >= 100)

tommy <- qbs %>%
  filter(passer_player_name == "T.Brady")

rest <- qbs %>%
  filter(passer_player_name != "T.Brady")

tommy_sb_wins <- c(2001, 2003, 2004, 2014, 2016, 2018, 2020)

sb_trophy <- "https://upload.wikimedia.org/wikipedia/commons/b/b5/Lombardi_Trophy.png"

tommy <- tommy %>%
  mutate(sb_trophy = ifelse(season %in% tommy_sb_wins, sb_trophy, NA))

ggplot(data = rest, aes(x = season, y = epa)) +
  geom_jitter(fill = "#013369", color = "#D50A0A", alpha = 0.3, shape = 21,
              width = 0.05, size = 4) +
  geom_nfl_logos(data = tommy, aes(team_abbr = posteam), width = 0.06,
                 alpha = 0.8) +
  geom_image(data = tommy, aes(x = season + 0.4, y = epa + 0.085,
                               image = sb_trophy), asp = 16/9, size = 0.018) +
  scale_x_continuous(breaks = c(2001:2021)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, 0.2)) +
  theme_fivethirtyeight() +
  labs(title = "*The <span style = 'color: red;'>Tom Brady</span> Years*",
       subtitle = "*Minimum 100 dropbacks per season | Points represent rest of league QBs | Logos represent Brady*",
       x = "Season",
       y = "Expected Points Added Per Play",
       caption = "**Data:** nflfastR | **Plot:** @BenHorsley89") +
  theme(plot.title = element_markdown(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_markdown(size = 10, hjust = 0.5),
        plot.caption = element_markdown(size = 8),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  annotate("segment", x = 2007 + 0.7, xend = 2008 - 0.1, y = -0.49,
           yend = -0.51) +
  geom_text(aes(x = 2007 + 0.5, y = -0.45, label = "ACL\nInjury"),
            size = 2.5) +
  annotate("segment", x = 2018 - 0.6, xend = 2017 + 0.1, y = 0.32,
           yend = 0.3) +
  geom_text(aes(x = 2018 - 0.4, y = 0.34, label = "Turned\n40"), size = 2.5)

ggsave("Tommy.png", width = 10, height = 10 / 1.7, dpi = 500)
