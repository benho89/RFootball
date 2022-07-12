library(tidyverse)
library(cfbfastR)
library(cfbplotR)
library(gganimate)
library(ggbump)
library(ggtext)
library(glue)
library(showtext)

font_add_google("Anton", "Anton")
showtext_auto()

fbs <- cfbd_team_info(only_fbs = TRUE)

pbp <- data.frame()
seasons <- 2014:2021

progressr::with_progress({
  
  pbp <- load_cfb_pbp(seasons)
  
})

master <- pbp %>%
  filter(is.na(penalty_detail)) %>%
  filter(rush == 1 | pass == 1) %>%
  filter(pos_team %in% fbs$school & def_pos_team %in% fbs$school)

conf <- "SEC"

off_epa <- master %>%
  group_by(year, pos_team, offense_conference) %>%
  summarise(epa = mean(EPA, na.rm = TRUE)) %>%
  filter(offense_conference == conf) %>%
  group_by(year) %>%
  arrange(desc(epa), .by_group = TRUE) %>%
  mutate(rank = seq(1, length(year), 1))

off_2014 <- off_epa %>% filter(year == 2014)
off_2021 <- off_epa %>% filter(year == 2021)

off_conf_logo <- data.frame(x = 2017.5, y = (max(off_epa$rank) / 2) + 0.5,
                        conf = conf)


def_epa <- master %>%
  group_by(year, def_pos_team, defense_conference) %>%
  summarise(epa = mean(def_EPA, na.rm = TRUE)) %>%
  filter(defense_conference == conf) %>%
  group_by(year) %>%
  arrange(desc(epa), .by_group = TRUE) %>%
  mutate(rank = seq(1, length(year), 1))

def_2014 <- def_epa %>% filter(year == 2014)
def_2021 <- def_epa %>% filter(year == 2021)

def_conf_logo <- data.frame(x = 2017.5, y = (max(def_epa$rank) / 2) + 0.5,
                            conf = conf)


ggplot(off_epa, aes(x = year, rank)) +
  geom_cfb_logos(data = off_conf_logo, aes(x = x, y = y, team = conf),
                 width = 0.48, alpha = 0.08) +
  geom_bump(aes(colour = pos_team), size = 1) +
  geom_point(aes(colour = pos_team), size = 3) +
  scale_color_cfb() +
  scale_x_continuous(breaks = c(2014:2021)) +
  scale_y_reverse() +
  geom_cfb_logos(data = off_2014, aes(x = year - 0.2, y = rank,
                                      team = pos_team), width = 0.025) +
  geom_text(data = off_2014, aes(x = year - 0.4, y = rank, label = rank),
            family = "Anton", size = 8) +
  geom_cfb_logos(data = off_2021, aes(x = year + 0.2, y = rank,
                                      team = pos_team), width = 0.025) +
  geom_text(data = off_2021, aes(x = year + 0.4, y = rank, label = rank),
            family = "Anton", size = 8) +
  ggthemes::theme_fivethirtyeight() +
  labs(title = glue("*Within-Conference Offensive EPA Rank Across CFP Era: <span style = 'color: darkred;'>{conf}</span>*"),
       x = "Season",
       caption = "Data: cfbfastR | Plot: @BenHorsley89") +
  theme(plot.title = element_markdown(face = "bold", size = 36, hjust = 0.5),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_blank(),
        plot.caption = element_text(size = 18),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Anton"))

ggsave(paste0(conf, "_Rank_Offense.png"), width = 10, height = 10 / 1.7,
       dpi = 300)


ggplot(def_epa, aes(x = year, rank)) +
  geom_cfb_logos(data = def_conf_logo, aes(x = x, y = y, team = conf),
                 width = 0.48, alpha = 0.08) +
  geom_bump(aes(colour = def_pos_team), size = 1) +
  geom_point(aes(colour = def_pos_team), size = 3) +
  scale_color_cfb() +
  scale_x_continuous(breaks = c(2014:2021)) +
  scale_y_reverse() +
  geom_cfb_logos(data = def_2014, aes(x = year - 0.2, y = rank,
                                      team = def_pos_team), width = 0.025) +
  geom_text(data = def_2014, aes(x = year - 0.4, y = rank, label = rank),
            family = "Anton", size = 8) +
  geom_cfb_logos(data = def_2021, aes(x = year + 0.2, y = rank,
                                       team = def_pos_team), width = 0.025) +
  geom_text(data = def_2021, aes(x = year + 0.4, y = rank, label = rank),
            family = "Anton", size = 8) +
  ggthemes::theme_fivethirtyeight() +
  labs(title = glue("*Within-Conference Defensive EPA Rank Across CFP Era: <span style = 'color: darkred;'>{conf}</span>*"),
       x = "Season",
       caption = "Data: cfbfastR | Plot: @BenHorsley89") +
  theme(plot.title = element_markdown(face = "bold", size = 36, hjust = 0.5),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_blank(),
        plot.caption = element_text(size = 18),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Anton"))

ggsave(paste0(conf, "_Rank_Defense.png"), width = 10, height = 10 / 1.7,
       dpi = 300)
