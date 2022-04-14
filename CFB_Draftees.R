library(tidyverse)
library(cfbfastR)
library(cfbplotR)
library(ggforce)
library(ggimage)
library(ggtext)
library(ggthemes)

year <- c(2011:2021)
picks <- data.frame(matrix(nrow = 0, ncol = 23))
colnames(picks) <- colnames(cfbd_draft_picks(year = 2021))

for (i in year){
  
  x <- cfbd_draft_picks(year = i)
  picks <- rbind(picks, x)
  
}

team_logos <- read.csv("College_Team_Info.csv", fileEncoding = "UTF-8-BOM") %>%
  select(team, logo)

first_round <- picks %>%
  group_by(college_team) %>%
  summarise(n_first = sum(round == 1)) %>%
  arrange(-n_first) %>%
  head(28) %>%
  left_join(team_logos, by = c("college_team" = "team")) %>%
  mutate(college_team = reorder(college_team, n_first))

ggplot(first_round) +
  geom_link(aes(x = college_team, xend = college_team, y = 0, yend = n_first,
                colour = college_team, alpha = stat(index)),
            show.legend = FALSE, size = 4, n = 500) +
  scale_colour_cfb() +
  geom_image(aes(x = college_team, y = n_first, image = logo), asp = 1.7,
             size = 0.025) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 35, 5)) +
  theme_fivethirtyeight() +
  labs(title = "*<span style = 'color: #9E1B32;'>Bama's</span> Production Line of First-Round Talent*",
       subtitle = "*Total number of first-round draft selections between 2011-2021 | Schools with at least 5 draftees*",
       y = "Draftees (n)",
       caption = "**Data:** cfbfastR | **Plot:** @BenHorsley89") +
  theme(plot.title = element_markdown(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_markdown(size = 10, hjust = 0.5),
        plot.caption = element_markdown(size = 8),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave("CFB_Draftees.png", width = 10, height = 10 / 1.7, dpi = 300)
