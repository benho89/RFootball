library(tidyverse)
library(cfbfastR)
library(cfbplotR)
library(gganimate)
library(ggtext)
library(ggthemes)
library(glue)

year <- c(2011:2021)
picks <- data.frame(matrix(nrow = 0, ncol = 23))
colnames(picks) <- colnames(cfbd_draft_picks(year = 2021))

for (i in year){
  
  x <- cfbd_draft_picks(year = i)
  picks <- rbind(picks, x)
  
}

conf <- "SEC"
team_info <- cfbd_team_info(conference = conf)

all_draftees <- picks %>%
  group_by(college_team, year) %>%
  summarise(n = n())

conf_draftees <- all_draftees %>%
  filter(college_team %in% team_info$school)

if (conf == "ACC") {
  colour <- "#013CA6"
} else if (conf == "B1G") {
  colour <- "#0088CE"
} else if (conf == "B12") {
  colour <- "#EF483E"
} else if (conf == "PAC") {
  colour <- "#004B91"
} else {colour <- "#004B8D"}

p <- ggplot(conf_draftees, aes(x = college_team, y = n)) +
  geom_col(aes(fill = college_team, colour = college_team), width = 0.5,
           alpha = 0.8) +
  scale_fill_cfb() +
  scale_colour_cfb(alt_colors = conf_draftees$college_team) +
  theme_fivethirtyeight() +
  scale_x_cfb(size = 20) +
  theme_x_cfb() +
  labs(title = glue("*Draftees per School: <span style = 'color: {colour};'>{conf}</span>*"),
       y = "Draftees (n)",
       caption = "**Data:** cfbfastR | **Plot:** @BenHorsley89") +
  theme(plot.title = element_markdown(face = "bold", size = 14),
        plot.caption = element_markdown(size = 6),
        axis.title.y = element_text(face = "bold", size = 8),
        axis.text.y = element_text(size = 6),
        panel.grid.major.x = element_blank())

anim <- p +
  transition_time(year) +
  labs(subtitle = "*Year: {frame_time}*") +
  theme(plot.subtitle = element_markdown(size = 10))

if(max(conf_draftees$n) %% 2 == 0) {
  
  anim <- anim +
    scale_y_continuous(breaks = seq(0, max(conf_draftees$n), 2))

} else {
    
    anim <- anim +
      scale_y_continuous(limits = c(0, max(conf_draftees$n) + 1),
                         breaks = seq(0, max(conf_draftees$n) + 1, 2))
    
}

animate(anim, height = 800, width = 1300, res = 260)
anim_save(paste0("CFB_Draftees_", conf, ".gif"))
