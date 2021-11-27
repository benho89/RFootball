library(tidyverse)
library(ggtext)
library(ggthemes)
library(nflfastR)
library(nflplotR)
library(patchwork)

pbp <- load_pbp(2021)

# Top six rushers through Week 11.

rbs <- c("J.Taylor", "D.Henry", "N.Chubb", "M.Ingram", "J.Mixon", "D.Cook")

master <- pbp %>%
  filter(rush == 1) %>%
  filter(!is.na(epa) & !is.na(run_location) & penalty == 0)

rushes <- master %>%
  select(rusher_player_name, posteam, run_location, run_gap, epa) %>%
  unite(run_type, run_location, run_gap, sep = " - ") %>%
  mutate(run_type = gsub(" - NA", "", run_type),
         run_type = str_to_title(run_type),
         run_type = factor(run_type,
                           levels = c("Left - End", "Left - Tackle",
                                      "Left - Guard", "Middle",
                                      "Right - Guard", "Right - Tackle",
                                      "Right - End")))

# Renaming Joseph Justin Taylor from NE so as to not get mixed up with Jonathan Taylor from IND.

rushes <- within(rushes,
                 rusher_player_name[rusher_player_name == "J.Taylor" &
                                      posteam == "NE"] <- "J.J.Taylor")

# Using case_when() below as I only want to include the NO logo for Mark Ingram on the plot.

rushes_filt <- rushes %>%
  filter(rusher_player_name %in% rbs) %>%
  mutate(posteam = case_when(posteam == "HOU" ~ "NO",
                             TRUE ~ as.character(posteam))) %>%
  group_by(rusher_player_name, posteam, run_type) %>%
  summarise(epa = mean(epa))

rushes_taylor <- rushes_filt %>%
  filter(rusher_player_name == "J.Taylor")

rushes_henry <- rushes_filt %>%
  filter(rusher_player_name == "D.Henry")

rushes_chubb <- rushes_filt %>%
  filter(rusher_player_name == "N.Chubb")

rushes_ingram <- rushes_filt %>%
  filter(rusher_player_name == "M.Ingram")

rushes_mixon <- rushes_filt %>%
  filter(rusher_player_name == "J.Mixon")

rushes_cook <- rushes_filt %>%
  filter(rusher_player_name == "D.Cook")

### PLOT ###

plot_theme <- list(
  scale_y_continuous(limits = c(-0.5, 0.7)),
  theme_fivethirtyeight(),
  theme(plot.title = element_markdown(size = 14),
        plot.subtitle = element_text(face = "bold", size = 12),
        plot.caption = element_text(size = 8),
        axis.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank()))

p1 <- ggplot(rushes %>% filter(rusher_player_name != "J.Taylor"),
             aes(x = run_type, y = epa)) +
  geom_boxplot(fill = "#013369", color = "#D50A0A", alpha = 0.4,
               outlier.shape = NA, notch = TRUE, coef = 0) +
  geom_nfl_logos(rushes_taylor, mapping = aes(x = run_type, y = epa,
                                  team_abbr = posteam), width = 0.06) +
  labs(title = "*EPA per Run <span style = 'color: red;'>Location</span> and <span style = 'color: red;'>Gap</span>*",
       subtitle = "J Taylor") +
  plot_theme

p2 <- ggplot(rushes %>% filter(rusher_player_name != "D.Henry"),
                               aes(x = run_type, y = epa)) +
  geom_boxplot(fill = "#013369", color = "#D50A0A", alpha = 0.4,
               outlier.shape = NA, notch = TRUE, coef = 0) +
  geom_nfl_logos(rushes_henry, mapping = aes(x = run_type, y = epa,
                                              team_abbr = posteam),
                 width = 0.06) +
  labs(subtitle = "D Henry") +
  plot_theme

p3 <- ggplot(rushes %>% filter(rusher_player_name != "N.Chubb"),
             aes(x = run_type, y = epa)) +
  geom_boxplot(fill = "#013369", color = "#D50A0A", alpha = 0.4,
               outlier.shape = NA, notch = TRUE, coef = 0) +
  geom_nfl_logos(rushes_chubb, mapping = aes(x = run_type, y = epa,
                                             team_abbr = posteam),
                 width = 0.06) +
  labs(subtitle = "N Chubb") +
  plot_theme

p4 <- ggplot(rushes %>% filter(rusher_player_name != "M.Ingram"),
             aes(x = run_type, y = epa)) +
  geom_boxplot(fill = "#013369", color = "#D50A0A", alpha = 0.4,
               outlier.shape = NA, notch = TRUE, coef = 0) +
  geom_nfl_logos(rushes_ingram, mapping = aes(x = run_type, y = epa,
                                               team_abbr = posteam),
                 width = 0.06) +
  labs(subtitle = "M Ingram") +
  plot_theme

p5 <- ggplot(rushes  %>% filter(rusher_player_name != "J.Mixon"),
             aes(x = run_type, y = epa)) +
  geom_boxplot(fill = "#013369", color = "#D50A0A", alpha = 0.4,
               outlier.shape = NA, notch = TRUE, coef = 0) +
  geom_nfl_logos(rushes_mixon, mapping = aes(x = run_type, y = epa,
                                             team_abbr = posteam),
                 width = 0.06) +
  labs(subtitle = "J Mixon") +
  plot_theme

p6 <- ggplot(rushes %>% filter(rusher_player_name != "D.Cook"),
             aes(x = run_type, y = epa)) +
  geom_boxplot(fill = "#013369", color = "#D50A0A", alpha = 0.4,
               outlier.shape = NA, notch = TRUE, coef = 0) +
  geom_nfl_logos(rushes_cook, mapping = aes(x = run_type, y = epa,
                                             team_abbr = posteam),
                 width = 0.06) +
  labs(subtitle = "D Cook",
       caption = "Source data from @nflfastR | Plot by @BenHorsley89 | Inspired by @jacquietran") +
  plot_theme

(p1 + p2 + p3) / (p4 + p5 + p6)

ggsave("NFL_Run_Types_Plot.png", width = 12, height = 8, dpi = 300)
