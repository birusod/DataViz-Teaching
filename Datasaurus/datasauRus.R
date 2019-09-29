## Idea and sources
## DataSaurus - Blog Post by Alberto Cairo: thefunctionalart.com/2016/08/download-datasaurus-never-trust-summary.html
## Datasaurus Dozen - Paper by Matejka & Fitzmaurice: autodeskresearch.com/publications/samestats
## Data for R - package by Steph Locke: itsalocke.com/datasaurus/ and github.com/lockedata/datasauRus
## gganimate code by Tom Westlake: r-mageddon.netlify.com/post/reanimating-the-datasaurus
## Based on "Anscombe's Quartet": en.wikipedia.org/wiki/Anscombe%27s_quartet

library(datasauRus)
library(tidyverse)
library(gganimate)
library(extrafont)

extrafont::loadfonts()

theme_set(theme_bw(base_size = 13, base_family = "Montserrat"))

cols <- c("#FF6F00", "#C71000", "#008EA0", "#2B7C58", 
          "#8A4198", "#DC7C6B", "#20675C", "#FF7C9E", 
          "#7D7B4C", "#EDAD0D", "#2DB9A5", "#838587", "#0E00A5")

sauR <- 
  datasaurus_dozen %>% 
  group_by(dataset) %>% 
  mutate(
    id = group_indices(),
    mean_x = mean(x),
    mean_y = mean(y),
    sd_x = sd(x),
    sd_y = sd(y),
    cor = cor(x, y, method = "pearson"),
    label = glue::glue('
      mean(x)  = {formatC(mean_x, digits = 5, format = "f")}
      sd(x)    = {formatC(sd_x, digits = 5, format = "f")}
      
      mean(y)  = {formatC(mean_y, digits = 5, format = "f")}
      sd(y)    = {formatC(sd_y, digits = 5, format = "f")}
      
      cor(x,y) = {formatC(cor, digits = 5, format = "f")}
    '),
    label_short = glue::glue('
      mean(x)  = {floor(mean_x*100)/100}
      sd(x)    = {floor((sd_x + 0.00001)*100)/100}
      
      mean(y)  = {floor(mean_y*100)/100}
      sd(y)    = {floor((sd_y + 0.00001)*100)/100}
      
      cor(x,y) = {ceiling(cor*100)/100}
    '),
  ) %>% 
  ungroup() %>% 
  mutate(dataset = fct_reorder(dataset, id)) %>% 
  ggplot(aes(x = x, y = y,
             color = dataset, 
             group = 1)) +
    geom_smooth(method = "lm", se = F, color = "grey85") +
    geom_point(size = 3, alpha = 0.8) +
    geom_text(aes(x = 92, y = 100, label = label),
              family = "Roboto Mono", color = "grey85", size = 6, 
              hjust = 0, vjust = 1) +
    geom_text(aes(x = 92, y = 100, label = label_short),
              family = "Roboto Mono", color = "grey70", size = 6, 
              hjust = 0, vjust = 1) +
    scale_x_continuous(limits = c(NA, 125)) +
    scale_color_manual(values = cols, guide = F) +
    labs(x = NULL, y = NULL, 
         title = "The Datasaurus Dozen",
         subtitle = "Different datasets – nigh-identical summary statistics",
         caption = "\nIdea by Alberto Cairo, Justin Matejka & George Fitzmaurice\nVisualization by Tom Westlake & Cédric Scherer") +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 46, face = "bold", hjust = 0.5, 
                                    margin = margin(b = 2)),
          plot.subtitle = element_text(size = 18, color = "grey55", hjust = 0.5),
          plot.caption = element_text(size = 12, color = "grey55", hjust = 1)) +
    transition_states(dataset, 10, 3) + 
    ease_aes('cubic-in-out')

animate(sauR, nframes = 200, fps = 10, detail = 5, width = 750, height = 700, 
        renderer = gifski_renderer(here::here("Datasaurus", "datasauRus.gif"))) 
