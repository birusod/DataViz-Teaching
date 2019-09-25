library(datasauRus)
library(tidyverse)
library(gganimate)
library(extrafont)

extrafont::loadfonts()

theme_set(theme_bw(base_size = 13, base_family = "Montserrat"))

cols <- c("#FF6F00", "#C71000", "#008EA0", "#2B7C58", "#8A4198", 
          "#DC7C6B", "#20675C", "#FF7C9E", "#7D7B4C", "#EDAD0D", 
          "#2DB9A5", "#838587", "#0E00A5")

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
      mean(x)  = {formatC(mean_x, digits = 3, format = "f")}
      mean(x)  = {round(mean_x, 2)}
      sd(x)    = {round(sd_x, 2)}
      
      mean(y)  = {round(mean_y, 2)}
      sd(y)    = {round(sd_y, 2)}
      
      cor(x,y) = {round(cor, 2)}
    ')
  ) %>% 
  ungroup() %>% 
  mutate(dataset = fct_reorder(dataset, id)) %>% 
  ggplot(aes(x = x, y = y,
             color = dataset, 
             group = 1)) +
    geom_smooth(method = "lm", se = F, color = "grey80") +
    geom_point(size = 3) +
    geom_text(aes(x = 95, y = 100, label = label),
              family = "Roboto Mono", color = "grey80", size = 6, 
              hjust = 0, vjust = 1) +
    scale_x_continuous(limits = c(NA, 125)) +
    scale_color_manual(values = cols, guide = F) +
    labs(x = NULL, y = NULL, 
         title = "The Datasaurus Dozen",
         subtitle = "Different datasets – identical summary stats",
         caption = "\nIdea by Alberto Cairo, Justin Matejka & George Fitzmaurice\nVisualization by Tom Westlake & Cédric Scherer") +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 46, face = "bold", hjust = 0.5, 
                                    margin = margin(b = 2)),
          plot.subtitle = element_text(size = 18, color = "grey60", hjust = 0.5),
          plot.caption = element_text(size = 12, color = "grey60", hjust = 1)) +
    transition_states(dataset, 10, 3) + 
    ease_aes('cubic-in-out')

animate(sauR, nframes = 200, fps = 10, detail = 5, width = 750, height = 700, 
        renderer = gifski_renderer(here::here("Datasaurus", "datasauRus.gif"))) 
