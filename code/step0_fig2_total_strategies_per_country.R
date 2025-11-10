##########################
### Plots             ####
## Nadiya Kostyuk     ####
## April 29, 2025     ####
# Analysis ran using  ####
# R version 4.4.1     ####
##########################
rm(list=ls())


## Install & load packages (all at once)
list.of.packages <- c("dplyr","ggplot2", 'rnaturalearth', 'rnaturalearthdata')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


setwd("")
data= read.csv('data/inaugural_cybstrategies_2000_2024.csv', header = TRUE)


##########################
# Figure 2
##########################


# Load world map with ISO3 codes
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")

# Merge strategy data with world map data
world_strategies <- world %>%
  left_join(data, by = c("iso_a3" = "Countries")) %>%
  mutate(total_strategies = ifelse(is.na(total_strategies), 0, total_strategies),
         strategy_bin = cut(
           total_strategies,
           breaks = c(-1, 0, 1, 2, Inf),
           labels = c("0", "1", "2", "3+")
         ))

# Custom color palette: light grey to black
strategy_colors <- c("0" = "#d9d9d9", "1" = "#969696", "2" = "#525252", "3+" = "#000000")

ggplot(data = world_strategies) +
  geom_sf(aes(fill = strategy_bin), color = "white", linetype = "dotted", size = 0.2) +
  scale_fill_manual(values = strategy_colors, name = "Number of \n Strategies") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0, 0, 0, 0),  # remove plot margins
    legend.position = "right"
  ) +
  coord_sf(expand = FALSE, clip = "on")
#ggsave("output/fig2_strategies_per_country.pdf", width = 8, height = 6)

