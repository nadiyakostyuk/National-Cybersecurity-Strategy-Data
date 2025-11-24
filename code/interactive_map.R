#install.packages("countrycode")
#install.packages("lwgeom")
library(readr)
library(dplyr)
library(stringr)
library(countrycode)
library(sf)
library(rnaturalearth)
library(leaflet)
library(htmlwidgets)
library(lwgeom)
library(countrycode)


# https://r-graph-gallery.com/183-choropleth-map-with-leaflet.html


csv_path <- "inaugural_cybstrategies_2000_2024.csv"

cyber_strat_data <- read_csv(csv_path, show_col_types = FALSE) %>%
  mutate(
    Countries = Countries %>% as.character() %>% str_squish(),
    Countries = countrycode(Countries, "iso3c", "country.name", warn = FALSE),
    strat_bin = case_when(
      total_strategies == 0 ~ "0",
      total_strategies == 1 ~ "1",
      total_strategies == 2 ~ "2",
      total_strategies == 3 ~ "3",
      total_strategies >= 4 ~ "4+",
      TRUE ~ NA_character_
    ),
    strat_bin = factor(strat_bin, levels = c("0","1","2","3","4+"))
  )


cyber_strat_data <- cyber_strat_data %>%
  mutate(iso3 = countrycode(Countries, "country.name", "iso3c", warn = FALSE))




world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
  mutate(iso3 = iso_a3)


world <- world %>%
  mutate(
    iso3 = case_when(
      name == "France" ~ "FRA",
      name == "Norway" ~ "NOR",
      TRUE ~ iso3
    )
  )


world_join <- world %>%
  left_join(cyber_strat_data %>% dplyr::select(iso3, strat_bin), by = "iso3")

pal <- colorFactor(
  palette = "Reds",  
  domain = levels(cyber_strat_data$strat_bin),
  na.color = "#f2f2f2"  
)




# Leaflet map with OpenStreetMap tiles
m <- leaflet(world_join, options = leafletOptions(worldCopyJump = FALSE)) %>%
  addTiles() %>%
  addPolygons(
    fillColor   = ~pal(strat_bin),
    color       = "black",
    weight      = 0.5,
    fillOpacity = 0.9,
    label = ~paste0(name, ": ", ifelse(is.na(strat_bin), "No data", as.character(strat_bin))),
    highlightOptions = highlightOptions(weight = 2, bringToFront = TRUE)
  ) %>%
  addLegend(
    "bottomright",
    pal     = pal,
    values  = levels(cyber_strat_data$strat_bin),
    title   = "Number of Strategies",
    opacity = 0.85
  )
m


saveWidget(m, "world_strats_map.html", selfcontained = TRUE)

