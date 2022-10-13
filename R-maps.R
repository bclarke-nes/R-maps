## ----r------------------------------------------------------------------------
library(pacman)
p_load(tidyverse, geojsonio, broom, viridis, mapproj)
knitr::opts_chunk$set(echo = T, warning = F, message = F, results = 'asis', fig.width = 7, fig.height = 10)


## ----r------------------------------------------------------------------------
knitr::purl("R-maps.qmd")


## ----r------------------------------------------------------------------------
values_data <- read_csv("data/national_values.csv")

values_data %>% head() %>% knitr::kable()


## ----r------------------------------------------------------------------------
values_data %>%
  ggplot(aes(x=reorder(LAD13NM, -value), y=value, label=LAD13NM)) +
  geom_col() +
  coord_flip() +
  xlab("Local Authority District")


## ----r------------------------------------------------------------------------
library(pacman)
p_load(tidyverse, geojsonio, broom, viridis, mapproj)


## ----r------------------------------------------------------------------------
Scot_LAD <- geojson_read("data/topo_lad.json", what="sp")


## ----r------------------------------------------------------------------------
# make lookup to get human-readable regions
region_lookup <- tibble(id=1:length(Scot_LAD@data[["id"]]), id_json = Scot_LAD@data[["id"]], LAD13NM = Scot_LAD@data[["LAD13NM"]])


## ----r------------------------------------------------------------------------
# transform the shape file into a tidy tibble
Scot_LAD <- Scot_LAD %>%
  tidy() %>%
  mutate(id = as.numeric(id)) %>%
  left_join(region_lookup, on=id, keep=F) 


## ----r------------------------------------------------------------------------
# then join values to the shape file
Scot_LAD <- Scot_LAD %>%
  left_join(values_data, on=LAD13NM, keep=F)


## ----r------------------------------------------------------------------------

Scot_LAD %>%
  ggplot() +
  scale_fill_viridis() +
  geom_polygon(aes( x = long, y = lat, group=group, fill=value)) +
  theme_void() +
  coord_map()



## ----r------------------------------------------------------------------------
Scot_LAD %>%
  filter(LAD13NM == "Fife") %>%
  ggplot() +
  scale_fill_viridis() +
  geom_polygon(aes( x = long, y = lat, group = group, fill=value)) +
  theme_void() +
  coord_map()



## ----r------------------------------------------------------------------------
centroids <- Scot_LAD %>%
  group_by(LAD13NM) %>%
  summarise(mean_long = mean(long), mean_lat=mean(lat))

Scot_LAD %>%
  ggplot() +
  scale_fill_viridis() +
  geom_polygon(aes( x = long, y = lat, group = group, fill=value)) +
  geom_label(data=centroids, aes(x=mean_long, y=mean_lat, label=LAD13NM), size=3) +
  theme_void() +
  coord_map() +
  labs(id)


## ----r------------------------------------------------------------------------
fife_LAD <- geojson_read("data/topo_S12000015.json", what="sp")

fife_region_lookup <- tibble(id=1:length(fife_LAD@data[["id"]]), id_json = fife_LAD@data[["id"]], DZ_NAME = fife_LAD@data[["DZ_NAME"]])

fife_values_data <- read_csv("data/fife_values.csv") %>%
  mutate(id = as.numeric(id))

fife_LAD <- fife_LAD %>%
  tidy() %>%
  mutate(id = as.numeric(id)) %>%
  left_join(fife_values_data, on=id, keep=F) 

fife_LAD %>%
  ggplot() +
  scale_fill_viridis() +
  geom_polygon(aes( x = long, y = lat, group=group, fill=value)) +
  theme_void() +
  coord_map() +
  ggtitle("Fife map")


