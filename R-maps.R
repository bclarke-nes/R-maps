## ----r------------------------------------------------------------------------
library(pacman)
p_load(tidyverse, geojsonio, broom, viridis, mapproj, sf, PostcodesioR, stringr)
knitr::opts_chunk$set(echo = T, warning = F, message = F, results = 'asis', fig.width = 7, fig.height = 7)


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
Scot_LAD <- geojson_read("data/topo_lad.json", what="sp")


## ----r------------------------------------------------------------------------
region_lookup <- tibble(id=1:length(Scot_LAD@data[["id"]]), id_json = Scot_LAD@data[["id"]], LAD13NM = Scot_LAD@data[["LAD13NM"]])


## ----r------------------------------------------------------------------------
Scot_LAD <- Scot_LAD %>%
  tidy() %>%
  mutate(id = as.numeric(id)) %>%
  left_join(region_lookup, on=id, keep=F) 


## ----r------------------------------------------------------------------------
Scot_LAD <- Scot_LAD %>%
  left_join(values_data, on=LAD13NM, keep=F)


## ----r------------------------------------------------------------------------
scot <- Scot_LAD %>%
  ggplot() +
  scale_fill_viridis() +
  geom_polygon(aes( x = long, y = lat, group=group, fill=value)) +
  theme_void() +
  coord_map()

scot


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



## ----r------------------------------------------------------------------------
dz_2011 <- geojson_read("data/SG_DataZoneBdry_2011/SG_DataZone_Bdry_2011.shp", what="sp")


## ----r------------------------------------------------------------------------
dz_2011_lookup <- tibble(id=1:length(dz_2011@data[["DataZone"]]), DataZone = dz_2011@data[["DataZone"]], dz_name = dz_2011@data[["Name"]])

dz_2011 <- dz_2011 %>%
  tidy()


## ----r------------------------------------------------------------------------
SIMD2020v2 <- read_csv("https://www.opendata.nhs.scot/dataset/78d41fa9-1a62-4f7b-9edb-3e8522a93378/resource/acade396-8430-4b34-895a-b3e757fa346e/download/simd2020v2_22062020.csv")

SIMD2020v2_dz <- dz_2011_lookup %>%
  left_join(SIMD2020v2, by="DataZone")


## ----r------------------------------------------------------------------------
dz_2011_joined <- dz_2011 %>%
  mutate(id = as.numeric(id)) %>%
  left_join(SIMD2020v2_dz, on=id, keep=F) 


## ----r------------------------------------------------------------------------
dz_2011_joined %>%
  filter(HB == "S08000020") %>%
  ggplot() +
  scale_fill_viridis() +
  geom_polygon(aes( x = long, y = lat, group=group, fill=SIMD2020V2Rank)) +
  theme_void() +
  ggtitle("Grampian SIMD2020v2 rank by 2011 data zone") +
  coord_sf(default_crs = sf::st_crs(4326))


## ----r------------------------------------------------------------------------
knitr::include_graphics("images/grampian_map.png")


## ----r------------------------------------------------------------------------
# very slow code to create Scottish postcode data
# codepo_gn <- read_sf("data/codepo_gb.gpkg") %>%
# filter(str_detect(Admin_district_code, "S"))
# saveRDS(codepo_gn,file="data/codepo_SCOT.RDS")
# might consider dl directly from https://api.os.uk/downloads/v1/products/CodePointOpen/downloads?area=GB&format=GeoPackage&redirect


## ----r------------------------------------------------------------------------
# read the Scottish postcode data from file
codepo_SCOT <- readRDS(file="data/codepo_SCOT.RDS")


## ----r------------------------------------------------------------------------
# Simple map of postcode centroids using geom_sf()
codepo_SCOT %>%
  filter(str_detect(Postcode, "KY14")) %>%
  ggplot() +
  geom_sf()


## ----r------------------------------------------------------------------------
codepo_SCOT %>%
  filter(str_detect(Postcode, "KY14 ")) %>%
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label=Postcode))


## ----r------------------------------------------------------------------------
# trying to overlay the postcodes on a regional map
Scot_LAD %>%
  filter(LAD13NM == "Fife") %>%
  ggplot() +
  scale_fill_viridis() +
  geom_polygon(aes( x = long, y = lat, group = group, fill=value)) +
  theme_void() +
  geom_sf(data=codepo_SCOT %>% filter(str_detect(Postcode, "KY14 ")), aes(label=Postcode)) +
  coord_sf(default_crs = sf::st_crs(4326))


## ----r------------------------------------------------------------------------
fife_LAD %>%
  ggplot() +
  scale_fill_viridis() +
  geom_polygon(aes( x = long, y = lat, group=group, fill=value)) +
  theme_void() +
  geom_sf(data=codepo_SCOT %>% filter(str_detect(Postcode, "KY14")), color="white") +
  coord_sf(default_crs = sf::st_crs(4326)) +
  ggtitle("Fife map with KY14 postcodes")



## ----r------------------------------------------------------------------------
KY14_units <- geojson_read("data/KY14.geojson", what="sp")


## ----r------------------------------------------------------------------------
KY14_postcode_lookup <- tibble(id=1:length(KY14_units@data[["postcodes"]]), postcode = KY14_units@data[["postcodes"]])


## ----r------------------------------------------------------------------------
write_csv(KY14_postcode_lookup, "data/KY14_postcode_lookup.csv")


## ----r------------------------------------------------------------------------
KY14_postcode_lookup <- read_csv("data/KY14_postcode_lookup_values.csv")


## ----r------------------------------------------------------------------------
KY14_units_tidy <- KY14_units %>%
  tidy() %>%
  mutate(id = as.numeric(id)) %>%
  left_join(KY14_postcode_lookup, on=id, keep=F)


## ----r------------------------------------------------------------------------
KY14_centroids <- KY14_units_tidy %>%
  select(postcode, lat, long) %>%
  group_by(postcode) %>%
  mutate(mean_long = mean(long), mean_lat = mean(lat)) %>%
  ungroup() %>%
  select(-c(lat, long))  %>%
  distinct()


## ----r------------------------------------------------------------------------
ky14 <- KY14_units_tidy %>%
  ggplot() +
  geom_polygon(aes( x = long, y = lat, group=group, fill=value)) +
  theme_void() +
  coord_map() +
  ggtitle("KY14 postcodes") +
  theme(legend.position = "none")

ky14


## ----r------------------------------------------------------------------------
postcode_lookup("KY14 7AP") %>% t() %>% knitr::kable()


## ----r------------------------------------------------------------------------
ky14 +
  geom_point(data = postcode_lookup("KY14 7AP"), aes(x=longitude, y=latitude), size=3, color="white") +
  geom_label(data = postcode_lookup("KY14 7AP"), aes(x=(longitude+0.03), y=latitude, label=postcode))

scot +
  geom_point(data = postcode_lookup("KY14 7AP"), aes(x=longitude, y=latitude), size=5, shape=10, color="black") +
  geom_label(data = postcode_lookup("KY14 7AP"), aes(x=(longitude+1), y=latitude, label=postcode))


## ----r------------------------------------------------------------------------
KY14_units_tidy %>%
  #filter(str_detect(postcode, "IV2")) %>%
  ggplot() +
  geom_polygon(aes( x = long, y = lat, group=group, fill=value)) +
  geom_label(data = KY14_centroids, aes(x=mean_long, y=mean_lat, label=postcode), size=3) +
  theme_void() +
  coord_map() +
  ggtitle("KY14 postcodes choropleth") +
  theme(legend.position = "none")

