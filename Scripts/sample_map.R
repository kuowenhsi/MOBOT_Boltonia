library(tidyverse)
library(ggspatial)
library(scatterpie)
library(tigris)
library(cowplot)
library(rnaturalearth)
library(sf)
library(shadowtext)

####set inputs and adjustments####
setwd("/Users/User/Desktop/REUProject_2_Outgroups")
file_out <- "./Figures/Boltonia_Precipitation_Coldest_Quarter_BIO19_Map.png"
sample_data <- read_csv("./Data/Boltonia_merged_data_20240925_sample_group.csv") |>
  mutate(Sample_Name = paste("Boltonia", str_pad(index, 3, "left","0"), sep = "_"))

#loads in climate layer
climate.layer <- rast("./Data/MapFiles/wc2.1_30s_bio_19.tif")
climate.layer <- project(climate.layer, "EPSG:3857")
map_extent <- terra::ext(-10197842, -9851705, 4629999, 5107951)
climate.layer <- terra::crop(climate.layer, map_extent)

#Coordinate Adjustment to add labels
adj_coords <- tribble(
  ~Sample_Group, ~adj_X_shift, ~adj_Y_shift,
  "scott",       -30000,       -20000,
  "saint_clair",  45000,       -25000,
  "madison",      45000,        25000,
  "jersey",       25000,        38000,
  "cass",         30000,        -5000,
  "morgan",       30000,       -30000,
  "tazewell",     45000,       -10000,
  "peoria",      -30000,        15000,
  "schuyler",    -50000,       -12000,
  "frederick",   -45000,        25000,
  "woodford",     50000,        20000,
  "marshal",     -40000,        15000,
  "fulton1",     -20000,        30000,
  "fulton2",      40000,            0,
  "fulton3",     -20000,        25000,
  "alton",        20000,        25000,
  "hennepin",    -25000,        20000,
)

####Builds maps####

#builds usa map, along with illinois
usa_states <- ne_states(country = "United States of America", returnclass = "sf")
illinois <- usa_states |> filter(name == "Illinois") |> st_transform(3857)
missouri <- usa_states |> filter(name == "Missouri") |> st_transform(3857)
usa_states <- usa_states |>
  filter(!name %in% c("Alaska", "Hawaii"))
usa_states <- st_transform(usa_states, 3857)

#adds Illinois counties
options(tigris_class = "sf")
il_county <- counties(state = "IL", cb = TRUE) |> st_transform(3857)

#adds rivers
major_river <- st_read("./Data/MapFiles/Rivers/Major_River.shp")
major_river <- st_transform(major_river, crs = 3857)

minor_rivers <- st_read("./Data/MapFiles/Rivers/Minor_Rivers.shp")
minor_rivers <- st_transform(minor_rivers, crs = 3857)

#Herbarium Outgroup Sites
outgroup_sites <- tribble(
  ~ID, ~Lat, ~Long,
  "469",	31.153,	-89.115,  # Southern MS
  "470",	31.636,	-86.609,  # Southern AL
  "471",	38.475,	-90.8139, # Central  MO
  "475",  36.9789, -90.1319,# Southern MO
  "476",	39.246,	-94.232,  # Western  MO
  "477",	37.501,	-89.676,  # Southern MO (near river)
  "479",	38.406,	-96.314,  # Eastern  KS
  "481",	33.83,	-88.521,  # Northern MS
  "482",	33.512,	-92.69,   # Southern AK
  "483",	38.737,	-90.71287 # Central  MO
)
outgroup_sites <- outgroup_sites %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)
outgroup_sites <- st_transform(outgroup_sites, crs = 3857)
outgroup_sites <- outgroup_sites |>
  mutate(
    X = st_coordinates(geometry)[,1],
    Y = st_coordinates(geometry)[,2]
  )
outgroup_sites <- outgroup_sites |> 
  st_drop_geometry()
outgroup_sites$feature <- "Outgroup Sites"
  
sample_data$true_lat <- ifelse(!is.na(sample_data$Latitude), 
                               sample_data$Latitude, 
                               sample_data$Google_latitude)
sample_data$true_long <- ifelse(!is.na(sample_data$Longitude), 
                                sample_data$Longitude, 
                                sample_data$Google_longitude)

#adds a sample count
sample_data <- sample_data |> 
  add_count(Sample_Group, name = "N")

sample_data <- sample_data |> 
  distinct(Sample_Group, .keep_all = TRUE) |> 
  filter(Sample_Group != "N/A", Sample_Group != "misID")

#Adjust coordinates to a better format for making the map
sample_data <- st_as_sf(sample_data, coords = c("true_long", "true_lat"), crs = 4326) |>
  st_transform(3857)
  
sample_data <- sample_data |>
  mutate(
    X = st_coordinates(geometry)[,1],
    Y = st_coordinates(geometry)[,2]
  )
  
sample_data <- sample_data |> 
  st_drop_geometry()
  
#adds the adjustments of the position of labels in inset map
sample_data <- sample_data |>
  left_join(adj_coords, by = c("Sample_Group")) |>
  mutate(
    adj_X = X + adj_X_shift,
    adj_Y = Y + adj_Y_shift
  ) |>
  dplyr::select(-adj_X_shift, -adj_Y_shift)
  
# MAIN MAP
main_map <- ggplot() +
  geom_sf(data = usa_states, fill = "gray60", color = "white") +
  geom_segment(data = outgroup_sites,
               aes(x = X, y = Y, xend = X - 120000, yend = Y),
               linetype = "solid", color = "black") +
  geom_point(data = sample_data, aes(x = X, y = Y), color = "black", size = 1.5) +
  geom_point(data = outgroup_sites, aes(x = X, y = Y), color = "blue", size = 1.5) +
  geom_shadowtext(data = outgroup_sites, aes(x = X - 150000, y = Y, label = ID),
                  size = 1.8, color = "white", bg.color = "black") +
  coord_sf(xlim = c(-10721625 - 400000, -9641270 + 1200000),
           ylim = c(3652635 - 300000, 5063236 + 300000),
           expand = FALSE) +
  annotate("rect",
           xmin = -10197842,
           ymin = 4629999,
           xmax = -9851705,
           ymax = 5107951,
           fill = NA,
           color = "black",
           linewidth = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.4,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
    theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = c(0.85, 0.3),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    axis.title = element_blank(),
  ) +
  guides(color = guide_legend(override.aes = list(size = 4)))

# INSET MAP
inset_map <- ggplot() +
  geom_spatraster(data = climate.layer) +
  scale_fill_viridis_c(option = "plasma", name = "Prec. Coldest Quarter (mm)") +
  geom_sf(data = il_county, fill = NA, color = "gray") +
  geom_sf(data = illinois, fill = NA, color = "black") +
  geom_sf(data = major_river, color = "blue", size = 0.4, alpha = 1) +
  geom_sf(data = minor_rivers, color = "blue", size = 0.2, alpha = 0.5) +
  geom_segment(data = sample_data,
               aes(x = X, y = Y, xend = adj_X, yend = adj_Y),
               linetype = "solid", color = "black") +
  geom_point(data = sample_data, aes(x = X, y = Y), size = 1, color = "black") +
  geom_point(data = outgroup_sites, aes(x = X, y = Y), size = 1, color = "blue") +
  geom_shadowtext(data = outgroup_sites, aes(x = X - 20000, y = Y, label = ID),
                  size = 2, fontface = "bold", color = "white", bg.color = "black") +
  geom_shadowtext(data = sample_data, aes(x = adj_X, y = adj_Y, label = Sample_Group),
                  size = 2, fontface = "bold", color = "white", bg.color = "black") +
  coord_sf(xlim = c(-10197842, -9851705),
           ylim = c(4629999, 5107951),
           expand = FALSE) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5),
         color = "none") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = c(1.47, 0.5),
    legend.title = element_text(size = 8),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Combine inset and main map with cowplot
full_map <- ggdraw() +
  draw_plot(main_map, x = -0.12) +
  draw_plot(inset_map, x = 0.114, y = 0.145, scale = 0.7)

# Save with high resolution and white bg
ggsave(filename = file_out,
       plot = full_map,
       width = 7, height = 4,
       dpi = 300, units = "in",
       bg = "white")



