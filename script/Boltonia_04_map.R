# Load necessary libraries
library(tidyverse)     # For data manipulation and visualization
library(cowplot)       # For combining multiple plots
library(sf)            # For handling spatial data
library(rnaturalearth) # For accessing natural earth data
library(rnaturalearthdata)
library(scatterpie)    # For creating scatter pie plots
library(raster)        # For raster data manipulation
library(ggspatial)     # For spatial data visualization
library(ggrepel)
library(tigris)

# Set working directory
setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

# Read input data
Boltonia_data <- read_csv("./data/Boltonia_merged_data_20240627.csv")%>%
  mutate(Adapted_Longitude = case_when(is.na(Longitude) ~ Google_longitude, TRUE ~ Longitude), Adapted_Latitude = case_when(is.na(Latitude) ~ Google_latitude, TRUE ~ Latitude))

# Get and crop country boundaries
usa_state <- ne_states(country = "United States of America", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()

options(tigris_class = "sf")
IL_county <- counties(state = "IL", cb = TRUE)
MO_county <- counties(state = "MO", cb = TRUE)

canada_state <- ne_states(country = "canada", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()

mexico_state <- ne_states(country = "mexico", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()

rivers_usa <- st_read("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/GIS_material/Rivers/USA_Rivers_and_Streams-shp/9ae73184-d43c-4ab8-940a-c8687f61952f2020328-1-r9gw71.0odx9.shp")%>%
  filter(str_detect(State, "MO|IL|IA|KY|AR|TN"))%>%
  st_transform(crs = 3857)
    
three_rivers <- rivers_usa %>%
  filter(Name %in% c("Illinois River", "Mississippi River", "Missouri River", "Des Moines River", "Fox River"))
rivers_usa_filtered <- rivers_usa %>%
  filter(Miles > 25)


#adds rivers and dams
major_river <- st_read("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/GIS_material/Rivers/major_rivers_large.shp") %>%
  st_transform(crs = 3857)%>%
  mutate(feature = "Illinois River")

plot(major_river)
plot(rivers_sf)

minor_rivers <- st_read("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/GIS_material/Rivers/Minor_Rivers.shp") %>%
  st_transform(minor_rivers, crs = 3857) %>%
  mutate(feature = "Tributaries")

water_dam <- st_read("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/GIS_material/Rivers/US_Dams/US_Dams.shp")%>%
  # filter(STATE_NAME %in% c("ILLINOIS", "MISSOURI"))%>%
  filter(RIVER %in% c("ILLINOIS RIVER", "MISSOURI RIVER", "MISSISSIPPI"))


#Coordinate Adjustment to prevent piecharts from overlapping
adj_coords <- tribble(
  ~County_label, ~adj_X_shift, ~adj_Y_shift,
  "Scott",       -25000,       -20000,
  "Saint Clair",  45000,       -10000,
  "Madison",      45000,        35000,
  "Jersey",        8000,        25000,
  "Cass",         30000,        -5000,
  "Morgan",       30000,       -30000,
  "Tazewell",     25000,       -10000,
  "Peoria",      -25000,        15000,
  "Schuyler",    -50000,       -12000,
  "Frederick",   -27000,        25000,
  "Woodford",     30000,        10000,
  "Marshal",     -40000,        10000,
  "Fulton1",     -20000,        30000,
  "Fulton2",      40000,            0,
  "Fulton3",     -15000,        15000,
  "Alton",        20000,        25000,
  "Hennepin",    -25000,        10000,
)


Boltonia_data_label <- Boltonia_data %>%
  mutate(MaternalLine = case_when(MaternalLine == "2011-2644-1" ~ "2011-2661-1", TRUE ~ MaternalLine))%>%
  group_by(County, MaternalLine) %>%
  summarise(Longitude = mean(Adapted_Longitude), Latitude = mean(Adapted_Latitude), Sample_Size = n())%>%
  ungroup()%>%
  arrange(Latitude)%>%
  mutate(County_label = c("Saint Clair","Madison","Alton","Jersey","Scott","Morgan","Cass","Schuyler","Frederick","Fulton1","Fulton2","Fulton3", "Tazewell","Peoria","Woodford","Marshal","Hennepin"))%>%
  st_as_sf(coords = c("Longitude", "Latitude"), agr = "constant", crs = 4326)%>%
  st_transform(crs = 3857)%>%
  cbind(st_drop_geometry(.), st_coordinates(.))%>%
  left_join(adj_coords, by = "County_label")


# Step 1: Load raster
env_layer <- raster("/Users/kuowenhsi/OneDrive - Washington University in St. Louis/Undergrad/Grace/Env_varibles/current_30arcsec_minTempWarmest.tif")

# Step 2: Define crop extent in EPSG:3857 and convert to EPSG:4326 (WGS84)
extent_3857 <- st_sfc(st_polygon(list(rbind(
  c(-10197842, 4639999),
  c(-10197842, 5107951),
  c(-9851705, 5107951),
  c(-9851705, 4639999),
  c(-10197842, 4639999)
))), crs = 3857)

extent_4326 <- st_transform(extent_3857, crs = st_crs(env_layer))  # likely 4326

bbox_4326 <- st_bbox(extent_4326)
crop_extent <- extent(c(bbox_4326$xmin, bbox_4326$xmax, bbox_4326$ymin, bbox_4326$ymax))

# Step 3: Crop raster in native CRS
env_layer_cropped <- crop(env_layer, crop_extent)

plot(env_layer_cropped)

# Step 4: Project cropped raster to EPSG:3857
template <- projectExtent(env_layer_cropped, crs = CRS("+init=EPSG:3857"))
res(template) <- res(env_layer_cropped)  # Match original resolution

# Step 5: Reproject using bilinear method
env_layer_3857 <- projectRaster(env_layer_cropped, to = template, method = "bilinear")

# Check if values exist
print(cellStats(env_layer_3857, stat = 'mean'))

# Step 6: Convert to data frame
env_df <- as.data.frame(env_layer_3857, xy = TRUE, na.rm = TRUE)

head(env_df)

library(colorspace)

# Create PCA plot with scatter pies
p <- ggplot(data = Boltonia_data_label) +
  geom_raster(data = env_df,aes(x = x, y = y, fill = current_30arcsec_minTempWarmest)) +
  geom_sf(data = rivers_usa_filtered, color = "lightblue", linewidth = 0.5, alpha = 0.5) +
  geom_sf(data = three_rivers, color = "#00A9FF", linewidth = 1.5)+
  geom_sf(data = IL_county, fill = NA, color = "gray75")+
  geom_sf(data = MO_county, fill = NA, color = "gray75")+
  geom_sf(data = usa_state, fill = NA, color = "red3", linetype = 2) +
  geom_sf(data = water_dam, color = "red", size = 2.5, shape = 7)+
  geom_segment(aes(x = X, y = Y, xend = X + adj_X_shift, yend = Y + adj_Y_shift))+
  geom_label(aes(x = X + adj_X_shift, y = Y + adj_Y_shift, label = County_label, fill = County), show.legend = FALSE)+
  geom_sf()+
  xlab("") +
  ylab("") +
  scale_fill_manual(values = qualitative_hcl(20, palette = "Pastel 1"))+
  theme_minimal(base_size = 14) +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 7),
        panel.grid = element_blank())+
  coord_sf(xlim = c(-10197842, -9851705),
    ylim = c(4639999, 5107951),
    expand = FALSE)
p


usa_states <- ne_states(country = "United States of America", returnclass = "sf")
illinois <- usa_states |> filter(name == "Illinois")
missouri <- usa_states |> filter(name == "Missouri")
usa_states <- usa_states |>
  filter(!name %in% c("Alaska", "Hawaii"))

#creates the map inset
inset_map <- ggplot() +
  geom_sf(data = usa_states, fill = "gray85", color = "white") +
  geom_sf(data = illinois, fill = "red", color = "white", alpha = 0.5) +
  geom_sf(data = missouri, fill = "red", color = "white", alpha = 0.5) +
  geom_sf(data = Boltonia_data_label, color = "black", size = 0.5) +
  theme_void() 

ggsave("./figures/sampling_location_inset.png", width = 3.5, height = 2.5, dpi = 600)

#builds the final map with the inset and main map
full_map <- ggdraw() +
  draw_plot(p) +
  draw_plot(inset_map, x = 0.55, y = 0.04, width = 0.54, height = 0.54)
full_map

#save
ggsave("./figures/sampling_location.png", plot = p,
       width = 7, height = 7.5, dpi = 600, units = "in", bg = "white")


ggsave("./figures/Boltonia_map_20240802.png", width = 6, height = 6, dpi = 600)

# Extract legend from the PCA plot
p_legend <- get_legend(p + guides(color = "none", shape = "none") + theme(legend.background = element_blank()))

# Create environmental layer plot
p_env <- ggplot() +
  geom_raster(data = env_layer_df, aes(x = x, y = y, fill = current_30arcsec_growingDegDays5)) +
  geom_sf(data = usa_state, fill = NA) +
  geom_sf(data = canada_state, fill = NA) +
  geom_sf(data = mexico_state, fill = NA) +
  scale_fill_viridis_c(option = "inferno", name = "") +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.key.height = unit(0.1, "in"), legend.key.width = unit(0.6, "in"), legend.margin = margin(t = -0.02, b = -0.02, unit = "in"), legend.position = "top", panel.background = element_rect(fill = "white")) +
  xlab(expression("Growing Degree Days (>5" * degree * C * ")")) +
  ylab("") +
  coord_sf(xlim = c(-130, -65), ylim = c(25, 50), expand = FALSE, label_axes = "")

# Combine PCA plot and environmental plot
p_comb <- p + guides(fill = "none") +
  annotation_custom(p_legend, xmin = -20e6, ymax = 3e6) +
  annotation_custom(ggplotGrob(p_env), xmin = -16e6, ymin = 2.75e6, xmax = -11e6, ymax = 4.75e6)

# Save the combined plot
ggsave("./figures/figure_1.png", width = 10, height = 8, dpi = 600)