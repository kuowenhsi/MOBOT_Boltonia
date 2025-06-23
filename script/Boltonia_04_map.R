# Load necessary libraries
library(tidyverse)     # For data manipulation and visualization
# library(cowplot)       # For combining multiple plots
library(sf)            # For handling spatial data
library(rnaturalearth) # For accessing natural earth data
library(rnaturalearthdata)
# library(scatterpie)    # For creating scatter pie plots
# library(raster)        # For raster data manipulation
library(ggspatial)     # For spatial data visualization
library(ggrepel)

# Set working directory
setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

# Read input data
Boltonia_data <- read_csv("./data/Boltonia_merged_data_20240627.csv")

# Get and crop country boundaries
usa_state <- ne_states(country = "United States of America", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()

canada_state <- ne_states(country = "canada", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()

mexico_state <- ne_states(country = "mexico", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()

rivers_sf <- ne_download(scale = "large", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")

Boltonia_data_label <- Boltonia_data %>%
  group_by(County) %>%
  summarise(Longitude = mean(Google_longitude), Latitude = mean(Google_latitude))%>%
  ungroup()%>%
  st_as_sf(coords = c("Longitude", "Latitude"), agr = "constant", crs = 4326)%>%
  st_transform(crs = 3857)

Boltonia_data_label_3857 <- cbind(st_drop_geometry(Boltonia_data_label), st_coordinates(Boltonia_data_label))


# Create PCA plot with scatter pies
p <- ggplot(data = st_as_sf(Boltonia_data, coords = c("Google_longitude", "Google_latitude"), agr = "constant", crs = 4326)) +
  geom_sf(data = rivers_sf, color = "lightblue", linewidth = 1) +
  geom_sf(data = usa_state, fill = NA, color = "red3") +
  geom_sf(data = canada_state, fill = NA, color = "gray75") +
  geom_sf(data = mexico_state, fill = NA, color = "gray75") +
  geom_sf()+
  geom_text_repel(data = Boltonia_data_label_3857, aes(x = X, y = Y, label = County), force_pull = 0.01, force = 30, max.overlaps = 25, size = 5, inherit.aes = FALSE,min.segment.length = 0.1, color = "gray20")+
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "vertical", panel.grid = element_blank()) +
  coord_sf(xlim = c(-10.8e6, -9e6), ylim = c(4.1e6, 5.7e6), expand = FALSE, crs = 3857) +
  annotation_scale(location = "br", width_hint = 0.25)

p

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