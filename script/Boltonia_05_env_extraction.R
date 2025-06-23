library(sf)
library(sp)
library(raster)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
# library(stars)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

env_files <- list.files("/Users/kuowenhsi/OneDrive - Washington University in St. Louis/Undergrad/Grace/Env_varibles", pattern = "*.tif$")
env_files

Boltonia_data <- read_csv("./data/Boltonia_merged_data_20240925.csv") %>%
  mutate(Adapted_Longitude = case_when(is.na(Longitude) ~ Google_longitude, TRUE ~ Longitude), Adapted_Latitude = case_when(is.na(Latitude) ~ Google_latitude, TRUE ~ Latitude))

Boltonia_data_sf <- st_as_sf(Boltonia_data, coords = c("Adapted_Longitude", "Adapted_Latitude"), agr = "constant", crs = 4326)

plot(Boltonia_data_sf)

wc_names <- c(BIO1 = "Annual Mean Temperature",
              BIO2 = "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
              BIO3 = "Isothermality (BIO2/BIO7) (×100)",
              BIO4 = "Temperature Seasonality (standard deviation ×100)",
              BIO5 = "Max Temperature of Warmest Month",
              BIO6 = "Min Temperature of Coldest Month",
              BIO7 = "Temperature Annual Range (BIO5-BIO6)",
              BIO8 = "Mean Temperature of Wettest Quarter",
              BIO9 = "Mean Temperature of Driest Quarter",
              BIO10 = "Mean Temperature of Warmest Quarter",
              BIO11 = "Mean Temperature of Coldest Quarter",
              BIO12 = "Annual Precipitation",
              BIO13 = "Precipitation of Wettest Month",
              BIO14 = "Precipitation of Driest Month",
              BIO15 = "Precipitation Seasonality (Coefficient of Variation)",
              BIO16 = "Precipitation of Wettest Quarter",
              BIO17 = "Precipitation of Driest Quarter",
              BIO18 = "Precipitation of Warmest Quarter",
              BIO19 = "Precipitation of Coldest Quarter")


usa_state <- ne_states(country = "United States of America", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()
canada_state <- ne_states(country = "canada", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()
mexico_state <- ne_states(country = "mexico", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()

class(usa_state)
plot(usa_state, axes = TRUE)
plot(canada_state, axes = TRUE)
plot(mexico_state, axes = TRUE)


env_files[[19]]
env_layer <- raster(paste0("/Users/kuowenhsi/OneDrive\ -\ Washington\ University\ in\ St.\ Louis/Undergrad/Grace/Env_varibles/", env_files[[1]]))%>%
  crop(extent(-100, -82, 34, 45))

plot(env_layer, axes = TRUE)
points(Boltonia_data$Longitude, Boltonia_data$Latitude)

points(x = -75.06528, y = 38.61383)



extracted_buf_env_list <- list()

for (i in 1:length(env_files)) {
  print(env_files[[i]])
  env_layer <- raster(paste0("/Users/kuowenhsi/OneDrive\ -\ Washington\ University\ in\ St.\ Louis/Undergrad/Grace/Env_varibles/", env_files[[i]])) 
  class(env_layer)
  crs(env_layer) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  crs(env_layer)
  extracted_env <- raster::extract(env_layer, Boltonia_data_sf, buffer = 1000,fun = mean, na.rm = TRUE, df = TRUE)
  extracted_buf_env_list[[i]] <- as.tibble(extracted_env)[,2]
}

extracted_buf_env_df <- bind_cols(extracted_buf_env_list)

for (i in colnames(extracted_env_df)){
  plot(extracted_env_df[[i]], extracted_buf_env_df[[i]], main = i)
}

any(is.na(extracted_env_df))
any(is.na(extracted_buf_env_df))



Boltonia_climate_data <- bind_cols(Boltonia_data %>% dplyr::select(Sample_Name = index, Adapted_Longitude, Adapted_Latitude) %>% mutate(Sample_Name = paste("Boltonia", str_pad(Sample_Name, 3, pad = "0"), sep = "_")), extracted_buf_env_df)

write_csv(Boltonia_climate_data, "./data/Boltonia_buf_climate_data_20250421.csv")


for (clim in colnames(Boltonia_climate_data)[-1]){
  p <- ggplot(data = Boltonia_climate_data, aes(x = Adapted_Latitude, y = get(clim)))+
    geom_point()+
    ggtitle(clim)+
    theme_bw()
  print(p)
}

##############

plot(env_layer)
class(env_layer)

cyano_climate_data_sp <- st_as_sf(cyano_climate_data, coords = c("Longitude", "Latitude"), agr = "constant", crs = 4326)

colnames(cyano_climate_data_sp) <- colnames(cyano_climate_data_sp) %>%
   str_remove("current_30arcsec_")%>%
   str_remove(".tif")

colnames(cyano_climate_data_sp)

range(cyano_climate_data$current_30arcsec_annualPET)
range(cyano_climate_data_sp$wc2.1_30s_bio_1)

for (i in colnames(cyano_climate_data_sp)[8:42]){
  print(i)
  p <- ggplot(data = cyano_climate_data_sp)+
    geom_sf(data = usa_state)+
    geom_sf(data = canada_state)+
    geom_sf(data = mexico_state)+
    geom_sf(aes(color = get(i)), size = 3)+
    scale_colour_viridis_c(option = "inferno", name = "")+
    ggtitle(i)+
    theme_bw()+
    theme(panel.grid = element_blank())+
    coord_sf(xlim = c(-125, -65), ylim = c(25, 50))
  ggsave(paste0(i, ".png"), width = 16, height = 12)
  
}

setwd("/Users/kuowenhsi/OneDrive - Washington University in St. Louis/Genome_paper")

env_layer_croped <- crop(env_layer, extent(-125,-65,25,50))
plot(env_layer_croped)

env_layer_df <- as.data.frame(env_layer_croped, xy=TRUE)
env_layer_df <- env_layer_df[!is.na(env_layer_df$wc2.1_30s_bio_11),]
head(env_layer_df)

p <- ggplot(data = cyano_data_sf)+
  geom_raster(data = env_layer_df, aes(x=x, y=y,fill=wc2.1_30s_bio_11))+
  geom_sf(data = usa_state, fill = NA)+
  geom_sf(data = canada_state, fill = NA)+
  geom_sf(data = mexico_state, fill = NA)+
  geom_sf(size = 1, color = "yellow")+
  scale_fill_viridis_c(option = "inferno", name = "")+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.key.height = unit(0.05, "in"), legend.key.width = unit(0.6, "in"), legend.margin = margin(t=-0.02, b=-0.02, unit = "in"), legend.position = "bottom")+
  xlab("")+
  ylab("")+
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50), expand = FALSE)

ggsave("bio19_sample_indiv2.png", width = 4, height = 3)
 