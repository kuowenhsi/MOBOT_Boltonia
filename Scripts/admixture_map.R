library(tidyverse)
library(ggspatial)
library(scatterpie)
library(tigris)
library(cowplot)
library(rnaturalearth)
library(sf)


####set inputs and adjustments####
setwd("/Users/User/Desktop/REUProject_2_Outgroups")
add_dams <- FALSE
add_past_sites <- FALSE
add_major_river <- TRUE
add_minor_rivers <- TRUE
range_of_k <- 2:10
file_prefix <- "./Data/Boltonia_Chr_1_pruned_data_filtered." #file prefix (used for .Q and .fam)
file_out_prefix <- "./Figures/Boltonia_ancestry_map_filtered_K" #file output prefix and destination
metadata <- read_csv("./Data/Boltonia_merged_data_20240925_sample_group.csv") |>
  mutate(Sample_Name = paste("Boltonia", str_pad(index, 3, "left","0"), sep = "_"))

#Coordinate Adjustment to prevent piecharts from overlapping
adj_coords <- tribble(
  ~Sample_Group, ~adj_X_shift, ~adj_Y_shift,
  "scott",       -25000,       -20000,
  "saint_clair",  45000,       -10000,
  "madison",      45000,        35000,
  "jersey",        8000,        25000,
  "cass",         30000,        -5000,
  "morgan",       30000,       -30000,
  "tazewell",     25000,         8000,
  "peoria",      -25000,        15000,
  "schuyler",    -50000,       -12000,
  "frederick",   -27000,        25000,
  "woodford",     20000,        20000,
  "marshal",     -40000,        10000,
  "fulton1",     -20000,        30000,
  "fulton2",      40000,            0,
  "fulton3",     -15000,        15000,
  "alton",        20000,        25000,
  "hennepin",    -25000,        10000,
)

####Builds maps####

#builds usa map, along with illinois
usa_states <- ne_states(country = "United States of America", returnclass = "sf")
illinois <- usa_states |> filter(name == "Illinois") |> st_transform(3857)
missouri <- usa_states |> filter(name == "Missouri") |> st_transform(3857)
usa_states <- usa_states |>
  filter(!name %in% c("Alaska", "Hawaii"))

#adds Illinois counties
options(tigris_class = "sf")
il_county <- counties(state = "IL", cb = TRUE) |> st_transform(3857)

#adds rivers and dams
major_river <- st_read("./Data/MapFiles/Major_River.shp")
major_river <- st_transform(major_river, crs = 3857)
major_river$feature <- "Illinois River"

minor_rivers <- st_read("./Data/MapFiles/Minor_Rivers.shp")
minor_rivers <- st_transform(minor_rivers, crs = 3857)
minor_rivers$feature <- "Tributaries" 

dams <- tribble(
  ~dam, ~Long, ~Lat,
  "La Grange", -90.534302,39.9403990000001,
  "Peoria", -89.624496, 40.631699,
  "Melvin Price", -90.154877, 38.866913
)
dams <- dams %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)
dams <- st_transform(dams, crs = 3857)
dams <- dams |>
  mutate(
    X = st_coordinates(geometry)[,1],
    Y = st_coordinates(geometry)[,2]
  )
dams <- dams |> 
  st_drop_geometry()
dams$feature <- "Dams"

#Past Sites
past_sites <- tribble(
  ~ID, ~Lat, ~Long,
  "1",	41.25,	-89.35,
  "2",	40.93333333,	-89.46666667,
  "3",	40.88333333,	-89.45,
  "4",	40.71666667,	-89.55,
  "5",	40.5,	-89.9,
  "6",	40.3,	-90.06666667,
  "7",	40.2,	-90.2,
  "8",	39,	-90.55,
  "9",	39.86666667,	-90.55,
  "10",	38.96666667,	-90.5,
  "11",	38.96666667,	-90.5,
  "12",	38.68333333,	-90.1,
  "13",	38.66666667,	-90.11666667,
  "14",	38.65,	-90.11666667,
  "GL", 38.96816, -90.50626,
  "SL", 39.00643, -90.54653,
  "BT", 40.01138, -90.44514,
  "F",  40.06679, -90.42842
)
past_sites <- past_sites %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)
past_sites <- st_transform(past_sites, crs = 3857)
past_sites <- past_sites |>
  mutate(
    X = st_coordinates(geometry)[,1],
    Y = st_coordinates(geometry)[,2]
  )
past_sites <- past_sites |> 
  st_drop_geometry()
past_sites$feature <- "Past Sites"

###map loop###
for (i in range_of_k) {
  map_name <- paste0(file_out_prefix, i, ".png")
  file_name <- paste0(file_prefix, i, ".Q")
  pie_cols <- paste0("X", 1:i)
  
  #load in Q file for K = # as qmat
  qmat <- read_table(file_name, col_names = FALSE) |>
    mutate(Sample_Name = read_table(paste0(file_prefix, "fam"), 
                                    col_names = FALSE) |> pull(X2))|>
    left_join(metadata |> select(1:14, "Sample_Name"), by = "Sample_Name")
  
  qmat$true_lat <- ifelse(!is.na(qmat$Latitude), qmat$Latitude, qmat$Google_latitude)
  qmat$true_long <- ifelse(!is.na(qmat$Longitude), qmat$Longitude, qmat$Google_longitude)
  
  #Swap DNA results for Boltonia_126 & 127
  cols_to_swap <- paste0("X", 1:i)
  row_126 <- which(qmat$Sample_Name == "Boltonia_126")
  row_127 <- which(qmat$Sample_Name == "Boltonia_127")
  tmp <- qmat[row_126, cols_to_swap]
  qmat[row_126, cols_to_swap] <- qmat[row_127, cols_to_swap]
  qmat[row_127, cols_to_swap] <- tmp
  
  #pivot longer
  qmat<- qmat |>   
    pivot_longer(
      cols = starts_with("X"), 
      names_to = "Ancestry", 
      values_to = "Proportion"
    ) 
  
  #reformat qmat to show mean proportion for each county, along with coords of each county
  qmat_summary <- qmat |> 
    group_by(County, Ancestry, Sample_Group) |> 
    summarise(
      mean_proportion = mean(Proportion),
      Latitude = first(true_lat),
      Longitude = first(true_long),
      .groups = "drop"
    )
  
  #change how qmat_summary looks
  df_wide <- qmat_summary |>
    pivot_wider(names_from = Ancestry, values_from = mean_proportion) |>
    distinct(Sample_Group, .keep_all = TRUE)
  
  #adds a sample count to df_wide change (N = N / i) where i = K
  group_counts <- qmat |> 
    count(Sample_Group, name = "N") |> 
    mutate(N = N / i)
  df_wide <- df_wide |> 
    left_join(group_counts, by = "Sample_Group")
  
  #Adjust coordinates to a better format for making the map
  df_sf <- st_as_sf(df_wide, coords = c("Longitude", "Latitude"), crs = 4326) |>
    st_transform(3857)
  
  df_sf <- df_sf |>
    mutate(
      X = st_coordinates(geometry)[,1],
      Y = st_coordinates(geometry)[,2]
    )
  
  df_sf <- df_sf |> 
    st_drop_geometry()
  
  #Adjusts the position of the piecharts so they dont overlap in the final map
  df_pie <- df_sf |>
    left_join(adj_coords, by = c("Sample_Group")) |>
    mutate(
      adj_X = X + adj_X_shift,
      adj_Y = Y + adj_Y_shift
    ) |>
    select(-adj_X_shift, -adj_Y_shift)
  
  
  #creates the map
  main_map <- ggplot() +
    # County and state boundaries
    geom_sf(data = il_county, fill = NA, color = "gray") +
    geom_sf(data = illinois, fill = NA, color = "black") +
    
    # Conditional layers
    { if (add_major_river) geom_sf(data = major_river, aes(color = feature), size = 0.4, alpha = 0.9) else NULL } +
    { if (add_minor_rivers) geom_sf(data = minor_rivers, aes(color = feature), size = 0.2, alpha = 0.4) else NULL } +
    { if (add_dams) geom_point(data = dams, aes(x = X, y = Y, color = feature), size = 2.5, shape = 7) else NULL } +
    { if (add_past_sites) geom_point(data = past_sites, aes(x = X, y = Y, color = feature), size = 2.5, shape = 8) else NULL } +
    { if (add_past_sites) geom_text(data = df_pie, aes(x = adj_X, y = adj_Y, label = Sample_Group), size = 2, nudge_x = 10000, fontface = "bold") else NULL } +
    
    # Pies and labels
    geom_segment(
      data = df_pie,
      aes(x = X, y = Y, xend = adj_X, yend = adj_Y),
      linetype = "solid", color = "black"
    ) +
    geom_point(
      data = df_pie, 
      aes(x = X, y = Y), 
      size = 1, 
      color = "black"
    ) +
    geom_scatterpie(
      data = df_pie, 
      aes(x = adj_X, y = adj_Y, r = 15000), 
      cols = pie_cols,
      color = NA, 
      show.legend = FALSE
    ) +
    geom_text(
      data = df_pie,
      aes(x = adj_X, y = adj_Y, label = Sample_Group),
      size = 3,
      fontface = "bold"
    ) +
    geom_text(
      data = df_pie, 
      aes(x = adj_X, y = adj_Y, label = N), 
      size = 3, 
      fontface = "bold"
    ) +
    
    # Coordinate and map annotations
    coord_sf(
      xlim = c(-10197842, -9851705),
      ylim = c(4639999, 5107951),
      expand = FALSE
    ) +
    annotation_scale(
      location = "tl", 
      width_hint = 0.3, 
      line_width = 0.6, 
      pad_y = unit(0.05, "in")
    ) +
    annotation_north_arrow(
      location = "tl", 
      which_north = "true",
      pad_x = unit(0, "in"), 
      pad_y = unit(0.3, "in"),
      style = north_arrow_fancy_orienteering
    ) +
    
    # Ancestry color scale (for pies)
    scale_fill_brewer("Ancestry", palette = "Set3") +
    
    # Color for rivers/dams
    scale_color_manual(
      name = NULL,
      values = c("Illinois River" = "blue2", 
                 "Tributaries" = "blue", 
                 "Dams" = "red", 
                 "Past Sites" = "green"
      ),
      drop = TRUE
    ) +
    
    # Theme
    theme_minimal(base_size = 14) +
    theme(
      legend.position.inside = c(1, 1),
      legend.justification = c(1, 1),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_blank(),
      axis.text = element_text(size = 7),
      panel.grid = element_blank(),
      legend.key.height = unit(0.3, "cm"),
      legend.key.width  = unit(0.3, "cm")
    ) +
    labs(
      title = "Ancestry Proportions by County",
      fill = "Ancestry"
    )
  
  #small points on inset map
  county_points <- st_as_sf(df_wide, coords = c("Longitude", "Latitude"), crs = 4326) |>
    st_transform(crs = st_crs(illinois))
  outgroup_points <- st_as_sf(outgroup_sites, coords = c("X", "Y"), crs = 3857) |>
    st_transform(crs = st_crs(illinois))
  
  #creates the map inset
  inset_map <- ggplot() +
    geom_sf(data = usa_states, fill = "gray60", color = "white") +
    geom_sf(data = illinois, fill = "red", color = "white", alpha = 0.5) +
    geom_sf(data = missouri, fill = "red", color = "white", alpha = 0.2) +
    geom_sf(data = county_points, color = "black", size = 0.5) +
    geom_sf(data = outgroup_points, color = "blue", size = 0.5) +
    theme_void() 
  
  #builds the final map with the inset and main map
  full_map <- ggdraw() +
    draw_plot(main_map) +
    draw_plot(inset_map, x = 0.46, y = 0.04, width = 0.54, height = 0.54)
  full_map
  
  #save
  ggsave(map_name, plot = full_map,
         width = 7, height = 7.5, dpi = 300, units = "in", bg = "white")
}

