library(tidyverse)
library(ggspatial)
library(scatterpie)
library(tigris)
library(cowplot)
library(rnaturalearth)
library(sf)

#assuming you are working with the same group of populations, I have indicated
#lines of code to change if your files are different.

#lines to change look like below
### %%working directory%% ####
#set working directory
setwd("/Users/User/Desktop/REUProject/R/RforPCA&ADMIX")

#builds usa map, along with illinois
usa_states <- ne_states(country = "United States of America", returnclass = "sf")
illinois <- usa_states |> filter(name == "Illinois") |> st_transform(3857)
missouri <- usa_states |> filter(name == "Missouri") |> st_transform(3857)
usa_states <- usa_states |>
  filter(!name %in% c("Alaska", "Hawaii"))

#adds Illinois counties
options(tigris_class = "sf")
il_county <- counties(state = "IL", cb = TRUE) |> st_transform(3857)


#adds Illinois River
rivers <- st_read("UMR_ILR.shp")
rivers <- st_transform(rivers, crs = 3857)
rivers$feature <- "Illinois River"


### %%metadata file%% ####
#load in metadata
metadata <- read_csv("Boltonia_merged_data_20240925.csv") |>
  mutate(Sample_Name = paste("Boltonia", str_pad(index, 3, "left","0"), sep = "_"))


### %%piegraph adjustments%% ####
#Coordinate Adjustment to prevent piecharts from overlapping
adj_coords <- tribble(
  ~Sample_Group, ~adj_X_shift, ~adj_Y_shift,
  "Scott",        29000,       -28000,
  "Saint Clair",  55000,       -10000,
  "Madison",      45000,        35000,
  "Jersey",        8000,        25000,
  "Cass",         50000,       -20000,
  "Morgan",      -40000,       -20000,
  "Tazewell",     26000,       -45000,
  "Peoria",      -40000,        30000,
  "Frederick",   -55000,        4000,
  "Schuyler",    -15000,        25000,
  "Woodford",     25000,       -35000,
  "Marshal",      30000,        3000,
  "Fulton1",     -30000,        50000,
  "Fulton2",      40000,            0,
  "Fulton3",     -15000,        15000
)

### %%map loop, several changes in the following line of code%% ####
#I put a note after each line you could change
###map loop###
for (i in 2:10) { #change the "i in 2:10" to change the range of K values created
  map_name <- paste0("../../Figures/Boltonia_ancestry_map_K", i, ".png") #file path for figure output
  file_name <- paste0("Boltonia_Chr_1_1-23679183_pruned_data.", i, ".Q") #file name for .Q file
  pie_cols <- paste0("X", 1:i)
  
  #load in Q file for K = # as qmat
  qmat <- read_table(file_name, col_names = FALSE) |>
    mutate(Sample_Name = read_table("Boltonia_Chr_1_1-23679183_pruned_data.fam", #file name for .fam file 
                                    col_names = FALSE) |> pull(X2))|>
    left_join(metadata |> select(1:14, "Sample_Name"), by = "Sample_Name")
  
  #Swap DNA results for Boltonia_126 & 127
  cols_to_swap <- paste0("X", 1:i)
  row_126 <- which(qmat$Sample_Name == "Boltonia_126")
  row_127 <- which(qmat$Sample_Name == "Boltonia_127")
  tmp <- qmat[row_126, cols_to_swap]
  qmat[row_126, cols_to_swap] <- qmat[row_127, cols_to_swap]
  qmat[row_127, cols_to_swap] <- tmp
 
  qmat<- qmat |>   
    pivot_longer(
      cols = starts_with("X"), 
      names_to = "Ancestry", 
      values_to = "Proportion"
    ) 
  
  #nothing below should need to be adjusted for code to work
  
  qmat <- qmat |> 
    group_by(County) |> 
    mutate(
      n_MaternalLine = n_distinct(paste(MaternalLine)), 
      Sample_Group = if_else(n_MaternalLine == 1, County, paste0(County, row_number()))
    ) |> 
    ungroup() |> 
    select(-n_MaternalLine)
  
  #Adds a column sample_group to classify by instead of county
  qmat <- qmat |> 
    group_by(County, MaternalLine) |> 
    mutate(MaternalLine_group = cur_group_id()) |>
    ungroup() |> 
    group_by(County) |> 
    mutate(n_MaternalLine = n_distinct(paste(MaternalLine)),
           group_number = dense_rank(MaternalLine_group),
           Sample_Group = if_else(n_MaternalLine == 1,
                                  County,
                                  paste0(County, group_number))
    ) |> 
    ungroup() |> 
    select(-MaternalLine_group, -group_number, -n_MaternalLine)
  
  #removes seperate samplegroup for Tazewell county as there is only 1 individual in Tazewell
  qmat <- qmat |>
    mutate(
      Sample_Group = if_else(
        County == "Tazewell",
        str_remove(Sample_Group, "\\d+$"),  # remove trailing digits
        Sample_Group
      )
    )
  
  #reformat qmat to show mean proportion for each county, along with coords of each county
  qmat_summary <- qmat |> 
    group_by(County, Ancestry, Sample_Group) |> 
    summarise(
      mean_proportion = mean(Proportion),
      Latitude = first(Google_latitude),
      Longitude = first(Google_longitude),
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
      X = sf::st_coordinates(geometry)[,1],
      Y = sf::st_coordinates(geometry)[,2]
    )
  #Adjusts the position of the piecharts so they dont overlap in the final map
  df_sf <- df_sf |>
    left_join(adj_coords, by = c("Sample_Group")) |>
    mutate(
      adj_X = if_else(!is.na(adj_X_shift), X + adj_X_shift, X),
      adj_Y = if_else(!is.na(adj_Y_shift), Y + adj_Y_shift, Y)
    ) |>
    select(-adj_X_shift, -adj_Y_shift)

  #makes the code work (idk)
  df_pie <- df_sf |> 
    sf::st_drop_geometry()
  
  #creates the map
  main_map <- ggplot() +
    geom_sf(data = il_county, fill = NA, color = "gray") +
    geom_sf(data = illinois, fill = NA, color = "black") +
    geom_sf(data = rivers, aes(color = feature), size = 0.3, alpha = 0.8) +
    geom_segment(data = df_pie |> filter(adj_X != X | adj_Y != Y),
                 aes(x = X, y = Y, xend = adj_X, yend = adj_Y),
                 linetype = "solid", color = "gray50") +
    geom_point(data = df_pie,
               aes(x = X, y = Y),
               size = 1,
               color = "black") +
    geom_scatterpie(data = df_pie,
                    aes(x = adj_X, y = adj_Y, r = 15000),
                    cols = pie_cols,
                    color = NA) +
    geom_text(data = df_pie,
              aes(x = adj_X, y = adj_Y, label = Sample_Group),
              size = 2.5, nudge_y = 25000, fontface = "bold") +
    geom_text(data = df_pie,
              aes(x = adj_X, y = adj_Y, label = N),
              size = 3, fontface = "bold") +  
    coord_sf(
      xlim = c(-10197842, -9851705),
      ylim = c(4639999, 5107951),
      expand = FALSE
    )+
    annotation_scale(location = "bl", width_hint = 0.3, line_width = 0.6) +
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                           style = north_arrow_fancy_orienteering) +
    scale_fill_brewer("Ancestry", palette = "Set3") +
    scale_color_manual(
      name = NULL,
      values = c("Illinois River" = "blue")) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position.inside = c(1, 1),  
      legend.justification = c(1, 1),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_blank(), 
      axis.text = element_text(size = 7),
      panel.grid = element_blank()        
    ) +
    labs(title = "Ancestry Proportions by County",
         fill = "Ancestry")
  
  #small points on inset map
  county_points <- st_as_sf(df_wide, coords = c("Longitude", "Latitude"), crs = 4326) |>
    st_transform(crs = st_crs(illinois))
  
  #creates the map inset
  inset_map <- ggplot() +
    geom_sf(data = usa_states, fill = "gray60", color = "white") +
    geom_sf(data = illinois, fill = "red", color = "white", alpha = 0.5) +
    geom_sf(data = missouri, fill = "red", color = "white", alpha = 0.2) +
    geom_sf(data = county_points, color = "black", size = 0.5) +
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

