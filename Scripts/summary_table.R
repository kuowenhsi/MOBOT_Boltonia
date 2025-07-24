library(readr)
library(tidyverse)
library(gt)
library(webshot2)

setwd("/Users/User/Desktop/MOBOT_Boltonia")

# Read the CSV and treat "N/A", "", etc. as NA
df <- read_csv("./Data/Boltonia_Decurrens_sum.csv", na = c("", "N/A"))

df <- df %>%
  slice(1:(n() - 4)) 
df <- df %>%
  mutate(
    Latitude = sprintf("%.3f", round(Latitude, 3))
  )
df <- df %>%
  mutate(
    Longitude = sprintf("%.3f", round(Longitude, 3))
  )

df <- unite(df, "Coordinates", c(Latitude, Longitude), sep = ", ", remove = TRUE, na.rm = FALSE)

# Build the table
table <- df %>%
  select(`Maternal Line`, `Sample Group`, Location, `Location Details`, Coordinates, Count, `Flowerhead Count`) %>%
  gt() %>%
  cols_label(
    `Maternal Line` = "Maternal Line",
    `Sample Group` = "Sample Group",
    Location = "County",
    `Location Details` = "Location",
    Coordinates = "Coordinates",
    Count = "Individuals",
    `Flowerhead Count` = "Flowerheads",
  ) %>%
  fmt_number(columns = c(Count, `Flowerhead Count`), decimals = 0) %>%
  tab_source_note(
    source_note = "* coordinates from google estimates based on location details"
  ) %>%
  tab_source_note(
    source_note = "† 34 individuals sampled total, 26 were found to be misidentified"
  ) %>%
  tab_source_note(
    source_note = "‡ populations were combined for analysis"
  ) %>%
  tab_options(
    table.font.size = px(12),
    column_labels.font.weight = "bold",
    table.border.top.color = "black",
    table.border.bottom.color = "transparent"
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center", weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  )

# Print table
table

gtsave(table, "./Figures/summary_table.png")
