# packages
library(sf)
library(sp)
library(terra)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(raster)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

Boltonia_metadata <- Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20260205.xlsx", na = c("NA", "", "NA (NA)"))%>%
  filter(!is.na(Pop_Index))%>%
  arrange(Pop_Index)%>%
  group_by(Pop_Index)%>%
  summarize(Longitude = mean(Adapted_Longitude), Latitude = mean(Adapted_Latitude))%>%
  st_as_sf(coords = c("Longitude", "Latitude"), agr = "constant", crs = 4326)


list.files("/Users/kuowenhsi/OneDrive - Washington University in St. Louis/Drought_F3_paper/Env/wc2.1_2.5m_tavg")
tmp_layer <- raster::stack(c(paste0("/Users/kuowenhsi/OneDrive\ -\ Washington\ University\ in\ St.\ Louis/Drought_F3_paper/Env/wc2.1_2.5m_tavg/", list.files("/Users/kuowenhsi/OneDrive - Washington University in St. Louis/Drought_F3_paper/Env/wc2.1_2.5m_tavg"))))%>%raster::crop(extent(-135,-55,15,60))

tmp_extracted <- raster::extract(tmp_layer, Boltonia_metadata, df = TRUE)%>%
  mutate(ID = Boltonia_metadata$Pop_Index)%>%
  pivot_longer(cols = 2:13, names_to = "month", values_to = "avg_temp")%>%
  mutate(month = as.integer(str_remove(month, "wc2.1_2.5m_tavg_")))%>%
  group_by(ID)%>%
  mutate(GDD5 = (avg_temp - 5))%>%
  mutate(GDD5 = ifelse(GDD5 > 0, GDD5*30, 0))%>%
  mutate(GDD5 = cumsum(GDD5))%>%
  ungroup()

########

prep_layer <- stack(c(paste0("/Users/kuowenhsi/OneDrive\ -\ Washington\ University\ in\ St.\ Louis/Drought_F3_paper/Env/wc2.1_2.5m_prec/", list.files("/Users/kuowenhsi/OneDrive - Washington University in St. Louis/Drought_F3_paper/Env/wc2.1_2.5m_prec"))))%>%raster::crop(extent(-135,-55,15,60))

prep_extracted <- raster::extract(prep_layer, Boltonia_metadata, df = TRUE)%>%
  mutate(ID = Boltonia_metadata$Pop_Index)%>%
  pivot_longer(cols = 2:13, names_to = "month", values_to = "avg_prep")%>%
  mutate(month = as.integer(str_remove(month, "wc2.1_2.5m_prec_")))

#######

# ----------------------------
# SETTINGS
# ----------------------------
var_name   <- "def"  # "def" or "vpd"
start_year <- 1970
end_year   <- 2000

base_url <- "https://climate.northwestknowledge.net/TERRACLIMATE-DATA"

data_dir <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/Drought_F3_paper/Env/TerraClimate_data"
out_dir  <- file.path("./data/TerraClimate_extracted", var_name)
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir,  showWarnings = FALSE, recursive = TRUE)

# Boltonia_metadata: sf POINT with Pop_Index column
stopifnot(inherits(Boltonia_metadata, "sf"))
stopifnot("Pop_Index" %in% names(Boltonia_metadata))
if (sf::st_crs(Boltonia_metadata)$epsg != 4326) Boltonia_metadata <- sf::st_transform(Boltonia_metadata, 4326)
pts_v <- terra::vect(Boltonia_metadata)

# ----------------------------
# HELPERS
# ----------------------------
download_tc_year <- function(var_name, year, data_dir, base_url,
                             retries = 3, sleep_sec = 5) {
  fname <- paste0("TerraClimate_", var_name, "_", year, ".nc")
  fpath <- file.path(data_dir, fname)
  url   <- paste0(base_url, "/", fname)
  
  if (file.exists(fpath) && file.info(fpath)$size > 0) return(fpath)
  
  for (i in seq_len(retries)) {
    message("Downloading ", fname, " (attempt ", i, "/", retries, ")")
    ok <- tryCatch({
      download.file(url, destfile = fpath, mode = "wb", quiet = FALSE)
      TRUE
    }, error = function(e) {
      message("  Download error: ", conditionMessage(e))
      FALSE
    })
    
    # basic sanity check
    if (ok && file.exists(fpath) && file.info(fpath)$size > 1e6) return(fpath)
    
    # cleanup partial file
    if (file.exists(fpath)) file.remove(fpath)
    Sys.sleep(sleep_sec)
  }
  
  stop("Failed download after retries: ", fname)
}

extract_tc_year <- function(nc_file, pts_v, Boltonia_metadata, var_name) {
  r <- terra::rast(nc_file)
  
  vals <- terra::extract(r, pts_v)[, -1, drop = FALSE]
  tt <- terra::time(r)
  
  if (is.null(tt) || all(is.na(tt))) {
    y <- stringr::str_extract(basename(nc_file), "\\d{4}")
    tt <- seq.Date(as.Date(paste0(y, "-01-01")), by = "month", length.out = ncol(vals))
  }
  tt <- as.Date(tt)
  
  wide <- as.data.frame(vals)
  names(wide) <- paste0("m", seq_len(ncol(wide)))
  
  dplyr::bind_cols(
    tibble::tibble(Pop_Index = Boltonia_metadata$Pop_Index),
    wide
  ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("m"),
      names_to = "m",
      values_to = var_name
    ) |>
    dplyr::mutate(
      month_index = readr::parse_number(m),
      date  = tt[month_index],
      year  = as.integer(format(date, "%Y")),
      month = as.integer(format(date, "%m"))
    ) |>
    dplyr::select(Pop_Index, year, month, date, dplyr::all_of(var_name))
}

# ----------------------------
# PROCESS YEAR-BY-YEAR (save each year)
# ----------------------------
years <- start_year:end_year

status <- tibble::tibble(
  year = years,
  ok = FALSE,
  message = NA_character_
)

for (y in years) {
  out_rds <- file.path(out_dir, paste0("TerraClimate_", var_name, "_", y, ".rds"))
  out_csv <- file.path(out_dir, paste0("TerraClimate_", var_name, "_", y, ".csv"))
  
  # Resume: if already extracted, skip
  if (file.exists(out_rds) || file.exists(out_csv)) {
    message("Already extracted: ", y, " (skipping)")
    status$status[status$year == y] <- TRUE
    status$message[status$year == y] <- "already exists"
    next
  }
  
  res <- tryCatch({
    nc <- download_tc_year(var_name, y, data_dir, base_url, retries = 3, sleep_sec = 5)
    df <- extract_tc_year(nc, pts_v, Boltonia_metadata, var_name)
    
    # Save immediately (atomic-ish)
    saveRDS(df, out_rds)
    write.csv(df, out_csv, row.names = FALSE)
    
    list(ok = TRUE, msg = "ok")
  }, error = function(e) {
    list(ok = FALSE, msg = conditionMessage(e))
  })
  
  status$ok[status$year == y] <- res$ok
  status$message[status$year == y] <- res$msg
  
  if (!res$ok) message("Year ", y, " FAILED: ", res$msg)
}

print(status)

# ----------------------------
# COMBINE ALL SUCCESSFUL YEARS
# ----------------------------
year_files <- list.files(out_dir, pattern = "\\.rds$", full.names = TRUE)
tc_df <- purrr::map_dfr(year_files, readRDS) |>
  dplyr::arrange(Pop_Index, date)

tc_df

library(ggplot2)

ggplot(tc_df, aes(x = date, y = .data[[var_name]], group = Pop_Index, color = Pop_Index)) +
  geom_line() +
  theme_classic() +
  labs(x = NULL, y = var_name)

# 1) Average monthly DEF across years (climatology)
def_monthly_mean <- tc_df %>%
  group_by(Pop_Index, month) %>%
  summarise(def_mean = mean(def, na.rm = TRUE), .groups = "drop") 


readr::write_csv(def_monthly_mean, "./data/Pop_Index_monthly_DEF.csv")

# 2) Plot: three facets (one per Pop_Index)
ggplot(def_monthly_mean, aes(x = month, y = def_mean)) +
  geom_col() +
  facet_wrap(~ Pop_Index, ncol = 3) +
  theme_classic() +
  labs(x = NULL, y = "Mean monthly climatic water deficit (DEF)")


def_extracted <- readr::read_csv("./data/Pop_Index_monthly_DEF.csv")%>%
  rename(ID = Pop_Index)

comb_extracted <- left_join(tmp_extracted, prep_extracted, by = c("ID", "month"))%>%
  left_join(def_extracted, by = c("ID", "month"))%>%
  mutate(avg_prep = avg_prep, def_mean = def_mean)%>%
  pivot_longer(cols = 3:6, names_to = "env_varibles", values_to = "values")

str(comb_extracted)
unique(comb_extracted$env_varibles)
p <- ggplot(comb_extracted, aes(x = month, y = values)) +
  geom_line(aes(color = ID)) +
  stat_summary(
    aes(group = month),
    fun = mean,
    fun.min = min,
    fun.max = max,
    geom = "linerange",
    color = "black"
  ) +
  stat_summary(
    aes(group = month, label = round(after_stat(y), 1)),
    fun = min,
    geom = "text",
    vjust = 1.2
  ) +
  stat_summary(
    aes(group = month, label = round(after_stat(y), 1)),
    fun = max,
    geom = "text",
    vjust = -1.2
  ) +
  scale_color_viridis_d()+
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.12)))+
  facet_wrap(~env_varibles, scales = "free") +
  theme_bw()
p


################

library(ggplot2)
library(dplyr)
library(cowplot)
library(viridis)

# nicer y-axis labels
ylab_map <- c(
  avg_temp = "Monthly temperature (°C)",
  avg_prep = "Monthly precipitation (mm)",
  def_mean = "Monthly climate water deficit (mm)",
  GDD5     = "Cumulative growing degree days > 5°C"
)

# keep panel order the way you want in the final 2x2 layout
panel_order <- c("avg_temp", "avg_prep", "def_mean", "GDD5")

# a shared theme for all panels
pub_theme <- theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text = element_text(color = "black", size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.key.width = unit(1.2, "lines"),
    plot.margin = margin(6, 6, 6, 6)
  )

make_climate_plot <- function(var_name, tag = NULL, show_legend = TRUE) {
  dat <- comb_extracted %>%
    filter(env_varibles == var_name)
  
  ggplot(dat, aes(x = month, y = values, color = ID, group = ID)) +
    geom_line(linewidth = 0.6, alpha = 0.9) +
    
    # min-max range for each month
    stat_summary(
      aes(group = month),
      fun = mean,
      fun.min = min,
      fun.max = max,
      geom = "linerange",
      color = "black",
      linewidth = 0.5
    ) +
    
    # min labels
    stat_summary(
      aes(group = month, label = round(after_stat(y), 1)),
      fun = min,
      geom = "text",
      color = "black",
      size = 3,
      vjust = 1.3,
      show.legend = FALSE
    ) +
    
    # max labels
    stat_summary(
      aes(group = month, label = round(after_stat(y), 1)),
      fun = max,
      geom = "text",
      color = "black",
      size = 3,
      vjust = -0.8,
      show.legend = FALSE
    ) +
    
    scale_color_viridis_d(
      option = "turbo",
      end = 0.95,
      guide = guide_legend(nrow = 2)
    ) +
    scale_x_continuous(
      breaks = 1:12,
      limits = c(1, 12),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.12, 0.12))
    ) +
    labs(
      x = "Month",
      y = ylab_map[[var_name]],
      color = "Population",
      tag = tag
    ) +
    coord_cartesian(clip = "off") +
    pub_theme +
    theme(
      legend.position = if (show_legend) "bottom" else "none"
    )+
    theme(
      legend.key.width = unit(1.8, "lines"),
      legend.key.height = unit(0.8, "lines")
    ) +
    guides(
      color = guide_legend(
        nrow = 2,
        override.aes = list(linewidth = 1.8)
      )
    )
}

# make plots
p1 <- make_climate_plot("avg_temp", tag = "A", show_legend = TRUE)
p2 <- make_climate_plot("avg_prep", tag = "B", show_legend = FALSE)
p3 <- make_climate_plot("def_mean", tag = "C", show_legend = FALSE)
p4 <- make_climate_plot("GDD5", tag = "D", show_legend = FALSE)

# extract shared legend from one plot
legend <- get_legend(p1)

# remove legend from the first plot too
p1 <- p1 + theme(legend.position = "none")

# combine into 2x2 grid
panel_grid <- plot_grid(
  p1, p2, p3, p4,
  ncol = 2,
  align = "hv"
)

# add shared legend at bottom
final_plot <- plot_grid(
  panel_grid,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.12)
)

final_plot

ggsave("./figures/monthly_climate_per_pop.png", width = 10, height = 6, dpi = 600)
