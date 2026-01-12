# Along-river distances (sf + sfnetworks) — cleaned, same river semantics

suppressPackageStartupMessages({
  library(sf)
  library(readr)
  library(dplyr)
  library(lwgeom)
  library(sfnetworks)
  library(tidygraph)
  library(units)
})

# ----------------------------
# Inputs
# ----------------------------
csv_path  <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/Boltonia_metadata_Pop_Index.csv"
river_shp <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/GIS_material/Rivers/USA_Rivers_and_Streams-shp/9ae73184-d43c-4ab8-940a-c8687f61952f2020328-1-r9gw71.0odx9.shp"
lon_col   <- "Adapted_Longitude"
lat_col   <- "Adapted_Latitude"
crs_proj  <- 5070   # NAD83 / Conus Albers (m)

# ----------------------------
# Rivers (kept identical in meaning)
# ----------------------------
rivers <- st_read(river_shp, quiet = TRUE) |>
  st_zm(drop = TRUE, what = "ZM") %>%
  filter((Name == "Illinois River") | (Name == "Mississippi River") |
           (Name == "Big Bureau Creek") | (Name == "Macoupin Creek")) %>%
  filter(State %in% c("IL", "MO", "IL-MO")) %>%
  st_crop(xmin = -91, ymin = 38.5, xmax = -89, ymax = 41.5)

rivers_clean <- rivers %>% st_make_valid()

rivers_dissolved <- rivers_clean %>%
  summarise(n_parts = dplyr::n())  # union geometry into one feature

st_geometry(rivers_dissolved) <- st_line_merge(st_geometry(rivers_dissolved))
st_geometry(rivers_dissolved) <- st_collection_extract(st_geometry(rivers_dissolved), "LINESTRING")

# ----------------------------
# Points
# ----------------------------
df  <- readr::read_csv(csv_path, show_col_types = FALSE)
pts <- st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326)

# ----------------------------
# Project to meters
# ----------------------------
rivers_dissolved <- st_transform(rivers_dissolved, crs_proj)%>%
  st_cast("LINESTRING", warn = FALSE)
pts              <- st_transform(pts,              crs_proj)

# ----------------------------
# Network build -> blend -> distances
# ----------------------------
net <- rivers_dissolved %>%
  as_sfnetwork(directed = FALSE) %>%
  morph(to_spatial_subdivision) %>% unmorph() %>%
  activate("edges") %>%
  mutate(w = st_length(geometry))

# Blend points; recompute weights post-blend (important!)
net_b <- net %>%
  st_network_blend(st_geometry(pts)) %>%
  activate("edges") %>%
  mutate(w = st_length(geometry))

nodes_b <- net_b %>% activate("nodes") %>% st_as_sf()
idx     <- st_nearest_feature(pts, nodes_b)

D <- st_network_cost(net_b, from = idx, to = idx, weights = "w")
D <- units::set_units(as.matrix(D), "m")

# Optional: attach readable row/col names
id_col <- intersect(c("Pop_Index"),
                    names(st_drop_geometry(pts)))
if (length(id_col)) {
  rn <- st_drop_geometry(pts)[[id_col[1]]]
  rownames(D) <- rn; colnames(D) <- rn
}
D

nms_r <- rownames(D)
nms_c <- colnames(D)
idx   <- which(upper.tri(D), arr.ind = TRUE)

df_pairs <- tibble(
  POP1        = nms_r[idx[,1]],
  POP2          = nms_c[idx[,2]],
  river_path_m  = units::drop_units(D[idx])
)

write_csv(df_pairs, "./data/Fst_by_pop/river_path_distance.csv")

# ----------------------------
# (Optional) route plotting for validation
# ----------------------------
# Precompute crop once (your bbox is in WGS84)
crop_wgs84 <- st_as_sfc(st_bbox(c(xmin=-91, ymin=38.5, xmax=-89, ymax=42), crs = 4326))
bbox_proj  <- st_bbox(st_transform(crop_wgs84, st_crs(rivers_dissolved)))

plot_route <- function(net_b, pts, idx, i, j,
                       bg_lines = rivers_dissolved,
                       bbox_proj = NULL,
                       main = NULL) {
  # shortest path edges
  sp   <- st_network_paths(net_b, from = idx[i], to = idx[j], weights = "w")
  eids <- unlist(sp$edge_paths[[1]])
  if (length(eids) == 0) {
    message(sprintf("Skip %d→%d: no path found", i, j))
    return(invisible(NULL))
  }
  
  es    <- net_b %>% activate("edges") %>% st_as_sf()
  route <- es[eids, ]
  L_km  <- as.numeric(sum(st_length(route))) / 1000
  
  # plot limits (crop to your bbox if provided)
  if (is.null(bbox_proj)) {
    bb <- st_bbox(route)
    xlim <- c(bb["xmin"], bb["xmax"])
    ylim <- c(bb["ymin"], bb["ymax"])
  } else {
    xlim <- c(bbox_proj["xmin"], bbox_proj["xmax"])
    ylim <- c(bbox_proj["ymin"], bbox_proj["ymax"])
  }
  
  # title text (default)
  if (is.null(main)) main <- sprintf("Shortest path %d → %d (%.2f km)", i, j, L_km)
  
  # tight layout (leave a sliver for title)
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
  par(mar = c(0, 0, 1.2, 0), xaxs = "i", yaxs = "i", bty = "n")
  
  if (!is.null(bg_lines)) {
    plot(st_geometry(bg_lines), xlim=xlim, ylim=ylim, col="grey85", lwd=0.6,
         axes=FALSE, xlab="", ylab="", asp=1)
  } else {
    plot(NA, xlim=xlim, ylim=ylim, axes=FALSE, xlab="", ylab="", asp=1)
  }
  plot(st_geometry(route), add=TRUE, lwd=3)
  plot(st_geometry(pts[i,]), add=TRUE, pch=21, bg="white", cex=1.2)
  plot(st_geometry(pts[j,]), add=TRUE, pch=21, bg="black", col="white", cex=1.2)
  title(main, line = 0)  # draw title
  
  invisible(route)
}

# single
plot_route(net_b, pts, idx, i=1, j=2, bbox_proj=bbox_proj)

# loop
n <- nrow(pts)
for (k in seq_len(n-1)) {
  png(filename = paste0("./figures/River_path_validation/path_", k, ".png"), width = 3, height = 5, units = "in", res = 600)
  plot_route(net_b, pts, idx, i=k, j=k+1, bbox_proj=bbox_proj)
  dev.off()
}
# ----------------------------
# (Optional) save outputs
# ----------------------------
# write.csv(as.data.frame(D), sub("\\.csv$", "_river_distance_matrix_m.csv", csv_path), row.names = TRUE)
# st_write(pts, sub("\\.csv$", "_points_projected.gpkg", csv_path), delete_dsn = TRUE)

# Quick quiet summary
message("points: ", nrow(pts), " | edges: ", nrow(net_b %>% activate('edges') %>% st_as_sf()))
