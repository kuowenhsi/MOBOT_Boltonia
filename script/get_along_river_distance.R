# along-river distances with sf + sfnetworks
# install.packages(c("sf","readr","dplyr","lwgeom","sfnetworks","tidygraph","units"))
library(sf)
library(readr)
library(dplyr)
library(lwgeom)
library(sfnetworks)
library(tidygraph)
library(units)

# ----------------------------
# 0) INPUTS (EDIT PATHS)
# ----------------------------
csv_path   <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/Boltonia_metadata_Pop_Index.csv"
river_shp  <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/GIS_material/Rivers/USA_Rivers_and_Streams-shp/9ae73184-d43c-4ab8-940a-c8687f61952f2020328-1-r9gw71.0odx9.shp"

# Optional: provide a river mouth/outlet (lon, lat) if you want distances FROM the mouth.
# If you leave this NULL, the script will instead compute a point-by-point distance matrix.
mouth_lonlat <- NULL
# mouth_lonlat <- c(-90.118, 38.620)  # example; set to your outlet if desired

# Choose a projected CRS in meters (good for CONUS)
crs_proj <- 5070  # NAD83 / Conus Albers (meters). Works well for IL/MO river systems.

# ----------------------------
# 1) READ DATA
# ----------------------------
rivers <- st_read(river_shp, quiet = TRUE) |> st_zm(drop = TRUE, what = "ZM")%>%
  filter((Name == "Illinois River") | (Name == "Mississippi River") | (Name == "Big Bureau Creek") | (Name == "Macoupin Creek") )%>%
  filter(State %in% c("IL", "MO", "IL-MO"))%>%
  st_crop(xmin = -91, ymin = 38.5, xmax = -89, ymax = 41.5)
plot(st_geometry(rivers), lwd = 0.5)
################

# 1) (Optional) clean a bit to avoid topology hiccups
rivers_clean <- rivers |>
  st_make_valid()

# 2) Dissolve everything into one feature (union geometry)
rivers_dissolved <- rivers_clean |>
  summarise(n_parts = dplyr::n())   # returns a single-row sf; geometry is unioned

# 3) Merge connected line segments into longer lines
st_geometry(rivers_dissolved) <- st_line_merge(st_geometry(rivers_dissolved))

# 4) If a collection appears, extract just the lines
st_geometry(rivers_dissolved) <- st_collection_extract(st_geometry(rivers_dissolved), "LINESTRING")

# 5) (Optional) force a single MULTILINESTRING geometry
st_geometry(rivers_dissolved) <- st_cast(st_geometry(rivers_dissolved), "MULTILINESTRING")

# Now you have ONE feature with all lines merged where they touch
plot(st_geometry(rivers_dissolved))

##############

# Read CSV and detect lon/lat columns
df <- read_csv(csv_path, show_col_types = FALSE)

pts <- st_as_sf(df, coords = c("Adapted_Longitude", "Adapted_Latitude"), crs = 4326)



# ----------------------------
# 2) PROJECT TO METERS
# ----------------------------
rivers_dissolved <- st_transform(rivers_dissolved, crs_proj)
pts    <- st_transform(pts,    crs_proj)
st_crs(rivers_dissolved)
st_crs(pts)

st_bbox(rivers_dissolved)
st_bbox(pts)


plot(st_geometry(rivers_dissolved), lwd = 0.5)
plot(st_geometry(pts), add = TRUE, pch = 16, cex = 1.2, col = "red")

rivers_dissolved <- rivers_dissolved |>
  st_make_valid() |>
  filter(!st_is_empty(geometry)) |>
  st_cast("MULTILINESTRING", warn = FALSE) |>
  st_cast("LINESTRING", warn = FALSE)

#######################

# 1) Build a clean network (auto-node at intersections)
net0 <- rivers_dissolved |>
  st_make_valid() |>
  st_set_precision(1) |>
  as_sfnetwork(directed = FALSE) |>
  morph(to_spatial_subdivision) |>
  unmorph() |>
  activate("edges") |>
  mutate(weight = st_length(geometry))

nodes0 <- st_as_sf(activate(net0, "nodes"))
g0     <- igraph::as.igraph(net0)
comp0  <- igraph::components(g0)$membership
nodes0$cc <- comp0

# Map each point to its blended node and component
net_blended <- st_network_blend(net0, st_geometry(pts))
nodes_b     <- st_as_sf(activate(net_blended, "nodes"))
pts_idx     <- st_nearest_feature(pts, nodes_b)

# Match those nodes back to net0's component labels (same topology after blend)
# (the blended graph has one-to-one node geometry with nodes_b)
# Attach component labels by nearest node in the original net0:
nn_in_net0  <- st_nearest_feature(nodes_b, nodes0)
pts_cc      <- nodes0$cc[nn_in_net0[pts_idx]]

table(pts_cc)             # how many points per component
print(pts_cc)             # which component each point belongs to

# For your specific case (point 4 vs 5):
i <- 4; j <- 5
cat("Point", i, "component:", pts_cc[i], "\n")
cat("Point", j, "component:", pts_cc[j], "\n")

# 2) If components differ, locate the nearest gap between the two components
if (pts_cc[i] != pts_cc[j]) {
  A <- nodes0 |> filter(cc == pts_cc[i])
  B <- nodes0 |> filter(cc == pts_cc[j])
  
  # distance between components (min over all node pairs)
  gap_len <- st_distance(st_union(A), st_union(B)) |> drop_units() |> as.numeric()
  cat("Nearest gap between components ~", round(gap_len, 2), "meters\n")
  
  # the actual shortest segment connecting them (for visualization/patching)
  gap_seg <- st_nearest_points(st_union(A), st_union(B)) |> st_cast("LINESTRING")
  gap_mid <- st_line_sample(gap_seg, sample = 0.5) |> st_cast("POINT")
  
  # quick plot to see where the gap is
  plot(st_geometry(rivers_dissolved), col="grey70", lwd=0.8)
  plot(st_geometry(A), add=TRUE, pch=16, cex=0.6, col="red")
  plot(st_geometry(B), add=TRUE, pch=16, cex=0.6, col="blue")
  plot(st_geometry(gap_seg), add=TRUE, lwd=10, col = "green")
}


# Rebuild network after snapping
net1 <- rivers_dissolved |>
  as_sfnetwork(directed = FALSE) |>
  morph(to_spatial_subdivision) |>
  unmorph() |>
  activate("edges") |>
  mutate(weight = st_length(geometry))

# Recompute distances
# IMPORTANT: recompute edge weights now that edges were split
net1_b  <- st_network_blend(net1, st_geometry(pts)) |>
  activate("edges") |>
  mutate(weight = st_length(geometry))  # meters (projected CRS)

# Recompute indices & distances
nodes_b <- st_as_sf(activate(net1_b, "nodes"))
pts_idx <- st_nearest_feature(pts, nodes_b)
Dmat2   <- st_network_cost(net1_b, from = pts_idx, to = pts_idx, weights = "weight")


# Pick two points to validate
i <- 4; j <- 5
stopifnot(is.finite(Dmat2[i, j]))

# Extract the exact path and compare lengths
sp    <- st_network_paths(net1_b, from = pts_idx[i], to = pts_idx[j], weights = "weight")
eids  <- unlist(sp$edge_paths[[1]])
edges <- st_as_sf(activate(net1_b, "edges"))[eids, ]

len_geom  <- sum(st_length(edges))           # sum of actual edge geometries
len_weight <- sum(edges$weight)              # sum of weights (should match len_geom)
cat("Sum edge geom  (m):", as.numeric(len_geom),  "\n")
cat("Sum edge weight(m):", as.numeric(len_weight),"\n")
cat("Dmat2[i,j]      (m):", as.numeric(Dmat2[i,j]), "\n")

nodes1b <- st_as_sf(activate(net1_b, "nodes"))
pts_idx1 <- st_nearest_feature(pts, nodes1b)

plot(net1_b, lwd = .5)
plot(st_geometry(nodes1b[pts_idx1,]), add = T, pch = 16, cex = 1.2, col = "red")
dim(nodes1b[pts_idx1,])


###############

# Choose the two points (indices in your pts object / Dmat2)
i <- 1
j <- 17

# Sanity check: they should be connected
stopifnot(is.finite(Dmat2[i, j]))

# Use the blended network and the indices you already created
# If your objects are named differently, rename here:
net_for_paths <- net1_b     # or net1_b / whatever you used for Dmat2
idx            <- pts_idx  # or pts_idx1

# 1) Get the edge sequence for the shortest path
sp   <- st_network_paths(net_for_paths, from = idx[i], to = idx[j], weights = "weight")
eids <- unlist(sp$edge_paths[[1]])

edges_sf <- st_as_sf(activate(net_for_paths, "edges"))
route_edges <- edges_sf[eids, ]

# 2) Merge edges to a single (multi)line and check length
route_line <- route_edges |>
  st_union() |>
  st_line_merge() |>
  st_collection_extract("LINESTRING") |>
  st_sf(geometry = _, crs = st_crs(route_edges))

len_m <- sum(st_length(route_line))
cat("Route length (m):", as.numeric(len_m), "\n")
cat("Dmat2[i,j] (m):  ", as.numeric(Dmat2[i, j]), "\n")

# 3) Plot (base graphics)
plot(st_geometry(rivers_dissolved), col = "grey85", lwd = 0.6,
     main = sprintf("Shortest path %d → %d (%.2f km)", i, j, as.numeric(len_m)/1000))
plot(st_geometry(route_edges), add = TRUE, lwd = 3)            # path
plot(st_geometry(pts[i, ]), add = TRUE, pch = 21, bg = "white", cex = 1.3) # start
plot(st_geometry(pts[j, ]), add = TRUE, pch = 21, bg = "black", col = "white", cex = 1.3) # end

# 4) (Optional) save the route as a shapefile/GeoPackage
# st_write(route_line, "route_i4_j5.shp", delete_layer = TRUE)
# st_write(route_line, "routes.gpkg", layer = sprintf("route_%02d_%02d", i, j), append = TRUE)
