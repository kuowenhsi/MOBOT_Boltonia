library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

midpoint_from_range <- function(x, return = c("numeric", "expr")) {
  return <- match.arg(return)
  # Extract start/end with a simple regex: "<chr>:<start>-<end>"
  parts <- utils::strcapture(
    pattern = "^[^:]+:(\\d+)-(\\d+)$",
    x = x,
    proto = list(start = double(), end = double())
  )
  # parts$start / parts$end will be NA if a string doesn't match the pattern
  mid <- (parts$start + parts$end) / 2
  
  if (return == "numeric") {
    return(mid)
  } else {
    # Build "(start + end)/2" as a character vector
    out <- ifelse(
      is.na(mid),
      NA_character_,
      paste0("(", format(parts$start, scientific = FALSE), " + ",
             format(parts$end, scientific = FALSE), ")/2")
    )
    return(out)
  }
}

bld <- read_tsv("./data/LD_heatmap/BIN_2_CHR1_19305925.binned_ld.tsv")


extract_start <- function(x) as.integer(sub(".*:(\\d+)-.*", "\\1", x))

min(extract_start(bld$bin_i))
max(extract_start(bld$bin_i))

all_bins <- bld %>%
  transmute(
    bin = bin_i,
    start = extract_start(bin_i)
  ) %>%
  bind_rows(
    bld %>%
      transmute(
        bin = bin_j,
        start = extract_start(bin_j)
      )
  ) %>%
  distinct(bin, .keep_all = TRUE) %>%
  arrange(start) 

starts <- all_bins$start
bins   <- all_bins$bin

# chromosome string from bin, e.g. "1" from "1:17310001-17320000"
chr <- sub(":.*", "", bins[1])

# all expected starts between min and max, step 10000
full_starts <- seq(min(starts), max(starts), by = 10000L)

# which starts are missing?
missing_starts <- setdiff(full_starts, starts)

missing_bins <- tibble(
  start = sort(missing_starts),
  end   = start + 9999L,
  bin   = sprintf("%s:%d-%d", chr, start, end)
)

all_bins_filled <- all_bins %>%
  bind_rows(missing_bins %>% select(bin, start)) %>%
  arrange(start)%>%
  pull(bin)



bld2 <- bld %>%
  mutate(
    bin_i = factor(bin_i, levels = all_bins_filled),
    bin_j = factor(bin_j, levels = all_bins_filled)
  ) %>%
  tidyr::complete(bin_i, bin_j)

# after you built `all_bins` and `bld2` like before
n_bins <- length(all_bins_filled)

bin_labels <- bld2 %>%
  filter(bin_i == bin_j) %>%
  mutate(POSITION = midpoint_from_range(bin_i))%>%
  arrange(POSITION) %>%
  mutate(x = -0.5 + seq(n_bins), y = -0.5 + seq(n_bins))

targets <- c(18e6, 19e6, 20e6, 21e6)

closest_bins <- map_dfr(targets, ~ {
  tgt <- .
  bin_labels %>%
    mutate(dist = abs(POSITION - tgt)) %>%
    slice_min(dist, n = 1) %>%   # pick the closest row
    mutate(target = tgt)
})

closest_bins  

p <- ggplot(bld2, aes(y = bin_i, x = bin_j, fill = mean_r2)) +
  geom_tile(color = NA) +
  geom_segment(data = closest_bins, aes(x = x, y = y, xend = x - 3, yend = y +3), linewidth = 0.2)+
  geom_text(data = closest_bins, aes(x = x, y = y, label = round(POSITION/1e6, 2)), angle = -45, hjust = 1.2)+
  scale_fill_viridis_c(
    option = "D",
    na.value = "white",
    limits = c(0, 0.015),    # <-- focus low values
    oob = scales::squish    # >0.3 will just look like 0.3
  ) +
  coord_fixed() +
  labs(
    x = NULL,
    y = NULL,
    fill = expression(mean~r^2)
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = NA, color = NA),
    legend.position = "none"
  )

p

ggsave(filename = "./figures/LD_heatmap/BIN2_20251118.png", height = 3.5, width = 3.5)

library(grid)
library(ggplot2)

g <- ggplotGrob(p)

grid.newpage()
pushViewport(viewport(angle = -45))  # rotate whole thing 45° clockwise
grid.draw(g)
popViewport()

p <- ggplot(bld_wide, aes(x = bin_i, y = bin_j, fill = mean_r2)) +
  geom_tile() +
  scale_fill_viridis_c(option = "D", na.value = "white", limits = c(0, 1)) +
  coord_fixed() +
  labs(
    x = NULL,
    y = NULL,
    fill = expression(mean~r^2)
  ) +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid = element_blank()
  )

p

