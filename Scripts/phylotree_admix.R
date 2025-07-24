library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggtree)
library(ape)
library(ggstance)
library(RColorBrewer)

# ======= USER SETTINGS: CHANGE THESE AS NEEDED =======

# Working directory
work_dir <- "/Users/User/Desktop/MOBOT_Boltonia"

# Tree file (Nexus format)
tree_fp <- "./Data/IQTREE_all/Boltonia_all_ID_LD.min1.phy.treefile"

# Pop info CSV path
pop_info_fp <- "./Data/population_id.csv"

# Admixture file prefix (used for both .Q and .fam files)
admix_prefix <- "./Data/Hybrid_dataset/Boltonia_admixture/Boltonia_hybrid_ID_fillmissing_LD."

# Number of ancestries (corresponds to the number in ".K.Q" filename)
k <- 8

# Color palette for ancestry bars (optional: choose any RColorBrewer palette)
color_palette <- "Set1"

# ======= END USER SETTINGS =======

# Set working directory
setwd(work_dir)

# Read tree
tree <- read.tree(tree_fp)

# --- Root the tree ---
outgroup_tips <- "Boltonia_489"  # modify here if needed
rerooted_tree <- root(tree, outgroup_tips)
tree_ordered <- ladderize(rerooted_tree, right = TRUE)

# Read population info
pop_info <- read.csv(pop_info_fp) |> 
  select(-Pop)  # remove 'Pop' column if exists

# Swap rows 126 and 127 if needed (custom logic - comment out if unused)
cols_to_swap <- !(colnames(pop_info) %in% "IID")
row_126 <- which(pop_info$IID == "126")
row_127 <- which(pop_info$IID == "127")
tmp <- pop_info[row_126, cols_to_swap]
pop_info[row_126, cols_to_swap] <- pop_info[row_127, cols_to_swap]
pop_info[row_127, cols_to_swap] <- tmp

# Read admixture files
admix <- read_table(paste0(admix_prefix, k, ".Q"), col_names = FALSE)
fam <- read_table(paste0(admix_prefix, "fam"), col_names = FALSE)

# Use IID as-is from fam file
fam_ids <- fam |>
  rename(IID = X2) |>
  select(IID)

# Bind IID to admixture data and join pop info
admix <- bind_cols(admix, fam_ids)

# Fix IID format to match tree tip labels
pop_info <- pop_info |>
  mutate(IID = paste0("Boltonia_", str_pad(as.character(IID), width = 3, pad = "0")))
admix <- admix |>
  mutate(IID = paste0("Boltonia_", str_pad(as.character(IID), width = 3, pad = "0")))

# Join population info with admixture
admix_pop <- admix |>
  left_join(pop_info, by = "IID") |>
  filter(IID %in% tree_ordered$tip.label) |>
  arrange(match(IID, tree_ordered$tip.label))

# Create new labels: "001 (cass)"
label_df <- admix_pop |>
  mutate(
    short_ID = str_extract(IID, "\\d+$"),
    new_label = paste0(str_pad(short_ID, 3, pad = "0"), " (", Sample_Group, ")")
  ) |>
  select(IID, new_label)

# Replace tip labels AFTER filtering
tree_ordered$tip.label <- label_df$new_label[match(tree_ordered$tip.label, label_df$IID)]

# Prepare long-format data for plotting
ancestry_cols <- paste0("X", 1:k)
admix_long <- admix_pop |>
  pivot_longer(cols = all_of(ancestry_cols), names_to = "Ancestry", values_to = "Proportion") |>
  left_join(label_df, by = "IID") |>
  mutate(IID = factor(new_label, levels = label_df$new_label))
# Define fill colors for ancestries
fill_colors <- brewer.pal(max(3, k), color_palette)[1:k]
names(fill_colors) <- ancestry_cols

#### Plotting ####

# Base tree with new tip labels
p <- ggtree(tree_ordered, branch.length = "none") +
  geom_tiplab(size = 3)

# Add admixture bars as side panel
tree_plot <- p + 
  geom_facet(
    panel = paste0("Admixture K", k),
    data = admix_long,
    geom = ggstance::geom_barh,
    mapping = aes(x = Proportion, fill = Ancestry),
    stat = "identity",
    width = 1
  ) +
  scale_fill_manual(values = fill_colors) +
  theme(legend.position = "none")

tree_plot <- facet_widths(tree_plot, widths = c(2.5, 1))  # Tree : Admixture width ratio

# Display final plot
tree_plot

##################

ggsave(
  filename = paste0("./Figures/tree_with_admixture_K",k,".png"),  # Change filename as needed
  plot = tree_plot,
  width = 25,     # width in inches
  height = 49,    # height in inches
  dpi = 300       # resolution
)
