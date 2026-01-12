library(treedataverse)
library(readxl)
library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

replace_126_127 <- function(x){
  case_when(x == "Boltonia_126" ~ "Boltonia_127", x == "Boltonia_127" ~ "Boltonia_126", TRUE ~ x)
}

metadata <- readxl::read_excel("Boltonia_all_metadata_20251010.xlsx")%>%
  mutate(Sample_Species = str_remove(Sample_Species, pattern = " [(]sympatric[)]"))%>%
  mutate(Pop_Index = ifelse(is.na(Pop_Index), "", Pop_Index))%>%
  mutate(New_Label = paste0(Sample_Species, " ",str_remove(Pop_Index, "Pop_"), " (", str_remove(Sample_Name, "Boltonia_"), ")"))%>%
  mutate(Sample_Name2 = Sample_Name)%>%
  dplyr::select(Sample_Name, Sample_Name2,Pop, Pop_Index, Sample_Species, New_Label)

treefile_path <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/IQTREE_all/Boltonia_all_good_ld100kb08.pruned.polymorphic.vcf.gz.min1.phy.varsites.phy.treefile"

tree_file1 <- read.iqtree(treefile_path)%>%
  mutate(label = replace_126_127(label))%>%
  mutate(UFboot = case_when(UFboot > 70 ~ UFboot, TRUE ~ as.numeric(NA)))%>%
  left_join(metadata[c(1:6)], by = c("label" = "Sample_Name"))

tree_file2 <- read.iqtree(treefile_path)%>%
  mutate(label = replace_126_127(label))%>%
  mutate(UFboot = case_when(UFboot > 70 ~ UFboot, TRUE ~ as.numeric(NA)))%>%
  left_join(metadata[c(1:6)], by = c("label" = "Sample_Name"))%>%
  as.phylo()%>%
  ape::root(outgroup = "Boltonia_489", resolve.root = TRUE) %>%
  as.treedata()

Sample_Outgroup <- metadata %>%
  filter(Sample_Species != "B. decurrens") %>%
  pull(Sample_Name)

Sample_Hybrid <- metadata %>%
  filter(Sample_Name %in% (read_tsv("./data/Hybrid_Index/samples_for_analysis.txt", col_names = "Sample_Name") %>% pull(Sample_Name)))%>%
  pull(Sample_Name)

Sample_Keep <- c(Sample_Outgroup, Sample_Hybrid) %>% unique() %>% sort() %>% intersect(tree_file2@phylo$tip.label)
Sample_Keep

# class(tree_file1)
# class(tree_file2)
# 
# drop.tip()
# 
# getMRCA()
# 
# extract.clade()
# 
# keep.tip()

plot(drop.tip(tree_file1, c(1:440)) %>% as.phylo())
drop.tip(tree_file1, extract.clade(tree_file1 %>% as.phylo(), getMRCA(as.phylo(tree_file1), c(450:470)))$tip.label)


tree_file1@data
tree_file2@data <- tree_file1@data
tree_file2@extraInfo <- tree_file1@extraInfo

tree_file2_keep <- tree_file2%>%
  keep.tip(Sample_Keep) %>%
  drop.tip(paste("Boltonia", c("216","082","223","314","347","319","451","439","464","068","419","069","340",
                               "010","166","252","005","106","256","178","042", "018", "199", "072", "461",
                               "255", "114", "368","064","196","035","466","321"), sep = "_"))

tree_file2_keep
tree_file2_keep@phylo
p <-ggtree(tree_file2_keep)+
  geom_nodelab(mapping = aes(x = branch, label = UFboot), size = 2, nudge_y = 0.38)+
  # geom_tippoint(mapping = aes(color = Sample_Species), size = 2.5) +
  geom_tiplab(geom = "text", aes(label = New_Label), offset = 0.01, align = TRUE)+
  xlim(0, 0.6)+
  theme(legend.position='none')

p

ggsave("./figures/iqtree/Boltonia_tree_keep_20251108.png", height = 10, width = 6, dpi = 900)


########################

list.files("./data/ADMIXTURE_hybrid")


# Files like: Bdecurrens_admix_20250928_K1_cv5.log ... K16 ...
files <- list.files(
  path = "./data/ADMIXTURE_hybrid/",
  pattern = "^BHybrid_admix_20251103_K\\d+_cv5\\.log$",
  full.names = TRUE
)

# Regex: capture K and the numeric value (supports decimals and scientific notation)
rx <- "^\\s*CV error \\(K=(\\d+)\\):\\s*([-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?\\d+)?)\\s*$"

CV_error <- do.call(
  rbind,
  lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    hits  <- grep(rx, lines, perl = TRUE)
    if (length(hits) == 0) return(NULL)          # no matching line in this file
    keep  <- lines[hits]                         # keep matching line(s); often length 1
    m     <- regexec(rx, keep, perl = TRUE)
    cap   <- regmatches(keep, m)
    
    # Build one row per matching line
    do.call(rbind, lapply(cap, function(x) {
      data.frame(
        file     = basename(f),
        K        = as.integer(x[2]),
        cv_error = as.numeric(x[3]),
        line     = x[1],
        stringsAsFactors = FALSE
      )
    }))
  })
)

p <- ggplot(data = CV_error, aes(x = K, y = cv_error)) +
  geom_line()+
  geom_point()+
  theme_bw()
p


ggsave("./figures/ADMIXTURE_hybrid/CV_errors_20251204.png", width = 5, height = 5, dpi = 600)
#########################################################

read_qmat <- function(K,
                      prefix = "./data/ADMIXTURE_hybrid/Boltonia_Hybrid_samples_maf0.01_ld100kb0.8.pruned.poly",
                      fam_file = paste0(prefix, ".fam"),
                      extra_info = tree_file2_keep@extraInfo) {
  
  q_file <- sprintf("%s.%d.Q", prefix, K)
  
  read_table(q_file, col_names = FALSE) %>%
    mutate(
      Sample_Name = read_table(fam_file, col_names = FALSE) %>% pull(X2),
      Sample_Name = replace_126_127(Sample_Name)
    ) %>%
    left_join(extra_info, by = c("Sample_Name" = "Sample_Name2")) %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = "Ancestry",
      values_to = "Proportion"
    )
}

qmat_list <- setNames(
  lapply(2:12, read_qmat),
  paste0("qmat_", 2:12)
)

p3 <- ggplot(qmat_2, aes(x = node, y = Proportion, fill = Ancestry)) +
  geom_col(position = position_stack(), width = 1) +
  theme_bw() +
  labs(
    title = paste("ADMIXTURE Plot (K =", "3", ")"),
    x = "Individuals",
    y = "Ancestry Proportion"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.ticks.x.top = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    plot.background = element_rect(fill = "white"),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3")

p3

tree_file2_keep@extraInfo$node
unique(qmat$node) %>% sort()

qmat_2 <- read_table("./data/ADMIXTURE_hybrid/Boltonia_Hybrid_samples_maf0.01_ld100kb0.8.pruned.poly.2.Q", col_names = FALSE) %>% mutate(Sample_Name = read_table("./data/ADMIXTURE_hybrid/Boltonia_Hybrid_samples_maf0.01_ld100kb0.8.pruned.poly.fam", col_names = FALSE) %>% pull(X2))%>% mutate(Sample_Name = replace_126_127(Sample_Name))%>% left_join(tree_file2_keep@extraInfo, by = c("Sample_Name" = "Sample_Name2"))%>% pivot_longer( cols = starts_with("X"), names_to = "Ancestry", values_to = "Proportion" )

p2 <- p + xlim(0,1.01) + 
  # geom_facet(panel = 'K=2', data = qmat_list[[1]], geom = geom_col, 
  #            mapping = aes(x = Proportion, fill = Ancestry), 
  #            orientation = 'y', width = 1, position = position_stack())+
  # geom_facet(panel = 'K=3', data = qmat_list[[2]], geom = geom_col, 
  #            mapping = aes(x = Proportion, fill = Ancestry), 
  #            orientation = 'y', width = 1, position = position_stack())+
  geom_facet(panel = 'K=4', data = qmat_list[[3]], geom = geom_col, 
                     mapping = aes(x = Proportion, fill = Ancestry), 
                     orientation = 'y', width = 1, position = position_stack())+
  geom_facet(panel = 'k=5', data = qmat_list[[4]], geom = geom_col, 
             mapping = aes(x = Proportion, fill = Ancestry), 
             orientation = 'y', width = 1, position = position_stack())+
  geom_facet(panel = 'k=6', data = qmat_list[[5]], geom = geom_col, 
             mapping = aes(x = Proportion, fill = Ancestry), 
             orientation = 'y', width = 1, position = position_stack())+
  geom_facet(panel = 'k=7', data = qmat_list[[6]], geom = geom_col, 
             mapping = aes(x = Proportion, fill = Ancestry), 
             orientation = 'y', width = 1, position = position_stack())+
  geom_facet(panel = 'k=8', data = qmat_list[[7]], geom = geom_col, 
             mapping = aes(x = Proportion, fill = Ancestry), 
             orientation = 'y', width = 1, position = position_stack())+
  geom_facet(panel = 'k=9', data = qmat_list[[8]], geom = geom_col, 
             mapping = aes(x = Proportion, fill = Ancestry), 
             orientation = 'y', width = 1, position = position_stack())+
  geom_facet(panel = 'k=10', data = qmat_list[[9]], geom = geom_col, 
             mapping = aes(x = Proportion, fill = Ancestry), 
             orientation = 'y', width = 1, position = position_stack())+
  geom_facet(panel = 'k=11', data = qmat_list[[10]], geom = geom_col, 
             mapping = aes(x = Proportion, fill = Ancestry), 
             orientation = 'y', width = 1, position = position_stack())+
  geom_facet(panel = 'k=12', data = qmat_list[[11]], geom = geom_col, 
             mapping = aes(x = Proportion, fill = Ancestry), 
             orientation = 'y', width = 1, position = position_stack())+
  scale_fill_brewer(palette = "Set3")
  

p2


  scale_fill_manual(values = c("#FB8072","#FFFFB3", "#80B1D3", "#8DD3C7",  "#BEBADA"))

p2

facet_widths(p2, widths = 5)

ggsave("./figures/iqtree/Boltonia_all_tree_admix4_12_20251204.png", height = 10, width = 25, dpi = 900)

+theme(strip.background = element_blank(), strip.text.x.top = element_blank(), legend.position = "none", panel.spacing.x = unit(0, "in"))
  
facet_labeller(p2, c(Tree = "phylogeny", bar = "HELLO"))+theme(strip.background = element_blank(), strip.text.x.top = element_blank(), legend.position = "none", panel.spacing.x = unit(0, "in"))


ggsave("./figures/iqtree/Boltonia_all_tree_admix_20251108.svg", height = 10, width = 25)


p2 <- p_label + geom_facet(panel = 'bar', data = qmat, geom = geom_bar, 
                     mapping = aes(x = Proportion, fill = Ancestry), 
                     orientation = 'y', width = 1, stat='identity')+scale_fill_brewer(palette = "Set3")


facet_labeller(p2, c(Tree = "phylogeny", bar = "HELLO"))+theme(strip.background = element_blank(), strip.text.x.top = element_blank(), legend.position = "none", panel.spacing.x = unit(0, "in"))

ggsave("./figures/iqtree/Boltonia_all_tree_admix_20251108.png", height = 70, width = 16, limitsize = FALSE)

tree_file
class(tree_file)
