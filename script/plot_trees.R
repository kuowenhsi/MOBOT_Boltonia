library(treedataverse)
library(readxl)
library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

replace_126_127 <- function(x){
  case_when(x == "Boltonia_126" ~ "Boltonia_127", x == "Boltonia_127" ~ "Boltonia_126", TRUE ~ x)
}

metadata <- read_xlsx("./data/DNA_stock_Boltonia.xlsx")[1:4] %>%
  left_join(read_csv("./data/Boltonia_merged_data_20240925.csv")[c(1,7:12)], by = "index")%>%
  mutate(Sample_Name = paste("Boltonia", str_pad(index, 3, "left","0"), sep = "_"))%>%
  mutate(Adapted_Longitude = case_when(is.na(Longitude) ~ Google_longitude, TRUE ~ Longitude), Adapted_Latitude = case_when(is.na(Latitude) ~ Google_latitude, TRUE ~ Latitude))%>%
  mutate(Sample_Species = case_when(index <= 468 ~ "B. decurrens", TRUE ~ MaternalLine))%>%
  select(Sample_Name,Sample_Species, everything())%>%
  mutate(Sample_Name_2 = Sample_Name)


tree_file <- read.iqtree("./data/IQTREE_all/Boltonia_all_ID_LD.min1.phy.treefile")%>%
  mutate(label = replace_126_127(label))%>%
  mutate(UFboot = case_when(UFboot > 70 ~ UFboot, TRUE ~ as.numeric(NA)))%>%
  left_join(metadata[c(1:8, 15)], by = c("label" = "Sample_Name"))%>%
  root(outgroup = "Boltonia_489", edgelabel = TRUE)


Sample_info <- tree_file@extraInfo%>%
  filter(!is.na(Sample_Name_2))%>%
  select(-node)%>%
  arrange(Sample_Name_2)

is.rooted(tree_file)
ggtree(tree_file)
  

class(tree_file@phylo)
class(tree_with_length)
ggtree(tree_with_length)

p <- tree_file %>%
  ggtree()+
  geom_nodelab(
    mapping = aes(
      x = branch,
      label = UFboot,
    ),
    size = 2,
    nudge_y = 0.38
  )+
  geom_tippoint(
    mapping = aes(
      color = Sample_Species,
      ),
    size = 2.5
  ) +
  geom_tiplab(geom = "text", aes(label = Sample_Species), offset = .2)+
  xlim(0, 43)+
  theme(legend.position='none')

p

ggsave("Boltonia_all_tree.png", height = 70, width = 8, limitsize = FALSE)


###################
p_label <- tree_file %>%
  ggtree()+
  geom_nodelab(
    mapping = aes(
      x = branch,
      label = UFboot,
    ),
    size = 2,
    nudge_y = 0.38
  )+
  geom_tippoint(
    mapping = aes(
      color = Sample_Species,
    ),
    size = 2.5
  ) +
  geom_tiplab(geom = "text", aes(label = paste(Sample_Name_2, County)), offset = .2)+
  xlim(0, 43)+
  theme(legend.position='none')

p_label

ggsave("Boltonia_all_tree_label.png", height = 70, width = 8, limitsize = FALSE)

########################

right_join(tree_file@extraInfo[c(1,9)] %>% rename(Sample_Name = Sample_Name_2), by = "Sample_Name")

qmat <- read_table("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/ADMIXTURE_hybrid/Boltonia_hybrid_ID_fillmissing_LD.5.Q", col_names = FALSE) %>%
  mutate(Sample_Name = read_table("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/ADMIXTURE_hybrid/Boltonia_hybrid_ID_fillmissing_LD.fam", col_names = FALSE) %>% pull(X2))%>%
  mutate(Sample_Name = paste("Boltonia", Sample_Name, sep = "_"))%>%
  mutate(Sample_Name = replace_126_127(Sample_Name))%>%
  left_join(tree_file@extraInfo[c(1,9)] %>% rename(Sample_Name = Sample_Name_2), by = "Sample_Name")%>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Ancestry", 
    values_to = "Proportion"
  )%>%
  select(node, Ancestry, Proportion)

tree_file


p3 <- ggplot(qmat, aes(x = Sample_Name, y = Proportion, fill = Ancestry)) +
  geom_bar(stat = "identity", width = 1) +
  theme_bw() +
  ggh4x::facet_nested(.~reorder(paste0(County, "\n",MaternalLine), Google_latitude), scales = "free_x", space = "free_x")+
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



p2 <- p + geom_facet(panel = 'bar', data = qmat, geom = geom_bar, 
                     mapping = aes(x = Proportion*20, fill = Ancestry), 
                     orientation = 'y', width = 1, stat='identity')+scale_fill_brewer(palette = "Set3")


facet_widths(p + p2, widths = c(1, 0.2))+theme(strip.background = element_blank(), strip.text.x.top = element_blank(), legend.position = "none")
  
facet_labeller(p2, c(Tree = "phylogeny", bar = "HELLO"))+theme(strip.background = element_blank(), strip.text.x.top = element_blank(), legend.position = "none", panel.spacing.x = unit(0, "in"))

ggsave("Boltonia_all_tree_admix.png", height = 70, width = 16, limitsize = FALSE)


p2 <- p_label + geom_facet(panel = 'bar', data = qmat, geom = geom_bar, 
                     mapping = aes(x = Proportion*20, fill = Ancestry), 
                     orientation = 'y', width = 1, stat='identity')+scale_fill_brewer(palette = "Set3")


facet_labeller(p2, c(Tree = "phylogeny", bar = "HELLO"))+theme(strip.background = element_blank(), strip.text.x.top = element_blank(), legend.position = "none", panel.spacing.x = unit(0, "in"))

ggsave("Boltonia_all_tree_admix_label.png", height = 70, width = 16, limitsize = FALSE)

tree_file
class(tree_file)
