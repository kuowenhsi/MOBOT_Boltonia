library(strawr)
library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

hic_file <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_inter_30.hic"

dim(readHicChroms(hic_file))
readHicChroms(hic_file)
Tr_chr <- readHicChroms(hic_file)[2:10,]$name
Tr_chr
readHicBpResolutions(hic_file)
readHicNormTypes(hic_file)

pairwise_chr <- as.data.frame(t(combn(Tr_chr,2)))%>%
  bind_rows(tibble(V1 = Tr_chr, V2 = Tr_chr))

hic_matrix_100K_KR_ALL <- tibble(x = numeric(), y = numeric(), counts=numeric())

for (i in 1:nrow(pairwise_chr)){
  
  print(i)
  CHR_pair <- str_sort(as.character(pairwise_chr[i, 1:2]), numeric = TRUE, decreasing = FALSE)

  CHR1 <- CHR_pair[[1]]
  CHR2 <- CHR_pair[[2]]
  
  if (CHR1 == CHR2){
    hic_matrix_100K_KR <- straw(norm = "KR", hic_file, chr1loc = CHR1, chr2loc = CHR2, unit = "BP", binsize = 100000, matrix = "observed")%>%
      mutate(chr1loc = CHR1, chr2loc = CHR2)
    
    hic_matrix_100K_KR_ALL <- bind_rows(hic_matrix_100K_KR_ALL, hic_matrix_100K_KR)%>%
      bind_rows(rename(hic_matrix_100K_KR, x = y, y = x))
  } else{
    hic_matrix_100K_KR <- straw(norm = "KR", hic_file, chr1loc = CHR1, chr2loc = CHR2, unit = "BP", binsize = 100000, matrix = "observed")%>%
      mutate(chr1loc = CHR1, chr2loc = CHR2)
    
    hic_matrix_100K_KR_ALL <- bind_rows(hic_matrix_100K_KR_ALL, hic_matrix_100K_KR)%>%
      bind_rows(rename(hic_matrix_100K_KR, chr1loc = chr2loc, chr2loc = chr1loc, x = y, y = x))
  }
}

# hic_matrix_100K_KR_ALL <- tibble(x = numeric(), y = numeric(), counts=numeric())
# 
# hic_matrix_100K_KR <- straw(norm = "KR", hic_file, chr1loc = "drTriRepe4Chr1", chr2loc = "drTriRepe4Chr1", unit = "BP", binsize = 100000, matrix = "observed")%>%
#   mutate(chr1loc = "drTriRepe4Chr1", chr2loc = "drTriRepe4Chr1")
# 
# hic_matrix_100K_KR_ALL <- bind_rows(hic_matrix_100K_KR_ALL, hic_matrix_100K_KR)%>%
#   bind_rows(rename(hic_matrix_100K_KR, x = y, y = x))
# 
# hic_matrix_100K_KR <- straw(norm = "KR", hic_file, chr1loc = "drTriRepe4Chr2", chr2loc = "drTriRepe4Chr2", unit = "BP", binsize = 100000, matrix = "observed")%>%
#   mutate(chr1loc = "drTriRepe4Chr2", chr2loc = "drTriRepe4Chr2")
# 
# hic_matrix_100K_KR_ALL <- bind_rows(hic_matrix_100K_KR_ALL, hic_matrix_100K_KR)%>%
#   bind_rows(rename(hic_matrix_100K_KR, x = y, y = x))
# 
# hic_matrix_100K_KR <- straw(norm = "KR", hic_file, chr1loc = "drTriRepe4Chr1", chr2loc = "drTriRepe4Chr2", unit = "BP", binsize = 100000, matrix = "observed")%>%
#   mutate(chr1loc = "drTriRepe4Chr1", chr2loc = "drTriRepe4Chr2")
# 
# hic_matrix_100K_KR_ALL <- bind_rows(hic_matrix_100K_KR_ALL, hic_matrix_100K_KR)%>%
#   bind_rows(rename(hic_matrix_100K_KR, chr1loc = chr2loc, chr2loc = chr1loc, x = y, y = x))

# hic_matrix_100K_KR <- straw(norm = "KR", hic_file, chr1loc = "drTriRepe4Chr2", chr2loc = "drTriRepe4Chr1", unit = "BP", binsize = 100000, matrix = "observed")%>%
#   mutate(chr1loc = "drTriRepe4Chr2", chr2loc = "drTriRepe4Chr1")
# 
# hic_matrix_100K_KR_ALL <- bind_rows(hic_matrix_100K_KR_ALL, hic_matrix_100K_KR)

hic_matrix_100K_KR_ALL <- hic_matrix_100K_KR_ALL %>%
  mutate(chr1loc = str_remove(chr1loc, "drTriRepe4Chr"), chr2loc = str_remove(chr2loc, "drTriRepe4Chr"))%>%
  mutate(chr2loc = factor(chr2loc, levels = str_sort(unique(chr2loc), numeric = TRUE, decreasing = TRUE)))%>%
  mutate(chr1loc = factor(chr1loc, levels = str_sort(unique(chr1loc), numeric = TRUE, decreasing = FALSE)))
  

p <- ggplot(data = hic_matrix_100K_KR_ALL, aes(x = x, y = y, fill = log10(counts)))+
  geom_raster()+
  scale_fill_distiller(palette = "RdYlBu", direction = -1, values = scales::rescale(c(0,0.3,0.7,1.7)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0,"line"),
        panel.spacing.y = unit(0,"line"),
        panel.border = element_rect(color = "gray60", fill = NA, size = 0.25),
        legend.position = c(0.88,0.12),
        legend.background = element_blank(),
        legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size = 10),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_x_continuous(name = "", expand = c(0,0))+
  scale_y_continuous(name = "", expand = c(0,0))+
  facet_grid(chr2loc ~ chr1loc, scales='free', space='free', switch = "both")

ggsave("Omnic_hap1_heatmap_small.png", width = 5, height = 5)

hist(hic_matrix_100K_KR$counts)

##################################################

hic_matrix_1M_KR_ALL <- tibble(x = numeric(), y = numeric(), counts=numeric())

for (i in 1:nrow(pairwise_chr)){
  
  print(i)
  CHR_pair <- str_sort(as.character(pairwise_chr[i, 1:2]), numeric = TRUE, decreasing = FALSE)
  
  CHR1 <- CHR_pair[[1]]
  CHR2 <- CHR_pair[[2]]
  
  if (CHR1 == CHR2){
    hic_matrix_1M_KR <- straw(norm = "NONE", hic_file, chr1loc = CHR1, chr2loc = CHR2, unit = "BP", binsize = 1e6, matrix = "observed")%>%
      mutate(chr1loc = CHR1, chr2loc = CHR2)
    
    hic_matrix_1M_KR_ALL <- bind_rows(hic_matrix_1M_KR_ALL, hic_matrix_1M_KR)%>%
      bind_rows(rename(hic_matrix_1M_KR, x = y, y = x))
  } else{
    hic_matrix_1M_KR <- straw(norm = "NONE", hic_file, chr1loc = CHR1, chr2loc = CHR2, unit = "BP", binsize = 1e6, matrix = "observed")%>%
      mutate(chr1loc = CHR1, chr2loc = CHR2)
    
    hic_matrix_1M_KR_ALL <- bind_rows(hic_matrix_1M_KR_ALL, hic_matrix_1M_KR)%>%
      bind_rows(rename(hic_matrix_1M_KR, chr1loc = chr2loc, chr2loc = chr1loc, x = y, y = x))
  }
}

hic_matrix_1M_KR_ALL <- hic_matrix_1M_KR_ALL %>%
  mutate(chr1loc = str_remove(chr1loc, "Chr_"), chr2loc = str_remove(chr2loc, "Chr_"))%>%
  mutate(chr2loc = factor(chr2loc, levels = str_sort(unique(chr2loc), numeric = TRUE, decreasing = TRUE)))%>%
  mutate(chr1loc = factor(chr1loc, levels = str_sort(unique(chr1loc), numeric = TRUE, decreasing = FALSE)))


p <- ggplot(data = hic_matrix_1M_KR_ALL, aes(x = x, y = y, fill = log10(counts)))+
  geom_raster()+
  scale_fill_distiller(palette = "RdYlBu", direction = -1, values = scales::rescale(c(0,1.7,2.2,3.5)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0,"line"),
        panel.spacing.y = unit(0,"line"),
        panel.border = element_rect(color = "gray60", fill = NA, size = 0.25),
        legend.position = c(0.88,0.12),
        legend.background = element_blank(),
        legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size = 10),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_x_continuous(name = "", expand = c(0,0))+
  scale_y_continuous(name = "", expand = c(0,0))+
  facet_grid(chr2loc ~ chr1loc, scales='free', space='free', switch = "both")

p

ggsave("./figures/Boltonia_hap1_heatmap_1M_small.png", width = 5, height = 5)

hist(log10(hic_matrix_1M_KR$counts))
