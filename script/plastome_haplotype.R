library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

haplotype_data <- read_delim("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_plastome/Boltonia_plastome.biallelic_snps.no491.variable.modified_tcs.json",
                              skip = 2, delim = ", ", col_names = c("Hap_ID", "Hap_Freq", "Hap_Title", "Hap_Sample"))%>%
  filter(str_detect(Hap_ID, "id"), str_detect(Hap_Title, "Boltonia"))%>%
  mutate(Hap_ID = str_remove(Hap_ID, "\\{\"id\":"), Hap_Freq = str_remove(Hap_Freq, "frequency: "),
         Hap_Title = str_remove(Hap_Title, "title1: "), Hap_Sample = str_remove(Hap_Sample, "title2: "),
         Hap_Sample = str_remove(Hap_Sample, "\\},"))%>%
  select(Hap_ID, Hap_Freq, Hap_Sample)%>%
  # clean up types/spaces first
  mutate(
    Hap_ID   = str_trim(Hap_ID),
    Hap_Freq = as.numeric(Hap_Freq)
  ) %>%
  # split "Boltonia_001;Boltonia_002;..." into multiple rows
  separate_rows(Hap_Sample, sep = ";") %>%
  # remove any accidental spaces around sample names
  mutate(Hap_Sample = str_trim(Hap_Sample)) %>%
  left_join(read_tsv("popmap.tsv", col_names = c("Hap_Sample", "Pop_Index")), by = "Hap_Sample")%>%
  filter(!is.na(Pop_Index))%>%
  mutate(Pop_Index = factor(Pop_Index, levels = str_sort(unique(Pop_Index))))

haplotype_data_s <- haplotype_data %>%
  group_by(Hap_ID, Hap_Freq)%>%
  summarize()%>%
  ungroup() %>%
  arrange(Hap_Freq, Hap_ID)

haplotype_data <- haplotype_data %>%
  mutate(Hap_ID = factor(Hap_ID, levels = haplotype_data_s$Hap_ID))

haplotype_data_pop <- haplotype_data %>%
  group_by(Pop_Index)%>%
  mutate(Pop_Size = n())%>%
  ungroup()%>%
  group_by(Pop_Index, Pop_Size, Hap_ID)%>%
  summarize()%>%
  group_by(Pop_Index, Pop_Size)%>%
  summarise(Hap_Num = n())


haplotype_data_ID <- haplotype_data %>%
  group_by(Hap_ID, Pop_Index)%>%
  summarize()%>%
  group_by(Hap_ID)%>%
  summarize(Pop_Num = n())%>%
  ungroup()%>%
  filter((Pop_Num == 1) | (Pop_Num == max(Pop_Num)))%>%
  arrange(Pop_Num)%>%
  mutate(fill_color = c(rep("Private", n() - 1), "Ancestral"))

haplotype_data <- haplotype_data %>%
  group_by(Pop_Index, Hap_ID)%>%
  summarize(count = n())%>%
  left_join(haplotype_data_ID, by = "Hap_ID") %>%
  ungroup()%>%
  mutate(fill_color = ifelse(is.na(fill_color), "Other", fill_color))

p <- ggplot(data = haplotype_data, aes(x = count, y = Pop_Index)) +
  geom_col(position = position_stack(), aes(group = Hap_ID,fill = fill_color), color = "black", linewidth = 0.2) +
  # geom_text(data = haplotype_data_pop, aes(x = Pop_Size +3,label = Hap_Num), check_overlap = TRUE) +
  geom_line(data = haplotype_data_pop, aes(x = Hap_Num *5), group = 1, color = "#FF6580") +
  scale_fill_manual(values = c("#CCCCFF", NA,"#B1EDFF"))+
  scale_x_continuous("Haplotype composition", expand = c(0,0,0.1,0), sec.axis = sec_axis(transform = function(x){x/5}, name = "Number of distinct haplotypes"))+
  scale_y_discrete(NULL, labels = 1:17)+
  theme_bw()+
  theme(panel.grid = element_blank(), axis.title.x.top = element_text(color = "#FF6580"), axis.text.x.top = element_text(color = "#FF6580"), axis.ticks.x.top = element_line(color = "#FF6580"), legend.title = element_blank(), legend.position = c(0.85, 0.65),
        legend.key.size = unit(0.1, "in"), legend.background = element_blank())

p

ggsave("./figures/Haplotype_network/Haplotype_composition.png", height = 5, width = 3, dpi = 600)

