library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

replace_126_127 <- function(x){
  case_when(x == "Boltonia_126" ~ "Boltonia_127", x == "Boltonia_127" ~ "Boltonia_126", TRUE ~ x)
}

Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20250815.xlsx")%>%
  mutate(Pop = case_when(Pop == "Cooper Park (1995)" ~ "Cooper Park (2000)", TRUE ~ Pop))%>%
  mutate(Sample_Name = replace_126_127(Sample_Name))


Morgan_samples <- Boltonia_metadata %>%
  filter(County == "Morgan")%>%
  arrange(FlowerHead)%>%
  group_by(FlowerHead) %>%
  slice_sample(n = 1) %>%
  ungroup()%>%
  select(Sample_Name)


write_tsv(Morgan_samples, "Morgan_independent_sample_list.txt",col_names = FALSE)

ROH_data <- read_tsv("./data/ROH_HET/Boltonia_decurrens_imputed_ROH.roh.RG.tsv", 
                     col_names = c("Sample_Name", "chrom", "start", "end", "length_bp", "n_snps", "qual"))%>%
  mutate(color_category = case_when(length_bp >= 40e3 ~ ">40Kb", length_bp >= 20e3 ~ "20Kb-40Kb", length_bp >=5e3 ~ "5Kb-20Kb", TRUE ~ as.character(NA)))%>%
  filter(!is.na(color_category))%>%
  left_join(Boltonia_metadata, by = "Sample_Name")

hist(ROH_data$length_bp, breaks = 100)
quantile(ROH_data$length_bp)

ROH_data_sumamry <- ROH_data %>%
  group_by(Sample_Name, Pop, Adapted_Latitude)%>%
  summarize(ROH_length = sum(length_bp))%>%
  group_by(Pop)%>%
  mutate(n_sample = n())


p <- ggplot(data = ROH_data, aes(y = Sample_Name, x = length_bp/1e6))+
  geom_col(aes(fill = color_category), position = "stack", width = 1)+
  geom_boxplot(data = ROH_data_sumamry, aes(x = ROH_length/1e6, y = n_sample/2, group = Pop), width = 5, outlier.color = NA, fill= "white", alpha =0.5)+
  scale_y_discrete("", expand = c(0, 0))+
  scale_x_continuous("Summed ROH length (Mb)", expand = c(0,0))+
  scale_fill_manual(values = c("#bbdc59", "#8acd9a", "#1d7052"))+
  facet_wrap(.~reorder(Pop, desc(Adapted_Latitude)), scale = "free_y", space = "free_y", strip.position = "right")+
  theme_bw()+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),strip.text.y = element_text(angle = 0), panel.spacing = unit(0.1, "in"), panel.grid = element_blank())

ggsave("ROH.png", height = 10, width = 6, dpi = 600)


ROH_data_40 <- ROH_data %>%
  filter(color_category == ">40Kb")%>%
  group_by(Sample_Name)%>%
  summarize(ROH_40 = sum(length_bp))%>%
  mutate(ROH_frac = ROH_40/437.8e6)

read_psc <- function(path) {
  lines <- readLines("./data/ROH_HET/Boltonia_decurrens_imputed_per_sample_het.tsv", warn = FALSE)
  
  # 1) first '# PSC...' header
  hdr_line <- grep("^#\\s*PSC\\b", lines, value = TRUE)[2]
  if (is.na(hdr_line)) stop("No '# PSC' header line found.")
  header <- strsplit(sub("^#\\s*", "", hdr_line), "\t", fixed = TRUE)[[1]]
  # strip bracketed indices like [2], [3], ...
  header <- trimws(gsub("^\\[\\d+\\]", "", header))
  
  # 2) PSC data lines only
  dat_lines <- grep("^PSC\\t", lines, value = TRUE)
  if (length(dat_lines) == 0) stop("No 'PSC' data lines found.")
  
  # 3) read PSC block
  tc <- textConnection(paste(dat_lines, collapse = "\n"))
  on.exit(close(tc), add = TRUE)
  df <- read.table(tc, sep = "\t", header = FALSE,
                   quote = "", comment.char = "", stringsAsFactors = FALSE,
                   na.strings = c(".", "NA"), check.names = FALSE)
  colnames(df) <- header
  
  # 4) keep PSC + sample as character, numeric-ify the rest
  keep_char <- intersect(c("PSC", "sample"), colnames(df))
  num_cols <- setdiff(colnames(df), keep_char)
  if (length(num_cols)) {
    df[num_cols] <- lapply(df[num_cols], function(x) type.convert(x, as.is = TRUE))
  }
  
  # quick sanity check
  if (!"sample" %in% colnames(df)) {
    warning("Column 'sample' not found after header normalization. Names are: ",
            paste(colnames(df), collapse = ", "))
  }
  
  df
}


HET_data <- read_psc("./data/ROH_HET/Boltonia_decurrens_imputed_per_sample_het.tsv")%>%
  mutate(Total_n = nRefHom + nNonRefHom + nHets + nIndels)%>%
  mutate(Heterozygosity = nHets / (nRefHom + nNonRefHom + nHets) )

ROH_data_40_HET <- ROH_data_40 %>%
  left_join(HET_data, by = c("Sample_Name" = "sample"))%>%
  left_join(Boltonia_metadata, by = "Sample_Name")

ROH_data_40_HET_Pop <-ROH_data_40_HET %>%
  group_by(Pop)%>%
  summarize(Heterozygosity_median = median(Heterozygosity), 
            Heterozygosity_max = quantile(Heterozygosity, 0.75), 
            Heterozygosity_min = quantile(Heterozygosity, 0.25), 
            ROH_frac_median = median(ROH_frac), 
            ROH_frac_max = quantile(ROH_frac, probs = 0.75), 
            ROH_frac_min = quantile(ROH_frac, 0.25),
            Adapted_Latitude = median(Adapted_Latitude))%>%
  arrange(Adapted_Latitude)%>%
  mutate(Pop_index = 1:n())%>%
  mutate(Pop = paste(Pop_index, Pop, sep = " - "))

p <- ggplot(data = ROH_data_40_HET, aes(x = Heterozygosity, y = ROH_frac))+
  geom_point(color = "gray80")+
  geom_point(data = ROH_data_40_HET_Pop, aes(color = reorder(Pop, desc(Adapted_Latitude)), shape = reorder(Pop, desc(Adapted_Latitude))))+
  scale_shape_manual(values = rep(c(21,22,23,24), length = 17))+
  theme_bw()

p

p <- ggplot(data = ROH_data_40_HET_Pop, aes(x = Heterozygosity_median, y = ROH_frac_median))+
  geom_errorbar(aes(ymax = ROH_frac_max, ymin = ROH_frac_min, color = reorder(Pop, desc(Adapted_Latitude))))+
  # geom_errorbar(aes(xmax = Heterozygosity_max, xmin = Heterozygosity_min, color = reorder(Pop, desc(Adapted_Latitude))))+
  geom_point(aes(fill = reorder(Pop, desc(Adapted_Latitude)), shape = reorder(Pop, desc(Adapted_Latitude))), size = 3)+
  geom_text(aes(y = ROH_frac_min - 0.01, label = Pop_index))+
  scale_y_continuous("Fraction of RON (>40Kb)")+
  scale_shape_manual(values = rep(c(21,22,23,24), length = 17))+
  theme_bw()+
  theme(legend.title = element_blank())

p

p + theme(legend.position = "none")
