library(tidyverse)
library(ggpmisc)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

replace_126_127 <- function(x){
  case_when(x == "Boltonia_126" ~ "Boltonia_127", x == "Boltonia_127" ~ "Boltonia_126", TRUE ~ x)
}

read_psc <- function(path) {
  lines <- readLines(path, warn = FALSE)
  
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

Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20250815.xlsx", na = c("NA", "", "NA (NA)"))%>%
  mutate(Pop = case_when(Pop == "Cooper Park (1995)" ~ "Cooper Park (2000)", TRUE ~ Pop))
  

Boltonia_metadata_Pop_Index <- Boltonia_metadata %>%
  group_by(Pop)%>%
  summarize(Adapted_Latitude = mean(Adapted_Latitude))%>%
  ungroup()%>%
  drop_na()%>%
  arrange(Adapted_Latitude, Pop)%>%
  mutate(Pop_Index = paste("Pop", str_pad(1:n(), width = 2, pad = "0"), sep = "_"))%>%
  select(Pop, Pop_Index,Adapted_Latitude)%>%
  mutate(Pop_Name = paste(seq(n()), Pop, sep = " - "))

Boltonia_metadata <- Boltonia_metadata %>%
  left_join(select(Boltonia_metadata_Pop_Index, c(1:2,4)), by = "Pop") %>%
  select(Sample_Name, Pop, Pop_Index, everything())%>%
  mutate(Sample_Name = replace_126_127(Sample_Name))


################

SIFT_syn <- read_tsv("./data/SIFT_result/Boltonia_decurrens_imputed_SIFTpredictions_SYNONYMOUS.tsv")%>%
  mutate(Ps = ALT_Allele_Sum/TotalAlleles)

SIFT_non <- read_tsv("./data/SIFT_result/Boltonia_decurrens_imputed_SIFTpredictions_NONSYNONYMOUS.tsv")%>%
  mutate(Pn = ALT_Allele_Sum/TotalAlleles)

SIFT_del <- read_tsv("./data/SIFT_result/Boltonia_decurrens_imputed_SIFTpredictions_NONSYNONYMOUS_DELETERIOUS.tsv")%>%
  mutate(Pd = ALT_Allele_Sum/TotalAlleles)


SIFT_combined <- SIFT_syn %>% select(Sample, Ps)%>%
  left_join(SIFT_non %>% select(Sample, Pn), by = "Sample")%>%
  left_join(SIFT_del %>% select(Sample, Pd), by = "Sample") %>%
  rename(Sample_Name = Sample)%>%
  left_join(Boltonia_metadata, by = "Sample_Name")%>%
  filter(Sample_Name != "Boltonia_149")

#########################
SIFT_syn_homo <- read_tsv("./data/SIFT_result/Boltonia_decurrens_imputed_SIFTpredictions_SYNONYMOUS_Homo.tsv")%>%
  mutate(Ps = Homozygous_ALT_Sites/NonMissingGenotypes)

SIFT_non_homo <- read_tsv("./data/SIFT_result/Boltonia_decurrens_imputed_SIFTpredictions_NONSYNONYMOUS_Homo.tsv")%>%
  mutate(Pn = Homozygous_ALT_Sites/NonMissingGenotypes)

SIFT_del_homo <- read_tsv("./data/SIFT_result/Boltonia_decurrens_imputed_SIFTpredictions_NONSYNONYMOUS_DELETERIOUS_Homo.tsv")%>%
  mutate(Pd = Homozygous_ALT_Sites/NonMissingGenotypes)

SIFT_combined_homo <- SIFT_syn_homo %>% select(Sample, Ps)%>%
  left_join(SIFT_non_homo %>% select(Sample, Pn), by = "Sample")%>%
  left_join(SIFT_del_homo %>% select(Sample, Pd), by = "Sample") %>%
  rename(Sample_Name = Sample)%>%
  left_join(Boltonia_metadata, by = "Sample_Name")%>%
  filter(Sample_Name != "Boltonia_149")

##########################

SIFT_syn_hete <- read_tsv("./data/SIFT_result/Boltonia_decurrens_imputed_SIFTpredictions_SYNONYMOUS_hete.tsv")%>%
  mutate(Ps = Heterozygous_ALT_Sites/NonMissingGenotypes)

SIFT_non_hete <- read_tsv("./data/SIFT_result/Boltonia_decurrens_imputed_SIFTpredictions_NONSYNONYMOUS_hete.tsv")%>%
  mutate(Pn = Heterozygous_ALT_Sites/NonMissingGenotypes)

SIFT_del_hete <- read_tsv("./data/SIFT_result/Boltonia_decurrens_imputed_SIFTpredictions_NONSYNONYMOUS_DELETERIOUS_hete.tsv")%>%
  mutate(Pd = Heterozygous_ALT_Sites/NonMissingGenotypes)

SIFT_combined_hete <- SIFT_syn_hete %>% select(Sample, Ps)%>%
  left_join(SIFT_non_hete %>% select(Sample, Pn), by = "Sample")%>%
  left_join(SIFT_del_hete %>% select(Sample, Pd), by = "Sample") %>%
  rename(Sample_Name = Sample)%>%
  left_join(Boltonia_metadata, by = "Sample_Name")%>%
  filter(Sample_Name != "Boltonia_149")


###############################

SIFT_total <-bind_rows(SIFT_combined %>% mutate(Load_type = "Total Load"),
                       SIFT_combined_hete %>% mutate(Load_type = "Heterozygous Load"),
                       SIFT_combined_homo %>% mutate(Load_type = "Homozygous Load"))


write_csv(SIFT_total, "./data/SIFT_result/SIFT_total_20260122.csv")


###############################

p <- ggplot(data = SIFT_combined, aes(x = Pn/(Pn + Ps), y = reorder(Pop, Adapted_Latitude)))+
  geom_violin(aes(fill = Pop), alpha = 0.7)+
  geom_point(size = 1)+
  geom_boxplot(width = 0.2, fill = NA, outlier.fill = NA, median.color = "red")+
  theme_bw()

p  

# https://nph.onlinelibrary.wiley.com/doi/10.1111/nph.70238
p <- ggplot(data = SIFT_total, aes(x = Pd/(Pn + Ps), y = reorder(Pop_Name, Adapted_Latitude)))+
  geom_violin(aes(fill = Pop), alpha = 0.7, color = NA)+
  geom_point(size = 0.5, color="gray30")+
  geom_boxplot(width = 0.2, fill = NA, outlier.shape = NA, median.color = "red", linewidth = 0.2)+
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  scale_y_discrete("")+
  ggh4x::facet_nested(.~Load_type, scales = "free_x")+
  theme_bw()+
  theme(legend.position = "none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

p  

ggsave("./figures/Genetic_load/SIFT_result_20251002.png", width = 10, height = 6, dpi = 600)

p <- ggplot(data = SIFT_combined, aes(x = Pd/(Pn + Ps), y = Pn/(Pn + Ps)))+
  geom_point(size = 1)+
  theme_bw()

p  


HET_data <- read_psc("./data/ROH_HET/Boltonia_decurrens_imputed_per_sample_het.tsv")%>%
  mutate(Total_n = nRefHom + nNonRefHom + nHets + nIndels)%>%
  mutate(Heterozygosity = nHets / (nRefHom + nNonRefHom + nHets) )%>%
  rename(Sample_Name = sample)

SIFT_combined_HET <- SIFT_combined %>%
  left_join(HET_data, by = "Sample_Name")

p <- ggplot(data = SIFT_combined_HET, aes(x = Heterozygosity, y = Pd/(Pn + Ps)))+
  geom_point(size = 1)+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  theme_bw()

p  



popmap <- SIFT_combined %>%
  select(Sample_Name, Pop_Index)

dir.create("./data/BayPass/")
write_tsv(popmap, "./data/BayPass/popmap.tsv", col_names = FALSE)
