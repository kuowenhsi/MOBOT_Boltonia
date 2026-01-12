library(tidyverse)
library(vcfR)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")


# get_allele_freq(): compute allele frequency from GT-only diploid genotypes
# - Default returns REF AF; set allele = "ALT" to get ALT AF
# - Use with rowwise() so it sees a single row at a time
get_allele_freq <- function(allele = c("REF","ALT"), samples = NULL) {
  allele <- match.arg(allele)
  # If no samples passed, auto-pick sample columns in the current dplyr row
  if (is.null(samples)) samples <- pick(starts_with("Boltonia_"))
  # flatten current-row sample genotypes to a character vector
  g <- as.character(unlist(samples, use.names = FALSE))
  
  # normalize and handle missing
  g2 <- gsub("[|/]", "", g)                 # "0|1" -> "01"
  g2[g2 %in% c("..", ".", NA)] <- NA       # treat missing as NA
  
  # ALT allele count per sample ("00"=0, "01/10"=1, "11"=2)
  alt_per <- nchar(gsub("[^1]", "", g2))
  
  alt_count <- sum(alt_per, na.rm = TRUE)
  n_called  <- sum(!is.na(g2))
  denom     <- 2L * n_called
  if (denom == 0L) return(NA_real_)
  
  alt_af <- alt_count / denom
  if (allele == "ALT") alt_af else (1 - alt_af)  # REF AF
}

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

GWAS_sites <- read_tsv("./data/GWAS/Boltonia_decurrens_imputed_snpeff_annotated.vcf.gz", comment = "##")

GWAS_sites$INFO

# Keep your original data
df <- GWAS_sites

# 1) Parse INFO into key/value pairs; flags (no '=') become TRUE
info_wide <- df %>%
  mutate(.row = row_number()) %>%
  select(.row, INFO) %>%
  separate_rows(INFO, sep = ";") %>%
  mutate(
    key   = str_extract(INFO, "^[^=]+"),
    value = if_else(str_detect(INFO, "="),
                    str_replace(INFO, "^[^=]+=", ""),
                    "TRUE") # flag-only fields (e.g., KEEP)
  ) %>%
  # If the same key appears multiple times (e.g., ANN), keep all values (comma-joined)
  group_by(.row, key) %>%
  summarise(value = paste0(value, collapse = ","), .groups = "drop") %>%
  pivot_wider(names_from = key, values_from = value)

# 2) Join back to your original table
GWAS_sites_parsed <- df %>%
  mutate(.row = row_number()) %>%
  left_join(info_wide, by = ".row") %>%
  mutate(POS_APP = floor(POS/1e5))%>%
  select(1:8, POS_APP, ANN, GWAS_BETA, GWAS_P, GWAS_SE, GWAS_TRAIT, LOF, everything())%>%
  select(-.row, -INFO)%>%
  separate_wider_delim(ANN, delim = ",", names = paste("snpEFF", 1:2, sep = "_"), too_many = "merge", too_few = "align_start")%>%
  rowwise() %>%
  mutate(
    REF_AF = get_allele_freq(),            # default REF
    ALT_AF = get_allele_freq("ALT")        # ALT if needed
  ) %>%
  ungroup()%>%
  mutate(GENE = str_split_i(snpEFF_1, pattern = "[|]", i = 5))%>%
  select(1:8, REF_AF, ALT_AF, GENE, everything())
  
Boltonia_metadata <- readxl::read_xlsx("Boltonia_all_metadata_20251010.xlsx")

head(GWAS_sites_parsed)
    
sort(unique(GWAS_sites_parsed$GWAS_TRAIT))

bin_bp <- 1e6

FlowerDays_2025 <- GWAS_sites_parsed %>% filter(GWAS_TRAIT == "FlowerDays_2025") %>%
  group_by(`#CHROM`)%>%
  mutate(
    win_start = ((POS - 1L) %/% bin_bp) * bin_bp + 1L,  # 1-based start
    win_end   = win_start + bin_bp - 1L,
    POS_100K  = win_start / 1000L,                       # optional compact label
    bin_id    = sprintf("%s:%d-%d", `#CHROM`, win_start, win_end)
  )%>%
  ungroup()%>%
  mutate(bin_Name  = match(bin_id, unique(bin_id)))%>%
  group_by(bin_Name)%>%
  mutate(GWAS_P = as.numeric(GWAS_P))%>%
  mutate(IF_peak  = (GWAS_P == min(GWAS_P)), POS = midpoint_from_range(bin_id)) %>%
  select(`#CHROM`, POS, bin_Name, bin_id,ID, REF, ALT, IF_peak, everything())

write_tsv(FlowerDays_2025, "./data/GWAS/FlowerDays_2025_bin.tsv")

Num_peak = seq(1, nrow(FlowerDays_2025))[FlowerDays_2025$IF_peak == TRUE]

for (i in Num_peak){

  FlowerDays_2025_peak <- FlowerDays_2025[i,] %>%
    mutate(GWAS_BETA = ifelse(REF_AF < 0.5, -as.numeric(GWAS_BETA), as.numeric(GWAS_BETA)))%>%
    pivot_longer(names_to = c("Sample_Name"), cols = starts_with("Boltonia"), values_to = "Genotype")%>%
    mutate(Genotype = case_when(Genotype == "0|0" ~ "Ref/Ref",
                                Genotype == "0|1" ~ "Ref/Alt",
                                Genotype == "1|0" ~ "Ref/Alt",
                                Genotype == "1|1" ~ "Alt/Alt"))%>%
    mutate(Genotype = factor(Genotype, levels = c("Ref/Ref", "Ref/Alt", "Alt/Alt")))%>%
    left_join(Boltonia_metadata, by = "Sample_Name")
  
  p <- ggplot(data = FlowerDays_2025_peak, aes(x = Genotype, y = FlowerDays.2025))+
    geom_violin(aes(fill = Genotype), alpha = 0.7, show.legend = FALSE, color = NA, width = 0.5)+
    geom_point(size = 0.2)+
    geom_boxplot(width = 0.1, outlier.shape = NA, fill = NA, median.color = "black")+
    geom_text(label = paste0("ID:", FlowerDays_2025_peak$bin_Name, "\n",
                            FlowerDays_2025_peak$ID[[1]], ", Beta =",
                            round(as.numeric(FlowerDays_2025_peak$GWAS_BETA[[1]]), 2), ", -log(p) =",
                            round(-log10(as.numeric(FlowerDays_2025_peak$GWAS_P[[1]])), 2)), x = 0.5, y = Inf, check_overlap = TRUE, vjust = 1.5, hjust = 0)+
    stat_summary(fun.data = function(y) {data.frame(ymin = sum(!is.na(y)), y = min(y, na.rm = TRUE))},        # return n for each x
                 geom = "text",
                 aes(label = after_stat(ymin)),              # y is the returned value
                 vjust = 2)+
    stat_summary(geom = "line", fun = "mean", color = "red", group = 1)+
    stat_summary(geom = "point", fun = "mean", color = "black", fill = "red", shape = 23, size = 4)+
    scale_x_discrete(name = NULL, labels = c(paste0(FlowerDays_2025_peak$REF[[1]], "/",FlowerDays_2025_peak$REF[[1]]),
                                paste0(FlowerDays_2025_peak$REF[[1]], "/",FlowerDays_2025_peak$ALT[[1]]),
                                paste0(FlowerDays_2025_peak$ALT[[1]], "/",FlowerDays_2025_peak$ALT[[1]])))+
    scale_y_continuous(expand = c(0.2,0.1,0.2,0.1), 
                       limits = c(min(FlowerDays_2025_peak$FlowerDays.2025, na.rm = TRUE), 
                                  max(FlowerDays_2025_peak$FlowerDays.2025, na.rm = TRUE)))+
    theme_bw()
  p
  
  ggsave(paste0("./figures/glm_output/FlowerDays_2025_", i, ".png"), height = 4, width = 4, dpi = 600)
}

############
FlowerDays_2024 <- GWAS_sites_parsed %>% filter(GWAS_TRAIT == "FlowerDays_2024") %>%
  group_by(`#CHROM`)%>%
  mutate(
    win_start = ((POS - 1L) %/% bin_bp) * bin_bp + 1L,  # 1-based start
    win_end   = win_start + bin_bp - 1L,
    POS_100K  = win_start / 1000L,                       # optional compact label
    bin_id    = sprintf("%s:%d-%d", `#CHROM`, win_start, win_end)
  )%>%
  ungroup()%>%
  mutate(bin_Name  = match(bin_id, unique(bin_id)))%>%
  group_by(bin_Name)%>%
  mutate(GWAS_P = as.numeric(GWAS_P))%>%
  mutate(IF_peak  = (GWAS_P == min(GWAS_P))) %>%
  select(`#CHROM`, POS, bin_Name, bin_id,ID, REF, ALT, IF_peak, everything())

write_tsv(FlowerDays_2024, "./data/GWAS/FlowerDays_2024_bin.tsv")

Num_peak = sum(FlowerDays_2024$IF_peak)

for (i in 1:Num_peak){
  
  FlowerDays_2024_peak <- FlowerDays_2024 %>%
    filter(IF_peak == TRUE, bin_Name == i) %>%
    mutate(GWAS_BETA = ifelse(REF_AF < 0.5, -as.numeric(GWAS_BETA), as.numeric(GWAS_BETA)))%>%
    pivot_longer(names_to = c("Sample_Name"), cols = starts_with("Boltonia"), values_to = "Genotype")%>%
    mutate(Genotype = case_when(Genotype == "0|0" ~ "Ref/Ref",
                                Genotype == "0|1" ~ "Ref/Alt",
                                Genotype == "1|0" ~ "Ref/Alt",
                                Genotype == "1|1" ~ "Alt/Alt"))%>%
    mutate(Genotype = factor(Genotype, levels = c("Ref/Ref", "Ref/Alt", "Alt/Alt")))%>%
    left_join(Boltonia_metadata, by = "Sample_Name")
  
  p <- ggplot(data = FlowerDays_2024_peak, aes(x = Genotype, y = FlowerDays.2024))+
    geom_violin(aes(fill = Genotype), alpha = 0.7, show.legend = FALSE, color = NA, width = 0.5)+
    geom_point(size = 0.2)+
    geom_boxplot(width = 0.1, outlier.shape = NA, fill = NA, median.color = "black")+
    geom_text(label = paste0("ID:", FlowerDays_2024_peak$bin_Name, "\n",
                             FlowerDays_2024_peak$ID[[1]], ", Beta =",
                             round(as.numeric(FlowerDays_2024_peak$GWAS_BETA[[1]]), 2), ", -log(p) =",
                             round(-log10(as.numeric(FlowerDays_2024_peak$GWAS_P[[1]])), 2)), x = 0.5, y = Inf, check_overlap = TRUE, vjust = 1.5, hjust = 0)+
    stat_summary(fun.data = function(y) {data.frame(ymin = sum(!is.na(y)), y = min(y, na.rm = TRUE))},        # return n for each x
                 geom = "text",
                 aes(label = after_stat(ymin)),              # y is the returned value
                 vjust = 2)+
    stat_summary(geom = "line", fun = "mean", color = "red", group = 1)+
    stat_summary(geom = "point", fun = "mean", color = "black", fill = "red", shape = 23, size = 4)+
    scale_x_discrete(name = NULL, labels = c(paste0(FlowerDays_2024_peak$REF[[1]], "/",FlowerDays_2024_peak$REF[[1]]),
                                             paste0(FlowerDays_2024_peak$REF[[1]], "/",FlowerDays_2024_peak$ALT[[1]]),
                                             paste0(FlowerDays_2024_peak$ALT[[1]], "/",FlowerDays_2024_peak$ALT[[1]])))+
    scale_y_continuous(expand = c(0.2,0.1,0.2,0.1), 
                       limits = c(min(FlowerDays_2024_peak$FlowerDays.2024, na.rm = TRUE), 
                                  max(FlowerDays_2024_peak$FlowerDays.2024, na.rm = TRUE)))+
    theme_bw()
  p
  
  ggsave(paste0("./figures/glm_output/FlowerDays_2024_", i, ".png"), height = 4, width = 4, dpi = 600)
}


###########
Num_Stems <- GWAS_sites_parsed %>% filter(GWAS_TRAIT == "Num_Stems") %>%
  group_by(`#CHROM`)%>%
  mutate(
    win_start = ((POS - 1L) %/% bin_bp) * bin_bp + 1L,  # 1-based start
    win_end   = win_start + bin_bp - 1L,
    POS_100K  = win_start / 1000L,                       # optional compact label
    bin_id    = sprintf("%s:%d-%d", `#CHROM`, win_start, win_end)
  )%>%
  ungroup()%>%
  mutate(bin_Name  = match(bin_id, unique(bin_id)))%>%
  group_by(bin_Name)%>%
  mutate(GWAS_P = as.numeric(GWAS_P))%>%
  mutate(IF_peak  = (GWAS_P == min(GWAS_P))) %>%
  select(`#CHROM`, POS, bin_Name, bin_id,ID, REF, ALT, IF_peak, everything())

write_tsv(Num_Stems, "./data/GWAS/Num_Stems_2025_bin.tsv")

Num_peak = seq(1, nrow(Num_Stems))[Num_Stems$IF_peak == TRUE]

for (i in Num_peak){
  
  Num_Stems_peak <- Num_Stems[i,] %>%
    mutate(GWAS_BETA = ifelse(REF_AF < 0.5, -as.numeric(GWAS_BETA), as.numeric(GWAS_BETA)))%>%
    pivot_longer(names_to = c("Sample_Name"), cols = starts_with("Boltonia"), values_to = "Genotype")%>%
    mutate(Genotype = case_when(Genotype == "0|0" ~ "Ref/Ref",
                                Genotype == "0|1" ~ "Ref/Alt",
                                Genotype == "1|0" ~ "Ref/Alt",
                                Genotype == "1|1" ~ "Alt/Alt"))%>%
    mutate(Genotype = factor(Genotype, levels = c("Ref/Ref", "Ref/Alt", "Alt/Alt")))%>%
    left_join(Boltonia_metadata, by = "Sample_Name")
  
  p <- ggplot(data = Num_Stems_peak, aes(x = Genotype, y = Num.Stems.2025))+
    geom_violin(aes(fill = Genotype), alpha = 0.7, show.legend = FALSE, color = NA, width = 0.5)+
    geom_point(size = 0.2)+
    geom_boxplot(width = 0.1, outlier.shape = NA, fill = NA, median.color = "black")+
    geom_text(label = paste0("ID:", Num_Stems_peak$bin_Name, "\n",
                             Num_Stems_peak$ID[[1]], ", Beta =",
                             round(as.numeric(Num_Stems_peak$GWAS_BETA[[1]]), 2), ", -log(p) =",
                             round(-log10(as.numeric(Num_Stems_peak$GWAS_P[[1]])), 2)), x = 0.5, y = Inf, check_overlap = TRUE, vjust = 1.5, hjust = 0)+
    stat_summary(fun.data = function(y) {data.frame(ymin = sum(!is.na(y)), y = min(y, na.rm = TRUE))},        # return n for each x
                 geom = "text",
                 aes(label = after_stat(ymin)),              # y is the returned value
                 vjust = 2)+
    stat_summary(geom = "line", fun = "mean", color = "red", group = 1)+
    stat_summary(geom = "point", fun = "mean", color = "black", fill = "red", shape = 23, size = 4)+
    scale_x_discrete(name = NULL, labels = c(paste0(Num_Stems_peak$REF[[1]], "/",Num_Stems_peak$REF[[1]]),
                                             paste0(Num_Stems_peak$REF[[1]], "/",Num_Stems_peak$ALT[[1]]),
                                             paste0(Num_Stems_peak$ALT[[1]], "/",Num_Stems_peak$ALT[[1]])))+
    scale_y_continuous(expand = c(0.2,0.1,0.2,0.1), 
                       limits = c(min(Num_Stems_peak$Num.Stems.2025, na.rm = TRUE), 
                                  max(Num_Stems_peak$Num.Stems.2025, na.rm = TRUE)))+
    theme_bw()
  p
  
  ggsave(paste0("./figures/glm_output/Num_Stems_2025_", i, ".png"), height = 4, width = 4, dpi = 600)
}


###########
###########
Stem_Length <- GWAS_sites_parsed %>% filter(GWAS_TRAIT == "Stem_Length") %>%
  group_by(`#CHROM`)%>%
  mutate(
    win_start = ((POS - 1L) %/% bin_bp) * bin_bp + 1L,  # 1-based start
    win_end   = win_start + bin_bp - 1L,
    POS_100K  = win_start / 1000L,                       # optional compact label
    bin_id    = sprintf("%s:%d-%d", `#CHROM`, win_start, win_end)
  )%>%
  ungroup()%>%
  mutate(bin_Name  = match(bin_id, unique(bin_id)))%>%
  group_by(bin_Name)%>%
  mutate(GWAS_P = as.numeric(GWAS_P))%>%
  mutate(IF_peak  = (GWAS_P == min(GWAS_P))) %>%
  select(`#CHROM`, POS, bin_Name, bin_id,ID, REF, ALT, IF_peak, everything())

write_tsv(Stem_Length, "./data/GWAS/Stem_Length_20242025_bin.tsv")

Num_peak = seq(1, nrow(Stem_Length))[Stem_Length$IF_peak == TRUE]

for (i in Num_peak){
  
  Stem_Length_peak <- Stem_Length[i,] %>%
    mutate(GWAS_BETA = ifelse(REF_AF < 0.5, -as.numeric(GWAS_BETA), as.numeric(GWAS_BETA)))%>%
    pivot_longer(names_to = c("Sample_Name"), cols = starts_with("Boltonia"), values_to = "Genotype")%>%
    mutate(Genotype = case_when(Genotype == "0|0" ~ "Ref/Ref",
                                Genotype == "0|1" ~ "Ref/Alt",
                                Genotype == "1|0" ~ "Ref/Alt",
                                Genotype == "1|1" ~ "Alt/Alt"))%>%
    mutate(Genotype = factor(Genotype, levels = c("Ref/Ref", "Ref/Alt", "Alt/Alt")))%>%
    left_join(Boltonia_metadata, by = "Sample_Name")
  
  p <- ggplot(data = Stem_Length_peak, aes(x = Genotype, y = Stem.Length.2025))+
    geom_violin(aes(fill = Genotype), alpha = 0.7, show.legend = FALSE, color = NA, width = 0.5)+
    geom_point(size = 0.2)+
    geom_boxplot(width = 0.1, outlier.shape = NA, fill = NA, median.color = "black")+
    geom_text(label = paste0("ID:", Stem_Length_peak$bin_Name, "\n",
                             Stem_Length_peak$ID[[1]], ", Beta =",
                             round(as.numeric(Stem_Length_peak$GWAS_BETA[[1]]), 2), ", -log(p) =",
                             round(-log10(as.numeric(Stem_Length_peak$GWAS_P[[1]])), 2)), x = 0.5, y = Inf, check_overlap = TRUE, vjust = 1.5, hjust = 0)+
    stat_summary(fun.data = function(y) {data.frame(ymin = sum(!is.na(y)), y = min(y, na.rm = TRUE))},        # return n for each x
                 geom = "text",
                 aes(label = after_stat(ymin)),              # y is the returned value
                 vjust = 2)+
    stat_summary(geom = "line", fun = "mean", color = "red", group = 1)+
    stat_summary(geom = "point", fun = "mean", color = "black", fill = "red", shape = 23, size = 4)+
    scale_x_discrete(name = NULL, labels = c(paste0(Stem_Length_peak$REF[[1]], "/",Stem_Length_peak$REF[[1]]),
                                             paste0(Stem_Length_peak$REF[[1]], "/",Stem_Length_peak$ALT[[1]]),
                                             paste0(Stem_Length_peak$ALT[[1]], "/",Stem_Length_peak$ALT[[1]])))+
    scale_y_continuous(expand = c(0.2,0.1,0.2,0.1), 
                       limits = c(min(Stem_Length_peak$Stem.Length.2025, na.rm = TRUE), 
                                  max(Stem_Length_peak$Stem.Length.2025, na.rm = TRUE)))+
    theme_bw()
  p
  
  ggsave(paste0("./figures/glm_output/Stem_Length_2025_", i, ".png"), height = 4, width = 4, dpi = 600)
}


###########
###########
FlowerDays_total <- GWAS_sites_parsed %>% filter(GWAS_TRAIT == "FlowerDays_total") %>%
  group_by(`#CHROM`)%>%
  mutate(
    win_start = ((POS - 1L) %/% bin_bp) * bin_bp + 1L,  # 1-based start
    win_end   = win_start + bin_bp - 1L,
    POS_100K  = win_start / 1000L,                       # optional compact label
    bin_id    = sprintf("%s:%d-%d", `#CHROM`, win_start, win_end)
  )%>%
  ungroup()%>%
  mutate(bin_Name  = match(bin_id, unique(bin_id)))%>%
  group_by(bin_Name)%>%
  mutate(GWAS_P = as.numeric(GWAS_P))%>%
  mutate(IF_peak  = (GWAS_P == min(GWAS_P))) %>%
  select(`#CHROM`, POS, bin_Name, bin_id,ID, REF, ALT, IF_peak, everything())

write_tsv(FlowerDays_total, "./data/GWAS/FlowerDays_total_bin.tsv")

Num_peak = seq(1, nrow(FlowerDays_total))[FlowerDays_total$IF_peak == TRUE]

for (i in Num_peak){
  
  FlowerDays_total_peak <- FlowerDays_total[i,] %>%
    mutate(GWAS_BETA = ifelse(REF_AF < 0.5, -as.numeric(GWAS_BETA), as.numeric(GWAS_BETA)))%>%
    pivot_longer(names_to = c("Sample_Name"), cols = starts_with("Boltonia"), values_to = "Genotype")%>%
    mutate(Genotype = case_when(Genotype == "0|0" ~ "Ref/Ref",
                                Genotype == "0|1" ~ "Ref/Alt",
                                Genotype == "1|0" ~ "Ref/Alt",
                                Genotype == "1|1" ~ "Alt/Alt"))%>%
    mutate(Genotype = factor(Genotype, levels = c("Ref/Ref", "Ref/Alt", "Alt/Alt")))%>%
    left_join(Boltonia_metadata, by = "Sample_Name")
  
  p <- ggplot(data = FlowerDays_total_peak, aes(x = Genotype, y = FlowerDays.total))+
    geom_violin(aes(fill = Genotype), alpha = 0.7, show.legend = FALSE, color = NA, width = 0.5)+
    geom_point(size = 0.2)+
    geom_boxplot(width = 0.1, outlier.shape = NA, fill = NA, median.color = "black")+
    geom_text(label = paste0("ID:", FlowerDays_total_peak$bin_Name, "\n",
                             FlowerDays_total_peak$ID[[1]], ", Beta =",
                             round(as.numeric(FlowerDays_total_peak$GWAS_BETA[[1]]), 2), ", -log(p) =",
                             round(-log10(as.numeric(FlowerDays_total_peak$GWAS_P[[1]])), 2)), x = 0.5, y = Inf, check_overlap = TRUE, vjust = 1.5, hjust = 0)+
    stat_summary(fun.data = function(y) {data.frame(ymin = sum(!is.na(y)), y = min(y, na.rm = TRUE))},        # return n for each x
                 geom = "text",
                 aes(label = after_stat(ymin)),              # y is the returned value
                 vjust = 2)+
    stat_summary(geom = "line", fun = "mean", color = "red", group = 1)+
    stat_summary(geom = "point", fun = "mean", color = "black", fill = "red", shape = 23, size = 4)+
    scale_x_discrete(name = NULL, labels = c(paste0(FlowerDays_total_peak$REF[[1]], "/",FlowerDays_total_peak$REF[[1]]),
                                             paste0(FlowerDays_total_peak$REF[[1]], "/",FlowerDays_total_peak$ALT[[1]]),
                                             paste0(FlowerDays_total_peak$ALT[[1]], "/",FlowerDays_total_peak$ALT[[1]])))+
    scale_y_continuous(expand = c(0.2,0.1,0.2,0.1), 
                       limits = c(min(FlowerDays_total_peak$FlowerDays.total, na.rm = TRUE), 
                                  max(FlowerDays_total_peak$FlowerDays.total, na.rm = TRUE)))+
    theme_bw()
  p
  
  ggsave(paste0("./figures/glm_output/FlowerDays_total_", i, ".png"), height = 4, width = 4, dpi = 600)
}


###########
FlowerDays_2024 <- GWAS_sites_parsed %>% filter(GWAS_TRAIT == "FlowerDays_2024")
FlowerDays_total <- GWAS_sites_parsed %>% filter(GWAS_TRAIT == "FlowerDays_total")
Num_Stems <- GWAS_sites_parsed %>% filter(GWAS_TRAIT == "Num_Stems")
Stem_Length <- GWAS_sites_parsed %>% filter(GWAS_TRAIT == "Stem_Length")
