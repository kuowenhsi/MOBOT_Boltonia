library(tidyverse)
library(vcfR)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

gt_to_base <- function(gt, ref, alt) {
  if (is.na(gt) || gt %in% c(".", "./.", ".|.")) {
    return(NA_character_)
  }
  
  # keep only GT if the field is something like 0|1:35:99
  gt <- sub(":.*$", "", gt)
  
  # split phased or unphased genotype
  allele_codes <- strsplit(gt, "[/|]")[[1]]
  
  if (any(allele_codes == ".")) {
    return(NA_character_)
  }
  
  # map 0 to REF, 1 to ALT
  allele_map <- c(
    "0" = ref,
    setNames(strsplit(alt, ",", fixed = TRUE)[[1]], 
             seq_along(strsplit(alt, ",", fixed = TRUE)[[1]]))
  )
  
  # make 1|0 and 0|1 both become REF/ALT, e.g. CT instead of TC
  allele_codes <- allele_codes[order(as.integer(allele_codes))]
  
  paste0(allele_map[allele_codes], collapse = "")
}

GWAS <- read.vcfR("./data/GWAS/Boltonia_decurrens_imputed_snpeff_annotated.vcf.gz")%>%
  { .[grepl("FlowerDays_total", .@fix[, "INFO"], fixed = TRUE), ] }

GWAS@fix

Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20251010.xlsx")%>%
  filter(Sample_Species == "B. decurrens", GVCF == TRUE)%>%
  mutate(Annual = !is.na(FlowerDays.2024), Biennial = is.na(FlowerDays.2024)) %>%
  select(Sample_Name, Annual, Biennial)

GWAS_gt_long <- GWAS@gt %>%
  as.data.frame(check.names = FALSE) %>%
  select(-FORMAT) %>%
  mutate(
    Variant_ID = GWAS@fix[, "ID"],
    REF = GWAS@fix[, "REF"],
    ALT = GWAS@fix[, "ALT"],
    .before = 1
  ) %>%
  pivot_longer(
    cols = -c(Variant_ID, REF, ALT),
    names_to = "Sample_Name",
    values_to = "GT"
  ) %>%
  mutate(
    Real_GT = pmap_chr(
      list(GT, REF, ALT),
      gt_to_base
    )
  )

GWAS_Chr_3_43601669 <- Boltonia_metadata %>%
  left_join(GWAS_gt_long, by = "Sample_Name")%>%
  filter(Variant_ID == "Chr_3_43601669")%>%
  mutate(Real_GT = factor(Real_GT, levels = c("CC","CT","TT")))%>%
  group_by(Real_GT)%>%
  summarize(Annual = sum(Annual), Biennial = sum(Biennial))%>%
  pivot_longer(names_to = "Life_history", cols = c("Annual", "Biennial"))
  
GWAS_Chr_3_43601741 <- Boltonia_metadata %>%
  left_join(GWAS_gt_long, by = "Sample_Name")%>%
  filter(Variant_ID == "Chr_3_43601741")%>%
  mutate(Real_GT = factor(Real_GT, levels = c("GG","GC","CC")))%>%
  group_by(Real_GT)%>%
  summarize(Annual = sum(Annual), Biennial = sum(Biennial))%>%
  pivot_longer(names_to = "Life_history", cols = c("Annual", "Biennial"))

p <- ggplot(data = GWAS_Chr_3_43601669, aes(x = Real_GT, y = value, fill = Life_history))+
  geom_col(position = "fill")+
  geom_text(
    aes(label = after_stat(y)),
    stat = "identity",
    position = position_fill(vjust = 0.5)
  ) +
  labs(x = "Genotype", y = "Percentage of individuals", title = "Genotype at -126 bp")+
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0))
  ) +
  ggokabeito::scale_fill_okabe_ito()+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5))

p

ggsave("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/figures/glm_output/m126effect.png", width = 2.5, height = 2.5, dpi = 600)

#######
p <- ggplot(data = GWAS_Chr_3_43601741, aes(x = Real_GT, y = value, fill = Life_history))+
  geom_col(position = "fill")+
  geom_text(
    aes(label = after_stat(y)),
    stat = "identity",
    position = position_fill(vjust = 0.5)
  ) +
  labs(x = "Genotype", y = "Percentage of individuals", title = "Genotype at -54 bp")+
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0))
  ) +
  ggokabeito::scale_fill_okabe_ito()+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5))

p

ggsave("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/figures/glm_output/m54effect.png", width = 2.5, height = 2.5, dpi = 600)

