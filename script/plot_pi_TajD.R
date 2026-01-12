library(tidyverse)
library(data.table)
library(qvalue)
library(cowplot)
library(zoo)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")


# Load chromosome information from a .fasta index file
chr_len_temp <- read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0.fasta.fai", col_names = c("chr", "end")) %>%
  select(1:2) %>%
  mutate(start = 1) %>%
  mutate(chr = as.integer(str_remove(chr, "Chr_"))) %>%
  arrange(chr) %>%
  mutate(lag_pos = lag(end, default = 0)) %>%
  mutate(pos_pad = cumsum(lag_pos)) %>%
  mutate(padded_start = start + pos_pad - 1, padded_end = end + pos_pad) %>%
  mutate(padded_chr_pos = (padded_start + padded_end) / 2)%>%
  drop_na()


# Download LFMM_output_20230606.txt from Dryad https://doi.org/10.5061/dryad.s7h44j1fd

glm_path <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/Pi_TajD/Pop_Group/"
glm_input <- sort(list.files("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/Pi_TajD/Pop_Group"))
length(glm_input)

xpclr_path <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/XPCLR/"
xpclr_input <- sort(list.files("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/XPCLR"))
length(glm_input)
xpclr_input

read_xpclr <- function(x){
  fread(file = paste0(xpclr_path, xpclr_input[[x]]))[,1:15]%>%
    rename(chr = chrom)%>%
    mutate(POSITION = (as.numeric(start) + as.numeric(stop))/2, chr = as.integer(chr), xpclr = as.numeric(xpclr))%>%
    mutate(xpclr = ifelse(is.na(xpclr), 0, xpclr))%>%
    range_sel()%>%
    left_join(chr_len_temp, by = "chr") %>%
    mutate(padded_pos = POSITION + pos_pad)%>%
    mutate(xpclr_smooth = rollapply(xpclr,
                                    width = 5,              # average across 5 consecutive windows
                                    FUN   = mean,
                                    fill  = NA,
                                    align = "center"))
}

Pi_input <- sort(list.files("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/Pi_TajD/Pop_Index", pattern = ".windowed.pi" ))

TajD_input <- sort(list.files("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/Pi_TajD/Pop_index", pattern = ".Tajima.D" ))

Pi_data <- bind_rows(lapply(paste0(glm_path, Pi_input), fread), .id = "Pop_Index")%>%
  filter(N_VARIANTS >= 100) %>%
  group_by(Pop_Index)%>%
  summarize(PI_median = median(PI), PI_25 = quantile(PI, 0.25), PI_75 = quantile(PI, 0.75))


TajD_data <- bind_rows(lapply(paste0(glm_path, TajD_input), fread), .id = "Pop_Index")%>%
  filter(N_SNPS >= 100)%>%
  group_by(Pop_Index)%>%
  summarize(TajimaD_median = median(TajimaD), TajimaD_25 = quantile(TajimaD, 0.25), TajimaD_75 = quantile(TajimaD, 0.75))

Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20251010.xlsx")%>%
  group_by(Pop, Pop_Index)%>%
  summarize()%>%
  drop_na()%>%
  ungroup()%>%
  arrange(Pop_Index)%>%
  mutate(Pop_Index = as.integer(factor(Pop_Index)))


PI_TajD <- Pi_data %>%
  left_join(TajD_data, by = "Pop_Index")%>%
  mutate(Pop_Index = as.integer(Pop_Index))%>%
  left_join(Boltonia_metadata, by = "Pop_Index")%>%
  arrange(Pop_Index)%>%
  mutate(shape_number = (Pop_Index + 3)%%4 + 21)

BayPass_data <- read_tsv("./data/BayPass/intersect_sig_tbl.tsv")%>%
  rename(chr = CHR, POSITION = POS)%>%
  left_join(chr_len_temp, by = "chr") %>%
  mutate(padded_pos = POSITION + pos_pad)

BayPass_Keys <- read_tsv("./data/BayPass/Intersect_keys.tsv")%>%
  rename(chr = CHROM, POSITION = POS)%>%
  left_join(chr_len_temp, by = "chr") %>%
  mutate(padded_pos = POSITION + pos_pad)

p <- ggplot(data = Pi_data, aes(x = Pop_Index, y = PI))+
  geom_violin(aes(fill = Pop_Index), alpha = 0.7, show.legend = FALSE)+
  geom_boxplot(width = 0.2, outlier.shape = NA)+
  scale_y_continuous(limits = c(0, 0.013))+
  theme_bw()

ggsave("./figures/Pi_TajD_output/Total_PI_Pop.png", width = 10, height = 5, dpi = 600)

p <- ggplot(data = TajD_data, aes(x = Pop_Index, y = TajimaD))+
  geom_violin(aes(fill = Pop_Index), alpha = 0.7, show.legend = FALSE)+
  geom_boxplot(width = 0.2, outlier.shape = NA)+
  theme_bw()

ggsave("./figures/Pi_TajD_output/Total_TajimaD_Pop.png", width = 10, height = 5, dpi = 600)


p <- ggplot(data = PI_TajD, aes(x = PI_median, y = TajimaD_median, fill = Pop))+
  geom_point(aes(shape = I(shape_number)), show.legend = FALSE, size = 4)+
  geom_text(aes(label = Pop_Index), size = 4, position = position_nudge(x = 0.000025, y = - 0.04), show.legend = FALSE)+
  theme_bw()
p

ggsave("./figures/Pi_TajD_output/Total_TajimaD_PI.png", width = 4, height = 4, dpi = 600)

XtX_BP <- fread("./data/BayPass/Boltonia_decurrens_imputed_maf_XtX_XtX_summary_pi_xtx.out.gz", header = TRUE)

Bay_BP <- fread("./data/BayPass/Boltonia_decurrens_imputed_maf_ENV_ENV_summary_betai_reg.out.gz", header = TRUE)

chr_pos_mrk <- fread("./data/BayPass/Boltonia_decurrens_imputed_maf_XtX.chr_pos_mrk.tsv.gz", header = TRUE)
head(chr_pos_mrk)

keys <- read_tsv("./data/BayPass/Intersect_keys.tsv")%>%
  left_join(chr_len_temp, by = c("CHROM" = "chr"))%>%
  mutate(padded_pos = POS + pos_pad)

GWAS_sig <- read_tsv("./data/GWAS/GLM_sig_markers_all.tsv")

XtX_BP_mrk <- left_join(XtX_BP, chr_pos_mrk, by = "MRK")%>%
  left_join(chr_len_temp, by = c("CHR" = "chr"))%>%
  left_join(GWAS_sig, by = c("CHR" = "#CHROM", "POS"))%>%
  mutate(padded_pos = POS + pos_pad)%>%
  mutate(color = "gray90")%>%
  mutate(color = ifelse(.$`log10(1/pval)` > 4.532257, "red", color))

XtX_BP_mrk_sig <- XtX_BP_mrk %>%
  filter(color == "red")

XtX_BP_mrk_GWAS <- XtX_BP_mrk %>%
  filter(!is.na(GWAS_TRAIT))


Bay_BP_mrk <- left_join(Bay_BP, chr_pos_mrk, by = "MRK")%>%
  filter(COVARIABLE == 2, eBPis < 10)%>%
  left_join(chr_len_temp, by = c("CHR" = "chr"))%>%
  left_join(GWAS_sig, by = c("CHR" = "#CHROM", "POS"))%>%
  mutate(padded_pos = POS + pos_pad)%>%
  mutate(color = "gray90")%>%
  mutate(color = ifelse(.$eBPis > 2.759296, "red", color))

Bay_BP_mrk_sig <- Bay_BP_mrk %>%
  filter(color == "red")

Bay_BP_mrk_GWAS <- Bay_BP_mrk %>%
  filter(!is.na(GWAS_TRAIT))

intersect_sig <- intersect(XtX_BP_mrk_sig$MRK, Bay_BP_mrk_sig$MRK)
glm_input
seq_along(glm_input)
i = 1
k = 1



for (i in 1:6){
  
  for (k in BayPass_Keys$Intersect_Name){
    range_sel <- function(x) filter(x, chr == BayPass_Keys$chr[[k]], (POSITION > BayPass_Keys$POSITION[[k]] - 3e6) & (POSITION < BayPass_Keys$POSITION[[k]] + 3e6))
    
    range_sel2 <- function(x) filter(x, CHR == BayPass_Keys$chr[[k]], (POS > BayPass_Keys$POSITION[[k]] - 3e6) & (POS < BayPass_Keys$POSITION[[k]] + 3e6))
    
    Pi_data <- fread(file = paste0(glm_path, glm_input[[2*i - 1]]))%>%
      filter(N_VARIANTS >= 10)%>%
      rename(chr = CHROM)%>%
      mutate(POSITION = (BIN_START + BIN_END)/2)%>%
      range_sel()%>%
      left_join(chr_len_temp, by = "chr") %>%
      mutate(padded_pos = POSITION + pos_pad)
    
    TajD_data <- fread(file = paste0(glm_path, glm_input[[2*i]]))%>%
      filter(N_SNPS >= 10)%>%
      rename(chr = CHROM)%>%
      mutate(POSITION = (BIN_START + 5000))%>%
      range_sel()%>%
      left_join(chr_len_temp, by = "chr") %>%
      mutate(padded_pos = POSITION + pos_pad)%>%
      mutate(N_SNPS_smooth = rollapply(N_SNPS,
                                      width = 10,              # average across 5 consecutive windows
                                      FUN   = mean,
                                      fill  = NA,
                                      align = "center"))
    
    xpclr_data <- read_xpclr(5*(i - 1) + 1)
    
    xpclr_p1 <- ggplot(data = xpclr_data, aes(x = POSITION, y = xpclr_smooth))+
      geom_vline(data = XtX_BP_mrk_sig %>% range_sel2(), aes(xintercept = POS), color = "#66DDAA", alpha = 0.7)+
      geom_vline(data = Bay_BP_mrk_sig %>% range_sel2(), aes(xintercept = POS), color = "#66DDAA", alpha = 0.7)+
      geom_vline(data = BayPass_data %>% range_sel(), aes(xintercept = POSITION), color = "red", alpha = 0.7)+
      geom_line(linewidth = 0.3, group = 1) +
      annotate(geom = "text",label = paste0(" Target: ", xpclr_data$POP_A, "\n", "Reference: ",xpclr_data$POP_B), x = -Inf, y = Inf, size = 3, hjust = -0.2, vjust = 1.2, check_overlap = TRUE)+
      # annotate(geom = "text",label = BayPass_Keys$bin_id[[k]], x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1)+
      scale_x_continuous(NULL, expand = c(0, 0), labels = function(x){x/1e6}) +
      scale_y_continuous(expression("XP-CLR")) +
      theme_bw() +
      theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
            axis.text.x = element_blank()) +
      guides(fill = "none")
    
    xpclr_data <- read_xpclr(5*(i - 1) + 2)
    
    xpclr_p2 <- ggplot(data = xpclr_data, aes(x = POSITION, y = xpclr_smooth))+
      geom_vline(data = XtX_BP_mrk_sig %>% range_sel2(), aes(xintercept = POS), color = "#66DDAA", alpha = 0.7)+
      geom_vline(data = Bay_BP_mrk_sig %>% range_sel2(), aes(xintercept = POS), color = "#66DDAA", alpha = 0.7)+
      geom_vline(data = BayPass_data %>% range_sel(), aes(xintercept = POSITION), color = "red", alpha = 0.7)+
      geom_line(linewidth = 0.3, group = 1) +
      annotate(geom = "text",label = paste0(" Target: ", xpclr_data$POP_A, "\n", "Reference: ",xpclr_data$POP_B), x = -Inf, y = Inf, size = 3, hjust = -0.2, vjust = 1.2, check_overlap = TRUE)+
      # annotate(geom = "text",label = BayPass_Keys$bin_id[[k]], x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1)+
      scale_x_continuous(NULL, expand = c(0, 0), labels = function(x){x/1e6}) +
      scale_y_continuous(expression("XP-CLR")) +
      theme_bw() +
      theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
            axis.text.x = element_blank()) +
      guides(fill = "none")
    
    xpclr_data <- read_xpclr(5*(i - 1) + 3)
    
    xpclr_p3 <- ggplot(data = xpclr_data, aes(x = POSITION, y = xpclr_smooth))+
      geom_vline(data = XtX_BP_mrk_sig %>% range_sel2(), aes(xintercept = POS), color = "#66DDAA", alpha = 0.7)+
      geom_vline(data = Bay_BP_mrk_sig %>% range_sel2(), aes(xintercept = POS), color = "#66DDAA", alpha = 0.7)+
      geom_vline(data = BayPass_data %>% range_sel(), aes(xintercept = POSITION), color = "red", alpha = 0.7)+
      geom_line(linewidth = 0.3, group = 1) +
      annotate(geom = "text",label = paste0(" Target: ", xpclr_data$POP_A, "\n", "Reference: ",xpclr_data$POP_B), x = -Inf, y = Inf, size = 3, hjust = -0.2, vjust = 1.2, check_overlap = TRUE)+
      # annotate(geom = "text",label = BayPass_Keys$bin_id[[k]], x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1)+
      scale_x_continuous(NULL, expand = c(0, 0), labels = function(x){x/1e6}) +
      scale_y_continuous(expression("XP-CLR")) +
      theme_bw() +
      theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
            axis.text.x = element_blank()) +
      guides(fill = "none")
    
    xpclr_data <- read_xpclr(5*(i - 1) + 4)
    
    xpclr_p4 <- ggplot(data = xpclr_data, aes(x = POSITION, y = xpclr_smooth))+
      geom_vline(data = XtX_BP_mrk_sig %>% range_sel2(), aes(xintercept = POS), color = "#66DDAA", alpha = 0.7)+
      geom_vline(data = Bay_BP_mrk_sig %>% range_sel2(), aes(xintercept = POS), color = "#66DDAA", alpha = 0.7)+
      geom_vline(data = BayPass_data %>% range_sel(), aes(xintercept = POSITION), color = "red", alpha = 0.7)+
      geom_line(linewidth = 0.3, group = 1) +
      annotate(geom = "text",label = paste0(" Target: ", xpclr_data$POP_A, "\n", "Reference: ",xpclr_data$POP_B), x = -Inf, y = Inf, size = 3, hjust = -0.2, vjust = 1.2, check_overlap = TRUE)+
      # annotate(geom = "text",label = BayPass_Keys$bin_id[[k]], x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1)+
      scale_x_continuous(NULL, expand = c(0, 0), labels = function(x){x/1e6}) +
      scale_y_continuous(expression("XP-CLR")) +
      theme_bw() +
      theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
            axis.text.x = element_blank()) +
      guides(fill = "none")
    
    xpclr_data <- read_xpclr(5*(i - 1) + 5)
    
    xpclr_p5 <- ggplot(data = xpclr_data, aes(x = POSITION, y = xpclr_smooth))+
      geom_vline(data = XtX_BP_mrk_sig %>% range_sel2(), aes(xintercept = POS), color = "#66DDAA", alpha = 0.7)+
      geom_vline(data = Bay_BP_mrk_sig %>% range_sel2(), aes(xintercept = POS), color = "#66DDAA", alpha = 0.7)+
      geom_vline(data = BayPass_data %>% range_sel(), aes(xintercept = POSITION), color = "red", alpha = 0.7)+
      geom_line(linewidth = 0.3, group = 1) +
      annotate(geom = "text",label = paste0(" Target: ", xpclr_data$POP_A, "\n", "Reference: ",xpclr_data$POP_B), x = -Inf, y = Inf, size = 3, hjust = -0.2, vjust = 1.2, check_overlap = TRUE)+
      # annotate(geom = "text",label = BayPass_Keys$bin_id[[k]], x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1)+
      scale_x_continuous(NULL, expand = c(0, 0), labels = function(x){x/1e6}) +
      scale_y_continuous(expression("XP-CLR")) +
      theme_bw() +
      theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
            axis.text.x = element_blank()) +
      guides(fill = "none")
    
    
    XtX_BP_mrk_x <- XtX_BP_mrk %>% range_sel2()
    XtX_BP_mrk_GWAS_x <- XtX_BP_mrk_GWAS %>% range_sel2()
    
    Bay_BP_mrk_x <- Bay_BP_mrk %>% range_sel2()
    Bay_BP_mrk_GWAS_x <- Bay_BP_mrk_GWAS %>% range_sel2()
    
    # Plot result of PCadapt analysis
    pPiTajD <- ggplot() +
      # geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
      # geom_vline(data = Stem_Length %>% rename(chr = `#CHROM`, POSITION = POS) %>% range_sel(), aes(xintercept = POSITION), color = "green")+
      geom_vline(data = XtX_BP_mrk_sig %>% range_sel2(), aes(xintercept = POS), color = "#66DDAA", alpha = 0.3)+
      geom_vline(data = Bay_BP_mrk_sig %>% range_sel2(), aes(xintercept = POS), color = "#66DDAA", alpha = 0.3)+
      geom_vline(data = BayPass_data %>% range_sel(), aes(xintercept = POSITION), color = "red", alpha = 1)+
      geom_line(data = Pi_data, aes(x = POSITION, y = PI), linewidth = 0.3) +
      geom_line(data = TajD_data, aes(x = POSITION, y = TajimaD/120 - 0.02), color = "#FF8B2A", linewidth = 0.3) +
      annotate(geom = "text",label = str_split_i(glm_input[[2*i - 1]], "[.]", i = 1), x = -Inf, y = Inf, size = 5, hjust = 0, vjust = 1)+
      # annotate(geom = "text",label = BayPass_Keys$bin_id[[k]], x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1)+
      scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
      scale_x_continuous(paste("Chromosome", unique(Pi_data$chr), "(Mbp)"), expand = c(0, 0), labels = function(x){x/1e6}) +
      scale_y_continuous(expression("Pi"), sec.axis = sec_axis(transform = function(x){(x + 0.02)*120}, name = "Tajima's D")) +
      theme_bw() +
      theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines"), axis.title.y.right = element_text(color = "#FF8B2A"),
            axis.text.y.right = element_text(color = "#FF8B2A"), axis.ticks.y.right = element_line(color = "#FF8B2A")) +
      guides(fill = "none")+
      coord_cartesian(clip = TRUE)
    
    pCov <- ggplot()+
      geom_ribbon(data = TajD_data, aes(x = POSITION, ymax = N_SNPS_smooth), ymin = 0,fill = "#00AAFF") +
      annotate(geom = "text",label = str_split_i(glm_input[[2*i - 1]], "[.]", i = 1), x = -Inf, y = Inf, size = 3, hjust = 0, vjust = 1)+
      # annotate(geom = "text",label = BayPass_Keys$bin_id[[k]], x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1)+
      scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
      scale_x_continuous(NULL, expand = c(0, 0), labels = function(x){x/1e6}) +
      scale_y_sqrt(expression("# sites"), expand = c(0,0.1,0.1,0.1)) +
      theme_bw() +
      theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
            axis.text.x = element_blank())
    pCov
    
    p1 <- ggplot(data = XtX_BP_mrk_x, aes(x = POS, y = XtXst))+
      geom_point(data = filter(XtX_BP_mrk_x, color == "gray90"), color = "gray90", size = .1, show.legend = FALSE)+
      geom_point(data = filter(XtX_BP_mrk_x, color == "red"), color = "#66DDAA", size = 0.3, show.legend = FALSE)+
      geom_point(data = XtX_BP_mrk_GWAS_x, color = "#00AAFF", size = 0.3)+
      geom_point(data = filter(XtX_BP_mrk_x, MRK %in% intersect_sig), color = "red", size = 0.3, show.legend = FALSE)+
      annotate(geom = "label",label = BayPass_Keys$Intersect_Name[[k]], x = -Inf, y = Inf, size = 5, hjust = -0.2, vjust = 1.1)+
      scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
      scale_x_continuous(NULL, expand = c(0, 0), labels = function(x){x/1e6}) +
      scale_y_continuous("XtXst", expand = c(0, 0, 0.1, 0.1)) +
      theme_bw() +
      theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
            axis.text.x = element_blank()) +
      guides(fill = "none")
    
    p2 <- ggplot(data = Bay_BP_mrk_x, aes(x = POS, y = eBPis))+
      geom_point(data = filter(Bay_BP_mrk_x, color == "gray90"), color = "gray90", size = .1, show.legend = FALSE)+
      geom_point(data = filter(Bay_BP_mrk_x, color == "red"), color = "#66DDAA", size = 0.3, show.legend = FALSE)+
      geom_point(data = Bay_BP_mrk_GWAS_x, color = "#00AAFF", size = 0.3)+
      geom_point(data = filter(Bay_BP_mrk_x, MRK %in% intersect_sig), color = "red", size = 0.3, show.legend = FALSE)+
      annotate(geom = "label",label = BayPass_Keys$Intersect_Name[[k]], x = -Inf, y = Inf, size = 5, hjust = -0.2, vjust = 1.1)+
      scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
      scale_x_continuous(NULL, expand = c(0, 0), labels = function(x){x/1e6}) +
      scale_y_continuous("eBPis", expand = c(0, 0, 0.1, 0.1)) +
      theme_bw() +
      theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
            axis.text.x = element_blank()) +
      guides(fill = "none")
    
    p_comb <- plot_grid(p1, p2, xpclr_p1, xpclr_p2,xpclr_p3,xpclr_p4,xpclr_p5, pCov,pPiTajD, ncol = 1, align = "v", rel_heights = c(1,1,1,1,1,1,1,0.5,2))
    
    ggsave(paste0("./figures/Pi_TajD_output/Pop_Group/", glm_input[[2*i - 1]], "_BIN_", k, ".txt",".png"), width = 5, height = 7, dpi = 600)
    
  }
} 
  
 
## just to plot windows

for (k in 1:19){
  
  k = 2

  range_sel2 <- function(x) filter(x, CHR == BayPass_Keys$chr[[k]], (POS > BayPass_Keys$POSITION[[k]] - 2e6) & (POS < BayPass_Keys$POSITION[[k]] + 2e6))

  XtX_BP_mrk_x <- XtX_BP_mrk %>% range_sel2()
  XtX_BP_mrk_GWAS_x <- XtX_BP_mrk_GWAS %>% range_sel2()
  
  Bay_BP_mrk_x <- Bay_BP_mrk %>% range_sel2()
  Bay_BP_mrk_GWAS_x <- Bay_BP_mrk_GWAS %>% range_sel2()

  
  p1 <- ggplot(data = XtX_BP_mrk_x, aes(x = POS, y = XtXst))+
    geom_point(data = filter(XtX_BP_mrk_x, color == "gray90"), color = "gray90", size = .1, show.legend = FALSE)+
    geom_point(data = filter(XtX_BP_mrk_x, color == "red"), color = "#66DDAA", size = 0.3, show.legend = FALSE)+
    geom_point(data = XtX_BP_mrk_GWAS_x, color = "#00AAFF", size = 0.3)+
    geom_point(data = filter(XtX_BP_mrk_x, MRK %in% intersect_sig), color = "red", size = 0.3, show.legend = FALSE)+
    # geom_vline(xintercept = BayPass_Keys$POSITION[[k]], color = "red")+
    annotate(geom = "label",label = BayPass_Keys$Intersect_Name[[k]], x = -Inf, y = Inf, size = 5, hjust = -0.2, vjust = 1.1)+
    scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
    scale_x_continuous(NULL, expand = c(0, 0), labels = function(x){x/1e6}) +
    scale_y_continuous("XtXst", expand = c(0, 0, 0.1, 0.1)) +
    theme_bw() +
    theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
          axis.text.x = element_blank()) +
    guides(fill = "none")
  
  p2 <- ggplot(data = Bay_BP_mrk_x, aes(x = POS, y = eBPis))+
    geom_point(data = filter(Bay_BP_mrk_x, color == "gray90"), color = "gray90", size = .1, show.legend = FALSE)+
    geom_point(data = filter(Bay_BP_mrk_x, color == "red"), color = "#66DDAA", size = 0.3, show.legend = FALSE)+
    geom_point(data = Bay_BP_mrk_GWAS_x, color = "#00AAFF", size = 0.3)+
    geom_point(data = filter(Bay_BP_mrk_x, MRK %in% intersect_sig), color = "red", size = 0.3, show.legend = FALSE)+
    annotate(geom = "label",label = BayPass_Keys$Intersect_Name[[k]], x = -Inf, y = Inf, size = 5, hjust = -0.2, vjust = 1.1)+
    scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
    scale_x_continuous(paste("Chromosome", unique(Bay_BP_mrk_x$CHR), "(Mbp)"), expand = c(0, 0), labels = function(x){x/1e6}) +
    scale_y_continuous("eBPis", expand = c(0, 0, 0.1, 0.1)) +
    theme_bw() +
    theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")) +
    guides(fill = "none")
  
  p_comb <- plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1,1.3))
  
  ggsave(paste0("./figures/Pi_TajD_output/","BayPass_BIN_", BayPass_Keys$Intersect_Name[[k]], ".txt",".png"), width = 4, height = 2.5, dpi = 600)
  
  
}





head(GLM_data)

## Extracting markers with p < 5e-8

glm_input

paste(str_split(glm_input[[1]], pattern = "[.]")[[1]][2:3], collapse = "_")

GLM_sig_markers = list()
for (i in c(1:3, 5:7)){
  
  GLM_data <- fread(file = paste0(glm_path, glm_input[[i]]))%>%
    filter(P < 5e-8)%>%
    select(`#CHROM`, POS, REF, ALT, P, BETA, SE)%>%
    mutate(Trait = paste(str_split(glm_input[[i]], pattern = "[.]")[[1]][2:3], collapse = "_"))
  
  GLM_sig_markers[[i]] <- GLM_data
  
  rm(GLM_data)
  gc()

}

GLM_sig_markers_all <- bind_rows(GLM_sig_markers)%>%
  rename(GWAS_P = P, GWAS_BETA = BETA, GWAS_SE = SE, GWAS_TRAIT = Trait)%>%
  arrange(`#CHROM`, POS)


write_tsv(GLM_sig_markers_all, "./data/GWAS/GLM_sig_markers_all.tsv")
GLM_sig_markers_all
