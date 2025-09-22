library(OptGenMix)
library(SNPRelate)
library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")


geno <- read.csv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/REU_alloutgroup/VCF_decurrens/Boltonia_decurrens_100kb0.8.012", sep = "\t", header = FALSE)[,-1]

opt_nei_sample50 <- optimize_single_objective( gt=geno[, 1:1000], N_t= 50, initial_weights = NULL, weights_max=NULL, measure="nei", max_steps=5000, max_t=0.0005, p_depends_delta=TRUE)

plot(opt_nei_sample50$value, type = "l")


########
# ---- Prep your genotype matrix (0/1/2; 3 = missing) ----
G <- as.matrix(geno)
storage.mode(G) <- "integer"
if (anyNA(G)) G[is.na(G)] <- 3L

samp.id <- rownames(G)
if (is.null(samp.id)) samp.id <- paste0("ind", seq_len(nrow(G)))
snp.id  <- colnames(G)
if (is.null(snp.id))  snp.id  <- paste0("snp", seq_len(ncol(G)))

# Sanity check
stopifnot(length(snp.id) == ncol(G))

# ---- Create GDS (SNPs x samples because snpfirstdim = TRUE) ----
fn <- tempfile(fileext = ".gds")
if (file.exists(fn)) unlink(fn)

snpgdsCreateGeno(
  fn,
  genmat = t(G),                     # SNPs x samples
  sample.id = samp.id,
  snp.id = snp.id,
  snpfirstdim = TRUE,
  snp.allele = rep("A/B", length(snp.id))  # <- match snp.id length!
  # You can also just omit snp.allele entirely.
)

# ---- Open and compute kinship/GRM ----
genofile <- snpgdsOpen(fn)

# KING kinship (IBD-style; robust to structure)
king <- snpgdsIBDKING(genofile, maf = 0.1, missing.rate = 0.2, autosome.only = TRUE, num.thread = 4)
K_KING <- king$kinship
dimnames(K_KING) <- list(king$sample.id, king$sample.id)

plot(king$IBS0, king$kinship, xlab="Proportion of Zero IBS",
     ylab="Estimated Kinship Coefficient (KING-robust)")

snpgdsClose(genofile)
ku = K_KING
ku[ku < 0] <- 0
ku
##############


opt_ku_sample50       <- optimize_single_objective(sm=ku, N_t=50, measure="negative_matrix_weighted_mean", max_steps=50000, max_t=0.001, p_depends_delta=TRUE)

plot(-opt_ku_296$value, type = "l")


opt_mutli_mx16_296_ik <- optimize_multi_objective( v1=ku[1:100, 1:100], v2=geno[1:100, 1:50], N_t=50, measure_1="negative_matrix_weighted_mean", measure_2="nei", initial_weights=NULL, max_steps=25000, weights_max=NULL, max_t=2, p_depends_delta=FALSE, c1=1,c2=1,cboth=1,nda=TRUE, min_t=0.001, nd_samples=100)

plot(-opt_mutli_mx16_296_ik$value_1, type = "l")
plot(opt_mutli_mx16_296_ik$value_2, type = "l")

plot(-opt_mutli_mx16_296_ik$archive$archive_values[,1], type = "l")
plot(opt_mutli_mx16_296_ik$archive$archive_values[,2], type = "l")

plot(-opt_mutli_mx16_296_ik$value_1)

plot(opt_mutli_mx16_296_ik$value_1, opt_mutli_mx16_296_ik$value_2,xlab = "-Kinship", ylab = "Nei Genetic Diversity", col = "gray80")
points(opt_mutli_mx16_296_ik$archive$archive_values[,1], opt_mutli_mx16_296_ik$archive$archive_values[,2], col = "blue", pch = 16)

opt_mutli_mx16_296_ik

generate_measure_value(v = geno[1:50, 1:1000], measure = "shannon")

##################


geno_indv <- read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/REU_alloutgroup/VCF_decurrens/Boltonia_decurrens_100kb0.8.012.indv", col_names = "Sample_Name")

temp <- left_join(geno_indv, read_csv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_buf_climate_data_20250421.csv"), by = "Sample_Name")

colnames(temp)

opt_temp  <- optimize_single_objective(sm=as.matrix(temp$current_30arcsec_minTempWarmest), N_t=50, measure="vector_weighted_mean", max_steps=50000, max_t=0.001, p_depends_delta=TRUE)

opt_nei <- optimize_single_objective(gt=geno[, idx_sorted], N_t= 50, measure="nei", max_steps=5000, max_t=0.0005, p_depends_delta=TRUE)

# plot single optimization result
plot(opt_temp$value, type = "l")
plot(opt_nei$value, type = "l")

# print the final optimized selection result
opt_temp_selection <- opt_temp$weight[50000,]
opt_nei_selection <- opt_nei$weight[5000,]


opt_temp_selection_n <- temp %>%
  mutate(sample_n = opt_temp_selection)%>%
  group_by(Adapted_Latitude, current_30arcsec_minTempWarmest)%>%
  summarize(sample_per_location = sum(sample_n))

opt_nei_selection_n <- temp %>%
  mutate(sample_n = opt_nei_selection)%>%
  group_by(Adapted_Latitude, current_30arcsec_minTempWarmest)%>%
  summarize(sample_per_location = sum(sample_n))


# plot tempature against latitude

p <- ggplot(data = temp, aes(x = Adapted_Latitude, y = current_30arcsec_minTempWarmest/10))+
  geom_point(color = "red", alpha = opt_temp_selection/sum(opt_temp_selection), size = 6)+
  geom_point()+
  geom_text(data = opt_temp_selection_n, aes(label=sample_per_location), size = 3, nudge_x = 0.05, nudge_y = 0.1)+
  labs(x = "Latitude", y = "min Temp of the Warmest quarter (C)")+
  theme_bw()

p

ggsave("./figures/Opt_temp_separately.png", width = 4, height = 4, dpi = 600)

p <- ggplot(data = temp, aes(x = Adapted_Latitude, y = current_30arcsec_minTempWarmest/10))+
  geom_point(color = "blue", alpha = opt_nei_selection/max(opt_nei_selection)/3, size = 4)+
  geom_point()+
  geom_text(data = opt_nei_selection_n, aes(label=sample_per_location), size = 3, nudge_x = 0.05, nudge_y = 0.1)+
  labs(x = "Latitude", y = "min Temp of the Warmest quarter (C)")+
  theme_bw()

p

ggsave("./figures/Opt_nei_separately.png", width = 4, height = 4, dpi = 600)

###################

idx <- sample(1:33834, size = 1000, replace = FALSE)

# if you want them sorted:
idx_sorted <- sort(idx)
idx_sorted

opt_mutli_temp_nei <- optimize_multi_objective(v1=as.matrix(temp$current_30arcsec_minTempWarmest), v2=geno[, idx_sorted], N_t=50, measure_1="vector_weighted_mean", measure_2="nei", initial_weights=c(rep(1, 50), rep(0, 374)), max_steps=25000, weights_max=NULL, max_t=2, p_depends_delta=FALSE, c1=1,c2=1,cboth=1,nda=TRUE, min_t=0.001, nd_samples=100)


plot(opt_mutli_temp_nei$value_1)
plot(opt_mutli_temp_nei$value_2)
plot(opt_mutli_temp_nei$value_1, opt_mutli_temp_nei$value_2)

opt_mutli_temp_selection <- opt_mutli_temp_nei$archive$archive_weights[1,]


p <- ggplot(data = data.frame(temp = opt_mutli_temp_nei$value_1, nei = opt_mutli_temp_nei$value_2), aes(x = temp, y = nei))+
  geom_point(color = "gray80")+
  geom_point(data = data.frame(temp = opt_mutli_temp_nei$archive$archive_values[,1], nei = opt_mutli_temp_nei$archive$archive_values[,2]), color = "blue")+
  labs(x = "min Temp of the Warmest quarter (C)", y = "Nei Genetic Diversity")+
  theme_bw()

p


ggsave("./figures/Opt_nei_simulated_annealing_process.png", width = 4, height = 4, dpi = 600)

opt_mutli_temp_selection_n <- temp %>%
  mutate(sample_n = opt_mutli_temp_selection)%>%
  group_by(Adapted_Latitude, current_30arcsec_minTempWarmest)%>%
  summarize(sample_per_location = sum(sample_n))

p <- ggplot(data = temp, aes(x = Adapted_Latitude, y = current_30arcsec_minTempWarmest/10))+
  geom_point(color = "green", alpha = opt_mutli_temp_selection/max(opt_mutli_temp_selection)/5, size = 4)+
  geom_point()+
  geom_text(data = opt_mutli_temp_selection_n, aes(label=sample_per_location), size = 3, nudge_x = 0.05, nudge_y = 0.1)+
  labs(x = "Latitude", y = "min Temp of the Warmest quarter (C)")+
  theme_bw()

p

ggsave("./figures/Opt_nei_simulated_annealing_result.png", width = 4, height = 4, dpi = 600)

##########################

opt_mutli_temp_nei <- optimize_multi_objective(v1=as.matrix(temp$current_30arcsec_minTempWarmest), v2=geno[, 1:1000], N_t=50, measure_1="vector_weighted_mean", measure_2="nei", initial_weights=NULL, max_steps=25000, weights_max=NULL, max_t=2, p_depends_delta=FALSE, c1=1,c2=1,cboth=1,nda=TRUE, min_t=0.001, nd_samples=100)

plot(opt_mutli_temp_nei$value_1)
plot(opt_mutli_temp_nei$value_2)
plot(opt_mutli_temp_nei$value_1, opt_mutli_temp_nei$value_2)

opt_mutli_temp_selection <- opt_mutli_temp_nei$archive$archive_weights[1,]


p <- ggplot(data = data.frame(temp = opt_mutli_temp_nei$value_1, nei = opt_mutli_temp_nei$value_2), aes(x = temp, y = nei))+
  geom_point(color = "gray80")+
  geom_point(data = data.frame(temp = opt_mutli_temp_nei$archive$archive_values[,1], nei = opt_mutli_temp_nei$archive$archive_values[,2]), color = "blue")+
  theme_bw()

p

opt_mutli_temp_selection_n <- temp %>%
  mutate(sample_n = opt_mutli_temp_selection)%>%
  group_by(Adapted_Latitude, current_30arcsec_minTempWarmest)%>%
  summarize(sample_per_location = sum(sample_n))

p <- ggplot(data = temp, aes(x = Adapted_Latitude, y = current_30arcsec_minTempWarmest/10))+
  geom_point(color = "green", alpha = opt_mutli_temp_selection/max(opt_mutli_temp_selection)/5, size = 4)+
  geom_point()+
  geom_text(data = opt_mutli_temp_selection_n, aes(label=sample_per_location), size = 3, nudge_x = 0.05, nudge_y = 0.1)+
  labs(x = "Latitude", y = "min Temp of the Warmest quarter (C)")+
  theme_bw()

p

