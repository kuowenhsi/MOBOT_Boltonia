library(tidyverse)

#Admixture Optimal K

setwd("/Users/User/Desktop/REUProject_2_Outgroups")

Kcv_error <- read_csv("./Data/Boltonia_admixture/K_cv_error_hybrids.csv")

ggplot(Kcv_error, aes(x = K, y = Error)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(Kcv_error$K), max(Kcv_error$K), by = 1)) +
  labs(title = "Cross-Validation Error vs K", x = "K", y = "CV Error")

ggsave("./Figures/Admixture_hybrids/Boltonia_ADMIXTURE_K_CV_error_hybrids.png", width = 10, height = 6, dpi = 100, bg = "white")

