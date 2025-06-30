library(tidyverse)

#Admixture Optimal K

setwd("/Users/User/Desktop/REUProject/R/RforPCA&ADMIX")


Kcv_error <- read_csv("K_cv_error.csv")

ggplot(Kcv_error, aes(x = K, y = Error)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(Kcv_error$K), max(Kcv_error$K), by = 1)) +
  labs(title = "Cross-Validation Error vs K", x = "K", y = "CV Error")

ggsave("../../Figures/Boltonia_ADMIXTURE_K_CV_error.png", width = 10, height = 6, dpi = 100, bg = "white")

