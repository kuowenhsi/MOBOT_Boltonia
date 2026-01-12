library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/fastsimcoal2")

list.files()

likelihood_data <- lapply(list.files(pattern = "*_CI.lhoods"), scan)
names(likelihood_data) <- list.files(pattern = "*_CI.lhoods")
likelihood_data <- bind_rows(likelihood_data, .id = "Model")

likelihood_data_l <- likelihood_data %>%
  pivot_longer(cols = 1:3, names_to = "Model", values_to = "ML")

p <- ggplot(data = likelihood_data_l, aes(x = Model, y = ML))+
  geom_boxplot()+
  scale_x_discrete(name = "", labels = c("Up↔Down", "Up→Down", "Up←Down"))+
  scale_y_continuous(name = "Composite log-likelihood")+
  theme_bw()+
  theme(panel.grid = element_blank())
p

ggsave("Likelihood_dist.svg", width = 3, height = 3)
