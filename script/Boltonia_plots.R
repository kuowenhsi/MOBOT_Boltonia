library(tidyverse)
library(ggplot2)

setwd("C:/Users/nyanw/Downloads/REU/MOBOT_Boltonia")

Boltonia_data <- read_csv("./data/tidy_boltonia_data.csv")
Boltonia_data_sub <- read_csv("./data/Merged_Boltonia_data_20240626.csv")

unique(Boltonia_data$variable_name)

Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(variable_name == "numDiscF") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date) %>%
  summarize(flowerRatio = sum(values)/n())

# make flowering ratio by Date into a plot


# Then plot flowering ratio by Date and County

# Then plot flowering ratio by Date and MaternalLine


# if we do not want those dead plants in flowering ratio calculation
mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA)))



p <- ggplot(data = filter(Boltonia_data, variable_name == "leafLong", Date == as.Date("2024-05-01")), aes(x = County, y = values))+
  geom_violin(aes(fill = County), color = NA, alpha = 0.7)+
  geom_point()

p
ggsave("./figures/county_figure.png", width = 10, height = 10, dpi = 600)


p <- ggplot(data = filter(Boltonia_data, variable_name == "stemLength", Date == as.Date("2024-06-19")), aes(x = Google_Latitude, y = values))+
  geom_point()
p


p <- ggplot(data = filter(Boltonia_data, variable_name == "stemLength", Date == as.Date("2024-06-19")), aes(x = Google_Longitude, y = values))+
  geom_point()
p

##MaternalLine plots#######
###########################
p <- ggplot(data = filter(Boltonia_data, variable_name == "stemLength", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_point()+
  labs(title = "Maternal Line & Stem Length")
p
ggsave("./figures/MaternalLine_stemlength.png", width = 15, height = 10, dpi = 600)

p <-ggplot(data = filter(Boltonia_data, variable_name == "numFlwrB", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_point()+
  labs(title = "Maternal Line & Numbder of Flower Buds")
p
ggsave("./figures/MaternalLine_numFlwrB.png", width = 15, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numLSt", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_point()
p
ggsave("./figures/MaternalLine_numLSt.png", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numRos", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_point()
p
ggsave("./figures/MaternalLine_numRos", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafLong", Date == as.Date("2024-05-15")), aes(x = MaternalLine, y = values))+
  geom_point()
p
ggsave("./figures/MaternalLine_leaflong.png", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafWide", Date == as.Date("2024-05-15")), aes(x = MaternalLine, y = values))+
  geom_point()
p
ggsave("./figures/MaternalLine_leafwide.png", width = 10, height = 10, dpi = 600)

##State plots##########
#######################

p <- ggplot(data = filter(Boltonia_data, variable_name == "stemLength", Date == as.Date("2024-06-19")), aes(x = County, y = values))+
  geom_point()+
  labs(title = "County & Stem Length")
p
ggsave("./figures/County_stemlength.png", width = 15, height = 10, dpi = 600)

p <-ggplot(data = filter(Boltonia_data, variable_name == "numFlwrB", Date == as.Date("2024-06-19")), aes(x = County, y = values))+
  geom_point()+
  labs(title = "County & Number of Flower Buds")
p
ggsave("./figures/County_numFlwrB.png", width = 15, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numLSt", Date == as.Date("2024-06-19")), aes(x = County, y = values))+
  geom_point()
p
ggsave("./figures/County_numLSt.png", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numRos", Date == as.Date("2024-06-19")), aes(x = County, y = values))+
  geom_point()
p
ggsave("./figures/County_numRos", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafLong", Date == as.Date("2024-05-15")), aes(x = County, y = values))+
  geom_point()
p
ggsave("./figures/County_leaflong.png", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafWide", Date == as.Date("2024-05-15")), aes(x = County, y = values))+
  geom_point()
p



