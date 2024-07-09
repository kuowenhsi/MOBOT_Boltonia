library(tidyverse)
library(ggpubr)

setwd("C:/Users/nyanw/Downloads/REU/MOBOT_Boltonia")

Boltonia_data <- read_csv("./data/tidy_boltonia_data.csv")

unique(Boltonia_data$variable_name)

#make flowering ratio by Date into a plot
Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(variable_name == "numDiscF") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date) %>%
  summarize(flowerRatio = sum(values)/n())

P <-ggplot(data = Boltonia_data_FlowerRatio, mapping = aes(x = Date, y = flowerRatio)) + 
  geom_col(color = "skyblue", fill = "skyblue")+
  scale_y_continuous(limits = c(0,1))+
  ggtitle("Flowering Ratio by Date")+
  theme_gray()
P
ggsave("./figures/flwrRatio_by_date.png", width = 10, height = 7, dpi = 600)

# Then plot flowering ratio by Date and County
Boltonia_data_FlowerRatio_county <- Boltonia_data %>%
  filter(variable_name == "numDiscF") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date, County) %>%
  summarize(flowerRatio = sum(values)/n(), mean_Google_Latitude = mean(Google_Latitude), 
            mean_Google_Longitude = mean(Google_Longitude))%>%
  arrange(County, Date)

P <- ggplot(data = Boltonia_data_FlowerRatio_county, aes(x = Date, y = flowerRatio)) +
  geom_col()+
  ggtitle("Flowering Ratio by Date & County")+
  facet_wrap(.~County)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "skyblue", color = "skyblue"),
        strip.text = element_text(color = "black"))
P
ggsave("./figures/flwrRatio_by_datecounty.png", width = 10, height = 7, dpi = 600)

###Then plot flowering ratio by Date and MaternalLine###
Boltonia_data_FlowerRatio_MaternalLine <- Boltonia_data %>%
  filter(variable_name == "numDiscF") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date, MaternalLine) %>%
  summarize(flowerRatio = sum(values)/n())

P <-ggplot(data = Boltonia_data_FlowerRatio_MaternalLine, aes(x = Date, y = flowerRatio)) + 
  geom_col()+
  ggtitle("Flowering Ratio by Date & Maternal Line")+
  facet_wrap(.~MaternalLine)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "skyblue", color = "skyblue"),
        strip.text = element_text(color = "black"))
P
ggsave("./figures/flwrRatio_by_datematernalline.png", width = 10, height = 7, dpi = 600)

##Plot flowering ratio by Longitude/Latitude###
P <- ggplot(data = Boltonia_data_FlowerRatio_county, aes(x = mean_Google_Latitude, y = flowerRatio)) +
  geom_point()+
  stat_smooth(method = "lm", color = "blue")+
  stat_cor(method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.95)+
  ggtitle("Flower Ratio by Latitude")+
  facet_wrap(.~Date)+
  theme(strip.background = element_rect(fill = "skyblue", color = "skyblue"),
        strip.text = element_text(color = "black"))
P
ggsave("./figures/flwrRatio_by_latitude.png", width = 10, height = 7, dpi = 600)

P <- ggplot(data = Boltonia_data_FlowerRatio_county, aes(x = mean_Google_Longitude, y = flowerRatio)) +
  geom_point()+
  stat_smooth(method = "lm", color = "blue")+
  stat_cor(method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.95)+
  ggtitle("Flower Ration by Longitude")+
  facet_wrap(.~Date)+
  theme(strip.background = element_rect(fill = "skyblue", color = "skyblue"),
        strip.text = element_text(color = "black"))

P
ggsave("./figures/flwrRatio_by_longitude.png", width = 10, height = 7, dpi = 600)

# if we do not want those dead plants in flowering ratio calculation
mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA)))


###Plot by days to flower##########
Boltonia_floral_phenology <- read_csv("./data/Boltonia Floral Phenology_20240624.csv")
unique(Boltonia_floral_phenology$MaternalLine)

Days_to_flwr <- Boltonia_floral_phenology%>%
  filter(date == as.Date("5/10/2024"), numRayF.1)



















###################################
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
###p <- ggplot(data = filter(Boltonia_data, variable_name == "stemLength", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
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



