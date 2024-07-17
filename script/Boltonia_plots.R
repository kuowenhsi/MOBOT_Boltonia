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
Boltonia_data_DaysToFlower <- Boltonia_data %>%
  mutate(PlantingDate = as.Date(PlantingDate, "%m/%d/%y"), FirstLeafDate = as.Date(FirstLeafDate, "%m/%d/%y"))%>%
  filter(variable_name %in% c("numFlwrB", "numRayF", "numDiscF"))%>%
  mutate(values = case_when(is.na(values) ~ 0, TRUE ~ values))%>%
  mutate(index = factor(index))%>%
  group_by(index, variable_name)%>%
  arrange(Date_index, .by_group = TRUE)%>%
  mutate(is_first = case_when((lag(values) == 0 & values > 0) ~ TRUE, TRUE ~ FALSE))%>%
  filter(is_first == TRUE)%>%
  filter(Date_index == min(Date_index))%>%
  select(index, PlantingDate, FirstLeafDate, variable_name, Date)%>%
  mutate(DaysToFlower = Date -PlantingDate)%>%
  ungroup()%>%
  complete(index, variable_name)%>%
  mutate(DaysToFlower = case_when(is.na(DaysToFlower) ~ as.difftime(150, "%d", "days"), TRUE ~ DaysToFlower))%>%
  select(index, variable_name, DaysToFlower)




p <- ggplot(data = Boltonia_data_DaysToFlower, aes(x = Google_latitude, y = as.integer(DaysToFlower)))+
  geom_point()+
  geom_point(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) == 150), color = "red")+
  stat_smooth(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150), method = "lm", color = "blue")+
  stat_cor(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150),label.x.npc = 0, label.y.npc = 0.95)+
  scale_y_continuous(name = "Days to flower")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p  


ggsave("./figures/Boltonia_days_to_flower_by_latitude.png", width = 10, height = 7)


p <- ggplot(data = Boltonia_data_DaysToFlower, aes(x = Google_longitude, y = as.integer(DaysToFlower)))+
  geom_point()+
  geom_point(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) == 150), color = "red")+
  stat_smooth(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150), method = "lm", color = "blue")+
  stat_cor(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150),label.x.npc = 0, label.y.npc = 0.95)+
  scale_y_continuous(name = "Days to flower")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p  


ggsave("./figures/Boltonia_days_to_flower_by_longitude.png", width = 10, height = 7)


######################variable_names###################################
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
  geom_point(color = "blue")+
  ggtitle("Maternal Line & Stem Length (06/19/24)")+
  ylab("Stem Length (cm)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_stemlength.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numStem", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_point(color = "blue")+
  ggtitle("Maternal Line & Number of Stems (06/19/24)")+
  ylab("numStem")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numStem.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numRos", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_point(color = "blue")+
  ggtitle("Maternal Line & Number of Rosettes (06/19/24)")+
  ylab("numRos")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numRos.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numRayF", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_point(color = "blue")+
  ggtitle("Maternal Line & Number of Ray flowers (06/19/24)")+
  ylab("numRayF")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numRayF.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numFlwrB", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_point(color = "blue")+
  ggtitle("Maternal Line & Number of Flowerbuds (06/19/24)")+
  ylab("numFlwrB")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numFlwrB.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numDiscF", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_point(color = "blue")+
  ggtitle("Maternal Line & Number of Disc Flowers (06/19/24)")+
  ylab("numDiscF")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numDiscF.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafWide", Date == as.Date("2024-05-15")), aes(x = MaternalLine, y = values))+
  geom_point(color = "blue")+
  ggtitle("Maternal Line & Leaf Width (05/15/24)")+
  ylab("leafWidth")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_leafWide.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafLong", Date == as.Date("2024-05-15")), aes(x = MaternalLine, y = values))+
  geom_point(color = "blue")+
  ggtitle("Maternal Line & Leaf Length (05/15/24)")+
  ylab("leafLength")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_leafLong.png", width = 10, height = 7, dpi = 600)
  
p <- ggplot(data = filter(Boltonia_data, variable_name == "numLSt", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_point(color = "blue")+
  ggtitle("Maternal Line & Number of Lateral Stems (06/19/24)")+
  ylab("numLSt")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numLSt.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numLvs", Date == as.Date("2024-05-15")), aes(x = MaternalLine, y = values))+
  geom_point(color = "blue")+
  ggtitle("Maternal Line & Number of Leaves (05/15/24)")+
  ylab("numLvs")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numLvs.png", width = 10, height = 7, dpi = 600)

  

##County plots##########
#######################

p <- ggplot(data = filter(Boltonia_data, variable_name == "stemLength", Date == as.Date("2024-06-19")), aes(x = County, y = values))+
  geom_violin(aes(fill = County), color = NA, alpha = 0.7)+
  geom_point()+
  ggtitle("County & Stem Length")+
  ylab("Stem Length (cm)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_stemlength.png", width = 15, height = 10, dpi = 600)

p <-ggplot(data = filter(Boltonia_data, variable_name == "numFlwrB", Date == as.Date("2024-06-19")), aes(x = County, y = values))+
  geom_point()+
  ggtitle("County & Number of Flower Buds")+
  ylab("numFlwrB")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
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



