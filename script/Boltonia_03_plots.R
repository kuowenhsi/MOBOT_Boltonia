library(tidyverse)
library(ggpubr)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

Boltonia_data <- read_csv("./data/tidy_boltonia_data.csv")


Boltonia_data_rep <- Boltonia_data %>%
  group_by(MaternalLine, County, FlowerHead, index)%>%
  summarize()%>%
  group_by(MaternalLine, County, FlowerHead)%>%
  summarise(rep = n())



unique(Boltonia_data$variable_name)

# make flowering ratio by Date into a plot

Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(variable_name %in% c("numFlwrB", "numRayF", "numDiscF")) %>%
  mutate(Date = factor(Date)) %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date, variable_name) %>%
  summarize(flowerRatio = sum(values)/n()) %>%
  ungroup() %>%
  complete(Date, variable_name) %>%
  mutate(flowerRatio = case_when(is.na(flowerRatio) ~ 0, TRUE ~ flowerRatio), Date = as.Date(Date))

# make days to flower data

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
  select(index, variable_name, DaysToFlower)%>%
  mutate(index = as.double(index))%>%
  left_join(read_csv("./data/Boltonia_merged_data_20240627.csv") %>% select(1:12), by = "index", relationship = "many-to-many")
  

# plot the total flower ration across all dates

p <- ggplot(data = Boltonia_data_FlowerRatio, mapping = aes(x = Date, y = flowerRatio)) + 
  geom_col(aes(fill = variable_name))+
  scale_y_continuous(name = "Total flower ratio", limits = c(0, 1), expand = c(0, 0, 0, 0))+
  scale_x_date(name = "", breaks = unique(Boltonia_data_FlowerRatio$Date), date_labels = "%b %d")+
  facet_wrap(.~variable_name)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid.minor = element_blank(), legend.position = "none")

ggsave("./figures/Boltonia_total_flower_ratio.png", width = 10, height = 4)

# Then plot flowering ratio by Date and County

# calculate county mean coordinations

Boltonia_data_county_coordinations <- Boltonia_data %>%
  group_by(County)%>%
  summarize(mean_Google_latitude = mean(Google_Latitude), mean_Google_longitude = mean(Google_Longitude))%>%
  arrange(mean_Google_latitude)%>%
  mutate(labels = paste0(County, "\n", "(", round(mean_Google_latitude, 2), ", ", round(mean_Google_longitude, 2), ")"))
  

Boltonia_data_FlowerRatio_county <- Boltonia_data %>%
  filter(variable_name %in% c("numFlwrB", "numRayF", "numDiscF")) %>%
  mutate(Date = factor(Date)) %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date, variable_name, County) %>%
  summarize(flowerRatio = sum(values)/n())%>%
  ungroup() %>%
  complete(Date, variable_name, County) %>%
  mutate(flowerRatio = case_when(is.na(flowerRatio) ~ 0, TRUE ~ flowerRatio), Date = as.Date(Date))%>%
  left_join(Boltonia_data_county_coordinations, by = "County")



# plot

# County first
p <- ggplot(data = Boltonia_data_FlowerRatio_county, aes(x = reorder(labels, mean_Google_latitude), y = flowerRatio))+
  geom_col(aes(group = as.character(Date), fill = as.character(Date)), position = position_dodge())+
  scale_fill_brewer(name = "", palette = "Spectral")+
  scale_x_discrete(name = "")+
  facet_wrap(.~variable_name, nrow = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("./figures/Boltonia_total_flower_ratio_by_county.png", width = 10, height = 6)


# buds
p <- ggplot(data = filter(Boltonia_data_FlowerRatio_county, variable_name == "numFlwrB"), aes(x = Date, y = flowerRatio))+
  geom_col(aes(fill = as.character(Date)))+
  scale_fill_brewer(name = "", palette = "Spectral")+
  scale_y_continuous(name = "Flower bud ratio")+
  scale_x_date(name = "")+
  facet_wrap(.~reorder(labels, mean_Google_latitude), nrow = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid = element_blank(), legend.position = "none")
ggsave("./figures/Boltonia_total_bud_ratio_by_county.png", width = 10, height = 10)


# Ray
p <- ggplot(data = filter(Boltonia_data_FlowerRatio_county, variable_name == "numRayF"), aes(x = Date, y = flowerRatio))+
  geom_col(aes(fill = as.character(Date)))+
  scale_fill_brewer(name = "", palette = "Spectral")+
  scale_y_continuous(name = "Flower ray flower ratio")+
  scale_x_date(name = "")+
  facet_wrap(.~reorder(labels, mean_Google_latitude), nrow = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid = element_blank(), legend.position = "none")
ggsave("./figures/Boltonia_total_ray_ratio_by_county.png", width = 10, height = 10)

# buds
p <- ggplot(data = filter(Boltonia_data_FlowerRatio_county, variable_name == "numDiscF"), aes(x = Date, y = flowerRatio))+
  geom_col(aes(fill = as.character(Date)))+
  scale_fill_brewer(name = "", palette = "Spectral")+
  scale_y_continuous(name = "Flower disc ratio")+
  scale_x_date(name = "")+
  facet_wrap(.~reorder(labels, mean_Google_latitude), nrow = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid = element_blank(), legend.position = "none")
ggsave("./figures/Boltonia_total_disk_ratio_by_county.png", width = 10, height = 10)


################

# Days to flower


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
  

ggsave("./figures/Boltonia_days_to_flower_by_latitude.png", width = 13, height = 5)



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


ggsave("./figures/Boltonia_days_to_flower_by_longitude.png", width = 13, height = 5)


# Date first
p <- ggplot(data = Boltonia_data_FlowerRatio_county, aes(x = as.character(Date), y = flowerRatio))+
  geom_col(aes(group = County, fill = County), position = position_dodge())
p

p <- ggplot(data = Boltonia_data_FlowerRatio_county, aes(x = County, y = flowerRatio))+
  geom_col()+
  facet_wrap(.~Date)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p


# by latitude

p <- ggplot(data = Boltonia_data_FlowerRatio_county, mapping = aes(x = mean_Google_latitude, y = flowerRatio))+
  geom_point()+
  stat_smooth(method = "lm", color = "blue")+
  stat_cor(method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.95)+
  facet_wrap(.~Date)+
  theme_bw()
p

# by longitude

StClair_Ratio <- data.frame(
  name = c("2024-05-15", "2024-05-22", "2024-05-29","2024-06-05", "2024-06-12", "2024-06-19"),
  value = c(0.00000000, 0.00000000, 0.00000000, 0.04761905, 0.09523810, 0.09523810))
ggplot(StClair_Ratio, aes(x = Date, y = Value)) + 
  geom_bar(stat = "identity")
###Fulton###
Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(variable_name == "numDiscF", County == "Fulton") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date) %>%
  summarize(flowerRatio = sum(values)/n())
Fulton_Ratio <- data.frame(
  name = c("2024-05-15", "2024-05-22", "2024-05-29","2024-06-05", "2024-06-12", "2024-06-19"),
  value = c(0.03846154, 0.02564103, 0.14102564, 0.16666667, 0.17948718, 0.19230769))
ggplot(Fulton_Ratio, aes(x = Date, y = Value)) + 
  geom_bar(stat = "identity")
###Frederick###
Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(variable_name == "numDiscF", County == "Frederick") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date) %>%
  summarize(flowerRatio = sum(values)/n())
Frederick_Ratio <- data.frame(
  name = c("2024-05-15", "2024-05-22", "2024-05-29","2024-06-05", "2024-06-12", "2024-06-19"),
  value = c(0.00, 0.00, 0.08, 0.20, 0.24, 0.28))
ggplot(Frederick_Ratio, aes(x = name, y = value)) + 
  geom_bar(stat = "identity")
###Tazewell###
Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(variable_name == "numDiscF", County == "Tazewell") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date) %>%
  summarize(flowerRatio = sum(values)/n())
Tazewell_Ratio <- data.frame(
  name = c("2024-05-15", "2024-05-22", "2024-05-29","2024-06-05", "2024-06-12", "2024-06-19"),
  value = c(0.00, 0.00, 0.08, 0.20, 0.24, 0.28))
ggplot(Tazewell_Ratio, aes(x = Date, y = Value)) + 
  geom_bar(stat = "identity")
###

# Then plot flowering ratio by Date and MaternalLine
Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(variable_name == "numDiscF", MaternalLine == "2011-2598-1") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date) %>%
  summarize(flowerRatio = sum(values)/n())
MaternalLine_Ratio <- data.frame(
  name = c("2024-05-15", "2024-05-22", "2024-05-29","2024-06-05", "2024-06-12", "2024-06-19"),
  value = c(0.00000000, 0.02631579, 0.15789474, 0.23684211, 0.23684211, 0.34210526))
ggplot(MaternalLine_Ratio, aes(x = name, y = value)) + 
  geom_bar(stat = "identity")

# if we do not want those dead plants in flowering ratio calculation
mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA)))

##########################

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



