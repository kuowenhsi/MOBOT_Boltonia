library(tidyverse)
library(ggpubr)

setwd("C:/Users/nyanw/Downloads/REU/MOBOT_Boltonia")

Boltonia_data <- read_csv("./data/Boltonia_merged_data_tidy_20240717.csv")

unique(Boltonia_data$num_traits)

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
#####calculate county mean coordinations####
Boltonia_data_county_coordinations <- Boltonia_data %>%
  group_by(County)%>%
  summarize(mean_Google_latitude = mean(Google_latitude), mean_Google_longitude = mean(Google_longitude))%>%
  arrange(mean_Google_latitude)%>%
  mutate(labels = paste0(County, "\n", "(", round(mean_Google_latitude, 2), ", ", round(mean_Google_longitude, 2), ")"))


Boltonia_data_FlowerRatio_county <- Boltonia_data %>%
  filter(num_traits %in% c("numFlwrB", "numRayF", "numDiscF")) %>%
  mutate(Date = factor(Date)) %>%
  mutate(num_values = case_when(is.na(num_values) ~ 0, num_values == 0 ~ 0, num_values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date, num_traits, County) %>%
  summarize(flowerRatio = sum(num_values)/n())%>%
  ungroup() %>%
  complete(Date, num_traits, County) %>%
  mutate(flowerRatio = case_when(is.na(flowerRatio) ~ 0, TRUE ~ flowerRatio), Date = as.Date(Date))%>%
  left_join(Boltonia_data_county_coordinations, by = "County")

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
  select(index, variable_name, DaysToFlower)%>%
  mutate(index = as.double(index))%>%
  left_join(read_csv("./data/Merged_Boltonia_data_20240627.csv") %>% select(1:12), by = "index", relationship = "many-to-many")

Boltonia_data_CountyLabels <- Boltonia_data %>%
  left_join(Boltonia_data_county_coordinations, by = "County")

p <- ggplot(data = Boltonia_data_FlowerRatio, mapping = aes(x = Date, y = flowerRatio)) + 
  geom_col(aes(fill = variable_name))+
  scale_y_continuous(name = "Total flower ratio", limits = c(0, 1), expand = c(0, 0, 0, 0))+
  scale_x_date(name = "", breaks = unique(Boltonia_data_FlowerRatio$Date), date_labels = "%b %d")+
  facet_wrap(.~variable_name)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid.minor = element_blank(), legend.position = "none")
p

ggsave("./figures/Boltonia_total_flower_ratio.png", width = 10, height = 4)


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

###########ANOVA########
stemLength_data <- filter(Boltonia_data_CountyLabels, num_traits == "stemLength")%>%
  filter(Date == as.Date("2024-07-03"))
stemLength_model <- as.formula(num_values ~ MaternalLine)
stemLength_aov <- aov(stemLength_model, data = stemLength_data)
print(stemLength_aov)
summary(stemLength_aov)

p <- ggplot(data = stemLength_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "County & Stem Length")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Stem Length (cm)")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/county_stemlength_anova.png", width = 10, height = 7)


numStem_data <- filter(Boltonia_data_CountyLabels, num_traits == "numStem")%>%
    filter(Date == as.Date("2024-07-03"))
numStem_model <- as.formula(num_values ~ MaternalLine)
numStem_aov <- aov(numStem_model, data = numStem_data)
print(numStem_aov)
summary(numStem_aov)
p <- ggplot(data = numStem_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "County & Number of Stems")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number Stems")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/county_numstem_anova.png", width = 10, height = 7)

numDiscF_data <- filter(Boltonia_data_CountyLabels, num_traits == "numDiscF")%>%
  filter(Date == as.Date("2024-07-03"))
numDiscF_model <- as.formula(num_values ~ MaternalLine)
numDiscF_aov <- aov(numDiscF_model, data = numDiscF_data)
print(numDiscF_aov)
summary(numDiscF_aov)
p <- ggplot(data = numDiscF_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "County & Number of Disc Flowers")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number Disc Flowers")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/county_discflwrs_anova.png", width = 10, height = 7)

numRayF_data <- filter(Boltonia_data_CountyLabels, num_traits == "numRayF")%>%
  filter(Date == as.Date("2024-07-03"))
numRayF_model <- as.formula(num_values ~ MaternalLine)
numRayF_aov <- aov(numRayF_model, data = numRayF_data)
print(numRayF_aov)
summary(numDiscF_aov)
p <- ggplot(data = numRayF_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "County & Number of Ray Flowers")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number Ray Flowers")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/county_rayflwrs_anova.png", width = 10, height = 7)

numFlwrB_data <- filter(Boltonia_data_CountyLabels, num_traits == "numFlwrB")%>%
  filter(Date == as.Date("2024-07-03"))
numFlwrB_model <- as.formula(num_values ~ MaternalLine)
numFlwrB_aov <- aov(numFlwrB_model, data = numFlwrB_data)
print(numFlwrB_aov)
summary(numFlwrB_aov)
p <- ggplot(data = numFlwrB_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "County & Number of Flower Buds")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number Flower Buds")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/county_flwrbud_anova.png", width = 10, height =7)

numRos_data <- filter(Boltonia_data_CountyLabels, num_traits == "numRos")%>%
  filter(Date == as.Date("2024-07-03"))
numRos_model <- as.formula(num_values ~ MaternalLine)
numRos_aov <- aov(numRos_model, data = numRos_data)
p <- ggplot(data = numRos_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "County & Number of Rosettes")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number of Rosettes)")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/county_rosettes_anova.png", width = 10, height = 7)

numDeadF_data <- filter(Boltonia_data_CountyLabels, num_traits == "numDeadF")%>%
  filter(Date == as.Date("2024-07-03"))
numDeadF_model <- as.formula(num_values ~ MaternalLine)
numDeadF_aov <- aov(numDeadF_model, data = numDeadF_data)

p <- ggplot(data = numDeadF_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "County & Number of Dead Flowers")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number Dead Flowers")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/county_deadflwr_anova.png", width = 10, height = 7)

numLSt_data <- filter(Boltonia_data_CountyLabels, num_traits == "numLSt")%>%
  filter(Date == as.Date("2024-07-03"))
numLSt_model <- as.formula(num_values ~ MaternalLine)
numLSt_aov <- aov(numLSt_model, data = numLSt_data)
print(numLSt_aov)
summary(numLSt_aov)
p <- ggplot(data = numLSt_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "County & Number of Lateral Stems")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number Lateral Stems")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/county_lateralstems_anova.png", width = 10, height = 7)

numLvs_data <- filter(Boltonia_data_CountyLabels, num_traits == "numLvs")%>%
  filter(Date == as.Date("2024-05-15"))
numLvs_model <- as.formula(num_values ~ MaternalLine)
numLvs_aov <- aov(numLvs_model, data = numLvs_data)
print(numLvs_aov)
p <- ggplot(data = numLvs_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "County & Number of Leaves")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number of Leaves")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/county_numleaves_anova.png", width = 10, height = 7)

latBuds_data <- filter(Boltonia_data_CountyLabels, num_traits == "latBuds")%>%
  filter(Date == as.Date("2024-05-01"))
latBuds_model <- as.formula(num_values ~ MaternalLine)
latBuds_aov <- aov(latBuds_model, data = latBuds_data)
p <- ggplot(data = latBuds_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "County & Number of Lateral Buds")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number Lateral Buds")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/county_latbuds_anova.png", width = 10, height = 7)

leafLong_data <- filter(Boltonia_data_CountyLabels, num_traits == "leafLong")%>%
  filter(Date == as.Date("2024-05-15"))
leafLong_model <- as.formula(num_values ~ MaternalLine)
leafLong_aov <- aov(leafLong_model, data = leafLong_data)
p <- ggplot(data = leafLong_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "County & Leaf Legnth")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Leaf Length")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/county_leaflength_anova.png", width = 10, height = 7)

leafWide_data <- filter(Boltonia_data_CountyLabels, num_traits == "leafWide")%>%
  filter(Date == as.Date("2024-05-15"))
leafWide_model <- as.formula(num_values ~ MaternalLine)
leafWide_aov <- aov(leafWide_model, date = leafWide_data)


unique(Boltonia_data$num_traits)


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
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("Maternal Line & Stem Length (06/19/24)")+
  ylab("Stem Length (cm)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_stemlength.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numStem", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("Maternal Line & Number of Stems (06/19/24)")+
  ylab("numStem")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numStem.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numRos", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("Maternal Line & Number of Rosettes (06/19/24)")+
  ylab("numRos")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numRos.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numRayF", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("Maternal Line & Number of Ray flowers (06/19/24)")+
  ylab("numRayF")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numRayF.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numFlwrB", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("Maternal Line & Number of Flowerbuds (06/19/24)")+
  ylab("numFlwrB")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numFlwrB.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numDiscF", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("Maternal Line & Number of Disc Flowers (06/19/24)")+
  ylab("numDiscF")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numDiscF.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafWide", Date == as.Date("2024-05-15")), aes(x = MaternalLine, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("Maternal Line & Leaf Width (05/15/24)")+
  ylab("leafWidth")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_leafWide.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafLong", Date == as.Date("2024-05-15")), aes(x = MaternalLine, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("Maternal Line & Leaf Length (05/15/24)")+
  ylab("leafLength")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_leafLong.png", width = 10, height = 7, dpi = 600)
  
p <- ggplot(data = filter(Boltonia_data, variable_name == "numLSt", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("Maternal Line & Number of Lateral Stems (06/19/24)")+
  ylab("numLSt")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numLSt.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numLvs", Date == as.Date("2024-05-15")), aes(x = MaternalLine, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
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
  ggtitle("County & Stem Length (06/19/24)")+
  ylab("Stem Length (cm)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_stemlength.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numStem", Date == as.Date("2024-06-19")), aes(x = County, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("County & Leaf Width (06/19/24)")+
  ylab("Leaf Width")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_numStem.png", width = 10, height = 7, dpi = 600)

p <-ggplot(data = filter(Boltonia_data, variable_name == "numFlwrB", Date == as.Date("2024-06-19")), aes(x = County, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("County & Number of Flower Buds (06/19/24)")+
  ylab("numFlwrB")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_numFlwrB.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numLSt", Date == as.Date("2024-06-19")), aes(x = County, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("County & Number of Lateral Stems (06/19/24)")+
  ylab("numLSt")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_numLSt.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numRos", Date == as.Date("2024-06-19")), aes(x = County, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("County & Number of Rosettes (06/19/24)")+
  ylab("numRos")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_numRos.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafLong", Date == as.Date("2024-05-15")), aes(x = County, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("County & Leaf Length (05/15/24)")+
  ylab("Leaf Length")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_leaflong.png", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafWide", Date == as.Date("2024-05-15")), aes(x = County, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("County & Leaf Width (05/15/24)")+
  ylab("Leaf Width")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_leafwide.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numRayF", Date == as.Date("2024-05-15")), aes(x = County, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("County & Leaf Width (05/15/24)")+
  ylab("Leaf Width")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p


p <- ggplot(data = filter(Boltonia_data, variable_name == "numDiscF", Date == as.Date("2024-05-15")), aes(x = County, y = values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  ggtitle("County & Leaf Width (05/15/24)")+
  ylab("Leaf Width")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p

