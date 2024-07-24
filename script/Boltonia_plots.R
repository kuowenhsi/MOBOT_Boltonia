library(tidyverse)
library(ggpubr)
library(lme4)

setwd("C:/Users/nyanw/Downloads/REU/MOBOT_Boltonia")

Boltonia_data <- read_csv("./data/Boltonia_merged_data_tidy_20240717.csv")

unique(Boltonia_data$num_traits)

#make flowering ratio by Date into a plot
Boltonia_data_FlowerRatio <- Boltonia_data %>%
 filter(num_traits == "numDiscF") %>%
  mutate(num_values = case_when(is.na(num_values) ~ 0, num_values == 0 ~ 0, num_values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date) %>%
  summarize(flowerRatio = sum(num_values)/n())

P <-ggplot(data = Boltonia_data_FlowerRatio, mapping = aes(x = Date, y = flowerRatio)) + 
  geom_col(color = "skyblue", fill = "skyblue")+
  scale_y_continuous(limits = c(0,1))+
  ggtitle("Flowering Ratio by Date")+
  theme_gray()
P
ggsave("./figures/flwrRatio_by_date.png", width = 10, height = 7, dpi = 600)

# Then plot flowering ratio by Date and County
Boltonia_data_FlowerRatio_county <- Boltonia_data %>%
  filter(num_traits == "numDiscF") %>%
  mutate(num_values = case_when(is.na(num_values) ~ 0, num_values == 0 ~ 0, num_values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date, County) %>%
  summarize(flowerRatio = sum(num_values)/n(), mean_Google_Latitude = mean(Google_latitude), 
            mean_Google_Longitude = mean(Google_longitude))%>%
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
  filter(num_traits == "numDiscF") %>%
  mutate(num_values = case_when(is.na(num_values) ~ 0, num_values == 0 ~ 0, num_values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date, MaternalLine) %>%
  summarize(flowerRatio = sum(num_values)/n())

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
mutate(num_values = case_when(is.na(num_values) ~ 0, num_values == 0 ~ 0, num_values >= 1 ~ 1, TRUE ~ as.numeric(NA)))


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
  mutate(index = as.factor(index))%>%
  mutate(PlantingDate = as.Date(PlantingDate, "%m/%d/%y"), FirstLeafDate = as.Date(FirstLeafDate, "%m/%d/%y"))%>%
  filter(num_traits %in% c("numFlwrB", "numRayF", "numDiscF"))%>%
  mutate(num_values = case_when(is.na(num_values) ~ 0, TRUE ~ num_values))%>%
  group_by(index, num_traits)%>%
  arrange(Date, .by_group = TRUE)%>%
  mutate(is_first = case_when((lag(num_values) == 0 & num_values > 0) ~ TRUE, TRUE ~ FALSE))%>%
  filter(is_first == TRUE)%>%
  filter(Date == min(Date))%>%
  select(index, PlantingDate, FirstLeafDate, num_traits, Date)%>%
  mutate(DaysToFlower = Date -PlantingDate)%>%
  ungroup()%>%
  select(index, num_traits, DaysToFlower)%>%
  complete(index, num_traits)%>%
  mutate(DaysToFlower = case_when(is.na(DaysToFlower) ~ as.difftime(150, "%d", "days"), TRUE ~ DaysToFlower))%>%
  mutate(index = as.integer(index), num_traits = factor(num_traits, levels = c("numFlwrB", "numRayF", "numDiscF"))) %>%
  left_join(read_csv("./data/Merged_Boltonia_data_20240627.csv") %>% select(1:12), by = "index", relationship = "many-to-many")

Boltonia_data_CountyLabels <- Boltonia_data %>%
  left_join(Boltonia_data_county_coordinations, by = "County")

p <- ggplot(data = Boltonia_data_FlowerRatio, mapping = aes(x = Date, y = flowerRatio)) + 
  geom_col(aes(fill = num_traits))+
  scale_y_continuous(name = "Total flower ratio", limits = c(0, 1), expand = c(0, 0, 0, 0))+
  scale_x_date(name = "", breaks = unique(Boltonia_data_FlowerRatio$Date), date_labels = "%b %d")+
  facet_wrap(.~num_traits)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid.minor = element_blank(), legend.position = "none")
p

ggsave("./figures/Boltonia_total_flower_ratio.png", width = 10, height = 4, dpi = 600)


p <- ggplot(data = Boltonia_data_DaysToFlower, aes(x = Google_latitude, y = as.integer(DaysToFlower)))+
  geom_point()+
  geom_point(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) == 150), color = "red")+
  stat_smooth(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150), method = "lm", color = "blue")+
  stat_cor(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150),label.x.npc = 0, label.y.npc = 0.95)+
  scale_y_continuous(name = "Days to flower")+
  facet_wrap(.~num_traits, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p  


ggsave("./figures/Boltonia_days_to_flower_by_latitude.png", width = 10, height = 7, dpi = 600)


p <- ggplot(data = Boltonia_data_DaysToFlower, aes(x = Google_longitude, y = as.integer(DaysToFlower)))+
  geom_point()+
  geom_point(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) == 150), color = "red")+
  stat_smooth(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150), method = "lm", color = "blue")+
  stat_cor(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150),label.x.npc = 0, label.y.npc = 0.95)+
  scale_y_continuous(name = "Days to flower")+
  facet_wrap(.~num_traits, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p  


ggsave("./figures/Boltonia_days_to_flower_by_longitude.png", width = 10, height = 7, dpi = 600)

#######Linear Regression######
ggplot(data = filter(Boltonia_data, num_traits == "numLvs"), aes(x = Google_latitude, y = num_values))+
  geom_point()+
  geom_point(data = filter(Boltonia_data, num_traits == "numLvs"), color = "blue")+
  stat_smooth(data = filter(Boltonia_data, num_traits == "numLvs"), method = "lm", color = "black")+
  stat_cor(data = filter(Boltonia_data, num_traits == "numLvs"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(Boltonia_data, num_traits == "numLvs"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Number of Leaves")+
  facet_wrap(.~num_traits, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", 
        strip.background = element_rect(fill = "skyblue", color = "black"),
        strip.text = element_text(color = "black"))
ggsave("./figures/numleaves_regression.png", width = 10, height = 7, dpi = 600)

ggplot(data = filter(Boltonia_data, num_traits == "leafLong"), aes(x = Google_latitude, y = num_values))+
  geom_point()+
  geom_point(data = filter(Boltonia_data, num_traits == "leafLong"), color = "blue")+
  stat_smooth(data = filter(Boltonia_data, num_traits == "leafLong"), method = "lm", color = "black")+
  stat_cor(data = filter(Boltonia_data, num_traits == "leafLong"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(Boltonia_data, num_traits == "leafLong"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Leaf Length")+
  facet_wrap(.~num_traits, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", 
        strip.background = element_rect(fill = "skyblue", color = "black"),
        strip.text = element_text(color = "black"))
ggsave("./figures/leaflong_regression.png", width = 10, height = 7, dpi = 600)

ggplot(data = filter(Boltonia_data, num_traits == "leafWide"), aes(x = Google_latitude, y = num_values))+
  geom_point()+
  geom_point(data = filter(Boltonia_data, num_traits == "leafWide"), color = "blue")+
  stat_smooth(data = filter(Boltonia_data, num_traits == "leafWide"), method = "lm", color = "black")+
  stat_cor(data = filter(Boltonia_data, num_traits == "leafWide"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(Boltonia_data, num_traits == "leafWide"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Leaf Width")+
  facet_wrap(.~num_traits, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", 
        strip.background = element_rect(fill = "skyblue", color = "black"),
        strip.text = element_text(color = "black"))
ggsave("./figures/leafwidth_regression.png", width = 10, height = 7, dpi = 600)


ggplot(data = filter(Boltonia_data, num_traits == "numDiscF"), aes(x = Google_latitude, y = num_values))+
  geom_point()+
  geom_point(data = filter(Boltonia_data, num_traits == "numDiscF"), color = "blue")+
  stat_smooth(data = filter(Boltonia_data, num_traits == "numDiscF"), method = "lm", color = "black")+
  stat_cor(data = filter(Boltonia_data, num_traits == "numDiscF"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(Boltonia_data, num_traits == "numDiscF"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Number of Open Capitula")+
  facet_wrap(.~num_traits, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", 
        strip.background = element_rect(fill = "skyblue", color = "black"),
        strip.text = element_text(color = "black"))
ggsave("./figures/capitula_regression.png", width = 10, height = 7, dpi = 600)

ggplot(data = filter(Boltonia_data, num_traits == "numFlwrB"), aes(x = Google_latitude, y = num_values))+
  geom_point()+
  geom_point(data = filter(Boltonia_data, num_traits == "numFlwrB"), color = "blue")+
  stat_smooth(data = filter(Boltonia_data, num_traits == "numFlwrB"), method = "lm", color = "black")+
  stat_cor(data = filter(Boltonia_data, num_traits == "numFlwrB"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(Boltonia_data, num_traits == "numFlwrB"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Number of Flower Buds")+
  facet_wrap(.~num_traits, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", 
        strip.background = element_rect(fill = "skyblue", color = "black"),
        strip.text = element_text(color = "black"))
ggsave("./figures/flwrbud_regression.png", width = 10, height = 7, dpi = 600)

ggplot(data = filter(Boltonia_data, num_traits == "numLSt"), aes(x = Google_latitude, y = num_values))+
  geom_point()+
  geom_point(data = filter(Boltonia_data, num_traits == "numLSt"), color = "blue")+
  stat_smooth(data = filter(Boltonia_data, num_traits == "numLSt"), method = "lm", color = "black")+
  stat_cor(data = filter(Boltonia_data, num_traits == "numLSt"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(Boltonia_data, num_traits == "numLSt"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Number of Lateral Stems")+
  facet_wrap(.~num_traits, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", 
        strip.background = element_rect(fill = "skyblue", color = "black"),
        strip.text = element_text(color = "black"))
ggsave("./figures/latstems_regression.png", width = 10, height = 7, dpi = 600)

ggplot(data = filter(Boltonia_data, num_traits == "numRayF"), aes(x = Google_latitude, y = num_values))+
  geom_point()+
  geom_point(data = filter(Boltonia_data, num_traits == "numRayF"), color = "blue")+
  stat_smooth(data = filter(Boltonia_data, num_traits == "numRayF"), method = "lm", color = "black")+
  stat_cor(data = filter(Boltonia_data, num_traits == "numRayF"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(Boltonia_data, num_traits == "numRayF"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Number of Ray Flowers")+
  facet_wrap(.~num_traits, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", 
        strip.background = element_rect(fill = "skyblue", color = "black"),
        strip.text = element_text(color = "black"))
ggsave("./figures/rayflwr_regression.png", width = 10, height = 7, dpi = 600)

ggplot(data = filter(Boltonia_data, num_traits == "numRos"), aes(x = Google_latitude, y = num_values))+
  geom_point()+
  geom_point(data = filter(Boltonia_data, num_traits == "numRos"), color = "blue")+
  stat_smooth(data = filter(Boltonia_data, num_traits == "numRos"), method = "lm", color = "black")+
  stat_cor(data = filter(Boltonia_data, num_traits == "numRos"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(Boltonia_data, num_traits == "numRos"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Number of Rosettes")+
  facet_wrap(.~num_traits, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", 
        strip.background = element_rect(fill = "skyblue", color = "black"),
        strip.text = element_text(color = "black"))
ggsave("./figures/rosette_regression.png", width = 10, height = 7, dpi = 600)

ggplot(data = filter(Boltonia_data, num_traits == "numStem"), aes(x = Google_latitude, y = num_values))+
  geom_point()+
  geom_point(data = filter(Boltonia_data, num_traits == "numStem"), color = "blue")+
  stat_smooth(data = filter(Boltonia_data, num_traits == "numStem"), method = "lm", color = "black")+
  stat_cor(data = filter(Boltonia_data, num_traits == "numStem"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(Boltonia_data, num_traits == "numStem"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Number of Stems")+
  facet_wrap(.~num_traits, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", 
        strip.background = element_rect(fill = "skyblue", color = "black"),
        strip.text = element_text(color = "black"))
ggsave("./figures/numstem_regression.png", width = 10, height = 7, dpi = 600)

ggplot(data = filter(Boltonia_data, num_traits == "stemLength"), aes(x = Google_latitude, y = num_values))+
  geom_point()+
  geom_point(data = filter(Boltonia_data, num_traits == "stemLength"), color = "blue")+
  stat_smooth(data = filter(Boltonia_data, num_traits == "stemLength"), method = "lm", color = "black")+
  stat_cor(data = filter(Boltonia_data, num_traits == "stemLength"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(Boltonia_data, num_traits == "stemLength"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Stem Length")+
  facet_wrap(.~num_traits, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", 
        strip.background = element_rect(fill = "skyblue", color = "black"),
        strip.text = element_text(color = "black"))
ggsave("./figures/stemlength_regression.png", width = 10, height = 7, dpi = 600)


###########Accession/County ANOVA########
stemLength_data <- filter(Boltonia_data_CountyLabels, num_traits == "stemLength")%>%
  filter(Date == as.Date("2024-07-03"))
stemLength_model <- as.formula(num_values ~ MaternalLine)
stemLength_aov <- aov(stemLength_model, data = stemLength_data)
print(stemLength_aov)
summary(stemLength_aov)
p <- ggplot(data = stemLength_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "Accession & Stem Length (07/03/24)")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Stem Length (cm)")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/accession_stemlength_anova.png", width = 10, height = 7, dpi = 600)


numStem_data <- filter(Boltonia_data_CountyLabels, num_traits == "numStem")%>%
    filter(Date == as.Date("2024-07-03"))
numStem_model <- as.formula(num_values ~ MaternalLine)
numStem_aov <- aov(numStem_model, data = numStem_data)
print(numStem_aov)
summary(numStem_aov)
p <- ggplot(data = numStem_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "Accession & Number of Stems (07/03/24)")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number Stems")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/accession_numstem_anova.png", width = 10, height = 7, dpi = 600)

numDiscF_data <- filter(Boltonia_data_CountyLabels, num_traits == "numDiscF")%>%
  filter(Date == as.Date("2024-07-03"))
numDiscF_model <- as.formula(num_values ~ MaternalLine)
numDiscF_aov <- aov(numDiscF_model, data = numDiscF_data)
print(numDiscF_aov)
summary(numDiscF_aov)
p <- ggplot(data = numDiscF_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "Accession & Number of Disc Flowers (07/03/24)")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number Disc Flowers")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/accession_discflwrs_anova.png", width = 10, height = 7, dpi = 600)

numRayF_data <- filter(Boltonia_data_CountyLabels, num_traits == "numRayF")%>%
  filter(Date == as.Date("2024-07-03"))
numRayF_model <- as.formula(num_values ~ MaternalLine)
numRayF_aov <- aov(numRayF_model, data = numRayF_data)
print(numRayF_aov)
summary(numDiscF_aov)
p <- ggplot(data = numRayF_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "Accession & Number of Ray Flowers (07/03/24)")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number Ray Flowers")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/accession_rayflwrs_anova.png", width = 10, height = 7, dpi = 600)

numFlwrB_data <- filter(Boltonia_data_CountyLabels, num_traits == "numFlwrB")%>%
  filter(Date == as.Date("2024-07-03"))
numFlwrB_model <- as.formula(num_values ~ MaternalLine)
numFlwrB_aov <- aov(numFlwrB_model, data = numFlwrB_data)
print(numFlwrB_aov)
summary(numFlwrB_aov)
p <- ggplot(data = numFlwrB_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "Accession & Number of Flower Buds (07/03/24)")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number Flower Buds")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/accession_flwrbud_anova.png", width = 10, height = 7, dpi = 600)

numRos_data <- filter(Boltonia_data_CountyLabels, num_traits == "numRos")%>%
  filter(Date == as.Date("2024-07-03"))
numRos_model <- as.formula(num_values ~ MaternalLine)
numRos_aov <- aov(numRos_model, data = numRos_data)
p <- ggplot(data = numRos_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "Accession & Number of Rosettes (07/03/24)")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number of Rosettes)")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/accession_rosettes_anova.png", width = 10, height = 7, dpi = 600)

numDeadF_data <- filter(Boltonia_data_CountyLabels, num_traits == "numDeadF")%>%
  filter(Date == as.Date("2024-07-03"))
numDeadF_model <- as.formula(num_values ~ MaternalLine)
numDeadF_aov <- aov(numDeadF_model, data = numDeadF_data)
p <- ggplot(data = numDeadF_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "Accession & Number of Dead Flowers (07/03/24)")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number Dead Flowers")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/accession_deadflwr_anova.png", width = 10, height = 7, dpi = 600)

numLSt_data <- filter(Boltonia_data_CountyLabels, num_traits == "numLSt")%>%
  filter(Date == as.Date("2024-07-03"))
numLSt_model <- as.formula(num_values ~ MaternalLine)
numLSt_aov <- aov(numLSt_model, data = numLSt_data)
print(numLSt_aov)
summary(numLSt_aov)
p <- ggplot(data = numLSt_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "Accession & Number of Lateral Stems (07/03/24)")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number Lateral Stems")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/accession_lateralstems_anova.png", width = 10, height = 7, dpi = 600)

numLvs_data <- filter(Boltonia_data_CountyLabels, num_traits == "numLvs")%>%
  filter(Date == as.Date("2024-05-15"))
numLvs_model <- as.formula(num_values ~ MaternalLine)
numLvs_aov <- aov(numLvs_model, data = numLvs_data)
print(numLvs_aov)
p <- ggplot(data = numLvs_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "Accession & Number of Leaves (05/15/24)")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number of Leaves")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/accession_numleaves_anova.png", width = 10, height = 7, dpi = 600)

latBuds_data <- filter(Boltonia_data_CountyLabels, num_traits == "latBuds")%>%
  filter(Date == as.Date("2024-05-01"))
latBuds_model <- as.formula(num_values ~ MaternalLine)
latBuds_aov <- aov(latBuds_model, data = latBuds_data)
p <- ggplot(data = latBuds_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "Accession & Number of Lateral Buds (05/01/24)")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Number Lateral Buds")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/accession_latbuds_anova.png", width = 10, height = 7, dpi = 600)

leafLong_data <- filter(Boltonia_data_CountyLabels, num_traits == "leafLong")%>%
  filter(Date == as.Date("2024-05-15"))
leafLong_model <- as.formula(num_values ~ MaternalLine)
leafLong_aov <- aov(leafLong_model, data = leafLong_data)
p <- ggplot(data = leafLong_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "Accession & Leaf Length (05/15/24)")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Leaf Length")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/accession_leaflength_anova.png", width = 10, height = 7, dpi = 600)

leafWide_data <- filter(Boltonia_data_CountyLabels, num_traits == "leafWide")%>%
  filter(Date == as.Date("2024-05-15"))
leafWide_model <- as.formula(num_values ~ MaternalLine)
leafWide_aov <- aov(leafWide_model, data = leafWide_data)
p <- ggplot(data = leafWide_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "Accession & Leaf Width (05/15/24)")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Leaf Width")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p
ggsave("./figures/accession_leafwide_anova.png", width = 10, height = 7, dpi = 600)

##MaternalLine plots#######
###########################
p <- ggplot(data = filter(Boltonia_data, num_traits == "stemLength", Date == as.Date("2024-07-03")), aes(x = MaternalLine, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("Maternal Line & Stem Length (07/03/24)")+
  ylab("Stem Length (cm)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_stemlength.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "numStem", Date == as.Date("2024-07-03")), aes(x = MaternalLine, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("Maternal Line & Number of Stems (07/03/24)")+
  ylab("numStem")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numStem.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "numRos", Date == as.Date("2024-07-03")), aes(x = MaternalLine, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("Maternal Line & Number of Rosettes (07/03/24)")+
  ylab("numRos")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numRos.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "numRayF", Date == as.Date("2024-07-03")), aes(x = MaternalLine, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("Maternal Line & Number of Ray flowers (07/03/24)")+
  ylab("numRayF")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numRayF.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "numFlwrB", Date == as.Date("2024-07-03")), aes(x = MaternalLine, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("Maternal Line & Number of Flowerbuds (07/03/24)")+
  ylab("numFlwrB")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numFlwrB.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "numDiscF", Date == as.Date("2024-07-03")), aes(x = MaternalLine, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("Maternal Line & Number of Disc Flowers (07/03/24)")+
  ylab("numDiscF")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numDiscF.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "leafWide", Date == as.Date("2024-05-15")), aes(x = MaternalLine, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("Maternal Line & Leaf Width (05/15/24)")+
  ylab("leafWidth")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_leafWide.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "leafLong", Date == as.Date("2024-05-15")), aes(x = MaternalLine, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("Maternal Line & Leaf Length (05/15/24)")+
  ylab("leafLength")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_leafLong.png", width = 10, height = 7, dpi = 600)
  
p <- ggplot(data = filter(Boltonia_data, num_traits == "numLSt", Date == as.Date("2024-07-03")), aes(x = MaternalLine, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("Maternal Line & Number of Lateral Stems (07/03/24)")+
  ylab("numLSt")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numLSt.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "numLvs", Date == as.Date("2024-05-15")), aes(x = MaternalLine, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("Maternal Line & Number of Leaves (05/15/24)")+
  ylab("numLvs")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/MaternalLine_numLvs.png", width = 10, height = 7, dpi = 600)


##County plots##########
#######################

p <- ggplot(data = filter(Boltonia_data, num_traits == "stemLength", Date == as.Date("2024-07-03")), aes(x = County, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("County & Stem Length (07/03/24)")+
  ylab("Stem Length (cm)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_stemlength.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "numStem", Date == as.Date("2024-07-03")), aes(x = County, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("County & Number of Stems (07/03/24)")+
  ylab("Number of Stems")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_numStem.png", width = 10, height = 7, dpi = 600)

p <-ggplot(data = filter(Boltonia_data, num_traits == "numFlwrB", Date == as.Date("2024-07-03")), aes(x = County, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("County & Number of Flower Buds (07/03/24)")+
  ylab("numFlwrB")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_numFlwrB.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "numLSt", Date == as.Date("2024-07-03")), aes(x = County, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("County & Number of Lateral Stems (07/03/24)")+
  ylab("numLSt")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_numLSt.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "numRos", Date == as.Date("2024-07-03")), aes(x = County, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("County & Number of Rosettes (07/03/24)")+
  ylab("numRos")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_numRos.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "leafLong", Date == as.Date("2024-05-15")), aes(x = County, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("County & Leaf Length (05/15/24)")+
  ylab("Leaf Length")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_leaflong.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "leafWide", Date == as.Date("2024-05-15")), aes(x = County, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("County & Leaf Width (05/15/24)")+
  ylab("Leaf Width")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_leafwide.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "numLvs", Date == as.Date("2024-05-15")), aes(x = County, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("County & Number of Leaves (05/15/24)")+
  ylab("numLvs")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_numLvs.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "numRayF", Date == as.Date("2024-07-03")), aes(x = County, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("County & Number of Ray Flowers (07/03/24)")+
  ylab("numRayF")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_numrayF.png", width = 10, height = 7, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "numDiscF", Date == as.Date("2024-07-03")), aes(x = County, y = num_values))+
  geom_boxplot(color = "blue", fill = "skyblue")+
  geom_point()+
  ggtitle("County & Number of Disc Flowers (07/03/24)")+
  ylab("numDiscF")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p
ggsave("./figures/County_numdiscF.png", width = 10, height = 7, dpi = 600)


