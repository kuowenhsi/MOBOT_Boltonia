library(tidyverse)
library(ggpubr)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

Boltonia_data <- read_csv("./data/Boltonia_merged_data_tidy_20240717.csv")

# MaternalLine is actually the site (Accession).
# FlowerHead is actually the individual (real maternal genotype).
Boltonia_data_rep <- Boltonia_data %>%
  group_by(MaternalLine, County, FlowerHead, index)%>%
  summarize()%>%
  group_by(MaternalLine, County, FlowerHead)%>%
  summarise(rep = n())

## We have 1-4 replications per individual (real maternal genotype).
## This number is insufficient for heritability analysis (ANOVA).
## Therefore, we use site (MaternalLine = Accession) as "genotype" for analyses.

unique(Boltonia_data$num_traits)

# make flowering ratio by Date into a plot

Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(num_traits %in% c("numFlwrB", "numRayF", "numDiscF")) %>%
  mutate(Date = factor(Date)) %>%
  mutate(num_values = case_when(is.na(num_values) ~ 0, num_values == 0 ~ 0, num_values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date, num_traits) %>%
  summarize(flowerRatio = sum(num_values)/n()) %>%
  ungroup() %>%
  complete(Date, num_traits) %>%
  mutate(flowerRatio = case_when(is.na(flowerRatio) ~ 0, TRUE ~ flowerRatio), Date = as.Date(Date))

# Then plot flowering ratio by Date and County

# calculate county mean coordinations

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


# make days to flower data

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
  left_join(read_csv("./data/Boltonia_merged_data_20240627.csv") %>% select(1:12), by = "index", relationship = "many-to-many")

Boltonia_data_CountyLabels <- Boltonia_data %>%
  left_join(Boltonia_data_county_coordinations, by = "County")
  
### TODO if we do not want those dead plants in flowering ratio calculation


# plot the total flower ration across all dates

p <- ggplot(data = Boltonia_data_FlowerRatio, mapping = aes(x = Date, y = flowerRatio)) + 
  geom_col(aes(fill = num_traits))+
  scale_y_continuous(name = "Total flower ratio", limits = c(0, 1), expand = c(0, 0, 0, 0))+
  scale_x_date(name = "", breaks = unique(Boltonia_data_FlowerRatio$Date), date_labels = "%b %d")+
  facet_wrap(.~num_traits, nrow = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid.minor = element_blank(), legend.position = "none")

ggsave("./figures/Boltonia_total_flower_ratio.png", width = 8, height = 12)



# plot

# County first
p <- ggplot(data = Boltonia_data_FlowerRatio_county, aes(x = reorder(labels, mean_Google_latitude), y = flowerRatio))+
  geom_col(aes(group = Date, fill = Date), position = position_dodge())+
  scale_x_discrete(name = "")+
  scale_fill_date(low = "yellow", high = "red")+
  facet_wrap(.~num_traits, nrow = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid.minor = element_blank(), legend.position = "bottom", legend.key.width = unit(1, "in") )

ggsave("./figures/Boltonia_total_flower_ratio_by_county.png", width = 10, height = 6)


# buds
p <- ggplot(data = filter(Boltonia_data_FlowerRatio_county, num_traits == "numFlwrB"), aes(x = Date, y = flowerRatio))+
  geom_col(aes(fill = as.character(Date)))+
  scale_y_continuous(name = "Flower bud ratio")+
  scale_x_date(name = "")+
  facet_wrap(.~reorder(labels, mean_Google_latitude), nrow = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid = element_blank(), legend.position = "none")
ggsave("./figures/Boltonia_total_bud_ratio_by_county.png", width = 10, height = 10)


# Ray
p <- ggplot(data = filter(Boltonia_data_FlowerRatio_county, num_traits == "numRayF"), aes(x = Date, y = flowerRatio))+
  geom_col(aes(fill = as.character(Date)))+
  scale_y_continuous(name = "Flower ray flower ratio")+
  scale_x_date(name = "")+
  facet_wrap(.~reorder(labels, mean_Google_latitude), nrow = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid = element_blank(), legend.position = "none")
ggsave("./figures/Boltonia_total_ray_ratio_by_county.png", width = 10, height = 10)

# buds
p <- ggplot(data = filter(Boltonia_data_FlowerRatio_county, num_traits == "numDiscF"), aes(x = Date, y = flowerRatio))+
  geom_col(aes(fill = as.character(Date)))+
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
  facet_wrap(.~num_traits, nrow = 1)+
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
  facet_wrap(.~num_traits, nrow = 1)+
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


# if we do not want those dead plants in flowering ratio calculation
mutate(num_values = case_when(is.na(num_values) ~ 0, num_values == 0 ~ 0, num_values >= 1 ~ 1, TRUE ~ as.numeric(NA)))

##########################

p <- ggplot(data = filter(Boltonia_data, num_traits == "leafLong", Date == as.Date("2024-05-01")), aes(x = County, y = num_values))+
  geom_violin(aes(fill = County), color = NA, alpha = 0.7)+
  geom_point()

p
ggsave("./figures/county_figure.png", width = 10, height = 10, dpi = 600)


p <- ggplot(data = filter(Boltonia_data, num_traits == "stemLength", Date == as.Date("2024-06-19")), aes(x = Google_Latitude, y = num_values))+
  geom_point()
p


p <- ggplot(data = filter(Boltonia_data, num_traits == "stemLength", Date == as.Date("2024-06-19")), aes(x = Google_Longitude, y = num_values))+
  geom_point()
p


##State plots##########
#######################

p <- ggplot(data = filter(Boltonia_data_CountyLabels, num_traits == "stemLength"), aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  labs(title = "County & Stem Length")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Stem Length (cm)")+
  facet_grid(~reorder(labels, mean_Google_latitude), scales = "free_x", space = "free_x")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("./figures/County_stemlength.png", width = 30, height = 8, dpi = 600)

stemLength_data <- filter(Boltonia_data_CountyLabels, num_traits == "stemLength")%>%
  filter(Date == as.Date("2024-07-03"))

p <- ggplot(data = stemLength_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "County & Stem Length")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Stem Length (cm)")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p

stemLength_model <- as.formula(num_values ~ MaternalLine)
## Method 1

stemLength_aov <- aov(stemLength_model, data = stemLength_data)
print(stemLength_aov)
summary(stemLength_aov)

## Method 2

stemLength_anova <- anova(lm(stemLength_model, data = stemLength_data))
print(stemLength_anova)
summary(stemLength_anova)

## Method 3
library(car)

stemLength_data <- filter(Boltonia_data_CountyLabels, num_traits == "stemLength")
stemLength_Anova <- Anova(lm(num_values ~ MaternalLine + Date + MaternalLine:Date, data = stemLength_data), type = "III")
print(stemLength_Anova)
summary(stemLength_Anova)

## Method 4
library(lmerTest)

stemLength_data <- filter(Boltonia_data_CountyLabels, num_traits == "stemLength")
stemLength_model_random_effect <- as.formula(num_values ~ MaternalLine + (1|Date))

stemLength_Anova <- anova(lmer(stemLength_model_random_effect, data = stemLength_data), type = "II")
print(stemLength_Anova)
summary(stemLength_Anova)

p <-ggplot(data = filter(Boltonia_data, num_traits == "numFlwrB", Date == as.Date("2024-06-19")), aes(x = County, y = num_values))+
  geom_point()+
  labs(title = "County & Number of Flower Buds")
p
ggsave("./figures/County_numFlwrB.png", width = 15, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "numLSt", Date == as.Date("2024-06-19")), aes(x = County, y = num_values))+
  geom_point()
p
ggsave("./figures/County_numLSt.png", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "numRos", Date == as.Date("2024-06-19")), aes(x = County, y = num_values))+
  geom_point()
p
ggsave("./figures/County_numRos", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "leafLong", Date == as.Date("2024-05-15")), aes(x = County, y = num_values))+
  geom_point()
p
ggsave("./figures/County_leaflong.png", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, num_traits == "leafWide", Date == as.Date("2024-05-15")), aes(x = County, y = num_values))+
  geom_point()
p



