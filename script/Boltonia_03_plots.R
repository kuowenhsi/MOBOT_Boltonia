library(tidyverse)
library(ggpubr)
library(lmerTest)
library(car)
library(cowplot)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

Boltonia_data <- read_csv("./data/Boltonia_merged_data_tidy_20240717.csv")

# MaternalLine is actually the site (Accession).
# FlowerHead is actually the individual (real maternal genotype).
Boltonia_data_rep <- Boltonia_data %>%
  group_by(MaternalLine, County, FlowerHead, index)%>%
  summarize()%>%
  group_by(MaternalLine, County, FlowerHead)%>%
  summarise(rep = n())

unique(Boltonia_data$MaternalLine)
unique(Boltonia_data$Google_latitude)

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
  ungroup()%>%
  arrange(mean_Google_latitude)%>%
  mutate(labels = paste0(County, "\n", "(", round(mean_Google_latitude, 2), ", ", round(mean_Google_longitude, 2), ")"))

Boltonia_data_site_coordinations <- Boltonia_data %>%
  group_by(MaternalLine, County)%>%
  summarize(mean_Google_latitude = mean(Google_latitude), mean_Google_longitude = mean(Google_longitude))%>%
  ungroup()%>%
  arrange(mean_Google_latitude)%>%
  mutate(labels = paste0(MaternalLine, "\n",County, "\n", "(", round(mean_Google_latitude, 2), ", ", round(mean_Google_longitude, 2), ")"))


Boltonia_data_FlowerRatio_county <- Boltonia_data %>%
  filter(num_traits %in% c("numFlwrB", "numRayF", "numDiscF")) %>%
  mutate(Date = factor(Date)) %>%
  mutate(num_values = case_when(is.na(num_values) ~ 0, num_values == 0 ~ 0, num_values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date, num_traits, County) %>%
  summarize(flowerRatio = sum(num_values)/n())%>%
  ungroup() %>%
  complete(Date, num_traits, County) %>%
  mutate(flowerRatio = case_when(is.na(flowerRatio) ~ 0, TRUE ~ flowerRatio), Date = as.Date(Date),
         num_traits = factor(num_traits, levels = c("numFlwrB", "numRayF", "numDiscF")))%>%
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
  left_join(read_csv("./data/Boltonia_merged_data_20240627.csv") %>% select(1:12), by = "index")

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

# ggsave("./figures/Boltonia_total_flower_ratio.png", width = 8, height = 12)


# plot

# County first

strip_labels = c(numFlwrB = "Presence of flower buds", numRayF = "Presence of open ray flowers", numDiscF = "Presence of open disk flowers")

p <- ggplot(data = Boltonia_data_FlowerRatio_county, aes(x = reorder(labels, mean_Google_latitude), y = flowerRatio))+
  geom_col(aes(group = Date, fill = Date), position = position_dodge())+
  scale_x_discrete(name = "")+
  scale_fill_date(low = "yellow", high = "red")+
  facet_wrap(.~num_traits, nrow = 3, labeller = labeller(num_traits = strip_labels))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid.minor = element_blank(), legend.position = "bottom", legend.key.width = unit(1, "in") )


# ggsave("./figures/Boltonia_total_flower_ratio_by_county.png", width = 10, height = 6)


p1 <- ggplot(data = filter(Boltonia_data_FlowerRatio_county, num_traits == "numDiscF"), aes(x = reorder(labels, mean_Google_latitude), y = flowerRatio))+
  geom_col(aes(group = Date, fill = Date), position = position_dodge())+
  scale_x_discrete(name = "")+
  scale_y_continuous("Flower ratio")+
  scale_fill_date(low = "yellow", high = "red")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid.minor = element_blank(), legend.position = c(0.7, 0.86), legend.key.width = unit(0.6, "in"), legend.key.height = unit(0.08, "in"),legend.direction = "horizontal", legend.title = element_blank(), legend.background = element_rect(fill = NA))

p
ggsave("./figures/Boltonia_disk_flower_ratio_by_county.png", width = 10, height = 2.5)


# buds
p <- ggplot(data = filter(Boltonia_data_FlowerRatio_county, num_traits == "numFlwrB"), aes(x = Date, y = flowerRatio))+
  geom_col(aes(fill = as.character(Date)))+
  scale_y_continuous(name = "Flower bud ratio")+
  scale_x_date(name = "")+
  facet_wrap(.~reorder(labels, mean_Google_latitude), nrow = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid = element_blank(), legend.position = "none")
# ggsave("./figures/Boltonia_total_bud_ratio_by_county.png", width = 10, height = 10)


# Ray
p <- ggplot(data = filter(Boltonia_data_FlowerRatio_county, num_traits == "numRayF"), aes(x = Date, y = flowerRatio))+
  geom_col(aes(fill = as.character(Date)))+
  scale_y_continuous(name = "Flower ray flower ratio")+
  scale_x_date(name = "")+
  facet_wrap(.~reorder(labels, mean_Google_latitude), nrow = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid = element_blank(), legend.position = "none")
# ggsave("./figures/Boltonia_total_ray_ratio_by_county.png", width = 10, height = 10)

# buds
p <- ggplot(data = filter(Boltonia_data_FlowerRatio_county, num_traits == "numDiscF"), aes(x = Date, y = flowerRatio))+
  geom_col(aes(fill = as.character(Date)))+
  scale_y_continuous(name = "Flower disc ratio")+
  scale_x_date(name = "")+
  facet_wrap(.~reorder(labels, mean_Google_latitude), nrow = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.grid = element_blank(), legend.position = "none")
# ggsave("./figures/Boltonia_total_disk_ratio_by_county.png", width = 10, height = 10)


################

### Days to flower


# simple linear model
DaysToFlower_bud_lm <- lm(as.integer(DaysToFlower) ~ Google_latitude, data = filter(Boltonia_data_DaysToFlower, num_traits == "numFlwrB", as.integer(DaysToFlower) < 150))
DaysToFlower_bud_lm
Anova(DaysToFlower_bud_lm, type = "II")

DaysToFlower_ray_lm <- lm(as.integer(DaysToFlower) ~ Google_latitude, data = filter(Boltonia_data_DaysToFlower, num_traits == "numRayF", as.integer(DaysToFlower) < 150))
DaysToFlower_ray_lm 
Anova(DaysToFlower_ray_lm, type = "II")

DaysToFlower_disc_lm <- lm(as.integer(DaysToFlower) ~ Google_latitude, data = filter(Boltonia_data_DaysToFlower, num_traits == "numDiscF", as.integer(DaysToFlower) < 150))
DaysToFlower_disc_lm
Anova(DaysToFlower_disc_lm, type = "II")


# mixed effect model

DaysToFlower_bud_lmm <- lmer(as.integer(DaysToFlower) ~ Google_latitude + (1|Google_latitude/FlowerHead), data = filter(Boltonia_data_DaysToFlower, num_traits == "numFlwrB", as.integer(DaysToFlower) < 150))
DaysToFlower_bud_lmm
as_tibble(VarCorr(DaysToFlower_bud_lmm))%>%select(grp, vcov)%>%pivot_wider(names_from = "grp", values_from = "vcov")
as_tibble_row(fixef(DaysToFlower_bud_lmm))%>%rename(slope = Google_latitude, intercept = `(Intercept)`)

DaysToFlower_bud_lmm_anova <- as_tibble(anova(DaysToFlower_bud_lmm, type = "II"))%>%
  mutate(num_traits = "numFlwrB", label_text = paste0("Days ~ Latitude + (1|Site/MaternalLine)", "\n", "Type II ANOVA, ", "F", "(", NumDF, ", ", round(DenDF, 0), ") = ", round(`F value`, 2), ", p = ", round(`Pr(>F)`, 2)))%>%
  bind_cols(as_tibble_row(fixef(DaysToFlower_bud_lmm))%>%rename(slope = Google_latitude, intercept = `(Intercept)`), as_tibble(VarCorr(DaysToFlower_bud_lmm))%>%select(grp, vcov)%>%pivot_wider(names_from = "grp", values_from = "vcov"))

DaysToFlower_ray_lmm <- lmer(as.integer(DaysToFlower) ~ Google_latitude + (1|Google_latitude/FlowerHead), data = filter(Boltonia_data_DaysToFlower, num_traits == "numRayF", as.integer(DaysToFlower) < 150))
VarCorr(DaysToFlower_ray_lmm)
DaysToFlower_ray_lmm_anova <- as_tibble(anova(DaysToFlower_ray_lmm, type = "II"))%>%
  mutate(num_traits = "numRayF", label_text = paste0("Days ~ Latitude + (1|Site/MaternalLine)", "\n", "Type II ANOVA, ", "F", "(", NumDF, ", ", round(DenDF, 0), ") = ", round(`F value`, 2), ", p = ", round(`Pr(>F)`, 2)))%>%
  bind_cols(as_tibble_row(fixef(DaysToFlower_ray_lmm))%>%rename(slope = Google_latitude, intercept = `(Intercept)`), as_tibble(VarCorr(DaysToFlower_ray_lmm))%>%select(grp, vcov)%>%pivot_wider(names_from = "grp", values_from = "vcov"))

DaysToFlower_disk_lmm <- lmer(as.integer(DaysToFlower) ~ Google_latitude + (1|Google_latitude/FlowerHead), data = filter(Boltonia_data_DaysToFlower, num_traits == "numDiscF", as.integer(DaysToFlower) < 150))
VarCorr(DaysToFlower_disk_lmm)
DaysToFlower_disk_lmm_anova <- as_tibble(anova(DaysToFlower_disk_lmm, type = "II"))%>%
  mutate(num_traits = "numDiscF", label_text = paste0("Days ~ Latitude + (1|Site/MaternalLine)", "\n", "Type II ANOVA, ", "F", "(", NumDF, ", ", round(DenDF, 0), ") = ", round(`F value`, 2), ", p = ", round(`Pr(>F)`, 2)))%>%
  bind_cols(as_tibble_row(fixef(DaysToFlower_disk_lmm))%>%rename(slope = Google_latitude, intercept = `(Intercept)`), as_tibble(VarCorr(DaysToFlower_disk_lmm))%>%select(grp, vcov)%>%pivot_wider(names_from = "grp", values_from = "vcov"))

DaysToFlower_lmm_anova <- bind_rows(DaysToFlower_bud_lmm_anova, DaysToFlower_ray_lmm_anova, DaysToFlower_disk_lmm_anova)%>%
  mutate(num_traits = factor(num_traits, levels = c("numFlwrB", "numRayF", "numDiscF")))
DaysToFlower_lmm_anova
colnames(DaysToFlower_lmm_anova)


### Heritability
DaysToFlower_bud_H <- lmer(as.integer(DaysToFlower) ~ (1|MaternalLine/FlowerHead), data = filter(Boltonia_data_DaysToFlower, num_traits == "numFlwrB", as.integer(DaysToFlower) < 150))%>%
  VarCorr() %>%
  as_tibble()%>%
  select(grp, vcov)%>%
  pivot_wider(names_from = "grp", values_from = "vcov") %>%
  mutate(num_traits = "numFlwrB", H_model = "Days ~ (1|Site/MaternalLine)")

DaysToFlower_ray_H <- lmer(as.integer(DaysToFlower) ~ (1|MaternalLine/FlowerHead), data = filter(Boltonia_data_DaysToFlower, num_traits == "numRayF", as.integer(DaysToFlower) < 150))%>%
  VarCorr() %>%
  as_tibble()%>%
  select(grp, vcov)%>%
  pivot_wider(names_from = "grp", values_from = "vcov") %>%
  mutate(num_traits = "numRayF", H_model = "Days ~ (1|Site/MaternalLine)")

DaysToFlower_disc_H <- lmer(as.integer(DaysToFlower) ~ (1|MaternalLine/FlowerHead), data = filter(Boltonia_data_DaysToFlower, num_traits == "numDiscF", as.integer(DaysToFlower) < 150))%>%
  VarCorr() %>%
  as_tibble()%>%
  select(grp, vcov)%>%
  pivot_wider(names_from = "grp", values_from = "vcov") %>%
  mutate(num_traits = "numDiscF", H_model = "Days ~ (1|Site/MaternalLine)")

DaysToFlower_H<- bind_rows(DaysToFlower_bud_H, DaysToFlower_ray_H, DaysToFlower_disc_H)%>%
  mutate(num_traits = factor(num_traits, levels = c("numFlwrB", "numRayF", "numDiscF")))%>%
  mutate(heritability = `FlowerHead:MaternalLine`/(`FlowerHead:MaternalLine` + MaternalLine + Residual))%>%
  select(num_traits, H_model, heritability)

DaysToFlower_H

DaysToFlower_lmm_anova_H <- DaysToFlower_lmm_anova %>%
  left_join(DaysToFlower_H, by = "num_traits")

DaysToFlower_lmm_anova_H

### plot for latitude


#### Stem Length

Boltonia_stemLength_data <- Boltonia_data %>%
  filter(Date == as.Date("2024-07-11"), num_traits %in% c("stemLength", "numDeadF", "numDiscF", "numFlwrB", "numRayF"))%>%
  pivot_wider(names_from = "num_traits", values_from = "num_values")%>%
  rowwise()%>%
  mutate(total_flowers = sum(numDeadF, numDiscF, numFlwrB, numRayF))%>%
  ungroup()%>%
  filter(total_flowers > 0)

## only Site (MaternalLine is included because otherwise the model cannot be fitted)

Boltonia_stemLength_lmm <- lmer(stemLength ~ Google_latitude + (1|MaternalLine), data = Boltonia_stemLength_data)
VarCorr(Boltonia_stemLength_lmm)
Boltonia_stemLength_lmm_anova <- as_tibble(anova(Boltonia_stemLength_lmm, type = "II"))%>%
  mutate(label_text = paste0("Days ~ Latitude + (1|Site)", "\n", "Type II ANOVA, ", "F", "(", NumDF, ", ", round(DenDF, 0), ") = ", round(`F value`, 2), ", p = ", round(`Pr(>F)`, 2)))%>%
  bind_cols(as_tibble_row(fixef(Boltonia_stemLength_lmm))%>%rename(slope = Google_latitude, intercept = `(Intercept)`), as_tibble(VarCorr(Boltonia_stemLength_lmm))%>%select(grp, vcov)%>%pivot_wider(names_from = "grp", values_from = "vcov"))

Boltonia_stemLength_lmm_anova
colnames(Boltonia_stemLength_lmm)


##################################
## Association plots for posters##
##################################


p1 <- ggplot(data = Boltonia_stemLength_data, aes(x = Google_latitude, y = stemLength))+
  geom_point()+
  geom_abline(data = Boltonia_stemLength_lmm_anova, aes(intercept = intercept, slope = slope), color = "blue")+
  geom_text(x = 38.6, y = 140, hjust = 0, mapping = aes(label = label_text), data = Boltonia_stemLength_lmm_anova)+
  geom_text(x = 38.6, y = 15, hjust = 0, mapping = aes(label = paste0("y = ", round(intercept, 2), round(slope, 2), "x")), data = Boltonia_stemLength_lmm_anova, color = "blue")+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Stem Length (cm)", limits = c(10, 150))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")
p1



p2 <- ggplot(data = filter(Boltonia_data_DaysToFlower, num_traits == "numDiscF"), aes(x = Google_latitude, y = as.integer(DaysToFlower)))+
  geom_point()+
  geom_point(data = filter(Boltonia_data_DaysToFlower, num_traits == "numDiscF", as.integer(DaysToFlower) == 150), color = "red")+
  geom_abline(data = filter(DaysToFlower_disk_lmm_anova, num_traits == "numDiscF"), aes(intercept = intercept, slope = slope), color = "blue")+
  geom_text(x = 38.6, y = 160, hjust = 0, mapping = aes(label = label_text), data = DaysToFlower_disk_lmm_anova)+
  geom_text(x = 38.6, y = 60, hjust = 0, mapping = aes(label = paste0("y = ", round(intercept, 2), round(slope, 2), "x")), data = filter(DaysToFlower_disk_lmm_anova, num_traits == "numDiscF"), color = "blue")+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Days since planting", limits = c(60, 165))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p2

p <- plot_grid(p1, p2, align = "hv")
p

ggsave("./figures/Boltonia_poster_regression.png", width = 12, height = 6, dpi = 600)

##############


strip_labels = c(numFlwrB = "First flower bud", numRayF = "First open ray flowers", numDiscF = "First open disk flowers")

p <- ggplot(data = Boltonia_data_DaysToFlower, aes(x = Google_latitude, y = as.integer(DaysToFlower)))+
  geom_point()+
  geom_point(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) == 150), color = "red")+
  # stat_smooth(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150, num_traits == "numDiscF"), method = "lm", color = "blue")+
  geom_abline(data = filter(DaysToFlower_lmm_anova_H, num_traits == "numDiscF"), aes(intercept = intercept, slope = slope), color = "blue")+
  # stat_cor(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  # stat_regline_equation(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150),label.x.npc = 0, label.y.npc = 0.95)+
  geom_text(x = 38.6, y = 160, hjust = 0, mapping = aes(label = label_text), data = DaysToFlower_lmm_anova_H)+
  geom_text(x = 38.6, y = 60, hjust = 0, mapping = aes(label = paste0("y = ", round(intercept, 2), round(slope, 2), "x")), data = filter(DaysToFlower_lmm_anova_H, num_traits == "numDiscF"), color = "blue")+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Days since planting", limits = c(60, 165))+
  facet_wrap(.~num_traits, nrow = 1, labeller = labeller(num_traits = strip_labels))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  

# ggsave("./figures/Boltonia_days_to_flower_by_latitude.png", width = 13, height = 5)

### plot for longitude

# mixed effect model

DaysToFlower_bud_lmm <- lmer(as.integer(DaysToFlower) ~ Google_longitude + (1|Google_longitude/FlowerHead), data = filter(Boltonia_data_DaysToFlower, num_traits == "numFlwrB", as.integer(DaysToFlower) < 150))
DaysToFlower_bud_lmm
as_tibble(VarCorr(DaysToFlower_bud_lmm))%>%select(grp, vcov)%>%pivot_wider(names_from = "grp", values_from = "vcov")
as_tibble_row(fixef(DaysToFlower_bud_lmm))%>%rename(slope = Google_longitude, intercept = `(Intercept)`)

DaysToFlower_bud_lmm_anova <- as_tibble(anova(DaysToFlower_bud_lmm, type = "II"))%>%
  mutate(num_traits = "numFlwrB", label_text = paste0("Days ~ Longitude + (1|Longitude/MaternalLine)", "\n", "Type II ANOVA, ", "F", "(", NumDF, ", ", round(DenDF, 0), ") = ", round(`F value`, 2), ", p = ", round(`Pr(>F)`, 2)))%>%
  bind_cols(as_tibble_row(fixef(DaysToFlower_bud_lmm))%>%rename(slope = Google_longitude, intercept = `(Intercept)`), as_tibble(VarCorr(DaysToFlower_bud_lmm))%>%select(grp, vcov)%>%pivot_wider(names_from = "grp", values_from = "vcov"))

DaysToFlower_ray_lmm <- lmer(as.integer(DaysToFlower) ~ Google_longitude + (1|Google_longitude/FlowerHead), data = filter(Boltonia_data_DaysToFlower, num_traits == "numRayF", as.integer(DaysToFlower) < 150))
VarCorr(DaysToFlower_ray_lmm)
DaysToFlower_ray_lmm_anova <- as_tibble(anova(DaysToFlower_ray_lmm, type = "II"))%>%
  mutate(num_traits = "numRayF", label_text = paste0("Days ~ Longitude + (1|Longitude/MaternalLine)", "\n", "Type II ANOVA, ", "F", "(", NumDF, ", ", round(DenDF, 0), ") = ", round(`F value`, 2), ", p = ", round(`Pr(>F)`, 2)))%>%
  bind_cols(as_tibble_row(fixef(DaysToFlower_ray_lmm))%>%rename(slope = Google_longitude, intercept = `(Intercept)`), as_tibble(VarCorr(DaysToFlower_ray_lmm))%>%select(grp, vcov)%>%pivot_wider(names_from = "grp", values_from = "vcov"))

DaysToFlower_disk_lmm <- lmer(as.integer(DaysToFlower) ~ Google_longitude + (1|Google_longitude/FlowerHead), data = filter(Boltonia_data_DaysToFlower, num_traits == "numDiscF", as.integer(DaysToFlower) < 150))
VarCorr(DaysToFlower_disk_lmm)
DaysToFlower_disk_lmm_anova <- as_tibble(anova(DaysToFlower_disk_lmm, type = "II"))%>%
  mutate(num_traits = "numDiscF", label_text = paste0("Days ~ Longitude + (1|Longitude/MaternalLine)", "\n", "Type II ANOVA, ", "F", "(", NumDF, ", ", round(DenDF, 0), ") = ", round(`F value`, 2), ", p = ", round(`Pr(>F)`, 2)))%>%
  bind_cols(as_tibble_row(fixef(DaysToFlower_disk_lmm))%>%rename(slope = Google_longitude, intercept = `(Intercept)`), as_tibble(VarCorr(DaysToFlower_disk_lmm))%>%select(grp, vcov)%>%pivot_wider(names_from = "grp", values_from = "vcov"))

DaysToFlower_lmm_anova <- bind_rows(DaysToFlower_bud_lmm_anova, DaysToFlower_ray_lmm_anova, DaysToFlower_disk_lmm_anova)%>%
  mutate(num_traits = factor(num_traits, levels = c("numFlwrB", "numRayF", "numDiscF")))
DaysToFlower_lmm_anova
colnames(DaysToFlower_lmm_anova)



p <- ggplot(data = Boltonia_data_DaysToFlower, aes(x = Google_longitude, y = as.integer(DaysToFlower)))+
  geom_point()+
  geom_point(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) == 150), color = "red")+
  # stat_smooth(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150, num_traits == "numDiscF"), method = "lm", color = "blue")+
  # geom_abline(data = filter(DaysToFlower_lmm_anova_H, num_traits == "numDiscF"), aes(intercept = intercept, slope = slope), color = "blue")+
  # stat_cor(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  # stat_regline_equation(data = filter(Boltonia_data_DaysToFlower, as.integer(DaysToFlower) < 150),label.x.npc = 0, label.y.npc = 0.95)+
  geom_text(x = -90.6, y = 160, hjust = 0, mapping = aes(label = label_text), data = DaysToFlower_lmm_anova)+
  # geom_text(x = 38.6, y = 60, hjust = 0, mapping = aes(label = paste0("y = ", round(intercept, 2), round(slope, 2), "x")), data = filter(DaysToFlower_lmm_anova_H, num_traits == "numDiscF"), color = "blue")+
  scale_x_continuous(name = "Longitude")+
  scale_y_continuous(name = "Days since planting", limits = c(60, 165))+
  facet_wrap(.~num_traits, nrow = 1, labeller = labeller(num_traits = strip_labels))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")


# ggsave("./figures/Boltonia_days_to_flower_by_longitude.png", width = 13, height = 5)

###########################################

unique(Boltonia_data$num_traits)


# by County
Boltonia_RosStem_data <- Boltonia_data %>%
  filter(num_traits %in% c("numRos", "numStem"))%>%
  group_by(County, num_traits, Date)%>%
  summarise(average = mean(num_values, na.rm = TRUE), SE = sd(num_values, na.rm = TRUE)/sqrt(n()), sample_size = n())%>%
  ungroup()%>%
  mutate(average = case_when(num_traits == "numStem" ~ -average, TRUE ~ average),
         SE = case_when(num_traits == "numStem" ~ -SE, TRUE ~ SE))%>%
  left_join(Boltonia_data_county_coordinations, by = "County")


p <- ggplot(data = Boltonia_RosStem_data, aes(x = Date, y = average))+
  geom_col(aes(fill = num_traits))+
  geom_errorbar(aes(ymin = average, ymax = average + SE, color = num_traits), width = 0.2)+
  scale_y_continuous(name = "", breaks = c(-1, 0, 1), labels = c("1 stem", "0", "1 rosette"))+
  scale_x_date(name = "")+
  facet_wrap(.~reorder(labels, mean_Google_latitude), nrow = 3)+
  theme_bw()+
  theme(legend.position = "none")
p

ggsave("./figures/Boltonia_RosStem_by_County.png", height = 10, width = 12)


p2 <- ggplot(data = filter(Boltonia_RosStem_data, Date == as.Date("2024-07-11")), aes(x = reorder(labels, mean_Google_latitude), y = average))+
  geom_col(aes(fill = num_traits))+
  geom_errorbar(aes(ymin = average, ymax = average + SE, color = num_traits), width = 0.2)+
  scale_y_continuous(name = "", breaks = c(-1, 0, 1), labels = c("1 stem", "0", "1 rosette"))+
  scale_x_discrete("")+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank())

p

ggsave("./figures/Boltonia_RosStem_by_County_20240711.png", width = 10, height = 1.75)


# by Site
Boltonia_RosStem_data <- Boltonia_data %>%
  filter(num_traits %in% c("numRos", "numStem"))%>%
  group_by(MaternalLine, num_traits, Date)%>%
  summarise(average = mean(num_values, na.rm = TRUE), SE = sd(num_values, na.rm = TRUE)/sqrt(n()), sample_size = n())%>%
  ungroup()%>%
  mutate(average = case_when(num_traits == "numStem" ~ -average, TRUE ~ average),
         SE = case_when(num_traits == "numStem" ~ -SE, TRUE ~ SE))%>%
  left_join(Boltonia_data_site_coordinations, by = "MaternalLine")


p <- ggplot(data = Boltonia_RosStem_data, aes(x = Date, y = average))+
  geom_col(aes(fill = num_traits))+
  geom_errorbar(aes(ymin = average, ymax = average + SE, color = num_traits), width = 0.2)+
  scale_y_continuous(name = "", breaks = c(-1, 0, 1), labels = c("1 stem", "0", "1 rosette"))+
  scale_x_date(name = "")+
  facet_wrap(.~reorder(labels, mean_Google_latitude), nrow = 3)+
  theme_bw()+
  theme(legend.position = "none")
p

ggsave("./figures/Boltonia_RosStem_by_Site.png", height = 10, width = 16)

# by longitude


# if we do not want those dead plants in flowering ratio calculation
mutate(num_values = case_when(is.na(num_values) ~ 0, num_values == 0 ~ 0, num_values >= 1 ~ 1, TRUE ~ as.numeric(NA)))

##########################

p3 <- ggplot(data = filter(Boltonia_data, num_traits == "leafLong", Date == as.Date("2024-05-15")), aes(x = reorder(County, Google_latitude), y = num_values))+
  geom_point(aes(color = County), alpha = 0.7, position = position_jitter(width = 0.1, height = 0))+
  geom_boxplot(aes(color = County), outlier.shape = NA, fill = NA)+
  stat_anova_test(label.y.npc = 0.1)+
  scale_x_discrete(name = "")+
  scale_y_continuous("Leaf length (mm)")+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank())

p
ggsave("./figures/Boltonia_leafLong_20240515.png", width = 10, height = 1.75, dpi = 600)

Boltonia_stemLength_data <- Boltonia_data %>%
  filter(Date == as.Date("2024-07-11"), num_traits %in% c("stemLength", "numDeadF", "numDiscF", "numFlwrB", "numRayF"))%>%
  pivot_wider(names_from = "num_traits", values_from = "num_values")%>%
  rowwise()%>%
  mutate(total_flowers = sum(numDeadF, numDiscF, numFlwrB, numRayF))%>%
  ungroup()%>%
  filter(total_flowers > 0)


p4 <- ggplot(data = Boltonia_stemLength_data, aes(x = reorder(County, Google_latitude), y = stemLength))+
  geom_point(aes(color = County), alpha = 0.7, position = position_jitter(width = 0.1, height = 0))+
  geom_boxplot(aes(color = County), outlier.shape = NA, fill = NA)+
  stat_anova_test(label.y.npc = 0.9, label.x.npc = 0.35)+
  scale_x_discrete(name = "")+
  scale_y_continuous("Stem length (cm)")+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank())

p4
ggsave("./figures/Boltonia_stemLength_20240711.png", width = 10, height = 1.75, dpi = 600)


Boltonia_total_flowers_data <- Boltonia_data %>%
  filter(num_traits %in% c("numDeadF", "numDiscF", "numFlwrB", "numRayF"), Date == as.Date("2024-07-15"))%>%
  group_by(index, MaternalLine, County)%>%
  summarise(mean_Google_latitude = mean(Google_latitude), mean_Google_longitude = mean(Google_longitude), total_flower = sum(num_values, na.rm = TRUE))

p5 <- ggplot(data = Boltonia_total_flowers_data, aes(x = reorder(County, mean_Google_latitude), y = total_flower))+
  geom_point(aes(color = County), alpha = 0.7, position = position_jitter(width = 0.1, height = 0))+
  geom_boxplot(aes(color = County), outlier.shape = NA, fill = NA)+
  stat_anova_test(label.y.npc = 0.9, label.x.npc = 0)+
  scale_x_discrete(name = "")+
  scale_y_continuous("Total flowers")+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank())

p
ggsave("./figures/Boltonia_totalFlower_20240715.png", width = 10, height = 1.75, dpi = 600)


library(cowplot)

p<- plot_grid(p2, p3, p4, p5, p1, ncol = 1, align = "v", rel_heights = c(1.75, 1.75, 1.75, 1.75, 2.5))
ggsave("./figures/combined_plot_for_poster.png", width = 10, height = 9.5, dpi = 600)


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


