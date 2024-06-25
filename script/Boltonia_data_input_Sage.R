library(tidyverse)

setwd("C:/Users/nyanw/Downloads/REU")


list.files()
boltonia_data <- read.csv("Boltonia Phenotype data_20240612.csv", na = c(" ", "NA", "N/A", "DNR"))

#%>%
#mutate(numLSt.6 = case_when((numLSt.6 == "N/A") ~ as.numeric(NA),
                              #TRUE ~ as.numeric(numLSt.6)))

# rm(physaria_data)

###simplifying our data####

correct_stem <- function(x){
  stem_present <- FALSE
  output <- as.vector(rep(NA, length(x)), mode = "numeric")
  
  for (i in 1:length(x)){
    if (stem_present == FALSE){
      if(is.na(x[[i]])){
        stem_present <- FALSE
      } else{
        stem_present <- TRUE
        output[[i]] <- x[[i]]
      } 
    } else{
      if(is.na(x[[i]])){
        stem_present <- FALSE
        output[1:i] <- as.numeric(rep(NA, i))
      } else{
        stem_present <- TRUE
        output[[i]] <- x[[i]]
      }
    }
  }
  return(output)
}
#######################
class(as.numeric(NA))
class(boltonia_data)

str(boltonia_data)

boltonia_data_sub <-boltonia_data[1:10, ]
dim(boltonia_data)
dim(boltonia_data_sub)

#old-fashioned##########
class(boltonia_data$stemLength.2[1:10])
boltonia_data_sub$stemLength.2 <- as.numeric(boltonia_data_sub$stemLength.2)
#fancy method
convert_Y_TRUE <- function(x){
  output <- case_when(x == "Y" ~ TRUE, TRUE ~ FALSE)
  return(output)
}

boltonia_data_sub <- boltonia_data %>%
  filter(index <= 10)%>%
  mutate(stemLength.2 = as.numeric(stemLength.2), label = factor(label, levels = label))%>%
  mutate_at(vars(matches("Surv.")), .funs = "convert_Y_TRUE")

########################

date_info <- boltonia_data_sub %>%
  select(str_c("Date.", 1:8))%>%
  .[1,]%>%
  pivot_longer(cols = starts_with("Date"), names_to = "Date_index", values_to = "Date")%>%
  mutate(Date_index = str_remove(Date_index, "Date."))%>%
  mutate(Date = str_replace(Date, pattern = "/24$", replacement = "/2024"))

boltonia_data_sub_1 <- boltonia_data_sub %>%
  select(label, index, starts_with("stemLength"), starts_with("leafLong"), starts_with("leafWide")) %>%
  mutate_at(vars(matches("leafLong")), .funs = "as.double") %>%
  mutate_at(vars(matches("leafWide")), .funs = "as.double") %>%
  pivot_longer(cols = c(starts_with("leafLong"), starts_with("stemLength"), starts_with("leafWide")), values_to = "values") %>%
  separate(col = "name", into = c("variable_name", "Date_index"), sep = "[.]") %>%
  left_join(date_info, by = "Date_index")%>%
  mutate(Date = as.Date(Date, "%m/%d/%Y"))%>%
  group_by(label, index, variable_name)%>%
  mutate(values = correct_stem(values))

P <- ggplot(data = filter(boltonia_data_sub_1, variable_name == "stemLength"),
            aes(x = Date, y = values))+
  geom_point()+
  ggtitle("Stem Length")+
  geom_line(aes(group = label), color = "hotpink")+
  geom_boxplot(aes(group = Date), fill = NA, color = "green", width = 0.5)+
  theme_bw()
P

P <- ggplot(data = filter(boltonia_data_sub_1, variable_name == "leafLong"),
            aes(x = Date, y = values))+
  geom_point()+
  ggtitle("Leaf length")+
  geom_line(aes(group = label), color = "hotpink")+
  geom_boxplot(aes(group = Date), fill = NA, color = "green", width = 0.5)+
  theme_bw()
P

P <- ggplot(data = filter(boltonia_data_sub_1, variable_name == "leafWide"),
            aes(x = Date, y = values))+
  geom_line(aes(group = label), color = "hotpink")+
  geom_boxplot(aes(group = Date), fill = NA, color = "green", width = 0.5)+
  scale_x_date(breaks = unique(boltonia_data_sub_1$Date), labels = c("W1", "W2", "W3", "W4", "W5"))+
  geom_point()+
  ggtitle("Leaf Width")+
  ggtitle("Centered Title")+
  theme_bw()+ 
  theme(plot.title = element_text(hjust = 0.5))
P

#######plots################
P <- ggplot(data = boltonia_data_sub, mapping = aes(x = reorder(label, desc(leafLong.1)), y = leafLong.1))+
  geom_col(fill = "darkgreen")+
  scale_x_discrete(name = "Plant ID")+
  scale_y_discrete(name = "Plant Height (cm)")+
  theme_bw()+
  theme(legend.position = c(0.1, 0.85))
P

############################

P <- ggplot(data = boltonia_data_sub, mapping = aes(x = label, y = stemLength.1))+
  geom_col(mapping = aes(fill = stemLength.1))+
  scale_fill_distiller(name = "Height (cm)", palette = "RdYlBu")+
  scale_x_discrete(name = "Plant ID")+
  scale_y_discrete(name = "Plant Height (cm)")+
  theme_bw()+
  theme(legend.position = c(0.1, 0.85))
P

#############################

P <- ggplot(data = boltonia_data_sub, mapping = aes(x = label, y = stemLength.1))+
  geom_col(mapping = aes(fill = stemLength.1))+
  geom_line(group = 1)+
  scale_fill_distiller(name = "Height (cm)", palette = "RdYlBu")+
  scale_x_discrete(name = "Plant ID")+
  scale_y_discrete(name = "Plant Height (cm)")+
  theme_bw()+
  theme(legend.position = c(0.1, 0.85))
P

ggsave("Boltanica_plant_height.png", width = 8, height = 6)

P <- ggplot(data = boltonia_data_sub, mapping = aes(x = reorder(label, index), y = stemLength.2))+
  geom_col(fill = "darkgreen")+
  scale_x_discrete(name = "Plant ID")+
  scale_y_discrete(name = "Plant Height (cm)")+
  theme_bw()+
  theme(legend.position = c(0.1, 0.85))
P

ggsave("Boltanica_plant_heightwk2.png", width = 8, height = 6)

P <- ggplot(data = boltonia_data_sub, mapping = aes(x = label, y = stemLength.3))+
  geom_col(mapping = aes(fill = as.numeric(stemLength.3)))+
  geom_line(group = 1)+
  scale_fill_distiller(name = "Week 3 Height (cm)", palette = "RdYlBu")+
  scale_x_discrete(name = "Plant ID")+
  scale_y_discrete(name = "Plant Height (cm)")+
  theme_bw()+
  theme(legend.position = c(0.1, 0.85))
P

ggsave("Boltanica_plant_heightwk3.png", width = 8, height = 6)

P <- ggplot(data = boltonia_data_sub, mapping = aes(x = label, y = stemLength.4))+
  geom_col(mapping = aes(fill = as.numeric(stemLength.4)))+
  geom_line(group = 1)+
  scale_fill_distiller(name = "Week 4 Height (cm)", palette = "RdYlBu")+
  scale_x_discrete(name = "Plant ID")+
  scale_y_discrete(name = "Plant Height (cm)")+
  theme_bw()+
  theme(legend.position = c(0.1, 0.85))
P

ggsave("Boltanica_plant_heightwk4.png", width = 8, height = 6)

P <- ggplot(data = boltonia_data_sub, mapping = aes(x = label, y = stemLength.5))+
  geom_col(mapping = aes(fill = as.numeric(stemLength.3)))+
  geom_line(group = 1)+
  scale_fill_distiller(name = "Week 4 Height (cm)", palette = "RdYlBu")+
  scale_x_discrete(name = "Plant ID")+
  scale_y_discrete(name = "Plant Height (cm)")+
  theme_bw()+
  theme(legend.position = c(0.1, 0.85))
P

ggsave("Boltanica_plant_heightwk4.png", width = 8, height = 6)

P <- ggplot(data = boltonia_data_sub, mapping = aes(x = label, y = stemLength.5))+
  geom_col(mapping = aes(fill = as.numeric(stemLength.5)))+
  geom_line(group = 1)+
  scale_fill_distiller(name = "Week 5 Height (cm)", palette = "RdYlBu")+
  scale_x_discrete(name = "Plant ID")+
  scale_y_discrete(name = "Plant Height (cm)")+
  theme_bw()+
  theme(legend.position = c(0.1, 0.85))
P

ggsave("Boltanica_plant_heightwk5.png", width = 8, height = 6)