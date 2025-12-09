library(here)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(broom)
library(performance) 
library(modelsummary)
library(tidymodels)
library(stringr)
library(glmmTMB)

Data_trial<-read.csv(here("Fish_Class", "Fish class data - Sheet1.csv"))
#View(Data_trial)


all_data<-Data_trial%>%
  mutate(
  Trophic_Guild = ifelse(
    # Define the logical condition for Group 1
    Species%in%c("A. triostegus", "Z. flavescens", "Z. velifer", "C. spilurus", "S. psittacus", "N. unicornis"),
    "Herbivore",
    "Carnivore"
  )
)
#View(all_data)





guild_counts <- all_data%>%
  group_by(Trophic_Guild) %>%
  summarise(
    Count = n()
  )
#View(guild_counts)
habitat_counts<-all_data%>%
  group_by(Habitat) %>%
  summarise(
    Count = n()
  )

#View(habitat_counts)

#all_data%>%
  #ggplot(aes(x=Body_Size_cm, y= reorder(Group_Size, as.numeric(Group_Size)), color=Trophic_Guild))+
#geom_jitter()

clean_data<-
  all_data%>%
  mutate(
    
    Group_Size = str_replace_all(Group_Size, pattern = "\\`", replacement = "")
  )

clean_data <- clean_data %>%
  mutate(
    Body_Size_cm = as.numeric(Body_Size_cm),
    Group_Size = as.numeric(Group_Size),
    Trophic_Guild = as.factor(Trophic_Guild),
    Habitat = as.factor(Habitat))


fish_model<-lm(Group_Size~Habitat+Body_Size_cm+Trophic_Guild, data=clean_data)
check_model(fish_model)

summary(fish_model)


library(car)
car::Anova(fish_model, type = 2)

clean_data%>%
  ggplot(aes(x=Group_Size))+
  geom_histogram(stat="count")

fish_model_gamma<-
  glmmTMB(
    Group_Size ~ Habitat + Body_Size_cm+Trophic_Guild,
    family = Gamma(link = "log"),
    data = clean_data)
check_model(fish_model_gamma)
car::Anova(fish_model_gamma, type=2)



