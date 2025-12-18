##Libraries

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
library(car)
library(emmeans)
library(DHARMa)
library(knitr)
library(broom)
library(kableExtra)
library(webshot2)
library
#Import data
Data_final<-read.csv(here("Fish_Class", "Data", "Fish_Data_Final.csv"))
#View(Data_final)

#cut any spaces in species names
all_data <- Data_final%>%
  mutate(
Species = str_trim(Species, side = "both"))



#Add colum that defines which species should be in each trophic group
all_data<-all_data%>%
  mutate(
  Trophic_Guild = ifelse(
    Species%in%c("A. triostegus", "Z. flavescens", "Z. velifer", "C. spilurus", "S. psittacus", "N. unicornis"),
    "Herbivore",
    "Carnivore"
  )
)

View(all_data)

#Random check

#all_data%>%
  #ggplot(aes(x=Body_Size_cm, y= reorder(Group_Size, as.numeric(Group_Size)), color=Trophic_Guild))+
#geom_jitter()

#Clean up extra spaces etc given two data editors
clean_data <- all_data %>%
  mutate(
    Habitat_trimmed = str_trim(Habitat, side = "both"),
    Habitat = as.factor(str_replace_all(Habitat_trimmed, pattern = "\\s+", replacement = " ")),
    Body_Size_cm = as.numeric(str_trim(Body_Size_cm, side = "both")),
    Group_Size = as.numeric(str_trim(Group_Size, side = "both")),
    Trophic_Guild = as.factor(str_trim(Trophic_Guild, side = "both"))
  )



#Look at how many of each trophic guild we have
guild_counts <- clean_data%>%
  group_by(Trophic_Guild) %>%
  summarise(
    Count = n()
  )
#View(guild_counts)

#Look at how many ind in each Habitat we have
habitat_counts<-clean_data%>%
  group_by(Habitat) %>%
  summarise(
    Count = n()
  )

Sand<-clean_data%>%
  group_by(Habitat, Trophic_Guild)%>%
  summarise(Count=n())
View(Sand)
#View(habitat_counts)



####--------Models-------####

#log transformed due to patters in the residuals
fish_model<-lm(log(Group_Size)~Habitat+Body_Size_cm+Trophic_Guild, data=clean_data)
check_model(fish_model)

summary(fish_model)


car::Anova(fish_model, type = 2)



# calculate emmeans
em_hab <- emmeans(fish_model, ~ Habitat)
em_trophic <- emmeans(fish_model, ~ Trophic_Guild)

pairs(em_hab)


#run some raw data checks
clean_data%>%
  ggplot(aes(x=Group_Size))+
  geom_histogram(stat="count")

clean_data%>%
  ggplot(aes(x=Body_Size_cm, y= reorder(Group_Size, as.numeric(Group_Size)), color=Trophic_Guild))+
  facet_wrap(~Habitat)+
  geom_jitter()

clean_data %>%
  group_by(Habitat) %>%
  summarize(mean=mean(Group_Size))


clean_data%>%
  ggplot(aes(x=Habitat, 
             y= Group_Size))+
           geom_boxplot()


#try another model
fish_model_poisson<-
  glmmTMB(
    Group_Size ~ Habitat + Body_Size_cm+Trophic_Guild,
    family = poisson(link="log"),
    data = clean_data)



plot(simulateResiduals(fish_model_poisson))
testDispersion(fish_model_poisson)

summary(fish_model_poisson)

check_model(fish_model_poisson)
car::Anova(fish_model_poisson, type=2)

 
#Could split herbs and carns because almost no herbs in sand...

##Herbivore and Carnivore separate


##CARNIVORE MODEL


carnivore_data<-clean_data%>%
  filter(Trophic_Guild=="Carnivore")

View(carnivore_data)

carnivore_model<-lm(Group_Size~Habitat+Body_Size_cm, data=carnivore_data)
check_model(carnivore_model)

summary(carnivore_model)
carnivore_anova<-car::Anova(carnivore_model, type=2)
car::Anova(carnivore_model, type=2)

#run emmeans
em_carn <- emmeans(carnivore_model, ~Habitat)
em_carn

pairs(em_carn)

#All habitats are significantly different from eachother

#have to tell model df our size options
ref.grid <- ref_grid(carnivore_model, at = list(Body_Size_cm =c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5)))
emmip(ref.grid, Habitat ~ Body_Size_cm)

model_df_carn <- emmip(ref.grid, Habitat ~ Body_Size_cm,
      plotit = FALSE)

# make a plot that has model predictions and our raw data


my_colors <- c(
  "Patchy_Reef" = "#009E73",
  "Reef_Flat" = "#D55E00",
  "Sand"  = "#56B4E9"
)

preds <- predict(carnivore_model, newdata = model_df_carn, se.fit = TRUE)
model_df_carn$yvar <- preds$fit
model_df_carn$lwr  <- preds$fit - (1.96 * preds$se.fit)
model_df_carn$upr  <- preds$fit + (1.96 * preds$se.fit)

ggplot()+
  geom_ribbon(data = model_df_carn,
              aes(x = Body_Size_cm, 
                  ymin = lwr, 
                  ymax = upr, 
                  fill = Habitat), 
              alpha = 0.15,    
              show.legend = FALSE) +
  geom_point(data=carnivore_data,
             aes(x=Body_Size_cm,
                 y=Group_Size, colour= Habitat)) +
  geom_line(data=model_df_carn,
            aes(x=Body_Size_cm,
                y = yvar,
                colour=Habitat,
                group=Habitat))+
  labs(
    x = "Body Size (Total Length cm)",
    y = "Group Size"
  )+
  scale_colour_manual(values = my_colors,
    labels = c("Patchy_Reef" = "Patch Reef", "Reef_Flat"="Reef Flat"))+
  labs(
    title= "Group Size Model Predictions Across Habitats for Carnivores",
    subtitle= "Group size and Body Size Model Predictions overlayed onto Raw Data for Carnivores"
  )+
  theme_bw()+
  theme(
    text = element_text(family = "serif", size = 12))

ggsave(here("Fish_Class", "Outputs", "Carnivore_Plot.png"))



## HERBIVORES
#remove sand since there are almost no occurrences


herbivore_data<-clean_data%>%
  filter(Trophic_Guild=="Herbivore")%>%
  filter(Habitat != "Sand")



herbivore_model<-lm(Group_Size~Habitat + Body_Size_cm, data=herbivore_data)
check_model(herbivore_model)
summary(herbivore_model)


herbivore_anova<-car::Anova(herbivore_model, type=2)
car::Anova(herbivore_model, type=2)

ref.grid <- ref_grid(herbivore_model, at = list(Body_Size_cm =c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5)))
emmip(ref.grid, ~Body_Size_cm)

model_df_herb <- emmip(ref.grid, ~ Body_Size_cm,
                  plotit = FALSE)




ggplot()+
  geom_point(data=herbivore_data,
             aes(x=Body_Size_cm,
                 y=Group_Size,
                 colour=Habitat)) +
  geom_line(data=model_df_herb,
            aes(x=Body_Size_cm,
                y = yvar))+
  scale_colour_manual(values = my_colors,
                      labels = c("Patchy_Reef" = "Patch Reef", "Reef_Flat"="Reef Flat"))+
  labs(
    x = "Body Size (Total Length cm)",
    y = "Group Size"
  )+
  labs(
    title= "Group Size Model Predictions Across Habitats for Herbivores",
    subtitle= "Group size and Body Size Model Predictions overlayed onto Raw Data for Herbivores"
  )+
  theme_bw()+
  theme(
    text = element_text(family = "serif", size = 12))
ggsave(here("Fish_Class","Outputs", "Herbivore_Plot.png"))
  


#another way to do a plot 
#ggplot(data=herbivore_data,
      #aes(x=Body_Size_cm,
          # y=Group_Size))+
  #geom_point() +
  #geom_smooth(method="lm")


#running as a poisson to see if model fit is better, since lm is giving bounds that aren't great for this data
fish_model_poisson<-
  glmmTMB(
    Group_Size ~ Habitat + Body_Size_cm,
    family = poisson(link="log"),
    data = herbivore_data)

check_model(fish_model_poisson)

ref.grid <- ref_grid(fish_model_poisson, at = list(Body_Size_cm =c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5)))
emmip(ref.grid, ~Body_Size_cm)

model_df_pois <- emmip(ref.grid, ~ Body_Size_cm,
                  plotit = FALSE)




ggplot()+
  geom_point(data=herbivore_data,
             aes(x=Body_Size_cm,
                 y=Group_Size,
                )) +
  geom_line(data=model_df_pois,
            aes(x=Body_Size_cm,
                y = yvar))



###This is me actually trying to export the model summaries
carn_summ_df <- tidy(carnivore_model) %>%
  mutate(
    estimate = formatC(estimate, digits = 2, format = "e"),
    std.error = formatC(std.error, digits = 2, format = "e"),
    p.value = formatC(p.value, digits = 2, format = "e")
  )


mod_glance <- glance(carnivore_model)
adj_r2_val <- format(round(mod_glance$adj.r.squared, 3), nsmall = 3)
n_obs <- mod_glance$nobs


summary_row <- data.frame(
  term = c("Adj. R2", "N"),
  estimate = c(adj_r2_val, as.character(n_obs)),
  std.error = "", 
  statistic = "", 
  p.value = ""
)

#Combine and make the table
carn_sum_model_full<-bind_rows(carn_summ_df, summary_row) %>%
  kable(
    caption = "Table 3. Carnivore Model Summary",
    col.names = c("Predictor", "Estimate", "Std. Error", "t-stat", "p-value"),
    align = "lcccc"
  ) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  # Make the last two rows (Adj R2 and N) bold to stand out
  row_spec((nrow(carn_summ_df) + 1):(nrow(carn_summ_df) + 2), bold = TRUE, italic = TRUE)

save_kable(carn_sum_model_full, file = "carn_summ_full.html")









#########
adj_r2_val <- glance(carnivore_model)$adj.r.squared

carn_summ<-tidy(carnivore_model) %>%
  mutate(
    across(where(is.numeric), ~ formatC(.x, digits = 2, format = "e"))
  ) %>%
  kable(caption = "Table 3. Carnivore Model Summary") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  footnote(general = paste("Adjusted R-squared:", 
                           round(glance(carnivore_model)$adj.r.squared, 3)))
save_kable(carn_summ, file = "carn_summ_done.html")

carn_anova<-tidy(carnivore_anova) %>%
  mutate(
    across(where(is.numeric), ~ formatC(.x, digits = 2, format = "e"))
  ) %>%
  kable(caption = "Table 4. Carnivore ANOVA results") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
save_kable(carn_anova, file = "carn_anova.html")

carn_posthoc<-tidy(pairs(em_carn))%>%
  mutate(
    across(where(is.numeric), ~ formatC(.x, digits = 2, format = "e"))
  ) %>%
  kable(caption = "Table 5. Carnivore Post- Hoc results") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
save_kable(carn_posthoc, file = "carn_posthoc.html")



adj_r2_val <- glance(herbivore_model)$adj.r.squared
herb_summ_done<- tidy(herbivore_model) %>%
  mutate(
    across(where(is.numeric), ~ formatC(.x, digits = 2, format = "e"))
  ) %>%
  kable(caption = "Table 1. Herbivore Model Summary") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  footnote(general = paste("Adjusted R-squared:", 
                           round(glance(herbivore_model)$adj.r.squared, 3)))
save_kable(herb_summ_done, file = "herb_summ_final.html")

herb_anova<-tidy(herbivore_anova)%>%
  mutate(
    across(where(is.numeric), ~ formatC(.x, digits = 2, format = "e"))
  ) %>%
  kable(caption = "Table 2. Herbivore ANOVA results") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
save_kable(herb_anova, file = "herb_anova.html")








