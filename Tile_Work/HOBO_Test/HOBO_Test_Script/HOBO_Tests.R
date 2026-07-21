library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(janitor)
library(lubridate)

HOBO1<-read.csv("HOBO_Test/HOBO_Test_Data/22263997 2026-07-21 09_21_42 HST (Data HST).xlsx - Data.csv")
HOBO2<-read.csv("HOBO_Test/HOBO_Test_Data/22264002 2026-07-21 09_24_03 HST (Data HST).xlsx - Data.csv")
Mica_HOBO



HOBO1<-mutate(HOBO1, HOBO_ID = "22263997")
HOBO2<-mutate(HOBO2, HOBO_ID = "22264002")
Mica_HOBO<-Mica_HOBO%>%
  clean_names()
Mica_HOBO<-Mica_HOBO%>%
  rename(Temperature....C=temperature_c, Date.Time..HST.=date_time_hst)%>%
  mutate(HOBO_ID="P5")

all_HOBO<- bind_rows(HOBO1,HOBO2, Mica_HOBO)





clean_HOBO<- all_HOBO%>%
  rename(Temp_C=Temperature....C, Date_Time_HST=Date.Time..HST.)%>%
  separate(`Date_Time_HST`, into = c("Date", "Time"), sep = " ", remove = FALSE)%>%
  select(-number,-`X.`)

clean_HOBO%>%
  Temp_C = as.numeric(gsub("[^0-9.-]", "", Temp_C))
)



# 1. Convert the character timestamps into real POSIXct date-time objects
clean_HOBO <- clean_HOBO %>%
  mutate(Date_Time_HST = mdy_hms(Date_Time_HST))


# 2. Filter the window using mdy_hms on both ends
test_window <- clean_HOBO %>%
  filter(
    Date_Time_HST >= mdy_hms("07/20/2026 16:00:00"),
    Date_Time_HST <= mdy_hms("07/21/2026 09:30:00")
  )


# 4. Plot your temperatures
ggplot(data = test_window, aes(x = Date_Time_HST, y = Temp_C, color = factor(HOBO_ID))) +
  geom_point() +
  geom_line()+
  ylim(24.5,25.8)


thermometer <- data.frame(
  Date_Time_HST = mdy_hms(c(
    "07/20/2026 15:14:00",
    "07/20/2026 16:22:00",
    "07/21/2026 07:49:00",
    "07/21/2026 09:15:00"
  )),
  Temp_C = c(25.259, 25.198, 25.155, 25.981),
  Label = "True Temp" 
)


thermometer_small <- data.frame(
  Date_Time_HST = mdy_hms(
    "07/21/2026 07:49:00"
  ),
  Temp_C =  25.155)
  

ggplot() +
  # Layer 1: Continuous HOBO logger data
  geom_line(data = test_window, aes(x = Date_Time_HST, y = Temp_C, color = factor(HOBO_ID))) +
  geom_point(data = test_window, aes(x = Date_Time_HST, y = Temp_C, color = factor(HOBO_ID)), alpha = 0.4) +
  
  
  geom_point(
    data = thermometer, 
    aes(x = Date_Time_HST, y = Temp_C),
    fill = "black",  
    size = 2        
  ) +
  ylim(24.3,25.8)+
  
  labs(
    x = "Date & Time (HST)",
    y = "Temperature (°C)",
    color = "HOBO ID"
  ) +
  theme_minimal()



hobo_wide <- test_window %>%
  pivot_wider(
    names_from = HOBO_ID,  
    values_from = Temp_C    
  )


hobo_compared <- hobo_wide %>%
  mutate(
    diff_1_vs_ref = `22263997` - P5,
    diff_2_vs_ref = `22264002`- P5
  )


hobo_averages <- hobo_compared %>%
  summarise(
    avg_diff_1 = mean(diff_1_vs_ref, na.rm = TRUE),
    avg_diff_2 = mean(diff_2_vs_ref, na.rm = TRUE),
    max_diff_1=max(diff_1_vs_ref, na.rm = TRUE),
    max_diff_2=max(diff_2_vs_ref, na.rm = TRUE))



hobo_model <- aov(Temp_C ~ factor(HOBO_ID), data = test_window)
summary(hobo_model)
TukeyHSD(hobo_model)



  
