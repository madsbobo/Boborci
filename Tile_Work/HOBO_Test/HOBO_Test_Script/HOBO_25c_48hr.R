library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(janitor)
library(lubridate)

HOBO1<-read.csv("HOBO_Test/HOBO_Test_Data/25c_48hr/22263997.csv")
HOBO2<-read.csv("HOBO_Test/HOBO_Test_Data/25c_48hr/22264002.csv")
Mica1<-read.csv("HOBO_Test/HOBO_Test_Data/25c_48hr/P3-SN20565256.csv")
Mica2<-read.csv("HOBO_Test/HOBO_Test_Data/25c_48hr/P12-SN20565261 .csv")



HOBO1<-mutate(HOBO1, HOBO_ID = "22263997")
HOBO2<-mutate(HOBO2, HOBO_ID = "22264002")
Mica1<-mutate(Mica1, HOBO_ID="P3")
Mica2<-mutate(Mica2, HOBO_ID="P12")

all_HOBO<- bind_rows(HOBO1,HOBO2, Mica1, Mica2)


clean_HOBO<- all_HOBO%>%
  select(-`Host.Connected`, -`End.of.File`)%>%
  mutate(Temp_C = coalesce(Temperature.....C., Temperature....C)) %>%
  select(-Temperature.....C., -Temperature....C)%>%
  rename(Date_Time_HST=Date.Time..HST.)%>% 
  separate(`Date_Time_HST`, into = c("Date", "Time"), sep = " ", remove = FALSE)
  
  


#clean_HOBO%>%
 # Temp_C = as.numeric(gsub("[^0-9.-]", "", Temp_C))



# 1. Convert the character timestamps into real POSIXct date-time objects
clean_HOBO <- clean_HOBO %>%
  mutate(Date_Time_HST = mdy_hms(Date_Time_HST))


# 2. Filter the window using mdy_hms on both ends
test_window <- clean_HOBO %>%
  filter(
    Date_Time_HST >= mdy_hms("08/04/2026 10:00:00"),
    Date_Time_HST <= mdy_hms("08/06/2026 12:10:59")
  )


# 4. Plot your temperatures
ggplot(data = test_window, aes(x = Date_Time_HST, y = Temp_C, color = factor(HOBO_ID))) +
  geom_point() +
  geom_line()+
  ylim(24.5,25.8)


thermometer <- data.frame(
  Date_Time_HST = mdy_hms(c(
    "08/04/2026 10:09:00",
    "08/04/2026 11:05:00",
    "08/04/2026 12:04:00",
    "08/04/2026 13:05:00",
    "08/04/2026 14:24:00",
    "08/04/2026 15:06:00",
    "08/04/2026 16:08:00",
    "08/04/2026 16:27:00",
    "08/05/2026 08:10:00",
    "08/05/2026 09:04:00",
    "08/05/2026 10:07:00",
    "08/05/2026 11:04:00",
    "08/05/2026 12:29:00",
    "08/05/2026 13:10:00",
    "08/05/2026 14:00:00",
    "08/05/2026 15:14:00",
    "08/05/2026 16:04:00",
    "08/06/2026 08:01:00",
    "08/06/2026 09:09:00",
    "08/06/2026 10:26:00",
    "08/06/2026 12:10:00")),
  Temp_C = c(25.002,24.687,24.826,25.000,25.157,24.823,24.880,24.709,24.954,24.979,25.027,24.546,24.617,24.826,24.519,24.745,24.708,24.939,24.656,25.027,25.052),
  Label = "True Temp" 
)






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



hobo_model <- aov(Temp_C ~ factor(HOBO_ID), data = test_window)
summary(hobo_model)
TukeyHSD(hobo_model)






# 2. Filter the window using mdy_hms on both ends
day_subset <- clean_HOBO %>%
  filter(
    Date_Time_HST >= mdy_hms("08/05/2026 08:00:00"),
    Date_Time_HST <= mdy_hms("08/05/2026 17:00:00")
  )

thermometer_subset <- data.frame(
  Date_Time_HST = mdy_hms(c(
    "08/05/2026 08:10:00",
    "08/05/2026 09:04:00",
    "08/05/2026 10:07:00",
    "08/05/2026 11:04:00",
    "08/05/2026 12:29:00",
    "08/05/2026 13:10:00",
    "08/05/2026 14:00:00",
    "08/05/2026 15:14:00",
    "08/05/2026 16:04:00")),
  Temp_C = c(24.954,24.979,25.027,24.546,24.617,24.826,24.519,24.745,24.708),
  Label = "True Temp" 
)


ggplot() +
  # Layer 1: Continuous HOBO logger data
  geom_line(data = day_subset, aes(x = Date_Time_HST, y = Temp_C, color = factor(HOBO_ID))) +
  geom_point(data = day_subset, aes(x = Date_Time_HST, y = Temp_C, color = factor(HOBO_ID)), alpha = 0.4) +
  
  
  geom_point(
    data = thermometer_subset, 
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



full_temp_data<- test_window%>%
  left_join(thermometer, by="Date_Time_HST")







