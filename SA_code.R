
##read files
divorce <- read.csv("divorce_rates.csv")
head(divorce)
washington <- divorce[49,1:26]

##need to unpivot data to pull divorce rates and year in 2 different columns
library(tidyverse)

wa_long <- washington %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Year",        
    values_to = "DivorceRate" 
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X")))

wa_long <- wa_long[2:26,]

ggplot(data = wa_long, aes(x=Year, y=DivorceRate)) +
  geom_point()+
  theme_minimal()

personal_income <- read.csv("SAINC70_WA_2000_2023.csv")

GDP <- read.csv("WA_GDP.csv")

WA_total_GDP <- GDP[1,9:34]

WA_GDP_long <- WA_total_GDP %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Year",        
    values_to = "TotalGDP_PercentChange" 
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X")))

##just seeing if commit works
