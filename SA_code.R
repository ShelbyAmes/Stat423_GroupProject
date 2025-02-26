
##read files
divorce <- read.csv("divorce_rates.csv")
head(divorce)
washington <- divorce[49,1:26]

##need to unpivot data to pull divorce rates and year in 2 different columns
library(tidyverse)

washington <- washington[,] %>%
  mutate(across(everything(), as.character))

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


GDP_percentage <- read.csv("WA_GDP.csv")

WA_total_GDP_perc <- GDP[1,9:34]

WA_GDP_long_perc <- WA_total_GDP %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Year",        
    values_to = "TotalGDP_PercentChange" 
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X")))

WA_expense <- read.csv("SASUMMARY_WA_1998_2023.csv")

WA_expense <- WA_expense[,] %>%
  mutate(across(everything(), as.character))

personal_income <- WA_expense[5,9:34]

pi_long <- personal_income %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Year",        
    values_to = "PersonalIncome" 
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X")))

RealGDP <- WA_expense[1,9:34]

rgdp_long <- RealGDP %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Year",        
    values_to = "RealGDP" 
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X")))

Expenditures <- WA_expense[7,9:34]

expenditures_long <- Expenditures %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Year",        
    values_to = "PersonalExpenditures" 
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X")))


wa_combined <- wa_long %>%
  left_join(WA_GDP_long_perc, by = "Year") %>%
  left_join(pi_long, by = "Year") %>%
  left_join(rgdp_long, by = "Year") %>%
  left_join(expenditures_long, by = "Year")

