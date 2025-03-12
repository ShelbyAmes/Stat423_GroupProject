
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

WA_total_GDP_perc <- GDP_percentage[1,9:34]

WA_GDP_long_perc <- WA_total_GDP_perc %>%
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

marriages <- read.csv("marriage_rates.csv")
wa_marriage <- marriages[48,2:26]

wa_marriage <- wa_marriage[,] %>%
  mutate(across(everything(), as.character))


wa_long_mar <- wa_marriage %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Year",        
    values_to = "MarriageRate" 
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X")))

wa_long_mar <- wa_long_mar[1:26,]

wa_combined <- wa_long %>%
  left_join(WA_GDP_long_perc, by = "Year") %>%
  left_join(pi_long, by = "Year") %>%
  left_join(rgdp_long, by = "Year") %>%
  left_join(expenditures_long, by = "Year") %>%
  left_join(wa_long_mar, by = "Year")

wa_combined <- wa_combined %>%
  mutate(across(c(DivorceRate, TotalGDP_PercentChange, PersonalIncome, 
                  RealGDP, PersonalExpenditures, MarriageRate), as.numeric))

wa_combined <-wa_combined[1:23,]




##correlation matrix
library(corrplot)

corrplot(cor(wa_combined[,]))
dev.off()

##check VIF
library(car)
mlr_model_1b <- lm(DivorceRate ~ RealGDP+PersonalExpenditures+PersonalIncome, 
                data = wa_combined)
vif(mlr_model_1b)

mlr_model_2c <- lm(RealGDP~I(PersonalExpenditures^3) + I(DivorceRate^2) + MarriageRate,
                   data=wa_combined)
vif(mlr_model_2c)

##pcr model 
library(pls)
pcr_model_1b <- pcr(DivorceRate ~ RealGDP+PersonalExpenditures+PersonalIncome, 
                 data = wa_combined, 
                 scale = TRUE, validation = "CV")
summary(pcr_model_1b)

pc_scores_1b <- pcr_model_1b$scores

cor(pc_scores_1b)
pcr_model$loadings

##regression

pc_scores_df <- as.data.frame(pcr_model$scores)

pc_scores_df <- as.data.frame(pc_scores_df[complete.cases(pc_scores_df), ])

divorce_rate_vector <- as.numeric(wa_combined$DivorceRate)

pc_scores_df <- pc_scores_df %>%
  mutate(DivorceRate = divorce_rate_vector)


pc_reg <- lm(DivorceRate ~ `Comp 1` + `Comp 2` + `Comp 3`, data = pc_scores_df)
summary(pc_reg)



##model 2a
par(mfrow = c(1,1))
plot(wa_combined$RealGDP, wa_combined$DivorceRate)
model_2a <- lm(RealGDP ~ DivorceRate, data = wa_combined)
summary(model_2a)
par(mfrow = c(2,2))
plot(model_2a, which = 1)
plot(model_2a, which=2)


model_2_log <- wa_combined %>%
  mutate(log_GDP = log(RealGDP))

plot(model_2_log$log_GDP, model_2_log$DivorceRate)
model_2a_log <-lm(log_GDP ~ DivorceRate, data = model_2_log)
summary(model_2a_log)
par(mfrow = c(1,2))
plot(model_2a_log, which = 1)
plot(model_2a_log, which=2)


par(mfrow = c(1,1))
two_a <- boxcox(model_2a, lambda = seq(-5, 5, 0.1), 
                        main = "Box-Cox Transformation")

lambda_2a <- two_a$x[which.max(two_a$y)]

wa_combined$RealGDP_transformed_2a <- wa_combined$RealGDP^lambda_2a

par(mfrow = c(1,1))
residuals_2a <- residuals(model_2a_log)

# Plot histogram
hist(residuals_2a, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "lightblue", 
     border = "black")

##model 2b
par(mfrow = c(1,1))
plot(wa_combined$RealGDP, wa_combined$MarriageRate)
model_2b <- lm(RealGDP ~ MarriageRate, data = wa_combined)
summary(model_2b)
par(mfrow = c(1,2))
plot(model_2b, which = 1)
plot(model_2b, which=2)

par(mfrow = c(1,1))
residuals_2b <- residuals(model_2b)

# Plot histogram
hist(residuals_2b, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "lightblue", 
     border = "black")

par(mfrow = c(1,1))
plot(model_2_log$log_GDP, model_2_log$MarriageRate)
model_2b_log <-lm(log_GDP ~ MarriageRate, data = model_2_log)
summary(model_2b_log)
par(mfrow = c(1,2))
plot(model_2b_log, which = 1)
plot(model_2b_log, which=2)

par(mfrow = c(1,1))
residuals_2b_log <- residuals(model_2b_log)

# Plot histogram
hist(residuals_2b, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "lightblue", 
     border = "black")



library(MASS)

# Perform Box-Cox transformation
par(mfrow = c(1,1))
two_b <- boxcox(model_2b, lambda = seq(-2, 2, 0.1), 
       main = "Box-Cox Transformation")

lambda <- two_b$x[which.max(two_b$y)]

wa_combined$RealGDP_transformed <- wa_combined$RealGDP^lambda

##with transformed data
par(mfrow=c(1,1))
plot(wa_combined$Year, wa_combined$MarriageRate)
ggplot(data = wa_combined, aes(x = Year, y = MarriageRate)) +
  geom_point() +  # Scatter plot of MarriageRate vs. Year
  geom_smooth(method = "lm") +  # Regression line
  labs(title = "Marriage Rate Over Time",
       x = "Year",
       y = "Marriage Rate") +
  theme_minimal()
model_transformed <- lm(RealGDP_transformed ~ MarriageRate, data = wa_combined)
summary(model_transformed)
par(mfrow = c(1,2))
plot(model_transformed, which = 1)
plot(model_transformed, which=2)

par(mfrow = c(1,1))
residuals_model_transformed <- residuals(model_transformed)

# Plot histogram
hist(residuals_model_transformed, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "lightblue", 
     border = "black")

##ggplot with highlighted residuals
library(ggplot2)
library(dplyr)

# Fit the linear model
model <- lm(MarriageRate ~ Year, data = wa_combined)

# Get predicted values and standard error of residuals
wa_combined <- wa_combined %>%
  mutate(pred = predict(model),  # Predicted MarriageRate
         resid = MarriageRate - pred,  # Residuals
         se = sd(resid),  # Standard error of residuals
         outlier = abs(resid) > 2 * se)  # Flag outliers beyond 2 SE
ggplot(wa_combined, aes(x = Year, y = MarriageRate, color = outlier)) +
  geom_point(size = 2) +  # Scatter plot
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Regression line with SE band
  scale_color_manual(values = c("black", "red"), labels = c("Within SE", "Outside SE")) + 
  labs(title = "Marriage Rate Over Time with Outliers Highlighted",
       x = "Year",
       y = "Marriage Rate",
       color = "Outliers") +
  theme_minimal()

recession_years <- c(2008, 2009, 2020)

# Create a new column in wa_combined
wa_combined <- wa_combined %>%
  mutate(Recession = ifelse(Year %in% recession_years, "Yes", "No"))


# Ensure Recession is a factor
wa_combined$Recession <- as.factor(wa_combined$Recession)

# Fit a linear regression model
model_recession <- lm(MarriageRate ~ Recession, data = wa_combined)
model_recession <- lm(RealGDP ~ MarriageRate + Recession, data = wa_combined)
# View summary of the model
summary(model_recession)
par(mfrow = c(1,2))
plot(model_recession, which = 1)
plot(model_recession, which=2)

##Marriage ~ GDP
twob_revised <- lm(MarriageRate ~ RealGDP, data= wa_combined)
summary(twob_revised)


library(MASS)

# Perform Box-Cox transformation
par(mfrow = c(1,1))
two_b_revised <- boxcox(twob_revised, lambda = seq(-5, 5, 0.1), 
                main = "Box-Cox Transformation")

lambda_new <- two_b_revised$x[which.max(two_b_revised$y)]

wa_combined$RealGDP_transformed_revised <- wa_combined$RealGDP^lambda_new
