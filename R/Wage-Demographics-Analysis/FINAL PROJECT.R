#Install packages and load them
rm(list = ls())
install.packages("ipumsr")
install.packages("stargazer") 
install.packages("tableone") 
install.packages("patchwork")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("purrr")  
library(purrr)
library(tidyr)
library("ipumsr")
library("ggplot2")
library("ddi")
library("readr")
library("haven")
library("dplyr")
library("stargazer")
library("tableone")
library("patchwork")
library("stargazer")
#-------------------------------------------------------------------------------

#Load the ipums data intp r and remove the empty variables
ddi <- read_ipums_ddi("cps_00004.xml")
data <- read_ipums_micro(ddi)
WAGE_DEMO <- subset(data, select = -c ( WTFINL, HWTFINL,COVIDTELEW,COVIDUNAW, COVIDPAID, 
                                       COVIDLOOK, ASECFLAG, ASECWTH, INCWAGE, INCTOT,
                                       ASECWT, HHINCOME, ASECFLAG, ASIAN))

#-------------------------------------------------------------------------------
#There were some variables that IPUMS pre-selected that didn't have any relation 
#to our topic and was displaying N/A so we got rid of them. As well as we selected 
#on variables that had connection to the Pandemic as our project is based on the
#year 2020 but didn't have a solid correlation to the relationship between wage
#and various demographics.
head(WAGE_DEMO)
tail(WAGE_DEMO)

#Dependent variable: Minimum wage
#Independent variable: employment status, labor force participation and hours 
#worked per week
#Control variables: age, sex, race, asian, hispanic, education level, 
#family income, full time or part time status
#Time trends - year and month
#Identifiers - person number and person records
#To weigh our analysis - Final basic weigh
#-------------------------------------------------------------------------------
#exploring the data from our extract 
summary(WAGE_DEMO)  
head(WAGE_DEMO)
var(WAGE_DEMO)

#clean our data
Clean_data <- WAGE_DEMO %>%
  filter(
    !is.na(RACE) &  
      HOURWAGE != 999.99 &
      AGE >= 14 & AGE <= 64 &
      SEX != 9 &
      EMPSTAT == 10 &
      LABFORCE == 2 &
      UHRSWORKT != 0 & UHRSWORKT != 997 & UHRSWORKT != 999 &
      WKSTAT %in% c(11, 42) &
      EDUC %in% c(123, 125, 111, 92, 73, 2)
  )

#selecting what I need 
Summary_data<- Clean_data%>%
  select(HOURWAGE,RACE, AGE, SEX, HISPAN, EDUC)

summary(Summary_data)
head(Summary_data)

#-------------------------------------------------------------------------------
Summary_data <- Summary_data %>%
  mutate(
    SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
    EDUC = factor(EDUC, levels = c(2, 73, 92, 111, 123, 125),
                  labels = c("No School", "High School Grad", "Associates Degree", 
                             "Bachelor's Degree", "Master's Degree", "Doctorate")),
    
    # Recode RACE
    RACE = case_when(
      HISPAN == 100 ~ "Hispanic",  
      HISPAN == 200 ~ "Hispanic",
      HISPAN == 300 ~ "Hispanic",
      HISPAN == 400 ~ "Hispanic",
      HISPAN == 500 ~ "Hispanic", 
      RACE == 100 ~ "White",  
      RACE == 200 ~ "Black",  
      RACE == 651 ~ "Asian",  
      TRUE ~ NA_character_  
    )
  ) %>%
  
# Convert RACE into a factor with levels after recoding
  mutate(RACE = factor(RACE, levels = c("White", "Black", "Asian", "Hispanic")))

#viewing data
head(Summary_data$RACE)
str(Summary_data)
glimpse(Summary_data)
summary(Summary_data)
View(Summary_data)

#removing na values
Summary_data <- Summary_data %>%
  filter(!is.na(RACE) & !is.na(SEX) & !is.na(HISPAN) & !is.na(EDUC))

#-------------------------------------------------------------------------------

#creating a regression model
#dropping WKSTAT because it only has full time hours

#model with only race and sex 
regressionmodel1 <- lm(HOURWAGE ~ RACE + SEX, data = Summary_data)
summary(regressionmodel)

#adding age 
regressionmodel2 <- lm(HOURWAGE ~ RACE + SEX + AGE, data = Summary_data)
summary(regressionmodel2)

#adding education; this is the best model of the 3 
regressionmodel3 <- lm(HOURWAGE ~ RACE + SEX + AGE + EDUC, data = Summary_data)
summary(regressionmodel3)

#white race & male are the reference groups are thus are omitted 
#No School is omitted used to compare with the higher education levels

#regression model  
stargazer(regressionmodel1, regressionmodel2, regressionmodel3, type = "text",
          title = "Regression Results",
          dep.var.labels = "Hourly Wage",
          covariate.labels = c("Black (vs White)", "Asian (vs White)", "Hispanic (vs White)", 
                               "Female (vs Male)", "Age", 
                               "High School Grad (vs No School)", "Associates Degree (vs No School)", 
                               "Bachelor's Degree (vs No School)", "Master's Degree (vs No School)", 
                               "Doctorate (vs No School)", "Part-time hours (vs Full-time hours)"),
          omit.stat = c("f", "ser"), no.space = TRUE)

#regression model with log wage  
Summary_data <- Summary_data %>%
  mutate(log_HOURWAGE = log(HOURWAGE))

regressionmodel4 <- lm(log_HOURWAGE ~ RACE + SEX + AGE + EDUC, data = Summary_data)
summary(regressionmodel4)

# log hourly wage table 
stargazer(regressionmodel1, regressionmodel2, regressionmodel3, regressionmodel4,
          type = "text",
          title =  "Hourly Wage vs Log Hourly Wage",
          dep.var.labels = c("Hourly Wage", "Hourly Wage", "Hourly Wage", "Log(Hourly Wage)"),
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          covariate.labels = c("Black (vs White)", "Asian (vs White)", "Hispanic (vs White)", 
                               "Female (vs Male)", "Age", 
                               "High School Grad (vs No School)", "Associates Degree (vs No School)", 
                               "Bachelor's Degree (vs No School)", "Master's Degree (vs No School)", 
                               "Doctorate (vs No School)"),
          omit.stat = c("f", "ser"),
          no.space = TRUE)

#log wage actually reduces my adjusted r^2 by a little

#-------------------------------------------------------------------------------

#testing to see if adding interaction terms improve my model

#does the impact of education differ by race?
interaction_model1 <- lm(HOURWAGE ~ RACE * EDUC + SEX + AGE, data = Summary_data)
summary(interaction_model1)
#improved the model

#does the impact of education differ by sex?
interaction_model2 <- lm(HOURWAGE ~ SEX * EDUC + RACE + AGE, data = Summary_data)
summary(interaction_model2)
#improved the model

#does race have an effect on hourly wage between different genders?
interaction_model3 <- lm(HOURWAGE ~ SEX * RACE + EDUC + AGE, data = Summary_data)
summary(interaction_model3)
#improved the model, but by less


stargazer(interaction_model1, interaction_model2, interaction_model3, type = "text")

#-------------------------------------------------------------------------------
#what if a linear model isn't the best fit?

# Model 1: Linear model 
regressionmodel3 <- lm(HOURWAGE ~ RACE + SEX + AGE + EDUC, data = Summary_data)
summary(regressionmodel3)

# Model 2: Log-linear model 
log_model <- lm(log_HOURWAGE ~ RACE + SEX + AGE + EDUC, data = Summary_data)
summary(log_model)
#this doesn't improve my model

# Model 3: Log-log model 
Summary_data <- Summary_data %>%
  mutate(log_AGE = log(AGE))
loglog_model <- lm(log_HOURWAGE ~ RACE + SEX + log_AGE + EDUC, data = Summary_data)
summary(loglog_model)
#loglog model improves my model

# Model 4: Polynomial model 
poly_model <- lm(HOURWAGE ~ RACE + SEX + AGE + I(AGE^2) + EDUC, data = Summary_data)
summary(poly_model)
#poly model is the best fit 

#-------------------------------------------------------------------------------
#comparing AIC & BIC
AIC(regressionmodel3, log_model, loglog_model, poly_model)
#regression model 3 has the highest

BIC(regressionmodel3, log_model, loglog_model, poly_model)
#the log log has the lowest

#-------------------------------------------------------------------------------
#making new stargazer regression analysis & descriptive stats 

#one with base models and polynomial model because it fits our data
stargazer(regressionmodel1, regressionmodel2, regressionmodel3, poly_model,
          type = "latex",
          title = "Regression Models: Baseline Models VS. Polynomial Model",
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          dep.var.labels = c("Hourly Wage"),
          no.space = TRUE)


#one shows our interaction terms 
stargazer(interaction_model1, interaction_model2, interaction_model3,
          type = "latex",
          title= "Interaction Models",
          column.labels = c("Model 1", "Model 2", "Model 3"),
          dep.var.labels = c("Hourly Wage"),
          no.space = TRUE)

#------------------------------------------------------------------------------

#descriptive stats for numeric variables 
descriptive_stats <- Summary_data %>%
  summarise(
    Min = min(HOURWAGE, na.rm = TRUE),
    Max = max(HOURWAGE, na.rm = TRUE),
    Mean = mean(HOURWAGE, na.rm = TRUE),
    SD = sd(HOURWAGE, na.rm = TRUE),
    Q25 = quantile(HOURWAGE, 0.25, na.rm = TRUE),
    Median = median(HOURWAGE, na.rm = TRUE),
    Q75 = quantile(HOURWAGE, 0.75, na.rm = TRUE),
    
    Min_Age = min(AGE, na.rm = TRUE),
    Max_Age = max(AGE, na.rm = TRUE),
    Mean_Age = mean(AGE, na.rm = TRUE),
    SD_Age = sd(AGE, na.rm = TRUE),
    Q25_Age = quantile(AGE, 0.25, na.rm = TRUE),
    Median_Age = median(AGE, na.rm = TRUE),
    Q75_Age = quantile(AGE, 0.75, na.rm = TRUE)
  )

descriptive_stats_tidy <- data.frame(
  Variable = c("HOURWAGE", "AGE"),
  Min = c(descriptive_stats$Min, descriptive_stats$Min_Age),
  Max = c(descriptive_stats$Max, descriptive_stats$Max_Age),
  Mean = c(descriptive_stats$Mean, descriptive_stats$Mean_Age),
  SD = c(descriptive_stats$SD, descriptive_stats$SD_Age),
  Q25 = c(descriptive_stats$Q25, descriptive_stats$Q25_Age),
  Median = c(descriptive_stats$Median, descriptive_stats$Median_Age),
  Q75 = c(descriptive_stats$Q75, descriptive_stats$Q75_Age)
)

stargazer(descriptive_stats_tidy,
          type = "latex",
          summary = FALSE,
          title = "Descriptive Statistics for HOURWAGE and AGE",
          digits = 2,
          align = TRUE)

#------------------------------------------------------------------------------

#making bar graphs for my data

#for race
ggplot(Summary_data, aes(x = as.factor(RACE), y = HOURWAGE)) +
  stat_summary(fun = mean, geom = "bar", fill = "lightblue", color = "black") +
  labs(title = "Average Hourly Wage by Race",
       x = "Race",
       y = "Average Hourly Wage") +
  theme_minimal()


#for sex 
ggplot(Summary_data, aes(x = as.factor(SEX), y = HOURWAGE)) +
  stat_summary(fun = mean, geom = "bar", fill = "lightblue", color = "black") +
  labs(title = "Average Hourly Wage by Sex",
       x = "Sex",
       y = "Average Hourly Wage") +
  theme_minimal()


#for education level
ggplot(Summary_data, aes(x = as.factor(EDUC), y = HOURWAGE)) +
  stat_summary(fun = mean, geom = "bar", fill = "lightblue", color = "black") +
  labs(title = "Average Hourly Wage by Educational Attainment",
       x = "Educational Attainment",
       y = "Average Hourly Wage") +
  theme_minimal()

#-------------------------------------------------------------------------------











































