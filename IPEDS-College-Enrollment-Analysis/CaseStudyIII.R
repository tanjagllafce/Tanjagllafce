rm(list = ls())
#cleaning the data______________________________________________________________

#loading the packages needed 
library("tidyr")
library("ggplot2")
library("readr")
library("dplyr")
library("stargazer")

#Import our data set into r and begin analyzing the structure
library(readxl)
Case_Study_3_Data_Updated <- read_excel("Desktop/Senior Year/Business Analytics/Case Study 3 Data Updated.xlsx")
View(Case_Study_3_Data_Updated)

#explore the data set
head(Case_Study_3_Data_Updated)
tail(Case_Study_3_Data_Updated)

#rename string variable "null" as na 
Case_Study_3_Data_Updated[Case_Study_3_Data_Updated == "null"] <- NA


#remove NA values from our data
clean_data <- Case_Study_3_Data_Updated %>%
  filter(!is.na(name), !is.na(undergrad_enrollment))

#convert variables into numeric values 
clean_data$undergrad_enrollment <- as.numeric(clean_data$undergrad_enrollment)
clean_data$percent_admitted <- as.numeric(clean_data$percent_admitted)
clean_data$admissions_yield <- as.numeric(clean_data$admissions_yield)
clean_data$tuition_fees <- as.numeric(clean_data$tuition_fees)
clean_data$room_charge <- as.numeric(clean_data$room_charge)
clean_data$board_charge <- as.numeric(clean_data$board_charge)
clean_data$total_staff <- as.numeric(clean_data$total_staff)
clean_data$graduation_rate <- as.numeric(clean_data$graduation_rate)
clean_data$full_time_retention_rate<- as.numeric(clean_data$full_time_retention_rate)
clean_data$first_time_undergrad_aide_percent <- as.numeric(clean_data$first_time_undergrad_aide_percent)
clean_data$average_aide <- as.numeric(clean_data$average_aide)
clean_data$average_pell <- as.numeric(clean_data$average_pell)


View(clean_data)
str(clean_data)


#------ENROLMENT TRENDS-----------------------------------------------------

avg_enrollment <- clean_data %>% group_by(year) %>% summarise(avg_enrollment = 
                                                    mean(undergrad_enrollment))
elmhurst <- clean_data %>% filter(name == "Elmhurst University") %>% group_by(year) 

%>% summarise(elmhurst_enrollment = mean(undergrad_enrollment))

compare_data <- left_join(elmhurst, avg_enrollment, by = "year")

ggplot(compare_data, aes(x = year)) + geom_line(aes(y = avg_enrollment), color = "gray") 
+ geom_line(aes(y = elmhurst_enrollment), color = "hotpink") + geom_point(aes(y = avg_enrollment), color = "gray") +
  geom_point(aes(y = elmhurst_enrollment), color = "hotpink") +
  labs(title = "Elmhurst University vs. Average Undergraduate Enrollment", x = "Year", y = "Enrollment") + theme_minimal()

#------TUITION TRENDS-----------------------------------------------------
avg_tuition <- clean_data %>% group_by(year) %>% summarise(avg_tuition = mean(tuition_fees))

elmhurst_tuition <- clean_data %>% filter(name == "Elmhurst University") %>% 
  group_by(year) %>% summarise(elmhurst_tuition = mean(tuition_fees))

tuition_data <- left_join(elmhurst_tuition, avg_tuition, by = "year")
ggplot(tuition_data, aes(x = year)) + geom_line(aes(y = avg_tuition), color = "lightgray") + 
  geom_line(aes(y = elmhurst_tuition), color = "hotpink") + geom_point(aes(y = avg_tuition), color = "lightgray") + 
  geom_point(aes(y = elmhurst_tuition), color = "hotpink") + 
  labs(title = "Elmhurst Tuition vs. Average Tuition", x = "Year", y = "Tuition Fees (USD)") + theme_minimal()

#------GRADUATION TRENDS-----------------------------------------------------
avg_grad <- clean_data %>% group_by(year) %>% summarise(avg_grad = mean(graduation_rate))

elmhurst_grad <- clean_data %>% filter(name == "Elmhurst University") %>% group_by(year) %>% 
  summarise(elmhurst_grad = mean(graduation_rate))

grad_data <- left_join(elmhurst_grad, avg_grad, by = "year")

ggplot(grad_data, aes(x = year)) + geom_line(aes(y = avg_grad), color = "gray") + 
  geom_line(aes(y = elmhurst_grad), color = "hotpink") + geom_point(aes(y = avg_grad), color = "gray") + 
  geom_point(aes(y = elmhurst_grad), color = "hotpink") + 
  labs(title = "Elmhurst Graduation Rate vs. Average Graduation Rate", x = "Year", y = "Graduation Rate (%)") + theme_minimal()

#------FINANCIAL AID TRENDS-----------------------------------------------------
avg_aid <- clean_data %>% group_by(year) %>% summarise(avg_aide = mean(average_aide))

elmhurst_aid <- clean_data %>% filter(name == "Elmhurst University") %>% group_by(year) %>% 
  summarise(elmhurst_aide = mean(average_aide))

aid_data <- left_join(elmhurst_aid, avg_aid, by = "year")

ggplot(aid_data, aes(x = year)) + geom_line(aes(y = avg_aide), color = "gray") +
  geom_line(aes(y = elmhurst_aide), color = "hotpink") + geom_point(aes(y = avg_aide), color = "gray") 
+ geom_point(aes(y = elmhurst_aide), color = "hotpink") + 
  labs(title = "Elmhurst Average Financial Aid vs. Average Aid", x = "Year", y = "Average Aid (USD)") + theme_minimal()

#-----------ADMISSIONS YIELD TRENDS------------------------------------------------------------------- 

#Make sure admissions_yield is numeric
clean_data$admissions_yield <- as.numeric(clean_data$admissions_yield)

#Average yield across all schools
avg_yield <- clean_data %>% group_by(year) %>% summarise(avg_yield = mean(admissions_yield))

#Elmhurst yield
elmhurst_yield <- clean_data %>% filter(name == "Elmhurst University") %>% 
  group_by(year) %>% summarise(elmhurst_yield = mean(admissions_yield))

#Combine Elmhurst and Average
yield_compare <- left_join(elmhurst_yield, avg_yield, by = "year")

ggplot(yield_compare, aes(x = year)) + geom_line(aes(y = avg_yield), 
      color = "gray") + geom_line(aes(y = elmhurst_yield), color = "hotpink") +
  geom_point(aes(y = avg_yield), color = "gray") + geom_point(aes(y = elmhurst_yield), color = "hotpink") + 
  labs(title = "Elmhurst University vs Average Admissions Yield", x = "Year", y = "Admissions Yield (%)") + theme_minimal()

#-----------------RETENTION TRENDS-------------------------------- 
avg_retention <- clean_data %>% group_by(year) %>% summarise(avg_retention = mean(full_time_retention_rate))

elmhurst_retention <- clean_data %>% filter(name == "Elmhurst University") %>% group_by(year) %>% 
  summarise(elmhurst_retention = mean(full_time_retention_rate))

retention_data <- left_join(elmhurst_retention, avg_retention, by = "year")
ggplot(retention_data, aes(x = year)) + geom_line(aes(y = avg_retention),
  color = "gray") + geom_line(aes(y = elmhurst_retention), color = "hotpink") +
  geom_point(aes(y = avg_retention), color = "gray") + 
  geom_point(aes(y = elmhurst_retention), color = "hotpink") + 
  labs(title = "Elmhurst Retention Rate vs. Average Retention Rate", x = "Year", y = "Retention Rate (%)") + theme_minimal()



