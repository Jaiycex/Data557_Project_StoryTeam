# Notes from Instructor on this Q : 
# What would be the rationales for the predictors in your regression model?
# When running the t-test, check whether the starting salary is normally 
# distributed. Otherwise, consider z-test given that the sample size is sufficiently large.

library(tidyverse)
library(ggplot2)
library(sandwich)
library(lmtest) 
library(corrplot)  
library(dplyr)

## ARLETTE SECTION

# Load the dataset
salary <- read.table("salary.txt", header = TRUE)

# Inspect structure
head(salary)
str(salary)

# Convert categorical variables to factors
salary$sex <- factor(salary$sex)
salary$field <- factor(salary$field)
salary$deg <- factor(salary$deg)
salary$rank <- factor(salary$rank)

start_salary <- salary %>%
  filter(year == startyr)

head(start_salary)

start_salary %>%
  group_by(sex) %>%
summarise (
  median_salary = median(salary),
  mean_salary = mean (salary),
  count = n()
)

start_salary$experience <- start_salary$startyr - start_salary$yrdeg
model <- lm(salary ~ sex + field + deg + experience + rank + admin,
            data=start_salary)

summary(model)

model2 <- lm(salary ~ sex * field + deg + experience + rank,
             data=start_salary)

summary(model2)

obs_diff <- with(start_salary,
                 mean(salary[sex=="M"]) - mean(salary[sex=="F"]))

set.seed(123)

perm_diffs <- replicate(5000, {
  
  shuffled_sex <- sample(start_salary$sex)
  
  mean(start_salary$salary[shuffled_sex=="M"]) -
    mean(start_salary$salary[shuffled_sex=="F"])
  
})

p_value <- mean(abs(perm_diffs) >= abs(obs_diff))
p_value

hist(perm_diffs,
     breaks = 40,
     main = "Permutation Distribution of Salary Difference",
     xlab = "Difference in Mean Salary (M - F)")

abline(v = obs_diff, col="red", lwd=3)


## JULIA SECTION 

Q1_data  <- read.table("salary.txt", header = TRUE)

# Selecting samples where it is the faculty's first year 
start_year_subset <- Q1_data[Q1_data$startyr == Q1_data$year, ]
assist_prof_subset <- start_year_subset[start_year_subset$rank == "Assist", ]

# Convert columns so they can go into the regression model
assist_prof_subset$admin <- factor(assist_prof_subset$admin)
assist_prof_subset$sex <- factor(assist_prof_subset$sex)
assist_prof_subset$field <- factor(assist_prof_subset$field)
assist_prof_subset$deg <- factor(assist_prof_subset$deg)

#Assist prof model (dropped admin bc only 1 point was admin == 1)
Q1_assist_lm <- lm(salary ~ sex + field + deg + yrdeg + year , data = assist_prof_subset)

print("Assistant Professor Only Model")
print(summary(Q1_assist_lm))

# Check residuals + plot
Q1_assist_residuals <- Q1_assist_lm$residuals
plot(fitted(Q1_assist_lm), Q1_assist_residuals, main = "Residuals of Assistant Professor Starting Salary Model", col = 'dark green')
abline(0,0)

# Apply robust standard errors 
Q1_lm_robust <- coeftest(Q1_assist_lm, vcov = vcovHC(Q1_assist_lm, type = "HC3"))
print("Final Model with Robust Standard Errors:")
print(Q1_lm_robust)

# Subset into female vs male 
women_data <- assist_prof_subset[assist_prof_subset$sex == "F", ]
men_data <- assist_prof_subset[assist_prof_subset$sex == "M", ]

# Get the size of each just for reference
print(paste('Women data points:',nrow(women_data)))
print(paste('Men data points:',nrow(men_data)))

# Look at variance of men vs women salary for startyr
print(paste('Women variance:',var(women_data$salary)))
print(paste('Men variance:',var(men_data$salary)))

# Q-Q plots for starting salary 
windows()
qqnorm(women_data$salary, col = 'pink', main = "Q-Q Plot for Female Assistant Prof Starting Salary" )
qqline(women_data$salary)

windows()
qqnorm(men_data$salary, col = 'blue', main = "Q-Q Plot for Male Assistant Prof Starting Salary" )
qqline(men_data$salary)

# Permutation test for Assistant Profs
obs_diff <- with(start_year_subset,
                 mean(salary[sex=="M"]) - mean(salary[sex=="F"]))

perm_diffs <- replicate(5000, {
  shuffled_sex <- sample(start_year_subset$sex)
  mean(start_year_subset$salary[shuffled_sex=="M"]) -
    mean(start_year_subset$salary[shuffled_sex=="F"])
})

assist_p_value <- mean(abs(perm_diffs) >= abs(obs_diff))

print(paste("Permutation Test For Assistant Professors p value:", assist_p_value))