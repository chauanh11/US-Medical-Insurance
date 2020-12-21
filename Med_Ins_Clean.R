library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggstance)
library(car)

setwd("/Users/anhhc/Desktop/Final")

raw_data <- read.csv('https://raw.githubusercontent.com/chauanh11/US-Medical-Insurance/main/Medical%20Insurance%20Data.csv', 
                         header=TRUE)

# Preparing variables by 'factoring' non-numeric variables
mod_data <- labelled::to_factor(raw_data)

# Creating shortcut for variable names
n = 1338
age <- mod_data$age
sex <- mod_data$sex
bmi <- mod_data$bmi
children <- mod_data$children
smoker <- mod_data$smoker
region <- mod_data$region
charges <- mod_data$charges

# Check Linearity of variables and charges
plot(age, charges,main = "Age vs. Charge")
abline(lm(charges~age), col="red")

plot(children, charges, main = "Number of children vs. Charges")

plot(bmi, charges, main="BMI vs.charge")
abline(lm(charges~bmi), col="red")

plot(age, bmi, main="BMI vs.age")
abline(lm(bmi~age), col="red")

# Histogram for numerical variables
hist(age, 
     main = "Density Histogram for Age of Policy Member", 
     xlab = "Age",
     xlim =c(10, 70), 
     freq = FALSE)
hist(bmi, 
     main = "Density Histogram for BMI of Policy Member", 
     xlab = "BMI",
     xlim = c(10, 60), 
     freq = FALSE)
hist(children, 
     breaks=5,
     main = "Density Histogram for Children per Policy Member", 
     xlab = "Number of children",
     freq = FALSE)
hist(charges,
     main = "Density of Insurance Cost",
     xlab = "Cost ($)",
     breaks = c(1000, 7000, 13000, 19000, 25000, 31000, 40000, 55000, 69000),
     freq = FALSE)

mean(bmi)
mean(charges) 
# $13,270.42

# Regional Analysis
ex1 <- mod_data %>%
  filter(region == "northwest") %>%
  select(charges)
# Min for northwest = $1621.34 and max = 60021.4
hist(ex1$charges, main = "Cost distribution in Northwest region", xlim =c(1000, 67000), freq = FALSE)

ex2 <-medical_data %>%
  filter(region == "northeast") %>%
  select(charges)
#min = 1694.796, max = 58571
hist(ex2$charges, main = "Cost distribution in Northeast region", xlim =c(1000, 62000), freq = FALSE)

ex3 <-medical_data %>%
  filter(region == "southwest") %>%
  select(charges)
hist(ex3$charges, main = "Cost distribution in Southwest region", xlim =c(1000, 67000), freq = FALSE)

ex4 <-medical_data %>%
  filter(region == "southeast") %>%
  select(charges)
hist(ex4$charges, main = "Cost distribution in Southeast region", xlim =c(1000, 63000), freq = FALSE)

## Converting non-numerical to numerical
# Everything factored at the beginning of the code, ignore this section
# male = 2, female = 1
gender <- ifelse(medical_data$sex=="female", 1, 0)
gender <- as.integer(gender)
hist(gender, main="Distribution of gender", freq = FALSE)

# smoker = 1, non-smoker = 0
smoker <- ifelse(mod_data$smoker=="yes", 1, 0)
smoker <- as.integer(smoker)
hist(smoker, main="Distribution of smokers", freq=FALSE)
mod_data$smoker_num = smoker

# southwest = 3, southeast = 4, northwest = 1, and northeast = 2
#location <- as.factor(location)
#location <- as.integer(location)
mod_data$loc_num = location
mod_data$gender_num = gender

## Correlations between variables
pairs(~ age + bmi + children + smoker, data = mod_data)

# Variance inflation factor for each variable
vif(multi.fit) %>%    
  knitr::kable()

# Model Development
multi.fit1 <- lm(charges ~ age + sex + bmi + children + smoker + region, data = mod_data)
summary(multi.fit1)
#look at the p-values of the estimated coefficients above, we see that not all the coefficients are statistically significant (i.e <0.05). 
#Since the p-value of the variable sex and location is greater than 0.05, it's removed from the modeL:
multi.fit <- lm(charges ~ age + bmi + children + smoker + region, data = mod_data)
summary(multi.fit)
sumres <- sum(multi.fit$residuals)
sumres
# Sum of res not equal to 1, sum of errors = 0 assumption violated

# A high value of F statistic, with a very low p-value (<2.2e-16), implies that the null hypothesis can be rejected, there is a potential relationship between the predictors and the outcome
# Residual Standard Error is the estimate of the standard deviation of the error which can’t be reduced even if we knew the true regression line (large value of RSE means a high deviation of our model from the true regression line)
#The Std. Error gives us the average amount that the estimated coefficient of a predictor differs from the actual coefficient of predictor

## Plot of Fit
plot(multi.fit)
# Note that slight linear relation in the squareroot(resid) vs. fitted values, this implies an autocorrelation that has significant effects on the model
# the method to adjust this issue is not within the scope of this class, we just have to acknowledge the limitations it'll impose on the model

# F-Test 
anova(multi.fit, test = "F")

### Confidence interval
#Final Model
summ(multi.fit, confint = TRUE, digits = 3)

### Assessing Influential Observations:
cutoff <- 4/(nrow(data)-length(model$coefficients)-2)
plot(model, which=4, cook.levels=cutoff)

# Generating 
write_csv(mod_data, "output/medical_data.csv")




