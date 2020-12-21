library(haven)
library(tidyverse)

# Read raw data.
setwd("/Users/anhhc/Desktop/Final")
raw_data2 <- read_dta("data/usa_00005.dta.gz")

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data2 %>% 
  select(sex, 
         age, 
         hcovany,
         hcovpriv,
         hcovpub,
         hinsemp,
         hinspur,
         hcovpub,
         inctot)

# unique(reduced_data$sex)
# 1=male and 2=female
# Altering gender string
reduced_data$sex <- gsub(1, "Male", reduced_data$sex)
reduced_data$sex <- gsub(2, "Female", reduced_data$sex)
reduced_data$sex <- as.factor(reduced_data$sex)

#unique(reduced_data$hcovany)
# 1 = no health insurance & 2 = has health insurance
# Altering existence of insurance string
reduced_data$hcovany <- gsub(1, "No Insurance", reduced_data$hcovany)
reduced_data$hcovany <- gsub(2, "Has Insurance", reduced_data$hcovany)
reduced_data$hcovany <- as.factor(reduced_data$hcovany)

#unique(reduced_data$hcovpriv)
# 1 = no private insurance & 2 = has private insurance
# Altering private insurance string
reduced_data$hcovpriv <- gsub(1, "Has private insurance", reduced_data$hcovpriv)
reduced_data$hcovpriv <- gsub(2, "Has private insurance", reduced_data$hcovpriv)
reduced_data$hcovpriv <- as.factor(reduced_data$hcovpriv)

#unique(reduced_data$hcovpub)
# 1 = No public insurance & 2 = Has public insurance
# Altering public insurance string
reduced_data$hcovpub <- gsub(1, "No public insurance", reduced_data$hcovpub)
reduced_data$hcovpub <- gsub(2, "Has public insurance", reduced_data$hcovpub)
reduced_data$hcovpub <- as.factor(reduced_data$hcovpub)

#unique(reduced_data$hcovpub)
# 1 = No public insurance & 2 = Has public insurance
# Altering public insurance string
reduced_data$hcovpub <- gsub(1, "No public insurance", reduced_data$hcovpub)
reduced_data$hcovpub <- gsub(2, "Has public insurance", reduced_data$hcovpub)
reduced_data$hcovpub <- as.factor(reduced_data$hcovpub)

#unique(reduced_data$hinsemp)
# 1 = No insurance through employer/union & 2 = Has insurance through employer/union
# Altering public insurance string
reduced_data$hinsemp <- gsub(1, "No insurance through employer/union", reduced_data$hinsemp)
reduced_data$hinsemp <- gsub(2, "Has insurance through employer/union", reduced_data$hinsemp)
reduced_data$hinsemp <- as.factor(reduced_data$hinsemp)

#unique(reduced_data$hinspur)
# 1 = No insurance through employer/union & 2 = Has insurance through employer/union
# Altering public insurance string
reduced_data$hinspur <- gsub(1, "No insurance purchased directly", reduced_data$hinspur)
reduced_data$hinspur <- gsub(2, "Has insurance purchased directly", reduced_data$hinspur)
reduced_data$hinspur <- as.factor(reduced_data$hinspur)

# Removing ambiguous income status
reduced_data <- 
  reduced_data %>% 
  filter(inctot != "9999999") 

# Dividing different range of income into groups
# max(reduced_data$inctot) = 1629000
# mean(reduced_data$inctot) = $45302

# Removing negative income
reduced_data$inctot <- as.integer(reduced_data$inctot)
reduced_data <- 
  reduced_data %>% 
  filter(inctot > 0)

hist(reduced_data$inctot,
     main = "Density of Insurance Cost",
     xlab = "Income (USD)",
     breaks = 5)

# Creating new column then create groups for income
reduced_data$incgrp <- reduced_data$inctot
reduced_data$incgrp <- cut(reduced_data$incgrp, c(-1, 24999,49999,99999,249999,2000000), 
                            labels = c("Less than $25,000", "$25,000 to $49,999", "$50,000 to $99,999", "$100,000 to $249,999","More than $249,999"))


# Saving data as a csv file in working directory
write_csv(reduced_data, "output/IPUMS_data.csv")


  