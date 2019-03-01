##### DATA CLEANING AND PROCESSING #####

# set working directory
setwd("C:/Users/Alex/Desktop/user-2019-datathon-contribution")

# load libraries
library(tidyverse)

# disable scientific notation
options(scipen=999)

# load raw data and mapping files (mapping data available at: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups)
raw_data <-  read.csv("HNP_StatsData.csv", header=TRUE, sep=",", dec=".")
region_and_income_group_mapping <- read.csv("region_and_income_group_mapping.csv", header=TRUE, sep=";")

# drop and rename columns
raw_data$X <- NULL
names(raw_data)[1] <- "Country.Name"
names(raw_data) <- gsub("X", "", names(raw_data))
names(region_and_income_group_mapping)[1] <- "Country.Name"

# create separate dataframe for countries excluding groups
country_data <- raw_data[which(raw_data$Country.Name %in% region_and_income_group_mapping$Country.Name), ]

# select indicators related to nutrition
nutrition_indicators <- c("Number of people who are undernourished",
                          "Prevalence of undernourishment (% of population)",
                          "Cause of death, by communicable diseases and maternal, prenatal and nutrition conditions (% of total)",
                          "Consumption of iodized salt (% of households)",
                          "Diabetes prevalence (% of population ages 20 to 79)",
                          "Diarrhea treatment (% of children under 5 receiving oral rehydration and continued feeding)",
                          "Diarrhea treatment (% of children under 5 who received ORS packet)",
                          "Exclusive breastfeeding (% of children under 6 months)",
                          "Infant and young child feeding practices, all 3 IYCF (% children ages 6-23 months)",
                          "Low-birthweight babies (% of births)",
                          "Malnutrition prevalence, height for age (% of children under 5)",
                          "Malnutrition prevalence, height for age, female (% of children under 5)",
                          "Malnutrition prevalence, height for age, male (% of children under 5)",
                          "Malnutrition prevalence, weight for age (% of children under 5)",
                          "Malnutrition prevalence, weight for age, female (% of children under 5)",
                          "Malnutrition prevalence, weight for age, male (% of children under 5)",
                          "Prevalence of overweight (% of adults)",
                          "Prevalence of overweight (% of children under 5)",
                          "Prevalence of overweight, female (% of children under 5)",
                          "Prevalence of overweight, female (% of female adults)",
                          "Prevalence of overweight, male (% of children under 5)",
                          "Prevalence of overweight, male (% of male adults)",
                          "Prevalence of severe wasting, weight for height (% of children under 5)",
                          "Prevalence of severe wasting, weight for height, female (% of children under 5)",
                          "Prevalence of severe wasting, weight for height, male (% of children under 5)",
                          "Prevalence of wasting (% of children under 5)",
                          "Prevalence of wasting, female (% of children under 5)",
                          "Prevalence of wasting, male (% of children under 5)",
                          "Number of people who are undernourished",
                          "Prevalence of undernourishment (% of population)",
                          "Vitamin A supplementation coverage rate (% of children ages 6-59 months)")

# select socio-demographic indicators
socio_demographic_indicators <- c("Population, total",
                                  "Rural population",
                                  "Rural population (% of total population)",
                                  "Urban population",
                                  "Urban population (% of total)",
                                  "Population ages 00-14 (% of total)",
                                  "Population ages 00-14, female (% of total)",
                                  "Population ages 00-14, male (% of total)",
                                  "Population ages 00-14, total",
                                  "Population ages 0-14, female",
                                  "Population ages 0-14, male",
                                  "Population growth (annual %)",
                                  "Population, female",
                                  "Population, female (% of total)",
                                  "Population, male",
                                  "Population, male (% of total)",
                                  "Poverty headcount ratio at national poverty line (% of population)",
                                  "Rural poverty headcount ratio at national poverty lines (% of rural population)",
                                  "Urban poverty headcount ratio at national poverty lines (% of urban population)",
                                  "Adolescent fertility rate (births per 1,000 women ages 15-19)",
                                  "Birth rate, crude (per 1# Death rate, crude (per 1,000 people)",
                                  "Fertility rate, total (births per woman)",
                                  "Wanted fertility rate (births per woman)",
                                  "Life expectancy at birth, female (years)",
                                  "Life expectancy at birth, male (years)",
                                  "Life expectancy at birth, total (years)",
                                  "Mortality rate, infant (per 1,000 live births)",
                                  "Mortality rate, infant, female (per 1,000 live births)",
                                  "Mortality rate, infant, male (per 1,000 live births)",
                                  "Mortality rate, adult, female (per 1,000 female adults)",
                                  "Mortality rate, adult, male (per 1,000 male adults)",
                                  "Mortality rate, under-5 (per 1,000)",
                                  "Mortality rate, under-5, female (per 1,000)",
                                  "Mortality rate, under-5, male (per 1,000)",
                                  "Public spending on education, total (% of GDP)",
                                  "Literacy rate, adult female (% of females ages 15 and above)",
                                  "Literacy rate, adult male (% of males ages 15 and above)",
                                  "Literacy rate, adult total (% of people ages 15 and above)",
                                  "Literacy rate, youth male (% of males ages 15-24)",
                                  "Literacy rate, youth total (% of people ages 15-24)",
                                  "Current health expenditure (% of GDP)",
                                  "Current health expenditure per capita (current US$)",
                                  "Current health expenditure per capita, PPP (current international $)",
                                  "Domestic general government health expenditure (% of current health expenditure)",
                                  "Domestic general government health expenditure (% of GDP)",
                                  "Domestic general government health expenditure (% of general government expenditure)",
                                  "Domestic general government health expenditure per capita (current US$)",
                                  "Domestic general government health expenditure per capita, PPP (current international $)",
                                  "Domestic private health expenditure (% of current health expenditure)",
                                  "Domestic private health expenditure per capita (current US$)",
                                  "Domestic private health expenditure per capita, PPP (current international $)",
                                  "External health expenditure (% of current health expenditure)",
                                  "External health expenditure channeled through government (% of external health expenditure)",
                                  "External health expenditure per capita (current US$)",
                                  "External health expenditure per capita, PPP (current international $)",
                                  "GNI per capita, Atlas method (current US$)",
                                  "Unemployment, female (% of female labor force)",
                                  "Unemployment, male (% of male labor force)",
                                  "Unemployment, total (% of total labor force)")

# reshape data and filter indicators of interest
country_data_filtered_and_reshaped <- country_data %>%
  gather(Year, Value, -Country.Name, -Country.Code, -Indicator.Name, -Indicator.Code) %>% 
  filter(Indicator.Name %in% nutrition_indicators | Indicator.Name %in% socio_demographic_indicators) %>% 
  select(-Indicator.Code) %>% 
  spread(Indicator.Name, Value)

# map region and income information to filtered country data
country_data_final <- merge(country_data_filtered_and_reshaped, region_and_income_group_mapping, by=c("Country.Name", "Country.Code"), type="left")

# change order of columns
country_data_final <- country_data_final[,c(1,2,3,90,91,4:(ncol(country_data_final)-2))]

# clean column names and indicator list
names(country_data_final) <- gsub("\\(|\\)", "", names(country_data_final))
names(country_data_final) <- gsub("\\(|\\)", "", names(country_data_final))
names(country_data_final) <- gsub("%", "per cent", names(country_data_final))
names(country_data_final) <- gsub(",", "", names(country_data_final))
names(country_data_final) <- gsub(" ", ".", names(country_data_final))

socio_demographic_indicators <- gsub("\\(|\\)", "", socio_demographic_indicators)
socio_demographic_indicators <- gsub("\\(|\\)", "", socio_demographic_indicators)
socio_demographic_indicators <- gsub("%", "per cent", socio_demographic_indicators)
socio_demographic_indicators <- gsub(",", "", socio_demographic_indicators)
socio_demographic_indicators <- gsub(" ", ".", socio_demographic_indicators)

nutrition_indicators <- gsub("\\(|\\)", "", nutrition_indicators)
nutrition_indicators <- gsub("\\(|\\)", "", nutrition_indicators)
nutrition_indicators <- gsub("%", "per cent", nutrition_indicators)
nutrition_indicators <- gsub(",", "", nutrition_indicators)
nutrition_indicators <- gsub(" ", ".", nutrition_indicators)




