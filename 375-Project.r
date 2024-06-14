# Load tidyverse
library(tidyverse)

# Read the two data files
covid_data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
population_data <- read_csv("/home/dharbo/Downloads/twb.csv")

# Keep only country-level data (remove all rows where country code != 3 letters)
covid_data <- covid_data %>% filter(nchar(iso_code) == 3)

# Remove countries with less than 1 million total population
covid_data <- covid_data %>% filter(population >= 1000000)

# Add new_deaths_smoothed_2wk column, which has the new_deaths_smoothed value 2 weeks ahead
covid_data <- covid_data %>% group_by(iso_code) %>% mutate(new_deaths_smoothed_2wk = lead(new_deaths_smoothed, n = 14))

# Tidy the Population data (population_data)
population_data <- population_data %>% select(-`Series Name`) %>% pivot_wider(names_from = `Series Code`, values_from = `2023 [YR2023]`)

# Merge the two tables using the 3-letter ISO code
merged_table <- covid_data %>% inner_join(population_data, join_by(iso_code == `Country Code`), relationship = "many-to-many")

# Transform variables
merged_table <- merged_table %>% mutate(diabetic_population = (diabetes_prevalence * 0.01) * population,
                                        total_smokers = female_smokers + male_smokers,
                                        SP.POP.80UP.TOT = as.numeric(SP.POP.80UP.FE) + as.numeric(SP.POP.80UP.MA))

# Split the data into train and test subsets
merged_2022 <- merged_table %>% filter(date >= as.Date("2022-01-01"), date <= as.Date("2022-12-31"))
merged_2023 <- merged_table %>% filter(date >= as.Date("2023-01-01"), date <= as.Date("2023-06-30"))

# Drop NAs in test data
merged_2023 <- merged_2023 %>% drop_na(diabetic_population, total_smokers, SP.POP.80UP.TOT)

# Linear Models
model_1 <- lm(new_deaths_smoothed_2wk~new_cases_smoothed+diabetic_population, data=merged_2022)
model_2 <- lm(new_deaths_smoothed_2wk~new_cases_smoothed+total_smokers+cardiovasc_death_rate+life_expectancy, data=merged_2022)
model_3 <- lm(new_deaths_smoothed_2wk~new_cases_smoothed+people_fully_vaccinated+total_cases, data=merged_2022)
model_4 <- lm(new_deaths_smoothed_2wk~icu_patients+diabetic_population+total_smokers+SP.POP.80UP.TOT+people_fully_vaccinated, data=merged_2022)
model_5 <- lm(new_deaths_smoothed_2wk~life_expectancy+icu_patients+total_vaccinations, data=merged_2022)

# Display the R2 values with summary() for all models
summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)
summary(model_5)

# Load modelr
library(modelr)

# Use RSME to evaluate the models
rmse_model_1 <- rmse(model_1, data = merged_2023)
rmse_model_2 <- rmse(model_2, data = merged_2023)
rmse_model_3 <- rmse(model_3, data = merged_2023)
rmse_model_4 <- rmse(model_4, data = merged_2023)
rmse_model_5 <- rmse(model_5, data = merged_2023)

# Display the RMSE values for all models
print(rmse_model_1)
print(rmse_model_2)
print(rmse_model_3)
print(rmse_model_4)
print(rmse_model_5)

# Get RMSE for each country in our best model (model_5)
merged_2023 %>% group_by(location) %>% summarise(rmse = rmse(model=model_5, data=cur_data()), population) %>%
  drop_na(rmse) %>% distinct() %>% arrange(-population)
