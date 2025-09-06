countries <- c("AU", "BR", "CN", "FR", "DE", "IN", "JP", "RU", "GB", "US")
#install.packages("WDI")
library(WDI)
library(dplyr)
library(readr)

gdp_data <- WDI(country = countries,
                indicator = "NY.GDP.MKTP.CD",
                start = 2015, end = 2024)

gdp_clean <- gdp_data %>%
  rename(country = country, year = year, gdp = NY.GDP.MKTP.CD) %>%
  select(country, year, gdp)
internet_data <- WDI(country = countries,
                     indicator = "IT.NET.USER.ZS",
                     start = 2015, end = 2024)

internet_clean <- internet_data %>%
  rename(country = country, year = year, internet_penetration = IT.NET.USER.ZS) %>% # nolint
  select(country, year, internet_penetration)
# Merge GDP and Internet Penetration Data
socioeconomic_data <- left_join(gdp_clean, internet_clean, by = c("country", "year")) # nolint
# Print the first few rows of the socioeconomic data
print(head(socioeconomic_data))

install.packages("zoo") # Install zoo package for na.approx function
library(zoo)
# Interpolate missing values for internet penetration by country and year  
socioeconomic_data <- socioeconomic_data %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(internet_penetration = na.approx(internet_penetration, year, rule = 2)) %>% # nolint
  ungroup()

write_csv(socioeconomic_data, "C:/Users/srinithya/Desktop/NITHYA/Data Analysis Project/Global-Cybersecurity/data/raw_data/socioeconomic_indicators.csv") # nolint


