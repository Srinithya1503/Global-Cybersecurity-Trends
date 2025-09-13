# Load libraries
library(tidyverse)
library(lubridate)



# Load data
cyber_data <- read_csv("C:/Users/Global-Cybersecurity-Trends/data/raw_data/Global_Cybersecurity_Threats_2015-2024.csv") %>% # nolint
  clean_names()

# Print column names to check
print(colnames(cyber_data))

#Print year column
print(cyber_data$year)

# Convert year column
cyber_data <- cyber_data %>% # nolint
  mutate(year = as.integer(year)) # nolint

print(cyber_data$attack_type)

# Threat frequency over time
threat_trend <- cyber_data %>%
  count(year, attack_type, name = "n") %>%
  group_by(year)

#Print Summary table
print(threat_trend)

# Create a severity score based on available columns
# First, check available columns
print(colnames(cyber_data))

# Using available columns: financial_loss_in_million, number_of_affected_users
cyber_data <- cyber_data %>%
    mutate( # nolint
        severity_score = rowSums(select(., financial_loss_in_million, number_of_affected_users), na.rm = TRUE) # nolint
    ) # nolint

# Check structure of the data
glimpse(cyber_data)


## Grouping by country, industry, and threat type to summarize incident counts
# This will help in understanding the distribution of threats across different sectors and regions. # nolint
industry_threats <- cyber_data %>%
  group_by(country, target_industry, attack_type) %>%
  summarise(incident_count = n(), .groups = "drop")
# Print the first few rows of the summarized data
print(head(industry_threats))


# Save the cleaned data
write_csv(cyber_data, "C:/Users/Global-Cybersecurity-Trends/data/analysed_data/cleaned_cyber_data.csv") # nolint
print("Data cleaning and analysis complete. Cleaned data saved as cleaned_cyber_data.csv") # nolint

##### Combining Socioeconomic Indicators with Cybersecurity Data #####

# Load cleaned cyber data
cyber_data <- read_csv("C:/Users/Global-Cybersecurity-Trends/data/analysed_data/cleaned_cyber_data.csv") # nolint

# Load socioeconomic indicators
socioeconomic_data <- read_csv("C:/Users/Global-Cybersecurity-Trends/data/raw_data/socioeconomic_indicators.csv") # nolint

# Merge the datasets on country and year
merged_data <- left_join(cyber_data, socioeconomic_data, by = c("country", "year")) # nolint

# Print the first few rows to check
print(head(merged_data))
# Print column names to check

# Check for missing values in the merged data
print(colSums(is.na(merged_data)))


# Save the merged data
write_csv(merged_data, "C:/Users/Global-Cybersecurity-Trends/data/analysed_data/merged_cyber_socioeconomic_data.csv") # nolint
print("Merged data saved as merged_cyber_socioeconomic_data.csv") # nolint

##### Objective 1: Identify the most common cybersecurity threats #####
most_common_threats <- cyber_data %>%
  count(attack_type, sort = TRUE) %>%
  arrange(desc(n))
print("Most common cybersecurity threats:")
print(most_common_threats)

##### Objective 2: Analyze trends over time and across regions #####
# Threat trends over years
threats_over_time <- cyber_data %>%
  group_by(year, attack_type) %>%
  summarise(incident_count = n(), .groups = "drop") %>%
  arrange(year, desc(incident_count))
print("Threat trends over time:")
print(threats_over_time)

# Threat trends by country
threats_by_country <- cyber_data %>%
  group_by(country, attack_type) %>%
  summarise(incident_count = n(), .groups = "drop") %>%
  arrange(country, desc(incident_count))
print("Threat trends across regions (by country):")
print(threats_by_country)

##### Objective 3: Threat distribution by industry and geography #####
threat_distribution <- cyber_data %>%
  group_by(target_industry, country, attack_type) %>%
  summarise(incident_count = n(), .groups = "drop") %>%
  arrange(target_industry, country, desc(incident_count))
print("Threat distribution by industry and geography:")
print(threat_distribution)

##### Objective 4: Actionable insights for mitigation #####
# 1. Industries/countries with highest incident counts
top_industries <- threat_distribution %>%
  group_by(target_industry) %>%
  summarise(total_incidents = sum(incident_count), .groups = "drop") %>%
  arrange(desc(total_incidents)) %>%
  slice_head(n = 5)
print("Industries most targeted by cyber threats:")
print(top_industries)

top_countries <- threat_distribution %>%
  group_by(country) %>%
  summarise(total_incidents = sum(incident_count), .groups = "drop") %>%
  arrange(desc(total_incidents)) %>%
  slice_head(n = 5)
print("Countries most targeted by cyber threats:")
print(top_countries)

# 2. Most frequent attack types per industry
top_threats_per_industry <- threat_distribution %>%
  group_by(target_industry, attack_type) %>%
  summarise(total_incidents = sum(incident_count), .groups = "drop") %>%
  arrange(target_industry, desc(total_incidents)) %>%
  group_by(target_industry) %>%
  slice_max(total_incidents, n = 1)
print("Most frequent attack type per industry:")
print(top_threats_per_industry)

