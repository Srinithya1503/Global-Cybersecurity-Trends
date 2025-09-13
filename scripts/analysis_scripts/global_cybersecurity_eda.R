# Load libraries
library(tidyverse)
library(lubridate)



# Load data
cyber_data <- read_csv("C:/Users/srinithya/Desktop/NITHYA/Data Analysis Project/Global-Cybersecurity/data/raw_data/Global_Cybersecurity_Threats_2015-2024.csv") %>% # nolint
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
write_csv(cyber_data, "C:/Users/srinithya/Desktop/NITHYA/Data Analysis Project/Global-Cybersecurity/data/analysed_data/cleaned_cyber_data.csv") # nolint
print("Data cleaning and analysis complete. Cleaned data saved as cleaned_cyber_data.csv") # nolint

##### Combining Socioeconomic Indicators with Cybersecurity Data #####

# Load cleaned cyber data
cyber_data <- read_csv("C:/Users/srinithya/Desktop/NITHYA/Data Analysis Project/Global-Cybersecurity/data/analysed_data/cleaned_cyber_data.csv") # nolint

# Load socioeconomic indicators
socioeconomic_data <- read_csv("C:/Users/srinithya/Desktop/NITHYA/Data Analysis Project/Global-Cybersecurity/data/raw_data/socioeconomic_indicators.csv") # nolint

# Merge the datasets on country and year
merged_data <- left_join(cyber_data, socioeconomic_data, by = c("country", "year")) # nolint

# Print the first few rows to check
print(head(merged_data))
# Print column names to check

# Check for missing values in the merged data
print(colSums(is.na(merged_data)))


# Save the merged data
write_csv(merged_data, "C:/Users/srinithya/Desktop/NITHYA/Data Analysis Project/Global-Cybersecurity/data/analysed_data/merged_cyber_socioeconomic_data.csv") # nolint
print("Merged data saved as merged_cyber_socioeconomic_data.csv") # nolint