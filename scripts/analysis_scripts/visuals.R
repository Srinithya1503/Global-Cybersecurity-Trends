# 📌 Global Cybersecurity Threats Analysis with Visuals (Save to Folder)
# ------------------------------------------------------

# Load libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(janitor)

# Load cleaned cyber data
cyber_data <- read_csv("C:/Users/srinithya/Desktop/NITHYA/Data Analysis Project/Global-Cybersecurity/github_repo/Global Cybersecurity Trends/data/analysed_data/cleaned_cyber_data.csv") %>%
  clean_names()

# ------------------------------------------------------
# 🎨 Dark Themes

# Bar plots: no grid lines
dark_theme_no_grid <- theme_minimal(base_size = 16) +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(fill = "black", color = NA),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white", size = 14),
    axis.title = element_text(color = "white", size = 16, face = "bold"),
    plot.title = element_text(color = "white", face = "bold", size = 20),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Line plot: keep horizontal grid lines, no labels
dark_theme_line <- dark_theme_no_grid +
  theme(
    panel.grid.major.y = element_line(color = "gray40", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# ------------------------------------------------------
# Create output folder
output_dir <- "C:/Users/srinithya/Desktop/NITHYA/Data Analysis Project/Global-Cybersecurity/github_repo/Global Cybersecurity Trends/visuals"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Utility function to save plots
save_plot <- function(plot, filename) {
  ggsave(
    filename = file.path(output_dir, paste0(filename, ".png")),
    plot = plot,
    width = 10, height = 6, dpi = 300, bg = "black"
  )
}

# ------------------------------------------------------
# Objective 1: Most Common Cybersecurity Threats
most_common_threats <- cyber_data %>% count(attack_type, sort = TRUE)

p1 <- ggplot(most_common_threats, aes(x = reorder(attack_type, n), y = n, fill = attack_type)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 4, color = "white") +
  coord_flip() +
  labs(title = "Most Common Cybersecurity Threats",
       x = "Attack Type", y = "Number of Incidents") +
  dark_theme_no_grid +
  theme(legend.position = "none")

save_plot(p1, "01_most_common_threats")

# ------------------------------------------------------
# Objective 2: Trends Over Time (no numerical labels)
threats_over_time <- cyber_data %>%
  group_by(year, attack_type) %>%
  summarise(incident_count = n(), .groups = "drop")

p2 <- ggplot(threats_over_time, aes(x = year, y = incident_count, color = attack_type, group = attack_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Cybersecurity Threats Over Time",
       x = "Year", y = "Number of Incidents") +
  dark_theme_line

save_plot(p2, "02_threats_over_time")

# ------------------------------------------------------
# Top Countries
top_countries <- cyber_data %>% count(country, sort = TRUE) %>% slice_head(n = 10)

p3 <- ggplot(top_countries, aes(x = reorder(country, n), y = n, fill = country)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 4, color = "white") +
  coord_flip() +
  labs(title = "Top 10 Countries by Cybersecurity Incidents",
       x = "Country", y = "Number of Incidents") +
  dark_theme_no_grid +
  theme(legend.position = "none")

save_plot(p3, "03_top_countries")

# ------------------------------------------------------
# Example: Additional bar plots can use dark_theme_no_grid
# e.g., Top Industries or Top Threats per Industry

# ------------------------------------------------------
print("All visualizations saved to output directory.") 
