# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# Load the CSV file
df <- read_csv("data/foodcrime_articles.csv")

# Inspect the first few rows
head(df)

# Convert the date column to a standard format
df$date <- parse_date_time(df$date, orders = c("mdy", "mdy HMS", "ymd", "dmy"))

# Identify rows where date conversion failed
invalid_dates <- df %>% filter(is.na(date))

# Display the rows with invalid dates
print(invalid_dates)

# Manually handle or drop rows with invalid dates
df_cleaned <- df %>% filter(!is.na(date))

# Ensure the date column is correctly formatted as a Date object
df_cleaned$date <- as.Date(df_cleaned$date)

# Add a unique identifier column with a custom format
df_cleaned <- df_cleaned %>%
  group_by(date) %>%
  mutate(id = row_number()) %>%
  ungroup() %>%
  mutate(id = str_c("FC_", format(date, "%Y%m%d"), "_", str_pad(id, 3, pad = "0")))

# Check the result
head(df_cleaned)

# Save the cleaned data with the unique identifier to a CSV file
write_csv(df_cleaned, "data/foodcrime_articles_clean.csv")

# Inspect the cleaned data
head(df_cleaned)


