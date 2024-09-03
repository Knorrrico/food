# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(countrycode)

# Load the cleaned data

df_cleaned <- read.csv("data/foodcrime_articles_clean.csv")

# Aggregate the news sources by counting the number of articles per source
source_aggregation <- df_cleaned %>%
  group_by(source) %>%
  summarise(article_count = n()) %>%
  arrange(desc(article_count))

# Select the top N sources (e.g., top 10)
top_n_sources <- source_aggregation %>% top_n(10, article_count)

# Plot the bar chart for the top N sources
ggplot(top_n_sources, aes(x = reorder(source, -article_count), y = article_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Sources by Number of Articles", x = "Source", y = "Number of Articles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create month and year columns
df_cleaned <- df_cleaned %>%
  mutate(month = month(date, label = TRUE),  # Convert month to a factor with month labels
         year = factor(year(date)))  # Convert year to a factor

# Group by year and month and count the number of articles
heatmap_data <- df_cleaned %>%
  group_by(year, month) %>%
  summarise(article_count = n()) %>%
  ungroup()

# Plot the heatmap with months on the x-axis
ggplot(heatmap_data, aes(x = month, y = year, fill = article_count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Article Frequency Heatmap", x = "Month", y = "Year", fill = "Number of Articles") +
  theme_minimal()

# Load the country mapping data
countries <- read.csv("data/updated_newspapers_countries.csv")

df_with_countries <- df_cleaned %>%
  left_join(countries, by = c("source" = "Newspaper"))

country_article_counts <- df_with_countries %>%
  group_by(Country) %>%
  summarise(article_count = n())

# Load the world map data using rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge the article counts with the world map data
# Ensure the join is based on the correct column (e.g., "name" for country names or "iso_a3" for ISO3 codes)
world_with_data <- world %>%
  left_join(country_article_counts, by = c("name" = "Country"))

# Plot the world map with article counts
ggplot(world_with_data) +
  geom_sf(aes(fill = article_count), color = "white") +
  scale_fill_continuous(name = "Article Count", na.value = "gray80") +
  theme_minimal() +
  labs(title = "World Map of Article Counts per Country", x = "", y = "") +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())





