# load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(viridis)

# aggregate by source and count the number of articles =========================
source_aggregation <- data_food |> 
  group_by(source) |> 
  summarise(article_count = n()) |> 
  arrange(desc(article_count))

# top n sources by number of articles
top_n_sources <- source_aggregation |> 
  top_n(10, article_count)

# plot bar chart of top n sources
ggplot(top_n_sources, aes(x = reorder(source, -article_count), y = article_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Sources by Number of Articles", x = "Source", y = "Number of Articles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# month and year columns
data_food <- data_food |> 
  mutate(month = month(date, label = TRUE),  
         year = factor(year(date)))  

# group by year and month
heatmap_data <- data_food |> 
  group_by(year, month) |> 
  summarise(article_count = n()) |> 
  ungroup()

# plot heatmap, x is month, y is year, fill is count
ggplot(heatmap_data, aes(x = month, y = year, fill = article_count)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C") +  
  labs(title = "Article Frequency Heatmap", x = "Month", y = "Year", fill = "Number of Articles") +
  theme_minimal()

# country mapping =============================================================
country_article_counts <- data_food |> 
  group_by(Country) |> 
  summarise(article_count = n()) 

# load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# merge article counts with world map data
world_with_data <- world |> 
  left_join(country_article_counts, by = c("name" = "Country"))

# plot world map with article counts
ggplot(world_with_data) +
  geom_sf(aes(fill = article_count), color = "white") +
  scale_fill_gradientn(colors = c("lightblue", "yellow", "orange", "red", "darkred"), 
                       na.value = "gray80", name = "Number of Articles") +  # Custom legend title
  theme_minimal() +
  labs(title = "World Map of Article Counts per Country", x = "", y = "") +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

