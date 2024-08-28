library(quanteda)
library(lexicon)
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

library(quanteda)
library(lexicon)
library(dplyr)

process_text_data <- function(data_frame, text_field = "body", group_field = NULL, 
                              remove_stopwords = TRUE, do_lemmatization = FALSE) {
  
  # Create a corpus from the specified text field
  corpus_data <- corpus(data_frame, text_field = text_field)
  
  # Tokenize the text, removing unwanted elements
  tokens_data <- tokens(corpus_data,
                        remove_punct = TRUE,
                        remove_symbols = TRUE,
                        remove_numbers = TRUE,
                        remove_url = TRUE,
                        remove_separators = TRUE)
  
  # Remove stopwords if specified
  if (remove_stopwords) {
    tokens_data <- tokens_remove(tokens_data, pattern = stopwords("en"))
    # Add any custom stopwords here
    custom_stopwords <- c("example_stopword1", "example_stopword2")
    tokens_data <- tokens_remove(tokens_data, pattern = custom_stopwords)
  }
  
  # Perform lemmatization if specified
  if (do_lemmatization) {
    tokens_data <- tokens_replace(tokens_data, pattern = lexicon::hash_lemmas$token, 
                                  replacement = lexicon::hash_lemmas$lemma)
  }
  
  # Group tokens by the specified group field if provided
  if (!is.null(group_field)) {
    tokens_data <- tokens_group(tokens_data, groups = data_frame[[group_field]])
  }
  
  # Create a Document-Feature Matrix (DFM) from the tokens
  dfm_data <- dfm(tokens_data)
  
  # Return the processed corpus, tokens, and dfm
  return(list(corpus = corpus_data, tokens = tokens_data, dfm = dfm_data))
}


food_corpus <- process_text_data(df_cleaned, text_field = "body", 
                            group_field = NULL, remove_stopwords = TRUE, 
                            do_lemmatization = TRUE)

# Save the cleaned data with the unique identifier to a CSV file
write_csv(df_cleaned, "data/foodcrime_articles_clean.csv")

# Inspect the cleaned data
head(df_cleaned)


