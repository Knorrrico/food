library(quanteda)
library(lexicon)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# Load the CSV file
data_food <- read_csv("data/foodcrime_articles.csv")
data_reference <- read_csv("data/reference.csv")

# Select and rename the relevant columns in the reference CSV
data_reference <- data_reference |> 
  select(
    title, 
    full_content, 
    source_name, 
    published_at, 
    category
  ) |> 
  rename(
    headline = title, 
    body = full_content, 
    source = source_name, 
    date = published_at, 
    country = category  
  ) |> 
  filter(!is.na(body))

clean_and_convert_date <- function(data_frame, date_column = "date") {
  # Attempt to parse the date column using the correct format for each dataset
  data_frame[[date_column]] <- parse_date_time(
    data_frame[[date_column]], 
    orders = c("ymd HMS", "mdy", "mdy HMS", "ymd", "dmy")
  )
  
  # Identify rows where date conversion failed
  invalid_dates <- data_frame |> filter(is.na(data_frame[[date_column]]))
  
  # Display the rows with invalid dates
  if (nrow(invalid_dates) > 0) {
    print("Rows with invalid dates:")
    print(invalid_dates)
  }
  
  # Drop rows with invalid dates
  data_frame <- data_frame |> filter(!is.na(data_frame[[date_column]]))
  
  # Ensure the date column is correctly formatted as a Date object
  data_frame[[date_column]] <- as.Date(data_frame[[date_column]])
  
  return(data_frame)
}

# Apply the function to the food dataset
data_food <- clean_and_convert_date(data_food, date_column = "date")

# Apply the function to the reference dataset
data_reference <- clean_and_convert_date(data_reference, date_column = "date")

# Add a unique identifier column with a custom format
generate_unique_id <- function(data_frame, id_prefix = "ID_") {
  data_frame <- data_frame %>%
    group_by(date) %>%
    mutate(id = row_number()) %>%
    ungroup() %>%
    mutate(id = str_c(id_prefix, format(date, "%Y%m%d"), "_", str_pad(id, 3, pad = "0")))
  
  return(data_frame)
}


# Apply the function to the food dataset
data_food <- generate_unique_id(data_food, id_prefix = "FC_")

# Apply the function to the reference dataset
data_reference <- generate_unique_id(data_reference, id_prefix = "RC_")

# Extract unique news sources
news_sources <- data_food %>%
  select(source) %>%
  distinct() %>%
  arrange(source)

news_sources_vector <- unlist(news_sources)

# Save the character vector to a text file
# writeLines(news_sources_vector, con = "data/news_sources_list.txt")

#Add country information to the data

countries <- read.csv("data/updated_newspapers_countries.csv")

data_food <- data_food %>%
  left_join(countries, by = c("source" = "Newspaper"))

# Define a function to process text data

process_text_data <- function(data_frame, text_field = "body", 
                              docid_field = "id", group_label = NULL, 
                              remove_stopwords = TRUE, do_lemmatization = FALSE) {
  
  # Create a corpus from the specified text field, using the docid_field for document names
  corpus_data <- corpus(data_frame, docid_field = docid_field, text_field = text_field)
  
  # Ensure unique document names by adding a prefix based on the group label if provided
  if (!is.null(group_label)) {
    docvars(corpus_data, "group") <- group_label
  }
  
  # Tokenize the text, removing unwanted elements
  tokens_data <- tokens(corpus_data,
                        remove_punct = TRUE,
                        remove_symbols = TRUE,
                        remove_numbers = TRUE,
                        remove_url = TRUE,
                        remove_separators = TRUE) |> 
    tokens_tolower()
  
  # Remove stopwords if specified
  if (remove_stopwords) {
    tokens_data <- tokens_remove(tokens_data, pattern = stopwords("en"))
    # Add any custom stopwords here
    custom_stopwords <- c("load-date", "contentservices@htlive.com")
    tokens_data <- tokens_remove(tokens_data, pattern = custom_stopwords)
  }
  
  # Perform lemmatization if specified
  if (do_lemmatization) {
    tokens_data <- tokens_replace(tokens_data, pattern = lexicon::hash_lemmas$token, 
                                  replacement = lexicon::hash_lemmas$lemma)
  }
  
  # Create a Document-Feature Matrix (DFM) from the tokens
  dfm_data <- dfm(tokens_data)
  
  # Return the processed corpus, tokens, and DFM
  return(list(corpus = corpus_data, tokens = tokens_data, dfm = dfm_data))
}



# Process the primary food corpus
corpus_food <- process_text_data(data_food, text_field = "body", docid_field = "id",
                                 group_label = "food", 
                                 remove_stopwords = TRUE, 
                                 do_lemmatization = TRUE)

# Process the reference corpus
corpus_reference <- process_text_data(data_reference, text_field = "full_content", docid_field = "id",
                                      group_label = "reference", 
                                      remove_stopwords = TRUE, 
                                      do_lemmatization = TRUE)

# Combine the two corpora
combined_corpus <- corpus_food$corpus + corpus_reference$corpus

# Tokenize the combined corpus
tokens_combined <- tokens(combined_corpus)

# Create a DFM from the tokens
dfm_combined <- dfm(tokens_combined)

# Group the DFM by the 'group' variable
dfm_grouped <- dfm_group(dfm_combined, groups = docvars(combined_corpus, "group"))

