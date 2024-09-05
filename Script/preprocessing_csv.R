# load libraries
library(quanteda)
library(lexicon)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# load news articles and reference data =======================================
data_food <- read_csv("data/foodcrime_articles.csv")
data_reference <- read_csv("data/reference.csv")

# select and rename columns in reference data =================================
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

# format date column to uniform format ========================================
clean_and_convert_date <- function(data_frame, date_column = "date") {
  
  data_frame[[date_column]] <- parse_date_time(
    data_frame[[date_column]], 
    orders = c("ymd HMS", "mdy", "mdy HMS", "ymd", "dmy")
  )
  
  invalid_dates <- data_frame |> filter(is.na(data_frame[[date_column]]))
  
  # show rows with invalid dates
  if (nrow(invalid_dates) > 0) {
    print("Rows with invalid dates:")
    print(invalid_dates)
  }
  
  # drop rows with invalid dates
  data_frame <- data_frame |> filter(!is.na(data_frame[[date_column]]))
  
  # format as date class
  data_frame[[date_column]] <- as.Date(data_frame[[date_column]])
  
  return(data_frame)
}

# clean date column in both datasets ==========================================
data_food <- clean_and_convert_date(data_food, date_column = "date")
data_reference <- clean_and_convert_date(data_reference, date_column = "date")

# add identifier to each row ==================================================
generate_unique_id <- function(data_frame, id_prefix = "ID_") {
  data_frame <- data_frame |> 
    group_by(date) |> 
    mutate(id = row_number()) |> 
    ungroup() |> 
    mutate(id = str_c(id_prefix, format(date, "%Y%m%d"), "_", str_pad(id, 3, pad = "0")))
  
  return(data_frame)
}


data_food <- generate_unique_id(data_food, id_prefix = "FC_")
data_reference <- generate_unique_id(data_reference, id_prefix = "RC_")

# create a list of unique news sources ========================================
news_sources <- data_food |> 
  select(source) |> 
  distinct() |> 
  arrange(source)

news_sources_vector <- unlist(news_sources)
## writeLines(news_sources_vector, con = "data/news_sources_list.txt")

# include country information to the data ======================================

countries <- read.csv("data/newspapers_countries.csv")

data_food <- data_food |> 
  left_join(countries, by = c("source" = "Newspaper"))

# process text data for text mining ============================================
process_text_data <- function(data_frame, text_field = "body", 
                              docid_field = "id", group_label = NULL, 
                              remove_stopwords = TRUE, do_lemmatization = FALSE) {
  
  # create corpus from specific column, add docid
  corpus_data <- corpus(data_frame, docid_field = docid_field, text_field = text_field)
  
  # option to add group label
  if (!is.null(group_label)) {
    docvars(corpus_data, "group") <- group_label
  }
  
  # tokenize the corpus, clean up the text
  tokens_data <- tokens(corpus_data,
                        remove_punct = TRUE,
                        remove_symbols = TRUE,
                        remove_numbers = TRUE,
                        remove_url = TRUE,
                        remove_separators = TRUE) |> 
    tokens_tolower()
  
  # remove stopwords
  if (remove_stopwords) {
    tokens_data <- tokens_remove(tokens_data, pattern = stopwords("en"))
    # add custom stopwords
    custom_stopwords <- c("load-date", "contentservices@htlive.com","'","ht")
    tokens_data <- tokens_remove(tokens_data, pattern = custom_stopwords)
  }
  
  # option to lemmatize the tokens
  if (do_lemmatization) {
    tokens_data <- tokens_replace(tokens_data, pattern = lexicon::hash_lemmas$token, 
                                  replacement = lexicon::hash_lemmas$lemma)
  }
  
  dfm_data <- dfm(tokens_data)
  
  # get corpus, dfm and tokens
  return(list(corpus = corpus_data, tokens = tokens_data, dfm = dfm_data))
}

corpus_food <- process_text_data(data_food, text_field = "body", docid_field = "id",
                                 group_label = "food", 
                                 remove_stopwords = TRUE, 
                                 do_lemmatization = TRUE)

corpus_reference <- process_text_data(data_reference, text_field = "body", docid_field = "id",
                                      group_label = "reference", 
                                      remove_stopwords = TRUE, 
                                      do_lemmatization = TRUE)

# combine corpora for keyness analysis =========================================
tokens_combined <- corpus_food$tokens + corpus_reference$tokens
tokens_combined <- tokens(tokens_combined)
dfm_combined <- dfm(tokens_combined)
dfm_grouped <- dfm_group(dfm_combined, groups = docvars(combined_corpus, "group"))
