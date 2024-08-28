# Load required libraries
library(striprtf)
library(stringr)

# Function to convert a single RTF file to plain text
convert_rtf_to_txt <- function(input_file, output_file) {
  rtf_text <- read_rtf(input_file)
  writeLines(rtf_text, output_file)
}

# Function to convert all RTF files in a directory to plain text
convert_all_rtf_in_directory <- function(input_directory, output_directory) {
  rtf_files <- list.files(input_directory, pattern = "\\.RTF$", full.names = TRUE)
  
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  
  for (rtf_file in rtf_files) {
    output_file <- file.path(output_directory, paste0(tools::file_path_sans_ext(basename(rtf_file)), ".txt"))
    convert_rtf_to_txt(rtf_file, output_file)
  }
}

# Example usage for converting RTF files to TXT
input_directory <- "data/rtf"
output_directory <- "data/txt"
convert_all_rtf_in_directory(input_directory, output_directory)

# Function to extract elements from a single article
extract_elements <- function(article_txt) {
  article_lines <- str_split_1(article_txt, "\\n")
  article_clean <- article_lines[!article_lines == ""]
  
  # Trim whitespace from each line
  article_clean <- trimws(article_clean)
  
  # Identify the headline
  headline <- article_clean[1]
  
  # If there is a blank line after the headline, adjust the indices
  if (article_clean[2] == "") {
    source <- article_clean[3]
    date <- article_clean[4]
    body_start_index <- 5
  } else {
    source <- article_clean[2]
    date <- article_clean[3]
    body_start_index <- 4
  }
  
  # Find the index of the "Body" section
  body_index <- which(tolower(article_clean) == "body")
  if (length(body_index) == 0) {
    message("Body start pattern not found in the article")
    body_index <- length(article_clean) # Default to the end if "Body" is not found
  }
  
  # Find the index of the line that starts with "Length:"
  length_index <- grep("^Length:", article_clean)
  length_value <- if (length(length_index) > 0) {
    article_clean[length_index]
  } else {
    message("Length pattern not found in the article")
    NA
  }
  
  # Extract the body of the article
  body <- article_clean[(body_index + 1):(length(article_clean))]
  
  return(list(headline = headline, body = body, source = source, date = date, length = length_value))
}

# Function to parse a file and extract elements from each article
parse_file <- function(file_name) {
  txt <- readLines(file_name)
  txt <- paste(txt, collapse = "\n")
  
  articles <- str_split_1(txt, "End of Document")
  
  if (articles[length(articles)] == "") {
    articles <- articles[-length(articles)]
  }
  
  parsed_articles <- lapply(articles, extract_elements)
  
  return(parsed_articles)
}

# Function to parse all files in a directory
parse_all_files_in_directory <- function(directory) {
  txt_files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)
  
  all_parsed_articles <- lapply(txt_files, parse_file)
  
  all_parsed_articles <- do.call(c, all_parsed_articles)
  
  return(all_parsed_articles)
}

# Example usage for parsing TXT files
directory <- "data/txt"
all_parsed_articles <- parse_all_files_in_directory(directory)

# Convert the parsed articles to a data frame
articles_df <- do.call(rbind, lapply(all_parsed_articles, function(x) {
  data.frame(
    headline = x$headline,
    body = paste(x$body, collapse = " "),  # Concatenating all elements in the body
    source = x$source,
    date = x$date,
    length = x$length,
    stringsAsFactors = FALSE
  )
}))

# Write the data frame to a CSV file
write.csv(articles_df, file = "data/foodcrime_articles.csv", row.names = FALSE)
