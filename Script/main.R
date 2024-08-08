#library(tidyverse)

#keywords <- read.csv2("data/keywords_food_fraud_en.csv")

#list <- unlist(keywords$Keyword)

###

library(striprtf)
library(stringr)

#Split into newsarticles
extract_article <- function(file_name){
  txt <- readLines(file_name)
  
  txt <- paste(txt, collapse = "\n")
  
  txt <- str_split_1(txt, "End of Document")
  
  return(txt)
  
}

#Extract article with index
extract_row <- function(row_number){
  
  txtsplit <- str_split_1(txt[row_number], "\\n")
  
  txt <- txtsplit[!txtsplit == ""]
  
  return(txt)
  
}

#Extract relevant parts from article
extract_elements <- function(txt){
  
  body <- txt[(which(txt == "Body")+1):(length(txt))]
  headline <- txt[1]
  source <- txt[2]
  date <- txt[3]
  
  
  return(list(headline = headline, body = body, source = source, date = date))
  
}
###
#Convert rtf into txt

library(striprtf)
# Function to convert a single RTF file to plain text
convert_rtf_to_txt <- function(input_file, output_file) {
  rtf_text <- read_rtf(input_file)
  writeLines(rtf_text, output_file)
}

# Function to convert all RTF files in a directory to plain text
convert_all_rtf_in_directory <- function(input_directory, output_directory) {
  # Get a list of all RTF files in the input directory
  rtf_files <- list.files(input_directory, pattern = "\\.RTF$", full.names = TRUE)
  
  # Ensure the output directory exists
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  
  # Convert each RTF file to a plain text file
  for (rtf_file in rtf_files) {
    output_file <- file.path(output_directory, paste0(tools::file_path_sans_ext(basename(rtf_file)), ".txt"))
    convert_rtf_to_txt(rtf_file, output_file)
  }
}

# Example usage
input_directory <- "data/rtf"
output_directory <- "data/txt"
convert_all_rtf_in_directory(input_directory, output_directory)

###

# Function to parse the file and extract elements from each article
parse_file <- function(file_name) {
  # Read the file content
  txt <- readLines(file_name)
  txt <- paste(txt, collapse = "\n")
  
  # Split the content into articles
  articles <- str_split_1(txt, "End of Document")
  
  # Remove the last article if it is NA or empty
  if (articles[length(articles)] == "") {
    articles <- articles[-length(articles)]
  }
  
  # Function to extract elements from a single article
  extract_elements <- function(article_txt) {
    article_lines <- str_split_1(article_txt, "\\n")
    article_clean <- article_lines[!article_lines == ""]
    
    # Trim whitespace from each line
    article_clean <- trimws(article_clean)
    
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
    
    # Extract elements
    body <- article_clean[(body_index + 1):(length(article_clean))]
    headline <- article_clean[1]
    source <- article_clean[2]
    date <- article_clean[3]
    
    return(list(headline = headline, body = body, source = source, date = date, length = length_value))
  }
  
  # Extract elements from each article
  parsed_articles <- lapply(articles, extract_elements)
  
  return(parsed_articles)
}

# Function to parse all files in a directory
parse_all_files_in_directory <- function(directory) {
  # List all .txt files in the directory
  txt_files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)
  
  # Parse each file and combine the results
  all_parsed_articles <- lapply(txt_files, parse_file)
  
  # Flatten the list of lists
  all_parsed_articles <- do.call(c, all_parsed_articles)
  
  return(all_parsed_articles)
}

# Example usage
directory <- "data/txt"
all_parsed_articles <- parse_all_files_in_directory(directory)

# Print the extracted elements of all articles
print(all_parsed_articles)
