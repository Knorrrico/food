library(tidyverse)
library(quanteda) 
library(quanteda.textstats) 
library(quanteda.textplots) 
library(quanteda.textmodels)
library(igraph)
library(ggraph)
library(seededlda)

## Term Frequency and Keywords ================================================

# Assuming 'dfm' is your document-feature matrix
term_freq <- as.data.frame(textstat_frequency(food_corpus$dfm))
term_freq <- term_freq[order(-term_freq$frequency), ]
head(term_freq)  # View the top terms by frequency

# Generate a comparison word cloud
textplot_wordcloud(food_corpus$dfm, min_count = 50, color = RColorBrewer::brewer.pal(8, "Dark2"))

# Perform keyness analysis using the grouped DFM
keyness_result <- textstat_keyness(dfm_grouped, target = "food")

# Extract top 20 keyness results
top_keyness <- head(keyness_result, 20)

# Plot keyness results using ggplot2
ggplot(top_keyness, aes(x = reorder(feature, -n_target), y = n_target)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Feature") +
  ylab("Target Frequency") +
  ggtitle("Top 20 Key Terms in 'Food' Articles")

# Visualize the keyness analysis with textplot_keyness
textplot_keyness(keyness_result)

plot_term_over_time <- function(dfm_corpus, keyword) {
  
  # Ensure the date variable is in the correct format in docvars
  # Assuming date is already in the correct format, this step is just to be sure
  docvars(dfm_corpus, "date") <- as.POSIXct(docvars(dfm_corpus, "date"))
  
  # Group the DFM by week (or other desired time interval)
  dfm_by_week <- dfm_group(dfm_corpus, 
                           groups = format(docvars(dfm_corpus, "date"), "%Y-%U"))
  
  # Get the token frequency over time for the specified keyword
  token_frequency_over_time <- dfm_by_week[, keyword]
  
  # Convert the DFM to a data frame for plotting
  token_freq_df <- convert(token_frequency_over_time, to = "data.frame")
  
  # Plot the frequency of the keyword over time
  ggplot(token_freq_df, aes(x = doc_id, y = as.numeric(!!sym(keyword)), group = 1)) +
    geom_line() +
    xlab("Week") +
    ylab(paste("Frequency of '", keyword, "'", sep = "")) +
    ggtitle(paste("Usage of the token '", keyword, "' over weeks", sep = "")) +
    theme_minimal() +
    theme(axis.text.x = element_blank())
}

# Example usage of the adapted function
plot_term_over_time(food_corpus$dfm, "food")

#n-gram network

# Create bigrams from tokens
tokens_bigrams <- tokens_ngrams(food_corpus$tokens, n = 2)

# Create a DFM for the bigrams
dfm_bigrams <- dfm(tokens_bigrams)

# Calculate bigram frequencies
bigram_freq <- textstat_frequency(dfm_bigrams)

# Filter for the most frequent bigrams (adjust the threshold as needed)
top_bigrams <- bigram_freq[order(-bigram_freq$frequency), ][1:50, ]

# Create an edge list for the bigrams
bigram_edges <- data.frame(
  from = sub("_.*", "", top_bigrams$feature),
  to = sub(".*_", "", top_bigrams$feature),
  weight = top_bigrams$frequency
)

# Create a graph object
bigram_graph <- graph_from_data_frame(bigram_edges, directed = FALSE)

# Plot the bigram network with adjusted layout and styling
set.seed(123)  # For reproducibility
ggraph(bigram_graph, layout = "fr") +  # Fruchterman-Reingold layout
  geom_edge_link(aes(edge_alpha = weight, edge_width = weight), color = "grey", show.legend = FALSE) +
  geom_node_point(color = "skyblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = TRUE) +
  theme_void() +
  labs(title = "Bigram Network of Most Frequent Word Pairs") +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  ggraph::theme_graph() +
  coord_fixed()  # Ensures the aspect ratio is fixed to prevent distortion

###
related_keyterms <- function(corpus_tokens, keyterms, window_size = 10) {
  
  # Keep tokens within window_size of keyterms
  tokens_inside <- tokens_keep(corpus_tokens, pattern = keyterms, window = window_size)
  
  # Remove keyterms from tokens_inside
  tokens_inside <- tokens_remove(tokens_inside, pattern = keyterms)
  
  # Remove tokens within window_size of keyterms
  tokens_outside <- tokens_remove(corpus_tokens, pattern = keyterms, window = window_size)
  
  # Create DFMs
  dfm_inside <- dfm(tokens_inside)
  dfm_outside <- dfm(tokens_outside)
  
  # Perform keyness analysis
  related_to_keyterms <- textstat_keyness(rbind(dfm_inside, dfm_outside),
                                          target = seq_len(ndoc(dfm_inside)))
  
  return(related_to_keyterms)
}


# Apply the function to the tokens in your food corpus
result <- related_keyterms(corpus_food$tokens, "food")

# Display the top 50 related terms
head(result, 50)

###

# Define a range of k values to test
k_values <- seq(2, 30, by = 2)

perplexities <- numeric(length(k_values))
divergences <- numeric(length(k_values))

# Assuming dfm_topics is already created from your corpus
dfm_topics <- corpus_food$dfm |>  
  dfm_trim(min_termfreq = 0.80, termfreq_type = "quantile",
           max_docfreq = 0.10, docfreq_type = "prop")

# Loop over each k value
for (i in seq_along(k_values)) {
  k <- k_values[i]
  
  # Set a seed for reproducibility
  set.seed(2023)
  
  # Fit the LDA model
  lda_model <- textmodel_lda(dfm_topics, k = k)
  
  # Calculate perplexity
  perplexities[i] <- perplexity(lda_model)
  
  # Calculate divergence
  divergences[i] <- divergence(lda_model)
}

# Combine the results into a data frame
results <- data.frame(k = k_values, perplexity = perplexities, divergence = divergences)

# Plot perplexity over different k values
ggplot(results, aes(x = k, y = perplexity)) +
  geom_line() +
  geom_point() +
  xlab("Number of Topics (k)") +
  ylab("Perplexity") +
  ggtitle("Perplexity vs Number of Topics")

# Plot divergence over different k values
ggplot(results, aes(x = k, y = divergence)) +
  geom_line() +
  geom_point() +
  xlab("Number of Topics (k)") +
  ylab("Divergence") +
  ggtitle("Divergence vs Number of Topics")

#############################

# Trim the DFM
dfm_topics <- corpus_food$dfm |>  
  dfm_trim(min_termfreq = 0.80, termfreq_type = "quantile",
           max_docfreq = 0.10, docfreq_type = "prop")

# Fit the LDA model
set.seed(2023)
topics_lda <- textmodel_lda(dfm_topics, k = 10)

terms_lda <- terms(topics_lda, 100)

# Extract topics for each document
topics_document <- topics(topics_lda)

# Convert the topics to a data frame with IDs
lda_topics_df <- data.frame(id = names(topics_document), topic = topics_document)

# Merge topics back into the original dataset using the consistent IDs
merged_topic <- left_join(data_food, lda_topics_df, by = "id")

# Extract the topic distribution for each document
topic_distributions <- topics(lda_model)

# Count the occurrences of each topic
topic_counts <- table(topic_distributions)

# Calculate the percentage of each topic
topic_percentages <- prop.table(topic_counts) * 100

# Convert the topic percentages to a data frame
topic_df <- data.frame(
  Topic = names(topic_percentages),
  Percentage = as.numeric(topic_percentages)
)

# Create the pie chart
ggplot(topic_df, aes(x = "", y = Percentage, fill = Topic)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("Topic Distribution in LDA Model") +
  theme(legend.position = "right")


