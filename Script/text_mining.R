library(tidyverse)
library(quanteda) 
library(quanteda.textstats) 
library(quanteda.textplots) 
library(quanteda.textmodels)
library(igraph)
library(ggraph)
library(seededlda)
library(LDAvis)
library(reshape2)

# term frequency and keyness analysis ==========================================
term_freq <- as.data.frame(textstat_frequency(corpus_food$dfm))
term_freq <- term_freq[order(-term_freq$frequency), ]
head(term_freq)  # View the top terms by frequency

## wordcloud
textplot_wordcloud(corpus_food$dfm, 
                   min_count = 40, 
                   max_words = 100, 
                   color = brewer.pal(11, "Dark2"),
                   min_size = 1,  
                   max_size = 2.5)


## keyness analysis ============================================================
keyness_result <- textstat_keyness(dfm_grouped, target = "food")

## top n key terms
top_keyness <- head(keyness_result, 20)

ggplot(top_keyness, aes(x = reorder(feature, -n_target), y = n_target)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Feature") +
  ylab("Target Frequency") +
  ggtitle("Top 20 Key Terms in 'Food' Articles")

## visualize keyness
textplot_keyness(keyness_result)

# term over time plot ==========================================================
plot_term_over_time <- function(dfm_corpus, keyword) {
  
  # Ensure the date column is in proper datetime format
  docvars(dfm_corpus, "date") <- as.POSIXct(docvars(dfm_corpus, "date"))
  
  # group by month
  dfm_by_month <- dfm_group(dfm_corpus, 
                            groups = format(docvars(dfm_corpus, "date"), "%Y-%m"))
  
  # get frequency of the keyword over time
  token_frequency_over_time <- dfm_by_month[, keyword]
  
  # convert to data frame
  token_freq_df <- convert(token_frequency_over_time, to = "data.frame")
  
  # convert doc_id back to date format
  token_freq_df$doc_id <- as.Date(paste0(token_freq_df$doc_id, "-01"), format = "%Y-%m-%d")
  
  ggplot(token_freq_df, aes(x = doc_id, y = as.numeric(!!sym(keyword)), group = 1)) +
    geom_line(color = "black", size = 0.7) +  
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", se = FALSE, size = 0.8) +  # Smoother trend with GAM
    xlab("Date") +
    ylab(paste("Frequency of '", keyword, "'", sep = "")) +
    ggtitle(paste("Usage of the token '", keyword, "' over time", sep = "")) +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +  
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  
      panel.grid.major = element_line(color = "gray80"),  
      panel.grid.minor = element_blank(),  
      panel.background = element_rect(fill = "white"),  
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")  
    )
}

plot_term_over_time(corpus_food$dfm, "pettovello")

#n-gram network ===============================================================
# get bigram frequencies
tokens_bigrams <- tokens_ngrams(corpus_food$tokens, n = 2)
dfm_bigrams <- dfm(tokens_bigrams)
bigram_freq <- textstat_frequency(dfm_bigrams)

# filter top 100 bigrams
top_bigrams <- bigram_freq[order(-bigram_freq$frequency), ][1:100, ]

# create edges for bigram network
bigram_edges <- data.frame(
  from = sub("_.*", "", top_bigrams$feature),
  to = sub(".*_", "", top_bigrams$feature),
  weight = top_bigrams$frequency
)

# create graph from edges
bigram_graph <- graph_from_data_frame(bigram_edges, directed = FALSE)

# plot bigram network
set.seed(123)
ggraph(bigram_graph, layout = "kk") +  
  geom_edge_link(aes(edge_alpha = weight, edge_width = weight), color = "grey", show.legend = FALSE) +
  geom_node_point(color = "skyblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +  
  theme_void() +
  labs(title = "Bigram Network of Top 100 Most Frequent Word Pairs") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  ggraph::theme_graph() +
  coord_fixed()  

# find words related to keyterms ===============================================
related_keyterms <- function(corpus_tokens, keyterms, window_size = 10) {
  
  # keep tokens within window_size of keyterms
  tokens_inside <- tokens_keep(corpus_tokens, pattern = keyterms, window = window_size)
  
  # remove keyterms from tokens_inside
  tokens_inside <- tokens_remove(tokens_inside, pattern = keyterms)
  
  # remove tokens within window_size of keyterms
  tokens_outside <- tokens_remove(corpus_tokens, pattern = keyterms, window = window_size)
  
  # Create DFMs
  dfm_inside <- dfm(tokens_inside)
  dfm_outside <- dfm(tokens_outside)
  
  # Perform keyness analysis
  related_to_keyterms <- textstat_keyness(rbind(dfm_inside, dfm_outside),
                                          target = seq_len(ndoc(dfm_inside)))
  
  return(related_to_keyterms)
}

result <- related_keyterms(corpus_food$tokens, "food")

head(result, 50)

# topic modeling ==============================================================
# test different number of topics
# define a range of k values 
k_values <- seq(2, 30, by = 2)

perplexities <- numeric(length(k_values))
divergences <- numeric(length(k_values))
coherences <- numeric(length(k_values)) 

dfm_topics <- corpus_food$dfm |>  
  dfm_trim(min_termfreq = 0.80, termfreq_type = "quantile",
           max_docfreq = 0.10, docfreq_type = "prop")

calculate_coherence <- function(model, dfm, top_n = 10) {
  top_terms <- terms(model, top_n)
  coherence_scores <- numeric(ncol(top_terms))
  
  for (topic_idx in seq_len(ncol(top_terms))) {
    terms_in_topic <- top_terms[, topic_idx]
    term_combinations <- combn(terms_in_topic, 2)
    
    score <- 0
    for (j in seq_len(ncol(term_combinations))) {
      term1 <- term_combinations[1, j]
      term2 <- term_combinations[2, j]
      
      term1_count <- sum(dfm[, term1])
      term2_count <- sum(dfm[, term2])
      co_occurrence <- sum(dfm[, term1] & dfm[, term2])
      
      if (co_occurrence > 0) {
        score <- score + log((co_occurrence + 1) / term1_count)
      }
    }
    coherence_scores[topic_idx] <- score
  }
  return(mean(coherence_scores))  
}

# loop over different k values 
for (i in seq_along(k_values)) {
  k <- k_values[i]
  
  set.seed(2023)
  
  lda_model <- textmodel_lda(dfm_topics, k = k)
  
  # calculate perplexity, divergence, and coherence
  perplexities[i] <- perplexity(lda_model)
  divergences[i] <- divergence(lda_model)
  coherences[i] <- calculate_coherence(lda_model, dfm_topics, top_n = 10)
}

results <- data.frame(k = k_values, perplexity = perplexities, divergence = divergences, coherence = coherences)

# plot perplexity over different k values
ggplot(results, aes(x = k, y = perplexity)) +
  geom_line() +
  geom_point() +
  xlab("Number of Topics (k)") +
  ylab("Perplexity") +
  ggtitle("Perplexity vs Number of Topics")

# plot divergence over different k values
ggplot(results, aes(x = k, y = divergence)) +
  geom_line() +
  geom_point() +
  xlab("Number of Topics (k)") +
  ylab("Divergence") +
  ggtitle("Divergence vs Number of Topics")

# plot coherence over different k values
ggplot(results, aes(x = k, y = coherence)) +
  geom_line() +
  geom_point() +
  xlab("Number of Topics (k)") +
  ylab("Coherence") +
  ggtitle("Coherence vs Number of Topics")

### visual comparison of distribution of topics in data_food

# Fit model with the optimal number of topics (k = 10)
set.seed(2023)
topics_lda <- textmodel_lda(dfm_topics, k = 10)

# Extract top terms
terms_lda <- terms(topics_lda, 100)

# Assign topics to documents
topics_document <- topics(topics_lda)

# Convert topics to data frame with document IDs
lda_topics_df <- data.frame(id = names(topics_document), topic = topics_document)

# Merge topics with the original dataset (assumes data_food has an 'id' column)
merged_topic <- left_join(data_food, lda_topics_df, by = "id")

# Distribution of topics across documents
topic_distributions <- topics(topics_lda)
topic_counts <- table(topic_distributions)
topic_percentages <- prop.table(topic_counts) * 100
topic_df <- data.frame(
  Topic = names(topic_percentages),
  Percentage = as.numeric(topic_percentages)
)

# pie chart of topic distribution
ggplot(topic_df, aes(x = "", y = Percentage, fill = Topic)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("Topic Distribution in LDA Model") +
  theme(legend.position = "right")

# interactive visualization =================================================

vis_data <- createJSON(phi = topics_lda$phi, theta = topics_lda$theta, 
                       doc.length = rowSums(dfm_topics), vocab = colnames(dfm_topics))

serVis(vis_data)

