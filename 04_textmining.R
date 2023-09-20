library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(seededlda)
library(ggrepel)
library(GGally)

#Term Frequency
term_freq <- as.data.frame(textstat_frequency(corpus_crime_documents$dfm))
term_freq <- term_freq[order(-term_freq$frequency), ]
head(term_freq)

#Visualize
ggplot(head(term_freq, 20), aes(x = reorder(feature, -frequency), y = frequency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequent Terms",
    x = "Terms",
    y = "Frequency"
  ) +
  theme_minimal()

tokens_subset(corpus_combined_category$tokens) |> 
  dfm() |> 
  dfm_group(groups = flair) |> 
  dfm_trim(min_termfreq = 100, verbose = FALSE) |> 
  textplot_wordcloud(comparison = TRUE)

#Keyword Analysis
keyness_crime <- textstat_keyness(corpus_combined_category$dfm, target = 1L)

#Visualize
ggplot(head(keyness_crime, 20), aes(x = reorder(feature, -n_target), y = n_target)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

textplot_keyness(keyness_crime)

#Term over Time
plot_term_over_time <- function(dfm_corpus, merged_data, keyword) {
  
  # Set date variable in docvars
  docvars(dfm_corpus$dfm, "date") <- as.POSIXct(merged_data$date)
  
  # Group by week
  dfm_by_week <- dfm_group(dfm_corpus$dfm, groups = format(docvars(dfm_corpus$dfm, "date"), "%Y-%U"))
  
  # Get token frequency over time
  token_frequency_over_time <- dfm_by_week[, keyword]
  
  # Convert to data frame
  token_freq_df <- convert(token_frequency_over_time, to = "data.frame")
  
  # Plot
  ggplot(token_freq_df, aes(x = doc_id, y = as.numeric(!!sym(keyword)), group = 1)) +
    geom_line() +
    xlab("Week") +
    ylab(paste("Frequency of '", keyword, "'", sep = "")) +
    ggtitle(paste("Usage of the token '", keyword, "' over weeks", sep = "")) +
    theme_minimal() +
    theme(axis.text.x = element_blank())
}

# Example usage
plot_term_over_time(corpus_crime_full, merged_crime, "mord")

#Keyword in context
keywordContext <- kwic(corpus_crime_full$tokens, pattern = "poliz*", window = 2)
print(keywordContext)

#Related key terms
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

related_to_keyterms <- c("mord")
result <- related_keyterms(corpus_crime_documents$tokens, related_to_keyterms)
head(result, 50)

#Topic Modeling

dfm_topics <- corpus_crime_documents$dfm |>  
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")


topics_lda <- textmodel_lda(dfm_topics, k = 10)
topics_document <- topics(topics_lda)
terms_by_topic <- terms(topics_lda, n = 20)
lda_topics_df <- data.frame(id = names(topics_document), topic = topics_document)
merged_topic <- left_join(submissions_crime, lda_topics_df, by = "id")

#PieChart

# Calculate frequencies
category_freq <- lda_topics_df %>%
  drop_na(topic) %>% 
  group_by(topic) |>
  summarise(value = n()) %>%
  arrange(topic) %>%
  mutate(freq = value / sum(value) * 100)

# Compute label positions
df2 <- category_freq %>%
  mutate(csum = rev(cumsum(rev(freq))), 
         pos = freq / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), freq / 2, pos))

# Create the pie chart
ggplot(category_freq, aes(x = "", y = freq, fill = fct_inorder(topic))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(freq, 1), "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Category")) +
  theme_void()

#Sentiment Analysis of Topics

analyze_sentiment_by_topic <- function(corpus_data, topic_data, dictionary_data, id_counts_data) {
  
  # Calculate sentiment scores for the tokenized documents
  sentiment <- corpus_data$tokens %>%
    textstat_polarity(dictionary = dictionary_data)
  
  # Merge the topic data with the newly computed sentiment scores
  merged_topic_sentiment <- left_join(topic_data, sentiment, by = c("id" = "doc_id"))
  
  # Remove rows with NA in either "topic" or "sentiment" columns
  merged_clean <- merged_topic_sentiment[complete.cases(merged_topic_sentiment[, c("topic", "sentiment")]), ]
  
  # Group the clean data by 'topic' and calculate the mean sentiment score for each topic
  grouped_data <- merged_clean %>%
    group_by(topic) %>%
    summarise(average_sentiment = mean(sentiment, na.rm = TRUE))
  
  # Merge with another dataset called id_counts, based on the 'id' column
  merged_clean <- merged_clean %>%
    left_join(id_counts_data, by = "id")
  
  return(list(grouped_data, merged_clean))
}

sentiment_rauh <- analyze_sentiment_by_topic(corpus_crime_documents, merged_topic, data_dictionary_Rauh, id_counts)
df_rauh <- sentiment_rauh[[2]]

sentiment_ws <- analyze_sentiment_by_topic(corpus_crime_documents, merged_topic, data_dictionary_sentiws, id_counts)
df_ws <- sentiment_ws[[2]]

df_ws <- df_ws |> 
  select(id, sentiment)

sentiments_of_topics  <- left_join(df_rauh, df_ws, by = "id")

sentiments_of_topics <-  sentiments_of_topics |> 
  mutate(
    sentiment_diff = abs(sentiment.x - sentiment.y)
  )

sentiments_of_topics <- sentiments_of_topics |> 
  rename(sentiment_rauh = sentiment.x,
         sentiment_ws = sentiment.y)

ggpairs(sentiments_of_topics[, c("sentiment_rauh", "sentiment_ws")])


sentiments_of_topics <- sentiments_of_topics %>%
  mutate(
    mean_sentiment = (sentiment_rauh + sentiment_ws) / 2
  )

average_sentiment_of_topic <- sentiments_of_topics %>%
  group_by(topic) %>%
  summarise(
    average_mean_sentiment = mean(mean_sentiment, na.rm = TRUE)
  )

#Sentiment over Time
plot_sentiment_by_week <- function(corpus, terms_to_filter) {
  tokens_term <- tokens_select(corpus$tokens, terms_to_filter, selection = "keep", window = 5)
  dfm_term <- dfm(tokens_term)
  
  sentiment_scores <- textstat_polarity(dfm_term, data_dictionary_Rauh)
  sentiment_scores$doc_id <- as.numeric(sentiment_scores$doc_id)
  
  sentiment_term <- left_join(merged_crime, sentiment_scores, by = c("comment_id" = "doc_id"))
  sentiment_term <- sentiment_term |> 
    filter(sentiment != 0.000000)
  
  sentiment_term$date <- as.Date(sentiment_term$date)
  sentiment_term$week <- floor_date(sentiment_term$date, "week")
  
  # Aggregate by week
  aggregated_data <- sentiment_term |> 
    group_by(week) |> 
    summarise(avg_sentiment = mean(sentiment, na.rm = TRUE))
  
  # Plot the timeline
  ggplot(aggregated_data, aes(x = week, y = avg_sentiment)) +
    geom_line() +
    geom_point() +
    xlab("Week") +
    ylab("Average Sentiment") +
    ggtitle(paste("Timeline of Average Sentiment by Week for terms:", paste(terms_to_filter, collapse=", "))) +
    theme_minimal()
}

# Using the function
plot_sentiment_by_week(corpus_crime_full, c("polizei", "polizist"))

#Sentiment auf Comment Ebene Verlauf
sentiment_scores <- textstat_polarity(corpus_crime_full$dfm, data_dictionary_Rauh)
sentiment_scores$doc_id <- as.numeric(sentiment_scores$doc_id)

sentiment_comment <- left_join(merged_crime, sentiment_scores, by = c("comment_id" = "doc_id"))

negative_entries <- sentiment_comment |> 
  arrange(id, date) |> 
  group_by(id) |> 
  filter(first(sentiment) < 0)

probability_negativity <- negative_entries |> 
  arrange(id, date) |> 
  group_by(id)|>
  summarise(prob_neg = mean(sentiment[-1] < 0, na.rm = TRUE))

probability_negativity

sentiment_comment <- sentiment_comment |> 
  mutate(is_negative = ifelse(sentiment < 0, 1, 0))

# Prepare the data: Assume df has columns 'document_id', 'datetime', and 'is_negative' (1 if negative, 0 otherwise)
sentiment_comment <- sentiment_comment|> 
  arrange(id, date)
sentiment_comment <- sentiment_comment |> 
  group_by(id) |>  
  mutate(first_is_negative = first(is_negative))

# Run the logistic regression
fit <- glm(is_negative ~ first_is_negative, data = sentiment_comment, family = binomial())

# Summary of the model
summary(fit)

#Sentiment / Topic / Interaction relation
linear_model <- lm(sentiment ~ topic, data = merged_clean)
linear_model <- lm(count ~ sentiment, data = merged_clean)
linear_model <- lm(count ~ topic, data = merged_clean)
linear_model <- lm(score ~ topic, data = merged_clean)
summary(linear_model)
