library(tidyverse)
library(quanteda)

#Term Frequency
library(quanteda.textstats)
library(quanteda.textplots)
term_freq <- as.data.frame(textstat_frequency(corpus_crime_processed$dfm))

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

#Keyword Analysis
keyness_crime <- textstat_keyness(corpus_combined_processed$dfm, target = 1L)

#Visualize
ggplot(head(keyness_crime, 20), aes(x = reorder(feature, -n_target), y = n_target)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

tokens_subset(corpus_combined_processed$tokens) |> 
  dfm() |> 
  dfm_group(groups = flair) |> 
  dfm_trim(min_termfreq = 100, verbose = FALSE) |> 
  textplot_wordcloud(comparison = TRUE)

textplot_keyness(keyness_crime)

#Term over Time
corpus_crime <- corpus(merged_crime, text_field = "body")
tokens_crime <- tokens(corpus_crime,
                       remove_punct = TRUE,
                       remove_symbols = TRUE,
                       remove_numbers = TRUE,
                       remove_url = TRUE,
                       remove_separators = TRUE)

custom_stopwords <- c("gt", "r", "#x200b", "jp", "i", "dass")
tokens_crime <- tokens_remove(tokens_crime, pattern = custom_stopwords)
dfm_crime <- dfm(tokens_crime)

docvars(dfm_crime, "date") <- as.POSIXct(merged_crime$date)

dfm_by_week <- dfm_group(dfm_crime, groups = format(docvars(dfm_crime, "date"), "%Y-%U"))

token_frequency_over_time <- dfm_by_week[, "polizei"]

token_freq_df <- convert(token_frequency_over_time, to = "data.frame")

ggplot(token_freq_df, aes(x = doc_id, y = as.numeric(polizei), group = 1)) +
  geom_line() +
  xlab("Week") +
  ylab("Frequency of 'polizei'") +
  ggtitle("Usage of the token 'polizei' over weeks") +
  theme_minimal()

#kwic
keywordContext <- kwic(tokens_crime, pattern = "poliz*", window = 2, n = 10)
print(keywordContext)

#Multi word expressions
mwe <- textstat_collocations(corpus_crime_processed$tokens, size = 2, min_count = 100)
mwe_crime <- tokens_compund(corpus_crime_processed$tokens, pattern = mwe)
print(mwe_crime)

#tf-idf

#Topic Modeling
library(quanteda.textmodels)

#Sentiment Analysis of Topics
library(quanteda.sentiment)

#Sentiment over Time

#Sentiment in Submissions / Comments
