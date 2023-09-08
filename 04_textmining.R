library(tidyverse)
library(quanteda)

#Term Frequency
library(quanteda.textstats)
library(quanteda.textplots)
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

#Keyword Analysis
keyness_crime <- textstat_keyness(corpus_combined_category$dfm, target = 1L)

#Visualize
ggplot(head(keyness_crime, 20), aes(x = reorder(feature, -n_target), y = n_target)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

tokens_subset(corpus_combined_category$tokens) |> 
  dfm() |> 
  dfm_group(groups = flair) |> 
  dfm_trim(min_termfreq = 100, verbose = FALSE) |> 
  textplot_wordcloud(comparison = TRUE)

textplot_keyness(keyness_crime)

#Term over Time
docvars(corpus_crime_full$dfm, "date") <- as.POSIXct(merged_crime$date)
dfm_by_week <- dfm_group(corpus_crime_full$dfm, groups = format(docvars(corpus_crime_full$dfm, "date"), "%Y-%U"))
token_frequency_over_time <- dfm_by_week[, "polizei"]
token_freq_df <- convert(token_frequency_over_time, to = "data.frame")

ggplot(token_freq_df, aes(x = doc_id, y = as.numeric(polizei), group = 1)) +
  geom_line() +
  xlab("Week") +
  ylab("Frequency of 'polizei'") +
  ggtitle("Usage of the token 'polizei' over weeks") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) 

#Keyword in context
keywordContext <- kwic(tokens_crime, pattern = "poliz*", window = 2)
print(keywordContext)

#Related key terms
pol <- c("polizei", "polizist")
tokens_inside <- tokens_keep(corpus_crime_documents$tokens, pattern = pol, window = 10)
tokens_inside <- tokens_remove(corpus_crime_documents$tokens, pattern = pol)
tokens_outside <- tokens_remove(corpus_crime_documents$tokens, pattern = pol, window = 10)
dfm_inside <- dfm(tokens_inside)
dfm_outside <- dfm(tokens_outside)

related_to_keyterms <- textstat_keyness(rbind(dfmi, dfmo),
                 target = seq_len(ndoc(dfmi)))
head(key, 50)

#Multi word expressions
corpus_crime_lemma <- process_text_data(merged_crime, remove_stopwords = TRUE, do_lemmatization = TRUE)
mwe <- textstat_collocations(corpus_crime_lemma, size = 2, min_count = 50)
mwe_crime <- tokens_compound(corpus_crime_lemma, pattern = mwe)
print(mwe_crime)

#FC-Matrix
dfm_crime_trimmed <- dfm_trim(corpus_crime_documents$dfm, min_termfreq = 100)
fcm_crime <- fcm(dfm_crime_trimmed)
topfeatures(fcm_crime)

feat <- names(topfeatures(fcm_crime, 50))
fcm_crime_selected <-  fcm_select(fcm_crime, pattern = feat, selection = "keep")

textplot_network(fcm_crime_selected, min_freq = 0.8, vertex_size = 3)

#tf-idf
tfidf_crime <- dfm_tfidf(corpus_crime_documents$dfm)
print(tfidf_crime)


#Topic Modeling
library(topicmodels)
library(tidytext)
library(reshape2)
library(seededlda)

dfm_topics <- dfm(corpus_crime_documents$tokens) %>% 
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")

# topics <- convert(dfm_topics, to = "topicmodels" )
# lda_fit <- LDA(topics, k = 5)
# crime_topics <- tidy(lda_fit, matrix = "beta")
# 
# crime_topics
# 
# crime_topic_top_terms <- crime_topics %>%
#   group_by(topic) %>%
#   slice_max(beta, n = 10) %>% 
#   ungroup() %>%
#   arrange(topic, -beta)
# 
# crime_topic_top_terms %>%
#   mutate(term = reorder_within(term, beta, topic)) %>%
#   ggplot(aes(beta, term, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   scale_y_reordered()

topics_lda <- textmodel_lda(dfm_topics, k = 10)
topics_document <- topics(topics_lda)
lda_topics_df <- data.frame(id = names(topics_document), topic = topics_document)
merged_data <- left_join(submissions_crime, lda_topics_df, by = "id")

#Sentiment Analysis of Topics
library(quanteda.sentiment)

corpus_crime_documents$tokens |>
  textstat_polarity(dictionary = data_dictionary_Rauh)

#Sentiment over Time

#Sentiment / Topic relation

#Sentiment auf Comment Ebene Verlauf
