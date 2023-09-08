#Preprocessing
library(tidyverse)

#Einlesen Rohdaten
submissions <- read.csv2("data_raw/submissions_hour.csv", 
                         sep = ",", colClasses=c(NA), header = FALSE)

#Flair Kategorien 
categories <- submissions |> 
  group_by(submissions$V6) |> 
  summarise(as.vector(submissions$V6))

submissions_crime <- submissions |> 
  filter(V6 == "category crime")

submissions_ref <- submissions |> 
  filter(V6 == "category society")

#ID Liste zum filtern von Kommentaren
list <- submissions_crime$V7
list_ref <- submissions_ref$V7

# lapply(list, write, "list.txt", append=TRUE)
# lapply(list_ref, write, "list_ref.txt", append=TRUE)

# -> Einsetzen der Listen in Python Script

#Laden der zugehörigen Kommentare
comments_crime <- read.csv2("data_raw/comments_hour.csv", 
                            sep = ",", colClasses=c(NA), header = FALSE)
comments_ref <- read.csv2("data_raw/comments_hour_ref.csv", 
                          sep = ",", colClasses=c(NA), header = FALSE)


#Entfernen von NA, Umbenennen der Variablen

#Funktion zum umbennen der Spalten
rename_submissions <- function(df) {
  df <- df |> 
    rename(
      score = V1, 
      date = V2, 
      title = V3, 
      user = V4, 
      link = V5, 
      flair = V6, 
      id = V7, 
      body = V8
    )
  return(df)
}

rename_comments <- function(df) {
  df <- df |> 
    rename(
      score = V1, 
      date = V2, 
      user = V3, 
      link = V4, 
      id = V5, 
      body = V6
    )
  return(df)
}

#Gelöschte und Entfernte Inhalte filtern
clean_and_tolower <- function(df) {
  df <- df |> 
    filter(!(body %in% c("[deleted]", "[removed]", ""))) |> 
    mutate(body = tolower(body)
    )
  
  if ("title" %in% names(df)) {
    df <- df |> 
      mutate(title = tolower(title))
  }
  
  return(df)
}

submissions_crime <- clean_and_tolower(rename_submissions(submissions_crime))
submissions_ref <- clean_and_tolower(rename_submissions(submissions_ref))
comments_crime <- clean_and_tolower(rename_comments(comments_crime))
comments_ref <- clean_and_tolower(rename_comments(comments_ref))

#ID anpassen 
remove_t3 <- function(df) {
  df_modified <- df |>  
    mutate(id = str_replace_all(id, "t3_", ""))
  return(df_modified)
}

comments_crime <- remove_t3(comments_crime)
comments_ref <- remove_t3(comments_ref)

#Nur selftext
selftext_crime <- submissions_crime |> 
  filter(!str_detect(body, "^http"))

#Date column from character to date
convert_date_column <- function(df) {
  df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S")
  return(df)
}

submissions_crime <- convert_date_column(submissions_crime)
submissions_ref <- convert_date_column(submissions_ref)
comments_crime <- convert_date_column(comments_crime)
comments_ref <- convert_date_column(comments_ref)

#Beide Dataframes mergen
merge_and_rename <- function(submissions_df, comments_df) {
  # Select specific columns from the submissions data frame
  selected_submissions <- submissions_df |>  
    select(id, body, flair, title)
  
  # Merge the selected submissions with the comments data frame
  merged_data <- merge(selected_submissions, comments_df, by.x = "id", by.y = "id")
  
  # Rename the columns as needed
  merged_data <- merged_data |>  
    rename(content = body.x, body = body.y)
  
  return(merged_data)
}

merged_crime <- merge_and_rename(submissions_crime, comments_crime)
merged_ref <- merge_and_rename(submissions_ref, comments_ref)

#descriptive analysis / exploration
library(tidyverse)
library(lubridate)

#Häufigkeit Kommentare und Beiträge
nrow(comments_crime)
nrow(submissions_crime)

#Start- und Enddatum
range(merged_crime$date)

#Count selftext
nrow(selftext_crime)

# User in Crime Content
merged_crime |>  
  summarise(n_unique_users = n_distinct(user))


#Populärste Beiträge nach Kommentarhäufigkeit
merged_crime |> 
  group_by(title) |> 
  summarize(count = n()) |> 
  mutate(percentage=paste0(round(count/sum(count)*100,2),"%")) |> 
  arrange(desc(count)) |> 
  print()

#Populärste Beiträge nach Score
top20 <- submissions_crime |> 
  arrange(desc(score)) |> 
  select(id, score, title) |> 
  head(20) |> 
  print()

top20_id <- top20$id

top_comments <- comments_crime |> 
  filter(id %in% top20_id) |> 
  group_by(id) |> 
  arrange(desc(score)) |> 
  summarise(top_comment_body = first(body))

top_sub_com <- left_join(top20, top_comments, by = c("id" = "id"))
print(top_sub_com)

#Comment frequency by time
plot_comments_over_time <- function(merged_df) {
  
  # Convert date to Date format
  merged_df <- merged_df |>  
    mutate(date = as.Date(date, format = "%Y-%m-%d"))
  
  # Count comments per day
  comments_per_day <- merged_df |>  
    group_by(date) |>  
    summarise(count = n())
  
  # Count comments per week
  comments_per_week <- merged_df |>  
    mutate(week = floor_date(date, unit = "week")) |>  
    group_by(week) |>  
    summarise(id = n())
  
  # Generate the plot
  p <- ggplot(comments_per_week, aes(x = week, y = id)) +
    geom_col() +
    geom_smooth() +
    labs(
      title = "Comment Distribution Over Time",
      x = "Date",
      y = "Number of Comments"
    ) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  return(list(plot = p, comments_per_week = comments_per_week))
}

plot_comments_over_time(merged_crime)
comments_over_time <- plot_comments_over_time(merged_crime)

#Heatmap of submissions
# Create year and week variable
submissions_crime$year <- year(submissions_crime$date)
submissions_crime$week <- week(submissions_crime$date)

# Aggregate data for the heatmap
agg_data <- submissions_crime |> 
  group_by(year, week) |> 
  summarise(count = n())

# Generate heatmap
ggplot(agg_data, aes(x = week, y = factor(year), fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Submissions Over Time",
       x = "Week of the Year",
       y = "Year",
       fill = "Number of Submissions") +
  theme_minimal()

# Wochen mit stärksten Ausprägungen
comments_per_week <- comments_over_time$comments_per_week|> 
  arrange(desc(id))

start_date <- comments_per_week$week[1]
end_date <- start_date + weeks(1)

most_comments_week <- comments_crime |> 
  filter(date >= start_date & date <= end_date)

summary_week <- most_comments_week |> 
  group_by(id) |> 
  summarise(n_comments = n()) |> 
  arrange(desc(n_comments))

top_3 <- as.list(head(summary_week$id, 3))

submissions_crime |> 
  filter(id %in% top_3) |> 
  select(title)

#Heatmap Kommentarbeiträg
comments_by_id <- filter(merged_crime, id == '')

#Wochentag, Stunde und Minute
comments_by_id$weekday <- weekdays(as.Date(comments_by_id$date))
comments_by_id$hour <-  hour(comments_by_id$date)
comments_by_id$minute <-  minute(comments_by_id$date)

#Stunden und Minuten zusammenfassen
comments_by_id$time = sprintf("%02d:%02d", specific_comments$hour, specific_comments$minute)

agg_comments <- comments_by_id |> 
  group_by(weekday, time) |> 
  summarise(count = n())

ggplot(agg_comments, aes(x = time, y = weekday, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Submissions Over Time",
       x = "Week of the Year",
       y = "Year",
       fill = "Number of Submissions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Aggregierte Nachrichtenseiten
domain_from_body <- function(df) {
  # Filter rows where the body column starts with "http"
  df_filtered <- df |> 
    filter(str_detect(body, "^http"))
  
  # Remove "http://" or "https://" and "www." from the body
  df_filtered$body <- gsub("^https?://(www\\.)?", "", df_filtered$body)
  
  # Remove everything after the domain in the body
  df_filtered$body <- gsub("/.*$", "", df_filtered$body)
  
  return(df_filtered)
}

submissions_crime <- domain_from_body(submissions_crime)

aggregate_and_plot_top_domains <- function(df, column_name, top_n = 20) {
  # Aggregate by domains
  aggregated_data <- df %>% 
    group_by(!!sym(column_name)) %>%
    summarise(count = n()) %>% 
    arrange(desc(count))
  
  # Limit to top N domains
  top_domains <- head(aggregated_data, top_n)
  
  # Print the top N aggregated data
  print(top_domains)
  
  # Plot the data
  ggplot(top_domains, aes(x = reorder(!!sym(column_name), -count), y = count)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = paste("Frequency of Top", top_n, "Domains"),
      x = "Domains",
      y = "Frequency"
    ) +
    theme_minimal()
}
aggregate_and_plot_top_domains(submissions_crime, "body")

#Text preprocessing
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(lemmar) #lemmar wurde von github geladen, da es einen Lemmatisierungs-Datensatz in deutscher Sprache enthält remotes::install_github("trinker/lemmar")

#Text Preprocessing
# replacements <- c("ä" = "ae", "ö" = "oe", "ü" = "ue", "ß" = "ss")
# 
# replace_umlaut <- function(text) {
#   for (char in names(replacements)) {
#     text <- stringi::stri_replace_all_fixed(text, char, replacements[char], case_insensitive = TRUE)
#   }
#   return(text)
# }
# 
# merged_crime <- merged_crime  |> 
#   mutate(across(c(body, user, title), replace_umlaut))
# 
# merged_ref <- merged_ref |> 
#   mutate(across(c(body, user, title), replace_umlaut))

process_text_data <- function(data_frame, group_field = NULL, remove_stopwords = TRUE, do_lemmatization = FALSE) {
  
  corpus_data <- corpus(data_frame, text_field = "body")
  
  tokens_data <- tokens(corpus_data,
                        remove_punct = TRUE,
                        remove_symbols = TRUE,
                        remove_numbers = TRUE,
                        remove_url = TRUE,
                        remove_separators = TRUE)
  
  if (do_lemmatization) {
    tokens_data <- tokens_replace(tokens_data, pattern = hash_lemma_de$token, replacement = hash_lemma_de$lemma)
  }
  
  if (remove_stopwords) {
    tokens_data <- tokens_remove(tokens_data, pattern = stopwords("de"))
    tokens_data <- tokens_remove(tokens_data, pattern = stopwords("en"))
    custom_stopwords <- c("gt", "r", "#x200b", "jp", "i", "dass", "amp")
    tokens_data <- tokens_remove(tokens_data, pattern = custom_stopwords)
  }
  
  if (!is.null(group_field)) {
    tokens_data <- tokens_group(tokens_data, groups = data_frame[[group_field]])
  }
  
  dfm_data <- dfm(tokens_data)
  
  return(list(corpus = corpus_data, tokens = tokens_data, dfm = dfm_data))
}


corpus_crime_documents <- process_text_data(merged_crime, group_field = "id")
corpus_crime_full <- process_text_data(merged_crime, remove_stopwords = FALSE)
corpus_combined <- rbind(merged_crime, merged_ref)
corpus_combined_category <- process_text_data(corpus_combined, group_field = "flair")

#Text Mining
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
