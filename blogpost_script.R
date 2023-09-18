#Preprocessing
library(tidyverse)
library(lubridate)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(lemmar)
library(seededlda)

#Einlesen Rohdaten
submissions <- read.csv2("data_raw/submissions_2022.csv", 
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
comments_crime <- read.csv2("data_raw/comments_2022.csv", 
                            sep = ",", colClasses=c(NA), header = FALSE)
comments_ref <- read.csv2("data_raw/comments_ref_2022.csv", 
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

add_unique_id <- function(df, id_column_name = "unique_id") {
  df <- df %>%
    mutate(!!id_column_name := row_number())
  return(df)
}

merged_crime <- add_unique_id(merged_crime, "comment_id")
merged_ref <- add_unique_id(merged_ref, "comment_id")

#descriptive analysis / exploration
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

#Anzahl an Kommentaren zu Beitrag hinzufügen
id_counts <- comments_crime |> 
  group_by(id) |> 
  summarise(count = n())

submissions_crime <-submissions_crime |> 
  left_join(id_counts, by = "id") |>
  replace_na(list(count= 0))

aggregated_frequency <- submissions_crime |> 
  group_by(count) |> 
  summarise(n = n())

ggplot(aggregated_frequency, aes(x = count, y = n)) +
  geom_bar(stat = "identity") +
  xlim(0, 400) +
  ylim(0, 400) +
  xlab("Anzahl an Kommentaren") +
  ylab("Anzahl an Dokumenten") +
  ggtitle("Verteilung der Kommentarhäufigkeit")

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
submissions_crime <- read.csv2("data/submissions_crime.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)

dfm_topics <- dfm(corpus_crime_documents$tokens) |>  
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")


topics_lda <- textmodel_lda(dfm_topics, k = 10)
topics_document <- topics(topics_lda)
terms_by_topic <- terms(topics_lda)
lda_topics_df <- data.frame(id = names(topics_document), topic = topics_document)
merged_topic <- left_join(submissions_crime, lda_topics_df, by = "id")

#Sentiment Analysis of Topics
library(quanteda.sentiment)

sentiment <- corpus_crime_documents$tokens |>
  textstat_polarity(dictionary = data_dictionary_Rauh)

merged_topic_sentiment <- left_join(merged_topic, sentiment, by = c("id" = "doc_id"))

merged_clean <- merged_topic_sentiment[complete.cases(merged_topic_sentiment[, c("topic", "sentiment")]), ]

merged_clean |> 
  group_by(topic) |> 
  summarise(average_sentiment = mean(sentiment, na.rm = TRUE))

merged_clean <-  merged_clean |> 
  left_join(id_counts, by = "id")

#Sentiment / Topic / Interaction relation
linear_model <- lm(sentiment ~ topic, data = merged_clean)
linear_model <- lm(count ~ sentiment, data = merged_clean)
linear_model <- lm(count ~ topic, data = merged_clean)
linear_model <- lm(score ~ topic, data = merged_clean)
summary(linear_model)

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
