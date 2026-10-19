library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(seededlda)
library(ggrepel)
library(GGally)
library(quanteda.sentiment) # data_dictionary_Rauh, data_dictionary_sentiws, textstat_polarity()
library(sandwich)
library(lmtest)

# Corpora from 03 and the data this script needs, so it runs on its own
source("03_corpus_preprocessing.R")

submissions_crime <- read.csv2("data/submissions_crime.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)
comments_crime <- read.csv2("data/comments_crime.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)

id_counts <- comments_crime |> 
  group_by(id) |> 
  summarise(count = n())

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

corpus_combined_category$tokens |> 
  dfm() |> 
  dfm_group(groups = flair) |> 
  dfm_trim(min_termfreq = 100, verbose = FALSE) |> 
  textplot_wordcloud(comparison = TRUE)

#Keyword Analysis
keyness_crime <- textstat_keyness(corpus_combined_category$dfm, target = "category crime")

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

#Step 1: unsupervised LDA with 20 topics to explore the themes in the corpus
set.seed(2023)
topics_lda <- textmodel_lda(dfm_topics, k = 20)
terms_by_topic <- terms(topics_lda, n = 100)
terms_by_topic

#Step 2: seeded LDA. Ten themes from step 1 were summarised under a label
#and their characteristic words serve as seed words
dict <- dictionary(list(
  sv = c("sexuell", "vergewaltigung", "sexuelle","missbrauch","vergewaltigen",
         "belästigung","abtreibung","nötigung","selbstbestimmung","pädophile",
         "belästigen","kinderpornografie"),

  pv = c("einzelfall", "polizeigewalt", "demo","kamera","polizistin", "einsatz",
         "bodycams", "bundespolizei","demonstrant", "versammlung","acab","fehlverhalten",
         "waffe","schießen","erschießen","notwehr",
         "verletzen","schusswaffe","pfefferspray",
         "körperverletzung","polizei", "polizist"),

  da = c("droge","alkohol","cannabis","legalisierung","konsum","dealer",
         "legalisieren", "rauchen", "konsumieren", "kiffen", "heroin", "mdma",
         "substanz", "thc", "kontrolle" ),

  disc = c("rassismus","jude","rassistisch","antisemitismus","israel","flüchtling",
           "migrationshintergrund","ausländer","diskriminierung","trans","schwul",
           "minderheit","rassisten","homophobie"),

  jur = c("ermittlung","anwalt","anklagen","beweise","ermittler","verdächtigen",
          "prozess","justiz","urteilen","vorwurf","staatsanwalt","verurteilung",
          "unschuldig","beweis","u-haft","rechtsstaat","unschuldsvermutung",
          "angeklagte","stpo","jurist","hausdurchsuchung","gericht","indiz",
          "stgb","strafrecht", "jugendstrafrecht", "urteile"),

  ds = c("klauen","fahrrad","wohnung","diebstahl","eigentum","dieb","versicherung",
         "stehlen","sprengen","bargeld"),

  car = c("radfahrer","fahrzeug","fahrer","unfall","lkw","führerschein","fahrrad",
          "autofahrer","geschwindigkeit","überholen","autobahn","fußgänger",
          "straßenverkehr","rasen","stvo","raser","überfahren"),

  pr = c("gefängnis", "urteilen", "knast", "bewährung", "todesstrafe",
         "haft","vorsatz", "lebenslang", "strafmaß", "justiz", "taten",
         "haftstrafe", "freiheitsstrafe", "strafrecht", "gerechtigkeit",
         "lebenslänglich", "urteile"),

  scam = c("nummer","handy","anruf","werbung","betrüger","scam","reinfallen"),

  pe = c("afd","rechtsextrem","linke","partei","politische","verfassungsschutz",
         "ideologie", "neonazi", "rechtsextreme", "antifa", "rechtsextremismus",
         "terrorist", "faschist", "terrorismus", "mitte","rechtsradikal", "nsu",
         "extremist", "volk","propaganda")
))

set.seed(2023)
tmod_slda <- textmodel_seededlda(dfm_topics, dictionary = dict)

topics_document <- topics(tmod_slda)
lda_topics_df <- data.frame(id = names(topics_document), topic = as.character(topics_document))
merged_topic <- left_join(submissions_crime, lda_topics_df, by = "id")

#Labels of the seeded topics
replacement_names <- c(
  sv = "Sexualisierte Gewalt",
  pv = "Polizeigewalt",
  da = "BtMG",
  disc = "Diskriminierung",
  jur = "Strafverfahren",
  ds = "Diebstahl/Raub",
  car = "Straßenverkehr",
  pr = "Strafvollzug",
  scam = "Telefon-/Internetbetrug",
  pe = "Extremismus"
)

#PieChart

# Calculate frequencies
category_freq <- lda_topics_df |>
  drop_na(topic) |>
  group_by(topic) |>
  summarise(value = n()) |>
  arrange(topic) |>
  mutate(freq = value / sum(value) * 100,
         topic = recode(topic, !!!replacement_names))

# Compute label positions
df2 <- category_freq |>
  mutate(csum = rev(cumsum(rev(freq))), 
         pos = freq / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), freq / 2, pos))

# Create the pie chart (Set3 has 12 colours, Pastel1 only 9)
ggplot(category_freq, aes(x = "", y = freq, fill = fct_inorder(topic))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(freq, 1), "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Topic")) +
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

# Sentiment is scored on tokens that still contain negations (see 03)
sentiment_rauh <- analyze_sentiment_by_topic(corpus_crime_sentiment, merged_topic, data_dictionary_Rauh, id_counts)
df_rauh <- sentiment_rauh[[2]]

sentiment_ws <- analyze_sentiment_by_topic(corpus_crime_sentiment, merged_topic, data_dictionary_sentiws, id_counts)
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
  ) |> 
  mutate(topic = recode(topic, !!!replacement_names))

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

# Run the logistic regression on the replies only: the first comment would
# otherwise predict itself. Comments in the same thread are not independent,
# so standard errors are clustered by thread.
replies <- sentiment_comment |> 
  filter(row_number() > 1) |> 
  ungroup()

fit <- glm(is_negative ~ first_is_negative, data = replies, family = binomial())

# Summary of the model with cluster-robust standard errors
coeftest(fit, vcov = vcovCL(fit, cluster = ~ id))

#Sentiment / Topic / Interaction relation (one row per submission)
model_sentiment_topic <- lm(sentiment ~ factor(topic), data = df_rauh)
model_count_sentiment <- lm(count ~ sentiment, data = df_rauh)
model_count_topic     <- lm(count ~ factor(topic), data = df_rauh)
model_score_topic     <- lm(score ~ factor(topic), data = df_rauh)

summary(model_sentiment_topic)
summary(model_count_sentiment)
summary(model_count_topic)
summary(model_score_topic)
