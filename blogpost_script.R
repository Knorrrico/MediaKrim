#Preprocessing
library(tidyverse)

#Einlesen Rohdaten
submissions <- read.csv2("data_blog/submissions_selftext.csv", 
                         sep = ",", colClasses=c(NA), header = FALSE)

#Flair Kategorien 
categories <- submissions |> 
  group_by(submissions$V6) |> 
  summarise(as.vector(submissions$v6) )

submissions_crime <- submissions |> 
  filter(V6 == "category crime")

submissions_ref <- submissions |> 
  filter(V6 == "category society")

#ID Liste zum filtern von Kommentaren
list <- submissions_crime$V7
list_ref <- submissions_ref$V7

lapply(list, write, "list_blogpost.txt", append=TRUE)
lapply(list_ref, write, "list_ref_blogpost.txt", append=TRUE)

#Comments laden
comments_crime <- read.csv2("data_blog/comments_selftext.csv", 
                            sep = ",", colClasses=c(NA), header = FALSE)
comments_ref <- read.csv2("data_blog/comments_selftext_ref.csv", 
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

#Vorher
names(comments_crime)

comments_crime |> 
  group_by(V6) |> 
  summarise(count = n()) |> 
  arrange(desc(count))

submissions_crime <- clean_and_tolower(rename_submissions(submissions_crime))
submissions_ref <- clean_and_tolower(rename_submissions(submissions_ref))
comments_crime <- clean_and_tolower(rename_comments(comments_crime))
comments_ref <- clean_and_tolower(rename_comments(comments_ref))

#Nachher
names(comments_crime)

comments_crime |> 
  group_by(body) |> 
  summarise(count = n()) |> 
  arrange(desc(count))

#Filter zugehörige Comments
#ID anpassen 
remove_t3 <- function(df) {
  df_modified <- df |>  
    mutate(id = str_replace_all(id, "t3_", ""))
  return(df_modified)
}

comments_crime <- remove_t3(comments_crime)
comments_ref <- remove_t3(comments_ref)

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

---
  
#Deskripitve Analyse

#Häufigkeit Kommentare und Beiträge
nrow(comments_crime)
nrow(submissions_crime)

# Anzahl Nutzer
unique_users <- merged_crime %>% 
  summarise(n_unique_users = n_distinct(user))

#Populärste Beiträge nach Kommentarhäufigkeit (NAs sind beiträge die vorher gefilter wurden)
merged_crime |> 
  group_by(title) |> 
  summarize(count = n()) |> 
  mutate(percentage=paste0(round(count/sum(count)*100,2),"%")) |> 
  arrange(desc(count)) |> 
  print()

#Populärste Beiträge nach Score
submissions_crime |> 
  arrange(desc(score)) |> 
  select(id, score, title) |> 
  head(10) |> 
  print()

#Start- und Enddatum
range(merged_crime$date)

#Kommentaraufkommen nach Zeit
merged_crime <- merged_crime |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

merged_crime |> 
  group_by(date) |> 
  summarise(count = n())

comments_per_week <- merged_crime |> 
  mutate(week = floor_date(date, unit = "week")) |> 
  group_by(week) |> 
  summarise(id = n())

ggplot(comments_per_week, aes(x = week, y = id)) +
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

# Wochen mit stärksten Ausprägungen
comments_per_week <- comments_per_week|> 
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

---
  
# Textanalyse mit Quanteda
library(quanteda)
library(quanteda.textstats)

#Text Preprocessing
replacements <- c("ä" = "ae", "ö" = "oe", "ü" = "ue", "ß" = "ss")

replace_umlaut <- function(text) {
  for (char in names(replacements)) {
    text <- stringi::stri_replace_all_fixed(text, char, replacements[char], case_insensitive = TRUE)
  }
  return(text)
}

merged_crime <- merged_crime  |> 
  mutate(across(c(body, user, title), replace_umlaut))

merged_ref <- merged_ref |> 
  mutate(across(c(body, user, title), replace_umlaut))

corpus_crime <- corpus(merged_crime, text_field = "body")
tokens_crime <- tokens(corpus_crime,
                       remove_punct = TRUE,
                       remove_symbols = TRUE,
                       remove_numbers = TRUE,
                       remove_url = TRUE,
                       remove_separators = TRUE)

tokens_crime <- tokens_remove(tokens_crime, pattern = stopwords("de"))
custom_stopwords <- c("gt", "r", "#x200b", "jp", "i")
tokens_crime <- tokens_remove(tokens_crime, pattern = custom_stopwords)
tokens_crime <- tokens_group(tokens_crime, groups = id)
dfm_crime <- dfm(tokens_crime)

comments_combined <- rbind(merged_crime, merged_ref)
corpus_combined <- corpus(comments_combined, text_field = "body")
tokens_combined <- tokens(corpus_combined,
                          remove_punct = TRUE,
                          remove_symbols = TRUE,
                          remove_numbers = TRUE,
                          remove_url = TRUE,
                          remove_separators = TRUE)

tokens_combined <- tokens_remove(tokens_combined, pattern = stopwords("de"))
custom_stopwords <- c("gt", "r", "#x200b", "jp", "i")
tokens_combined <- tokens_remove(tokens_combined, pattern = custom_stopwords)
tokens_combined <- tokens_group(tokens_combined, groups = id)
dfm_combined <- dfm(tokens_combined)

submissions_combined <- rbind(submissions_crime, submissions_ref)
corpus_submissions <- corpus(submissions_combined, text_field = "body")
tokens_submissions <- tokens(corpus_submissions,
                          remove_punct = TRUE,
                          remove_symbols = TRUE,
                          remove_numbers = TRUE,
                          remove_url = TRUE,
                          remove_separators = TRUE)

tokens_submissions <- tokens_remove(tokens_submissions, pattern = stopwords("de"))
custom_stopwords <- c("gt", "r", "#x200b", "jp", "i")
tokens_submissions <- tokens_remove(tokens_submissions, pattern = custom_stopwords)
tokens_submissions <- tokens_group(tokens_submissions, groups = id)
dfm_submissions <- dfm(tokens_submissions)

#Textmining

#Term Frequency

#Keyword Analysis

#Begriffe über Zeit
docvars(dfm_crime, "date") <- merged_crime$date

dfm_by_week <- dfm_group(dfm_crime, groups = format(docvars(dfm_crime, "date"), "%Y-%U"))

token_frequency_over_time <- dfm_by_week[, "polizei"]

token_freq_df <- convert(token_frequency_over_time, to = "data.frame")

ggplot(token_freq_df, aes(x = doc_id, y = as.numeric(polizei), group = 1)) +
  geom_line() +
  xlab("Week") +
  ylab("Frequency of 'polizei'") +
  ggtitle("Usage of the token 'polizei' over weeks") +
  theme_minimal()

