#descriptive analysis / exploration
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(lubridate)

comments_crime <- read.csv2("Data/comments_crime.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)
comments_ref <- read.csv2("Data/comments_ref.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)

submissions_crime <- read.csv2("Data/submissions_crime.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)
submissions_ref <- read.csv2("Data/submissions_ref.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)

#Posthäufigkeit von Nutzern zählen
comments_crime |> 
  group_by(user) |> 
  summarize(count = n()) |> 
  arrange(desc(count))

submissions_crime |> 
  group_by(user) |> 
  summarize(count = n()) |> 
  arrange(desc(count))

#Populärste Beiträge nach Kommentarhäufigkeit

comments_crime <- comments_crime |> 
  left_join(submissions_crime |> select(id, title), by = "id")

comments_ref <- comments_ref|> 
  left_join(submissions_ref |> select(id, title), by = "id")


comments_crime |> 
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

#Nur OC Beiträge. Von Nutzern formuliert.(Für den Blogpost nur noch Selftext)
selftext_crime <- submissions_crime |> 
  filter(!str_detect(body, "^http"))

#Beide Dataframes mergen

selected_crime_submissions <- submissions_crime |> 
  select(id, body, flair)

merged_crime <- merge(selected_crime_submissions, comments_crime, by.x = "id", by.y = "id")

merged_crime <- merged_crime |> 
  rename(content = body.x, body = body.y)

#reference

selected_ref_submissions <- submissions_ref |> 
  select(id, body, flair)

merged_ref <- merge(selected_ref_submissions, comments_ref, by.x = "id", by.y = "id")

merged_ref <- merged_ref |> 
  rename(content = body.x, body = body.y)

#Start- und Enddatum

min(merged_crime$date)
max(merged_crime$date)

min(selftext_crime$date)
max(selftext_crime$date)

#Kommentaraufkommen nach Zeit

merged_crime <- merged_crime |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

merged_crime |> 
  group_by(date) |> 
  summarise(count = n())

comments_per_day <- merged_crime |> 
  mutate(month = floor_date(date, unit = "month")) |> 
  group_by(month) |> 
  summarise(id = n())

ggplot(comments_per_day, aes(x = month, y = id)) +
  geom_line(color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(
    title = "Comment Distribution Over Time",
    x = "Date",
    y = "Number of Comments"
  ) +
  theme_minimal()

#Text Preprocessing

replacements <- c("ä" = "ae", "ö" = "oe", "ü" = "ue", "ß" = "ss")

replace_umlauts <- function(text) {
  for (char in names(replacements)) {
    text <- stringi::stri_replace_all_fixed(text, char, replacements[char], case_insensitive = TRUE)
  }
  return(text)
}

merged_crime <- merged_crime %>%
  mutate(body = replace_umlauts(body),
         user = replace_umlauts(user),
         title = replace_umlauts(title)
  )

merged_ref <- merged_ref %>%
  mutate(body = replace_umlauts(body),
         user = replace_umlauts(user),
         title = replace_umlauts(title)
  )

corpus_crime <- corpus(merged_crime, text_field = "body")
corpus_crime <- corpus_group(corpus_crime, groups = id)

df_combined <- rbind(merged_crime, merged_ref)
corpus_combined <- corpus(df_combined, text_field = "body")
#corpus_combined <- corpus_group(corpus_combined, groups = flair)

tokens_combined <- tokens(corpus_combined,
                          remove_punct = TRUE,
                          remove_symbols = TRUE,
                          remove_numbers = TRUE,
                          remove_url = TRUE,
                          remove_separators = TRUE)

tokens_combined <- tokens_remove(tokens_combined, pattern = stopwords("de"))



tokens_combined <- tokens_group(tokens_combined, groups = flair)

dfm_combined <- dfm(tokens_combined)
keyness <- textstat_keyness(dfm_combined)
