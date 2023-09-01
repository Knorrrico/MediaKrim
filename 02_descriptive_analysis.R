#descriptive analysis / exploration
library(tidyverse)
library(lubridate)

comments_crime <- read.csv2("data/comments_crime.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)
comments_ref <- read.csv2("data/comments_ref.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)

submissions_crime <- read.csv2("data/submissions_crime.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)
submissions_ref <- read.csv2("data/submissions_ref.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)

#Häufigkeit Kommentare und Beiträge
nrow(comments_crime)
nrow(submissions_ref)

#Posthäufigkeit von Nutzern zählen
comments_crime |> 
  group_by(user) |> 
  summarize(count = n()) |> 
  arrange(desc(count))

submissions_crime |> 
  group_by(user) |> 
  summarize(count = n()) |> 
  arrange(desc(count))

#Populärste Beiträge nach Kommentarhäufigkeit (NAs sind beiträge die vorher gefilter wurden)
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

comments_crime <- comments_crime |> 
  filter(!is.na(title))

comments_ref <- comments_ref |> 
  filter(!is.na(title))

#Populärste Beiträge nach Score (datensatz mehrmals aktualisiert)
submissions_crime |> 
  arrange(desc(score)) |> 
  select(id, score, title) |> 
  head(10) |> 
  print()

#Beide Dataframes mergen
selected_crime_submissions <- submissions_crime |> 
  select(id, body, flair)

merged_crime <- merge(selected_crime_submissions, comments_crime, by.x = "id", by.y = "id")

merged_crime <- merged_crime |> 
  rename(content = body.x, body = body.y)

#Reference
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

start_date <- as.Date("2020-06-14")
end_date <- as.Date("2020-06-28")

most_comments_week <- comments_crime |> 
  filter(date >= start_date & date <= end_date)

summary_week <- most_comments_week |> 
  group_by(id) |> 
  summarise(n_comments = n()) |> 
  arrange(desc(n_comments))

top_3 <- as.list(head(summary_week$id, 3))

summary_week
# Title der populärsten Beiträge

submissions_crime |> 
  filter(id %in% top_3) |> 
  select(title)

#Speicher zusammengefassten Datensatz aus Submission und Comment
write.csv2(merged_crime, "data/merged_crime.csv", row.names = FALSE)
write.csv2(merged_ref, "data/merged_ref.csv", row.names = FALSE)

