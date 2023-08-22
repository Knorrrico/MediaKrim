#Preprocessing
library(tidyverse)

#Einlesen Rohdaten
submissions <- read.csv2("data/submissions.csv", 
                         sep = ",", colClasses=c(NA), header = FALSE)

#Flair Kategorien 
categories <- submissions |> 
  group_by(submissions$V6) |> 
  summarise(as.vector(submissions$v6) )

submissions_crime <- submissions |> 
  filter(V6 == "category crime")

submissions_ref <- submissions |> 
  filter(V6 == c("category newsde", "category newseu", "category newswo"))

#ID Liste zum filtern von Kommentaren
list <- submissions_crime$V7
list_ref <- submissions_ref$V7

lapply(list, write, "list.txt", append=TRUE)
lapply(list_ref, write, "list_ref.txt", append=TRUE)

#Einsetzen der Listen in Python Script

comments_crime <- read.csv2("data/comments_crime.csv", 
                            sep = ",", colClasses=c(NA), header = FALSE)
comments_ref <- read.csv2("data/comments_ref.csv", 
                          sep = ",", colClasses=c(NA), header = FALSE)

#Umbenennen der Variablen
names(comments_crime)

comments_crime <- comments_crime |> 
  rename(score = V1, date = V2, user = V3, link = V4, id = V5, body = V6)
comments_ref <- comments_ref |> 
  rename(score = V1, date = V2, user = V3, link = V4, id = V5, body = V6)

submissions_ref <- submissions_ref |> 
  rename(score = V1, date = V2, title = V3, user = V4, link = V5, flair = V6, id = V7, body = V8)
submissions_crime <- submissions_crime |> 
  rename(score = V1, date = V2, title = V3, user = V4, link = V5, flair = V6, id = V7, body = V8)

#Entfernen von NA
submissions_crime <- submissions_crime |> 
  filter(body != "[deleted]") |> 
  filter(body != "[removed]") |> 
  filter(body != "") |> 
  mutate(title = tolower(title)) |> 
  mutate(body = tolower(body))

submissions_ref <- submissions_ref |> 
  filter(body != "[deleted]") |> 
  filter(body != "[removed]") |> 
  filter(body != "") |> 
  mutate(title = tolower(title)) |> 
  mutate(body = tolower(body))

comments_crime <- comments_crime |> 
  filter(body != "[deleted]") |> 
  filter(body != "[removed]") |> 
  filter(body != "") |> 
  mutate(body = tolower(body))

comments_ref <- comments_ref |> 
  filter(body != "[deleted]") |> 
  filter(body != "[removed]") |> 
  filter(body != "") |> 
  mutate(body = tolower(body))

comments_crime |> 
  group_by(body) |> 
  summarise(count = n()) |> 
  arrange(desc(count))

#Filter comments ohne submission und vice versa

comments_crime$id <- comments_crime$id |> 
  str_replace_all("t3_", "")

comments_ref$id <- comments_ref$id |> 
  str_replace_all("t3_", "")

#Schreibe gesäuberte Datensätze

write.csv2(submissions_crime, "data/submissions_crime.csv", row.names = FALSE, sep = ",")
write.csv2(submissions_ref, "data/submissions_ref.csv", row.names = FALSE, sep = ",")
write.csv2(comments_crime, "data/comments_crime.csv", row.names = FALSE)
write.csv2(comments_ref, "data/comments_ref.csv", row.names = FALSE)

#descriptive analysis
library(tidyverse)
library(quanteda)

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



