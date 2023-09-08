#Preprocessing
library(tidyverse)

#Einlesen Rohdaten
submissions <- read.csv2("data_raw/submissions.csv", 
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
comments_crime <- read.csv2("data_raw/comments.csv", 
                            sep = ",", colClasses=c(NA), header = FALSE)
comments_ref <- read.csv2("data_raw/comments_ref.csv", 
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
    filter(!grepl("^\\[", body), !(body %in% c("[deleted]", "[removed]", ""))) |> 
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

#remove user u/
comments_crime$user <- gsub("^u/", "", comments_crime$user)
submissions_crime$user <- gsub("^u/", "", submissions_crime$user)
comments_ref$user <- gsub("^u/", "", comments_ref$user)
submissions_ref$user <- gsub("^u/", "", submissions_ref$user)

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

#Speicher gesäuberten Datensatz
write.csv2(submissions_crime, "data/submissions_crime.csv", row.names = FALSE)
write.csv2(submissions_ref, "data/submissions_ref.csv", row.names = FALSE)
write.csv2(comments_crime, "data/comments_crime.csv", row.names = FALSE)
write.csv2(comments_ref, "data/comments_ref.csv", row.names = FALSE)

#Speicher zusammengefassten Datensatz aus Submission und Comment
write.csv2(merged_crime, "data/merged_crime.csv", row.names = FALSE)
write.csv2(merged_ref, "data/merged_ref.csv", row.names = FALSE)
