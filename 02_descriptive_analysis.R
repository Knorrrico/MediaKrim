#descriptive analysis / exploration
library(tidyverse)
library(lubridate)

comments_crime <- read.csv2("data/comments_crime.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)
comments_ref <- read.csv2("data/comments_ref.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)

submissions_crime <- read.csv2("data/submissions_crime.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)
submissions_ref <- read.csv2("data/submissions_ref.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)

merged_crime <- read.csv2("data/merged_crime.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)
merged_ref <- read.csv2("data/merged_ref.csv", colClasses=c(NA), header = TRUE, stringsAsFactors = FALSE)

#Häufigkeit Kommentare und Beiträge
nrow(comments_crime)
nrow(submissions_crime)

#Start- und Enddatum
range(merged_crime$date)

#Count selftext
nrow(selftext_crime)

#All User in Data
read_users <- function(file_path) {
  con <- file(file_path, "r")
  user_data <- readLines(con)
  close(con)
  return(data.frame(user = user_data))
}

user_sub_df <- read_users("data_raw/submissions_user.txt")
user_com_df <- read_users("data_raw/comments_user.txt")

# User in Crime Content
unique_users_crime <- merged_crime |>  
  summarise(n_unique_users = n_distinct(user))

#Posthäufigkeit von Nutzern zählen
user_post_frequency <- function(data_frame) {
  require(dplyr)
  
  result <- data_frame |> 
    group_by(user)  |> 
    summarize(count = n()) |> 
    arrange(desc(count))
  
  return(result)
}

remove_first_row <- function(df) {
  df[-1, ]
}

comments_portion <- user_post_frequency(comments_crime)
submissions_portion <- user_post_frequency(submissions_crime)
comments_total <- user_post_frequency(user_com_df)
submissions_total <- user_post_frequency(user_sub_df)

comments_portion <- remove_first_row(comments_portion)
submissions_portion <- remove_first_row(submissions_portion)
comments_total <- remove_first_row(comments_total)
submissions_total <- remove_first_row(submissions_total)

# Plotting User Activity
plot_top_users <- function(total_df, portion_df, type = "submission", n_top = 20) {
  
  # Select common users
  common_users <- intersect(total_df$user, portion_df$user)
  
  # Find the top n_top (or fewer) users by posting frequency in total_df
  top_users <- total_df %>%  
    filter(user %in% common_users) %>%  
    arrange(desc(count)) %>%  
    head(n_top) %>%  
    pull(user)
  
  # Filter both data frames to only include these top n_top users
  total_filtered <- total_df %>% filter(user %in% top_users)
  portion_filtered <- portion_df %>% filter(user %in% top_users)
  
  # Combine the data
  combined_df <- bind_rows(
    mutate(total_filtered, type = paste("Total", type)),
    mutate(portion_filtered, type = paste("Portion", type))
  )
  
  # Create the plot
  p <- ggplot(combined_df, aes(x = reorder(user, -count), y = count, fill = type)) +
    geom_bar(stat = "identity", position = "stack", width = 0.6) +
    labs(title = paste("Top", n_top, "User Posting Frequency"), x = "User", y = "Frequency") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

plot_top_users(submissions_total, submissions_portion, type = "submission", n_top = 20)
plot_top_users(comments_total, comments_portion, type = "comment", n_top = 20)

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
submissions_crime |> 
  arrange(desc(score)) |> 
  select(id, score, title) |> 
  head(10) |> 
  print()

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
  aggregated_data <- df |>  
    group_by(!!sym(column_name)) |> 
    summarise(count = n()) |>  
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
