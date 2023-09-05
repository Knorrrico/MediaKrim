library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.sentiment)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
library(lubridate)

#Term Frequency

#Keyword Analysis

#tf-idf

#Term over Time

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

#Topic Modeling

#Sentiment Analysis of Topics

#Sentiment over Time

#Sentiment in Submissions / Comments
