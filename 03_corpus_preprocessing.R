library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(lubridate)

merged_crime <- read.csv2("data/merged_crime.csv", 
                        colClasses=c(NA), header = TRUE)
merged_ref <- read.csv2("data/merged_ref.csv", 
                        colClasses=c(NA), header = TRUE)

#Text Preprocessing
replacements <- c("ä" = "ae", "ö" = "oe", "ü" = "ue", "ß" = "ss")

replace_umlaut <- function(text) {
  for (char in names(replacements)) {
    text <- stringi::stri_replace_all_fixed(text, char, replacements[char], case_insensitive = TRUE)
  }
  return(text)
}

merged_crime <- merged_crime %>%
  mutate(body = replace_umlaut(body),
         user = replace_umlaut(user),
         title = replace_umlaut(title)
  )

merged_ref <- merged_ref %>%
  mutate(body = replace_umlaut(body),
         user = replace_umlaut(user),
         title = replace_umlaut(title)
  )

corpus_crime <- corpus(merged_crime, text_field = "body")
corpus_crime <- corpus_group(corpus_crime, groups = id)

df_combined <- rbind(merged_crime, merged_ref)
corpus_combined <- corpus(df_combined, text_field = "body")

tokens_combined <- tokens(corpus_combined,
                          remove_punct = TRUE,
                          remove_symbols = TRUE,
                          remove_numbers = TRUE,
                          remove_url = TRUE,
                          remove_separators = TRUE)

tokens_combined <- tokens_remove(tokens_combined, pattern = stopwords("de"))

tokens_combined <- tokens_group(tokens_combined, groups = flair)

dfm_combined <- dfm(tokens_combined)




