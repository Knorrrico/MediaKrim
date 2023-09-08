library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(lemmar)

merged_crime <- read.csv2("data/merged_crime.csv", 
                        colClasses=c(NA), header = TRUE)
merged_ref <- read.csv2("data/merged_ref.csv", 
                        colClasses=c(NA), header = TRUE)

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
corpus_crime_full <- process_text_data(merged_crime, remove_stopwords = FALSE, group_field = "comment_id")

corpus_combined <- rbind(merged_crime, merged_ref)
corpus_combined_category <- process_text_data(corpus_combined, group_field = "flair", do_lemmatization = TRUE)

