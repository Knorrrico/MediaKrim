#descriptive analysis
library(tidyverse)

comments_crime <- read.csv2("/Users/Rico/Documents/Script/R/Projekte/Datenanalyse_Social_Media_Projekt/Data/comments_crime.csv", 
                            colClasses=c(NA), header = FALSE)
comments_ref <- read.csv2("/Users/Rico/Documents/Script/R/Projekte/Datenanalyse_Social_Media_Projekt/Data/comments_ref.csv", 
                          colClasses=c(NA), header = FALSE)

submissions_crime <- read.csv2("/Users/Rico/Documents/Script/R/Projekte/Datenanalyse_Social_Media_Projekt/Data/submissions_crime.csv", 
                            colClasses=c(NA), header = FALSE)
submissions_ref <- read.csv2("/Users/Rico/Documents/Script/R/Projekte/Datenanalyse_Social_Media_Projekt/Data/submissions_ref.csv", 
                             colClasses=c(NA), header = FALSE)

#Posthäufigkeit von Nutzern zählen
userfreq_c <- comments_crime |> 
  group_by(V3) |> 
  summarize(count = n()) |> 
  arrange(desc(count))

userfreq_s <- submissions_crime |> 
  group_by(V4) |> 
  summarize(count = n()) |> 
  arrange(desc(count))