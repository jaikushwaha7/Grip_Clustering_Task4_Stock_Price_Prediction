setwd("D:/Study/Internsala/Spark Foudation Grip/Task 4")

# Sentiment analysis
library(syuzhet) # for sentiment

# Read File

text_df = read.csv('india-news-headlines.csv', stringsAsFactors = F)
colnames((text_df)) # "publish_date"      "headline_category" "headline_text"    

summary(text_df)
# data from 20010101 till 20200630
str(text_df)
# headline category to be factor
text_df$headline_category<- factor(text_df$headline_category)

summary(text_df)
# total headlines 3297172

categories <- unique(text_df$headline_category) 
categories
numberOfCategories <- length(categories)
numberOfCategories  # total categories 1016

hist(table(categories))
hist(table(text_df$headline_category))
table(text_df$headline_category)
sort(table(text_df$headline_category), decreasing = TRUE)
library(tidyverse)
text_df %>%
  group_by(headline_category) %>%
  slice_max(headline_text, n = 2, with_ties = FALSE)      

text_df %>% select(headline_category) %>% group_by(headline_category) %>% top_n(10,headline_category)

library(tidyverse)
detach("package:dplyr", unload = TRUE)
detach("package:tidyverse", unload = TRUE)
library(dplyr)

my_table <- function(df, group_var) {
  group_var <- enquo(group_var)      # Create quosure
  df %>% 
    group_by(!!group_var) %>%        # Use !! to unquote the quosure
    summarise(n = n()) %>% arrange(desc(n))
}

my_table(text_df, headline_category)

# convert to character
review<- as.character(text_df$headline_text)

# Obtain sentiment scores
get_nrc_sentiment('happy')
get_nrc_sentiment('excitement')

s<- get_nrc_sentiment(review[3000000:3297172])
