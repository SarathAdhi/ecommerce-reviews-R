# install.packages("tm")
# install.packages("wordcloud")
# install.packages("syuzhet")
# install.packages("tidyverse")
# install.packages("tidytext")

library(tm)
library(syuzhet)

getSentimentalScore <- function() {
  reviews <-
    read.csv("S:/5th SEM/FDA - CSE3505/J Comp/project/app/reviews/all_reviews.csv")
  str(reviews)
  corpus <- iconv(reviews$text)
  corpus <- Corpus(VectorSource(corpus))
  inspect(corpus[1:5])
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <-
    tm_map(corpus, removeWords, c("samsung", "phone", "mobile", "product")) # can remove buy,
  # battery quality
  inspect(corpus[1:5])
  reviews_final <- corpus

  sentiment_data <- iconv(reviews$text)
  s <- get_nrc_sentiment((sentiment_data))
  s[1:5, ]

  reviews_new <- write.csv(x = s, file = "S:/5th SEM/FDA - CSE3505/J Comp/project/app/sentimentScores.csv")

  review_score <- colSums(s[, ])

  df <- read.csv("S:/5th SEM/FDA - CSE3505/J Comp/project/app/sentimentScores.csv")


  library(dplyr)
  df$rating <- NULL
  df$TruePositive <- rowSums(df[c(
    "joy", "surprise", "trust",
    "positive", "anticipation"
  )])

  df$TrueNegative <- rowSums(df[c(
    "anger", "fear", "disgust",
    "sadness", "negative"
  )])
  max(unique(df$TrueNegative))
  max(unique(df$TruePositive))

  df$impression <- df$TruePositive - df$TrueNegative

  df[df$impression > 0, length(df)] <- "Positive"
  df[df$impression < 0, length(df)] <- "Negative"
  df[df$impression == 0, length(df)] <- NA

  df <- na.omit(df) # Removing the NA or neutral values

  head(df)
  df[1:50, ]

  # find the unique value of each features
  lapply(df, function(x) {
    unique(x)
  }) %>% str()

  # encode the rating feature
  df$Scores <- factor(df$impression, labels = c(0, 1))
  head(df)

  write.csv(x = df, file = "S:/5th SEM/FDA - CSE3505/J Comp/project/app/sentimentScores.csv")

  length <- length(df$impression)

  negative_count <- length(which(df$impression == "Negative"))
  negative_avg <- negative_count / length
  negative_percentage <- negative_avg * 100

  positive_count <- length(which(df$impression == "Positive"))
  positive_avg <- positive_count / length
  positive_percentage <- positive_avg * 100

  value <- data.frame(positive = positive_percentage, negative = negative_percentage)
  print(head(value))

  return(value)
}
