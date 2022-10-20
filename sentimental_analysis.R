#install.packages("tm")
#install.packages("wordcloud")
#install.packages("syuzhet")
#install.packages("tidyverse")
#install.packages("tidytext")

library(tm)
library(wordcloud)
library(syuzhet)

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
  tm_map(corpus, removeWords, c("samsung", "phone", "mobile", "product"))#can remove buy,
#battery quality
inspect(corpus[1:5])
reviews_final <- corpus



tdm <- TermDocumentMatrix(reviews_final)
tdm <-
  as.matrix(tdm)#Each row represents each word that is there in the corpus
#and each word occured in the reviews.
tdm[1:10, 1:5]

par("mar")
par(mar = c(6, 3, 3, 1))
#par(mar=c(6,6,6,1))
par("mar")

#Bar plot of words
w <- rowSums(tdm)
w <- subset(w, w >= 100)
barplot(w, las = 2, col = "blue")

#Creating a wordcloud
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(3000)
wordcloud(
  words = names(w),
  freq = w,
  max.words = 70,
  random.order = 1,
  min.freq = 15,
  colors = brewer.pal(8, "Dark2"),
  scale = c(3, 0, 3)
)

sentiment_data <- iconv(reviews$text)
s <- get_nrc_sentiment((sentiment_data))
s[1:5, ]

reviews_new <-
  write.csv(x = s, file = "S:/5th SEM/FDA - CSE3505/J Comp/project/app/sentimentScores.csv")

review_score <- colSums(s[, ])
print(review_score)

barplot(
  colSums(s),
  las = 2,
  col = rainbow(10),
  ylab = 'Count',
  main = 'Sentiment'
)


library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions

df <-
  read.csv(
    "S:/5th SEM/FDA - CSE3505/J Comp/project/app/sentimentScores.csv"
  )
df <- df[2:length(df)]

#Setting the margins
par("mar")
par(mar = c(6, 3, 3, 1))
#par(mar=c(6,6,6,1))
par("mar")

#preprocess the dataset
head(df)
df[1:50, ]
nrow(df)
ncol(df)
library(dplyr)
df$rating <- NULL
head(df)
length(df)
df$TruePositive <- rowSums(df[c("joy", "surprise", "trust",
                                "positive", "anticipation")])
df$TrueNegative <- rowSums(df[c("anger", "fear", "disgust",
                                "sadness", "negative")])
max(unique(df$TrueNegative))
max(unique(df$TruePositive))

df$Scores <- df$TruePositive - df$TrueNegative

df[df$Scores > 0, length(df)] <- "Positive"
df[df$Scores < 0, length(df)] <- "Negative"
df[df$Scores == 0, length(df)] <- "Neutral"

head(df)
df[1:50, ]
#find if there is any missing values
lapply(df, function(x)
  sum(is.na(x))) %>% str()

#find the unique value of each features
lapply(df, function(x)
  unique(x)) %>% str()

#encode the rating feature
df$Scores <- factor(df$Scores, labels = c(0, 1, 2))
head(df)

#split the data
#install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(df$Scores, SplitRatio = 0.75)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

#fit naive bayes to training set
#install.packages("e1071")
library(e1071)
classifier = naiveBayes(x = training_set[-length(df)],
                        y = training_set$Scores)

#predict the test set results
y_pred = predict(classifier, newdata = test_set[-length(df)])

#make the confusion matrix
cm = table(test_set$Scores, y_pred)
cm
naive_accuracy <- (cm[1] + cm[5] + cm[9]) / sum(as.vector(cm)) * 100
naive_accuracy

#fit decison tree classifier to training set
#install.packages("rpart")
library(rpart)
classifier = rpart(formula = Scores ~ ., data = training_set)

#predict the test set results
y_pred = predict(classifier, newdata = test_set[-length(df)], type = 'class')

#confusion matrix
cm = table(test_set$Scores, y_pred)

decision_tree_accuracy <- (cm[1] + cm[5] + cm[9]) / sum(as.vector(cm)) * 100

#plot the decision tree
plot(classifier)
text(classifier)

#feature scaling
training_set[-length(training_set)] =
  scale(training_set[-length(training_set)])
test_set[-length(test_set)] = scale(test_set[-length(test_set)])

#fitting ANN to the training set
#install.packages("h2o")

library(h2o)
h2o.init(nthreads = -1)

classifier = h2o.deeplearning(
  y = 'Scores',
  training_frame = as.h2o(training_set),
  activation = 'Rectifier',
  hidden = c(8),
  epochs = 50
)

# predicting the test set results
prob_pred = h2o.predict(classifier, newdata =
                          as.h2o(test_set[-length(test_set)]))
y_pred = as.factor(prob_pred$predict)
y_pred = as.vector(y_pred)

#making the confusion matrix
cm = table(test_set[, length(test_set)], y_pred)
ann_accuracy = (cm[1] + cm[5] + cm[9]) / sum(as.vector(cm)) * 100

h2o.shutdown()

#plot the accuracy graph

library(tidyverse)

classification_models_accuracy <-
  tibble(
    models = c("Naive Bayes", "Decision Tree", "ANN"),
    
    accuracy = c(naive_accuracy, decision_tree_accuracy, ann_accuracy)
  )
classification_models_accuracy %>%
  ggplot(aes(models, accuracy, fill = models)) +
  geom_col(position = "dodge") +
  ggtitle("Classifications models accuracy scores")
