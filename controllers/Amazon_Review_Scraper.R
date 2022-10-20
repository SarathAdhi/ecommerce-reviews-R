getCurrentPath <- function(path) {
  currentPath <- paste(getwd(), path, sep = "")
}

library(rvest)

scrapReview <- function(productId) {
  res <- read.csv(getCurrentPath("/products/all_products.csv"), )
  res <- data.frame(res)
  res <- res[2:4] # Remove X

  product <- res[res$productId == productId, ]

  reviewUrl <- product$reviewUrl
  reviewUrl

  num <- 1:100

  library(rvest)

  productReviews <- c()

  for (n in num) {
    pageNumber <- paste("&pageNumber=", n, sep = "")
    url <- paste(reviewUrl, pageNumber, sep = "")

    webpage <- read_html(url)
    new_reviews <- html_nodes(webpage, ".review-title span")
    new_reviews <- html_text(new_reviews)

    productReviews <- c(productReviews, new_reviews)
  }

  df <- data.frame(sno = c(1:length(productReviews)), text = productReviews)
  print(head(productReviews))

  write.csv(df, getCurrentPath("/reviews/all_reviews.csv"))

  return(df)
}
