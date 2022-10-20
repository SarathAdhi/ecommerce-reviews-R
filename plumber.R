# install.packages("plumber")   //for installing plumber
# https://www.rplumber.io/
# install.packages('rvest')
# install.packages("stringr")

getCurrentPath <- function(path) {
  currentPath <- paste(getwd(), path, sep = "")
}

#* Home route
#* @get /
function() {
  list(msg = "Hello this an API made for J Component for foundation of data analytics.
       Sentimental analysis in E-Commerce products.
       Team members are Sarath Adhithya, Rohan, Sai Teja")
}

#* Return the product
#* @param product The first number to add
#* @post /current-product
function(product) {
  res <- read.csv(getCurrentPath("/products/all_products.csv"))
  res <- data.frame(res)
 
  reviews <- res[res$productId == product,]
  
  reviews
  
}




#* amazon product scraper
#* @param search Search for any product name
#* @post /amazon-product-scraper
function(search) {
  library(rvest)
  library(stringr)
  
  
  url <- paste("https://www.amazon.in/s?k=", search, sep="")
  
  webpage = read_html(url)
  links = html_nodes(webpage, ".a-section .sg-col-inner .a-size-mini a")
  href <-  links %>% html_attr('href')
  href <- as.array(href)
  
  productUrl <- c()
  reviewUrl <- c()
  
  for(x in href) {
    isValidUrl <- str_extract(x, "/dp/") == "/dp/"

    if(isTRUE(isValidUrl) && !is.na(isValidUrl)) {

      link <- paste("https://www.amazon.in", x, sep="")
      productUrl <- c(productUrl, link)
      
      review <- str_replace(link, "/dp/", "/product-review/")
      reviewUrl <- c(reviewUrl, review)
    }
  }
  
  
  
  df <- data.frame(productId = c(1:length(productUrl)) ,productUrl, reviewUrl)
  write.csv(df, getCurrentPath("/products/all_products.csv"))
  df
}




#* Web scraper
#* @get /amazon-review-scraper
function(productId) {
  library(rvest)
  
  res <- read.csv(getCurrentPath("/products/all_products.csv"),)
  res <- data.frame(res)
  res <- res[2:4]   # Remove X
  
  product <- res[res$productId == productId,]
  
  reviewUrl <- product$reviewUrl
  reviewUrl
  
  num <- 1:100
  
  library(rvest)
  
  productReviews <- c()
  
  for(n in num) {
    pageNumber <- paste("&pageNumber=", n, sep="")
    url <- paste(reviewUrl, pageNumber, sep="")
    
    webpage = read_html(url)
    new_reviews = html_nodes(webpage, ".review-title span")
    new_reviews <- html_text(new_reviews)
    
    productReviews <- c(productReviews, new_reviews)
  }
  
  productReviews
  
  df <- data.frame(sno = c(1:length(productReviews)) ,text = productReviews)
  write.csv(df, getCurrentPath("/reviews/all_reviews.csv"))
  
  df

  
}

#* Get score
#* @get /product-score
function() {
  source(getCurrentPath("/sentimental_analysis.R"))
  
  df<-read.csv(getCurrentPath("/sentimentScores.csv"))
  
  df$results = ifelse(df$negative > df$positive, 'negative',
                      ifelse(df$negative < df$positive, 'positive', 'none'))
  
  #write.csv(x=df,file=getCurrentPath("/sentimentScores.csv"))
  
  length <- length(df$results)
  
  negative_sum <- length(which(df$results == "negative"))
  negative_avg <- negative_sum/length
  negative_percentage <- negative_avg * 100 
  
  positive_sum <- length(which(df$results == "positive"))
  positive_avg <- positive_sum/length
  positive_percentage <- positive_avg * 100 
  
  value <- data.frame(positive = positive_percentage, negative = negative_percentage)
  
  return(value)
  
}







