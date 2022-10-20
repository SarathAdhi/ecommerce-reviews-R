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
  msg <- "Hello this an API made for J Component for foundation of data analytics.
       Sentimental analysis in E-Commerce products.
       Team members are Sarath Adhithya, Rohan, Sai Teja"

  return(msg)
}

#* Empty the dataset (CSV)
#* @post /empty
function() {
  res <- write.csv(x = c(), getCurrentPath("/products/all_products.csv"))
  res
}

#* Return all the current product
#* @post /current-product
function() {
  out <- tryCatch(
    {
      res <- read.csv(getCurrentPath("/products/all_products.csv"))
      return(res)
    },
    error = function(error) {
      error <- paste(error, "The file is empty.", sep = " ")
      return(error)
    }
  )
}


#* Amazon Product Scraper
#* @param search Search for any product name
#* @post /amazon-product-scraper
function(search) {
  source(getCurrentPath("/controllers/Amazon_Product_Scraper.R"))
  searchFunction(search = search)
}


#* Amazon Review Scraper
#* @get /amazon-review-scraper
function(productId) {
  source(getCurrentPath("/controllers/Amazon_Review_Scraper.R"))
  scrapReview(productId = productId)
}


#* Get Sentimental Score
#* @get /product-score
function() {
  source(getCurrentPath("/controllers/sentimental_analysis.R"))
  getSentimentalScore()
}
