getCurrentPath <- function(path) {
  currentPath <- paste(getwd(), path, sep = "")
}

library(rvest)
library(stringr)

searchFunction <- function(search) {
  url <- paste("https://www.amazon.in/s?k=", search, sep = "")

  webpage <- read_html(url)
  links <- html_nodes(webpage, ".a-section .sg-col-inner .a-size-mini a")
  href <- links %>% html_attr("href")
  href <- as.array(href)

  productUrl <- c()
  reviewUrl <- c()

  for (x in href) {
    isValidUrl <- str_extract(x, "/dp/") == "/dp/"

    if (isTRUE(isValidUrl) && !is.na(isValidUrl)) {
      link <- paste("https://www.amazon.in", x, sep = "")
      productUrl <- c(productUrl, link)

      review <- str_replace(link, "/dp/", "/product-review/")
      reviewUrl <- c(reviewUrl, review)
    }
  }


  df <- data.frame(productId = c(1:length(productUrl)), productUrl, reviewUrl)
  print(head(productUrl))

  write.csv(df, getCurrentPath("/products/all_products.csv"))

  return(df)
}
