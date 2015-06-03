###
### Wikipedia scraper
###

require(RCurl)
require(XML)
require(rjson)
require(stringr)

# one-off run through
url <- ("https://en.wikipedia.org/wiki/List_of_animated_feature_films_of_2014")
SOURCE <-  getURL(url, encoding = "UTF-8")
PARSED <- htmlParse(SOURCE, encoding = "UTF-8")
# pull out links
links <- (xpathSApply(PARSED, "//span[@class='citation news']/a/@href"))
browseURL(links[1])
head(tables[[2]])
top <- tables[[3]]

###
# automate: create functions ----------------------------------
###

# get links for wikipedia pages for animated films of the 2010s
getUrls <- function(y1, y2) {
      root <- "https://en.wikipedia.org/wiki/List_of_animated_feature_films_of_"
      urls <- NULL
      for (i in y1:y2) {
                  urls <- c(urls,(paste(root, i, sep = "")))
            }
      return(urls)
}

urls <- getUrls(2010, 2014)

# get the data table listing the top grossing films
getData <- function(url) {
      SOURCE <-  getURL(url, encoding = "UTF-8")
      PARSED <- htmlParse(SOURCE, encoding = "UTF-8")
      tables <- readHTMLTable(PARSED)
      top <- tables[[3]]
      top$Year <- str_sub(url, -4)
      top_gross <- top[c("Year", "Rank", "Title")]
      return(top_gross)
}

# getData(url)

top_gross <- NULL
for (url in urls) {
      top_gross <- rbind(top_gross, getData(url))
}

write.csv(top_gross, "top_animated_films.csv", row.names = FALSE)



