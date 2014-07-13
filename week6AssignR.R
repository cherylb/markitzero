# 
#XML library, strings library
require(XML)
require(stringr)

# website link
beer.url = "http://greatbrewers.com/top-50/beers"
#read top 50 in to R
beer.top50 <- readHTMLTable(beer.url, header = T, which = 1, stringsAsFactors = FALSE)

#clean up table,get rid of blank first col, col headings
beer.top50 <- beer.top50[2:7]
colnames(beer.top50)<- str_trim(names(beer.top50))

head(beer.top50)

summary(beer.top50)
