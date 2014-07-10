# 

require(xml)
beer.url = "http://greatbrewers.com/top-50/beers"
beer <- readHTMLTable(beer.url, header = T, which = 1)

