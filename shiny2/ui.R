library(shiny)
library(dplyr)
library(RCurl)
# options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
# ghub <- "https://raw.githubusercontent.com/jlaurito/CUNY_IS608/master/"
# fileurl <- 
#   paste(ghub,"lecture3/data/cleaned-cdc-mortality-1999-2010.csv", sep ="")
# data <- getURL(fileurl)
# df <- read.csv(text = data, stringsAsFactors = FALSE)

#get local
# df <- read.csv(
#  "C:/Users/Cheryl/Documents/GitHub/VA.csv")

Type <- as.list(sort(unique(dfvadata$Description)))
Geo <- as.list(sort(unique(dfvadata$State)))
x <- unlist(Type)
names(Type) <- x
y <- unlist(Geo)
names(Geo) <- y

# UI for vetrans detail
shinyUI(pageWithSidebar(
  
  # title
  h3(headerPanel("State VA Benefits ($'s in 1000's)")),
  #select option
  sidebarPanel(
    selectInput("type", "Type: ", Type, 
                selected = "TotalExpense"),
    selectInput("State", "State: ", Geo, selected='National') # use vis to select
  ),
  mainPanel(
    
    h4(textOutput("caption")),
    tableOutput("dplot"))
    
  )
)

