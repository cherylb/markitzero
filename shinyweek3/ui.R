library(shiny)
library(dplyr)
library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
ghub <- "https://raw.githubusercontent.com/jlaurito/CUNY_IS608/master/"
fileurl <- 
  paste(ghub,"lecture3/data/cleaned-cdc-mortality-1999-2010.csv", sep ="")
data <- getURL(fileurl)
df <- read.csv(text = data, stringsAsFactors = FALSE)

#get local
#df <- read.csv(
# "C:/Users/Cheryl/Documents/GitHub/CUNY_IS608/lecture3/data/cleaned-cdc-mortality-1999-2010.csv")

dth <- as.list(sort(unique(df$ICD.Chapter)))
sts <- as.list(sort(unique(df$State)))
x <- unlist(dth)
names(dth) <- x
y <- unlist(sts)
names(sts) <- y
# UI for mortality data view by state

shinyUI(pageWithSidebar(
  
  #title
  h3(headerPanel("Compare State Mortality Rates to National (per 100k)")),
  #select option
  sidebarPanel(
    selectInput("mort", "Cause of Mortality: ", dth, 
                selected = "Certain conditions originating in the perinatal period"),
    selectInput("state", "State: ", sts, selected='Alabama') # use vis to select
  ),
  mainPanel(
    
    h4(textOutput("caption")),
    tableOutput("dplot"))
    
  )
)

