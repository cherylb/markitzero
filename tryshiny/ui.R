library(shiny)
library(dplyr)

library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
ghub <- "https://raw.githubusercontent.com/jlaurito/CUNY_IS608/master/"
fileurl <- 
  paste(ghub,"lecture3/data/cleaned-cdc-mortality-1999-2010.csv", sep ="")
data <- getURL(fileurl)
df <- read.csv(text = data, stringsAsFactors = FALSE)

#read local
#df <- read.csv(
# "C:/Users/Cheryl/Documents/GitHub/CUNY_IS608/lecture3/data/cleaned-cdc-mortality-1999-2010.csv")

dth <- as.list(sort(unique(df$ICD.Chapter)))
x <- unlist(dth)
names(dth) <- x

# UI for mortality data view by state
shinyUI(pageWithSidebar(
  
  #title
  h3(headerPanel("Mortality Rates")),
  #select option
  sidebarPanel(
    selectInput("mort", "Cause of Mortality:", dth, 
                selected = "Certain conditions originating in the perinatal period")
  ),
  
  mainPanel(
    h4(textOutput("caption")),
  
    plotOutput("dplot"))
))

