library(shiny)
library(dplyr)
library(reshape)
library(RCurl)
library(stringr)
library(markdown)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
ghub <- "https://raw.githubusercontent.com/cherylb/markitzero/master/"
fileurl <- 
  paste(ghub,"VA.csv", sep ="")
data <- getURL(fileurl)
dfvadata <- read.csv(text = data, stringsAsFactors = FALSE)


# #'''''''''''''''''''''''''''''''''''''''''''''
# set up UI options

selecttype <- as.list(sort(unique(dfvadata$Description)))
selectgeo <- as.list(c("National",sort(unique(dfvadata$State))))

x <- unlist(selecttype)
names(selecttype) <- x
y <- unlist(selectgeo)
names(selectgeo) <- y

# UI for Veterans detail


shinyUI(navbarPage("US VA Expense",
  tabPanel("Charts",
  
      fluidPage(
      h3("VA Expenditures by Selected State(s) ($'s in 1000's)"),
      
      column(2,
        selectInput("Type", label = h4("Measure: "), 
                    choices = selecttype, 
                    selected = "AmtperVet"),
        br(),
        checkboxGroupInput("Geo", 
                           label = h4("Select States"), 
                           choices = selectgeo,
                           selected = "Alabama")
      ),
      column(3,
             tableOutput("dash")
        )
      )
    ),
  tabPanel("US Map",
           h3("Average VA Spend per State"),
           h4("1999-2013"),
           tableOutput("maptastic")
  ),
           
  tabPanel("Information",
           
           includeMarkdown("writeup.Rmd")
    )
  )
)
  



