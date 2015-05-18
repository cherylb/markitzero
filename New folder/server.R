library(shiny)
library(ggplot2)
library(dplyr)
library(googleVis)

# library(RCurl)
# options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
# ghub <- "https://raw.githubusercontent.com/jlaurito/CUNY_IS608/master/"
# fileurl <- 
#   paste(ghub,"lecture3/data/cleaned-cdc-mortality-1999-2010.csv", sep ="")
# data <- getURL(fileurl)
# df <- read.csv(text = data, stringsAsFactors = FALSE)

#get local
df <- read.csv(
  "C:/Users/Cheryl/Documents/GitHub/VA.csv")

df$State <- as.character(df$State)
# reshape data
df$MedicalGeneral <- df$medcare + df$genopex
df$OtherExpense <- df$insur + df$const + df$loan

df <- df[c(1,2,3,4,5,7,12,14)]

dfvadata <- melt(df, id=c("State","Year"))
names <- c("State", "Year", "Description", "Value")
names(dfvadata) <- names

addtotals <- dfvadata%>%group_by(Year, Description) %>% 
  summarise(Value = sum(Value))%>%
  mutate(State = "National")
dfvadata <- rbind(dfvadata,addtotals[c(4,1,2,3)])

shinyServer(function(input,output){
  
  formulaText <- reactive({
    paste(input$Type, " - ", input$State, " Data" )
  })
  
  output$caption <- renderText({
    formulaText()
  }) 
  
  org <- function(df){
    #flter data for selection
    type <- input$Type
    state <- input$Geo
  
    dfpt <-df%>%filter(Description == type, State %in% c("National", state))%>%
      arrange(state, Value)%>%mutate(xYear = as.numeric(Year))%>%
      select(State,Year,xYear, Value, Description)
      
    print(head(dfpt))
    return(dfpt)
  }
  
  ststr <- '
    {"xZoomedDataMax":2010,"yLambda":1,"iconType":"BUBBLE",
     "xAxisOption":"2","uniColorForNonSelected":false,
     "showTrails":true,"xLambda":1,"time":"1999",
      "yZoomedIn":false,"xZoomedIn":false,
      "iconKeySettings":[{"key":{"dim0":"Alabama"},"trailStart":"1999"},
    {"key":{"dim0":"National"},"trailStart":"1999"}],
      "yAxisOption":"3","yZoomedDataMax":7,
      "sizeOption":"3","xZoomedDataMin":1999,
      "orderedByX":false,"orderedByY":false,
      "yZoomedDataMin":3.9,"duration":{"timeUnit":"Y","multiplier":1},
      "colorOption":"_UNIQUE_COLOR","dimensions":{"iconDimensions":["dim0"]},
      "nonSelectedAlpha":0.4,"playDuration":15000}
  '

  
  output$dplot <- renderGvis({
    gvisMotionChart(org(df), idvar = "State", timevar="Year", 
                    yvar= "Value",
                    options = list( state= ststr))
  })
})