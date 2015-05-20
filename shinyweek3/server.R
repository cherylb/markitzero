library(shiny)
library(ggplot2)
library(dplyr)
library(googleVis)

library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
ghub <- "https://raw.githubusercontent.com/jlaurito/CUNY_IS608/master/"
fileurl <- 
  paste(ghub,"lecture3/data/cleaned-cdc-mortality-1999-2010.csv", sep ="")
data <- getURL(fileurl)
df <- read.csv(text = data, stringsAsFactors = FALSE)

#df <- df%>%select(ICD.Chapter, State, Year, Crude.Rate)%>%filter(Year == "2010")
dfnat <- df%>%group_by(ICD.Chapter, Year)%>%
  summarise(tdeath=sum(Deaths),tpop = sum(Population), 
            NRate = round(tdeath/tpop*100000,1))%>%mutate(State = "National")
nms <- c("ICD.Chapter", "Year", "Deaths", "Population", "Crude.Rate", "State")
names(dfnat)<- nms
dfs <- df%>%select(ICD.Chapter, Year, Deaths, Population, Crude.Rate, State)
dfs <- rbind(dfs,dfnat)


shinyServer(function(input,output){
  
  formulaText <- reactive({
    paste(input$mort, " - ", input$state, "vs. National" )
  })
  
  output$caption <- renderText({
    formulaText()
  }) 
  
  org <- function(dfs){
    #flter data for selection
    dth <- input$mort
    st <- input$state
  
    dfpt <-dfs%>%filter(ICD.Chapter == dth, State %in% c("National", st))%>%
      arrange(State, Crude.Rate)%>%mutate(xYear = as.numeric(Year))%>%
      select(State,Year,xYear, Crude.Rate, ICD.Chapter)
      
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
    gvisMotionChart(org(dfs), idvar = "State", timevar="Year", 
                    yvar= "Crude.Rate",
                    options = list( state= ststr))
  })
})