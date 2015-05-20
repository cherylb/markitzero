library(shiny)
library(ggplot2)
library(dplyr)
library(googleVis)
library(reshape2)
library(RCurl)
library(stringr)

library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
ghub <- "https://raw.githubusercontent.com/cherylb/markitzero/master/"
fileurl <- 
  paste(ghub,"VA.csv", sep ="")
data <- getURL(fileurl)
df <- read.csv(text = data, stringsAsFactors = FALSE)

#'''''''''''''''''''''''''''''''''''''''''''''
# Reshape data
df$State <- as.character(df$State)
df$State <- sapply(df$State,str_trim)

df$MedicalGeneral <- df$medcare + df$genopex
df$OtherExpense <- df$insur + df$const + df$loan
df$AmtperVet <- df$TotalExpense/df$NumOfVeterans

df <- df[c(2,3,4,5,6,8,13,15,16,17)]

dfvadata <- melt(df, id=c("State","Year"))
names <- c("State", "Year", "Description", "Value")
names(dfvadata) <- names
dfvadata <- dfvadata%>%filter(State != 0, Value != 0)

dfvadata$State <- as.character(dfvadata$State)
dfvadata$Description <- as.character(dfvadata$Description)

# Add national total
addtot <- dfvadata%>%group_by(Year, Description) %>% 
  summarise(Value = sum(Value))%>%
  mutate(State = "National")

addtot$Value[addtot$Description=="TotAmountperVet"] = 
  addtot$Value[addtot$Description =="TotalExpense"]/
  addtot$Value[addtot$Description== "NumOfVeterans"]

dfvadata <- rbind(dfvadata,addtot[c(4,1,2,3)])
dfvadata <- dfvadata%>% replace(is.na(.), 0)


#''''''''''''''''''''
#shiny

shinyServer(function(input,output){
  
  formulaText <- reactive({
    paste(input$Type, " - ", input$Geo, "vs. National" )
  })
  
  output$caption <- renderText({
    formulaText()
  }) 
  
  org <- function(dfvadata){
    #flter data for selection
    type <- input$Type
    state <- input$Geo
    dfpt <-dfvadata%>%filter(State %in% state, Description == type)%>%
      arrange(State, Value)%>%mutate(xYear = as.numeric(Year))%>%
      select(State,Year,xYear, Value, Description)
      
    print(head(dfpt))
    return(dfpt)
  }
  
  ststr <- '
    {"xZoomedDataMax":2013,"yLambda":1,"iconType":"BUBBLE",
     "xAxisOption":"2","uniColorForNonSelected":false,
     "showTrails":true,"xLambda":1,"time":"1999",
      "yZoomedIn":false,"xZoomedIn":false,
      "yAxisOption":"3","yZoomedDataMax":7,
      "sizeOption":"3","xZoomedDataMin":1999,
      "orderedByX":false,"orderedByY":false,
      "yZoomedDataMin":3.9,"duration":{"timeUnit":"Y","multiplier":1},
      "colorOption":"_UNIQUE_COLOR","dimensions":{"iconDimensions":["dim0"]},
      "nonSelectedAlpha":0.4,"playDuration":10000}
  '

  
  output$dash <- renderGvis({
    gvisMotionChart(org(dfvadata), idvar = "State", timevar="xYear", 
                    yvar= "Value",
                    options = list( state= ststr))
  })
})