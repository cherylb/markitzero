library(shiny)
library(ggplot2)
library(dplyr)
library(googleVis)
library(reshape2)
library(RCurl)
library(stringr)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
ghub <- "https://raw.githubusercontent.com/cherylb/markitzero/master/"
fileurl <- 
  paste(ghub,"VA.csv", sep ="")
data <- getURL(fileurl)
df <- read.csv(text = data, stringsAsFactors = FALSE)

#get local
# df <- read.csv(
# "C:/Users/Cheryl/Documents/GitHub/VA.csv")

#'''''''''''''''''''''''''''''''''''''''''''''
# Reshape data
df$State <- as.character(df$State)
df$State <- sapply(df$State,str_trim)

df$MedicalGeneral <- df$medcare + df$genopex
df$OtherExpense <- df$insur + df$const + df$loan
df$AmtperVet <- df$TotalExpense/df$NumOfVetrans

df <- df[c(2,3,4,5,6,8,13,15,16,17)]

dfvadata <- melt(df, id=c("State","Year"))
names <- c("State", "Year", "Description", "Value")
names(dfvadata) <- names
dfvadata <- dfvadata%>%filter(State != 0, Value != 0)

#'''''''''''''''''''''''''''''''''''''''''''''
# Add national total
addtot <- dfvadata%>%group_by(Year, Description) %>% 
  summarise(Value = sum(Value))%>%
  mutate(State = "National")

addtot$Value[addtot$Description=="TotAmountperVet"] = 
  addtot$Value[addtot$Description =="TotalExpense"]/
  addtot$Value[addtot$Description== "NumOfVetrans"]
                
dfvadata <- rbind(dfvadata,addtot[c(4,1,2,3)])
dfvadata <- dfvadata%>% replace(is.na(.), 0)
dfvadata$State <- as.character(dfvadata$State)
dfvadata$Description <- as.character(dfvadata$Description)

#'''''''''''''''''''''''''''''''''''''''''''''
#graph

shinyServer(function(input,output){
  
  formulaText <- reactive({
    paste(input$Type, " - ", input$Geo, " Data" )
  })
  
  output$caption <- renderText({
    formulaText()
  }) 
  
  org <- function(dfvadata){
    #flter data for selection
    type <- input$Type
    state <- input$Geo
  
    dfpt <-dfvadata%>%filter(Description == type, State %in% state)%>%
      arrange(State, Value)%>%mutate(xYear = as.numeric(Year))%>%
      select(State,Year,xYear, Value, Description)
      
    print(head(dfpt))
    return(dfpt)
  }
  
  numvets <- function(dfvadata){
    #flter data for selection
    type <- "NumOfVeterns"
    state <- input$Geo
    
    dfpt <-dfvadata%>%filter(Description == type, State %in% state)%>%
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
  
  bubble <- gvisMotionChart(org(dfvadata), idvar = "State", timevar="Year", 
                            yvar= "Value",
                            options = list( state= ststr))
  lines <-gvisLineChart(org(dfvadata), xvar="Year", yvar="Value", 
                        options = list(title="Veteran Population 1999 - 2013",
                                       vAxis="{title: 'Bounce Rate %'}",
                                       width=500, height=300,
                                       legend = 'none'))
  output$dplot <- renderGvis({
    gvisMotionChart(org(dfvadata), idvar = "State", timevar="Year", 
                    yvar= "Value",
                    options = list( state= ststr))
  })
})