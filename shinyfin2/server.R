library(shiny)
library(ggplot2)
library(dplyr)
library(googleVis)
library(reshape)
library(RCurl)
library(stringr)

library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
ghub <- "https://raw.githubusercontent.com/cherylb/markitzero/master/"
fileurl <- 
  paste(ghub,"VA.csv", sep ="")
data <- getURL(fileurl)
df <- read.csv(text = data, stringsAsFactors = FALSE)

# #'''''''''''''''''''''''''''''''''''''''''''''
# # Reshape data
# df$State <- as.character(df$State)
# df$State <- sapply(df$State,str_trim)
# 
# df$MedicalGeneral <- df$medcare + df$genopex
# df$OtherExpense <- df$insur + df$const + df$loan
# df$AmtperVet <- df$TotalExpense/df$NumOfVeterans
# 
# df <- df[c(2,3,4,5,6,8,13,15,16,17)]
# 
# dfvadata <- melt(df, id=c("State","Year"))
# names <- c("State", "Year", "Description", "Value")
# names(dfvadata) <- names
# dfvadata <- dfvadata%>%filter(State != 0, Value != 0)
# 
# dfvadata$State <- as.character(dfvadata$State)
# dfvadata$Description <- as.character(dfvadata$Description)
# 
# # Add national total
# addtot <- dfvadata%>%group_by(Year, Description) %>% 
#   summarise(Value = sum(Value))%>%
#   mutate(State = "National")
# 
# addtot$Value[addtot$Description=="AmtperVet"] = 
#   addtot$Value[addtot$Description =="TotalExpense"]/
#   addtot$Value[addtot$Description== "NumOfVeterans"]
# 
# dfvadata <- rbind(dfvadata,addtot[c(4,1,2,3)])
# dfvadata <- dfvadata%>% replace(is.na(.), 0)


#''''''''''''''''''''
#shiny
l <- list()

shinyServer(function(input,output){
  
  formulaText <- reactive({
    paste("Measure: ",input$Type )
  })
  
  output$caption <- renderText({
    formulaText()
  }) 
  
  org <- function(dfvadata){
    #flter data for selection
   type <- input$Type
   geo <- input$Geo
   dfbubble <- dfvadata%>%filter(State %in% geo, 
                                        Description == type)%>%
      arrange(State, Value)%>%mutate(xYear = as.numeric(Year))%>%
      select(State,Year,xYear, Value, Description)
 
    dfline <-dfvadata%>%filter(Description == "NumOfVeterans", 
                                 State %in% geo)%>%
       arrange(Year, Value)%>%mutate(xYear = as.numeric(Year))%>%
       select(State,Year,xYear, Value, Description)
    dfline <- cast(dfline, Year ~ State, value = c('Value'), fun = sum)
  
     dfdist <- dfvadata%>%filter(State %in% geo, State != "National", 
                                 Description != "NumOfVeterans",
                                 Description != "TotalExpense", 
                                 Description != "AmtperVet",
                                 Description != "NumofPatients")%>%
       select(Year,Description,Value)%>% group_by(Description,Year)%>%
       summarize(Value = sum(Value))%>%mutate(Year = as.character(Year))
   
      dfstep <- cast(dfdist, Year ~ Description, value = c('Value'), fun = sum)
      dfstep <- dfstep%>%select(Year,OtherExpense,
                             Education,Compensation,MedicalGeneral)
   
      notstate <- c("DC", "District of Columbia", 
                    "National", "Guam", "Puerto Rico")
      dfmap <-  subset(dfvadata, !(State %in% notstate))%>%
     filter(Description == "AmtperVet")%>%
     select(State, Value)%>%group_by(State)%>%
     summarise(AmtPerVeteran = mean(Value))
   
   
   l <- list(dfbubble, dfline,dfstep,dfmap)
  
    return(l)
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
    chtdata <- org(dfvadata)
    bubble <- gvisMotionChart(chtdata[[1]], idvar = "State", timevar="xYear", 
                    yvar= "Value",
                    options = list(state= ststr,
                                   width = 600,height =525))
            
  
    lines <-gvisLineChart(chtdata[[2]], xvar="Year", yvar=names(chtdata[[2]][-1]), 
                          options = list(title="Veteran Population 1999 - 2013",
                                         vAxis="{title: 'Number of Veterans'}",
                                         width= 450, height=275,
                                          legend = "{position: 'bottom',textStyle: {color: 'blue', fontSize: 8}}"))
    
    steps <- gvisSteppedAreaChart(chtdata[[3]],xvar="Year",
                                  names(chtdata[[3]][2:5]),
                                  options=list(isStacked=TRUE,
                                               title="VA Expense Distribution (1000's)",
                                               width=450, height=250,
                                          legend = "{position: 'bottom', 
                                            textStyle: {color: 'blue', fontSize: 8}}"
                                               ))
                                               
       lineandstep <- gvisMerge(lines,steps,horizontal = FALSE)
#     
    allcharts <- gvisMerge(bubble, lineandstep,horizontal = TRUE)
    allcharts
  })
output$maptastic <- renderGvis({
  chtdata <- org(dfvadata)[[4]]
  mappy <- gvisGeoChart(chtdata, "State", "AmtPerVeteran",
                        options=list(region="US", displayMode="regions", 
                                     resolution="provinces",
                                     width=900, height=440))
  mappy
})
  
})
