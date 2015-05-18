library(shiny)
library(ggplot2)
library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
ghub <- "https://raw.githubusercontent.com/jlaurito/CUNY_IS608/master/"
fileurl <- 
  paste(ghub,"lecture3/data/cleaned-cdc-mortality-1999-2010.csv", sep ="")
data <- getURL(fileurl)
df <- read.csv(text = data, stringsAsFactors = FALSE)

# read local
# df <- read.csv(file = 
#     "C:/Users/Cheryl/Documents/GitHub/markitzero/cdcmort.csv",head=TRUE,sep = ",")
df <- df%>%select(ICD.Chapter, State, Year, Crude.Rate)%>%filter(Year == "2010")

shinyServer(function(input,output){
  
  formulaText <- reactive({
    paste(input$mort)
  })
  
  output$caption <- renderText({
    formulaText()
  })
  
  grph <- function(){
    #flter data for selection
    dth <- input$mort
    
    dfpt <- subset(df, ICD.Chapter == dth,  
                   select=(c("ICD.Chapter","State", "Crude.Rate")))
    dfpt <- dfpt[order(dfpt$Crude.Rate),]
    #graph
    if(nrow(dfpt) == 0){
      print("No Values")
    }else{
#       p <- ggplot(dfpt, aes(x=State, fill = Crude.Rate))+  
#         geom_bar(data=dfpt, aes(y=Crude.Rate), stat="identity")
#       print(p) 
#       
      #show every 5 values
      showval <- rep(NA, nrow(dfpt))
      showval[seq(1,nrow(dfpt),5)] <- dfpt$Crude.Rate[seq(1,nrow(dfpt),5)]
      dfpt$showval <- showval
      dfpt$State <- factor(dfpt$State, levels = dfpt$State[order(dfpt$Crude.Rate)])
      # plot
      A <- ggplot(data=dfpt, aes(x=State, y = Crude.Rate),fill = Crude.Rate) + 
        geom_bar(stat="identity", fill = "darkcyan", colour = "white")+
        coord_flip() +
        ggtitle("2010 Mortality Rate by State")+ 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_blank(), 
              axis.ticks.y=element_blank(),  panel.border = element_blank(),
              axis.title.x = element_blank(),axis.title.y = element_blank()) +
        scale_y_continuous(expand = c(0,0),limits = c(0,max(dfpt$Crude.Rate)+.5)) +
        geom_text(label = dfpt$showval, hjust = -.1, size = 2.5)
      print(A)
    }
  }
  output$dplot <- renderPlot({grph()}, height = 600, width = 400
)
 
})