library(shiny)
library(dplyr)
library(reshape2)
library(RCurl)
library(stringr)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
ghub <- "https://raw.githubusercontent.com/cherylb/markitzero/master/"
fileurl <- 
  paste(ghub,"VA.csv", sep ="")
data <- getURL(fileurl)
df <- read.csv(text = data, stringsAsFactors = FALSE)

#'''''''''''''''''''''''''
# reshape data

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
#'''''''''''''''''''''''''''''''''''''''''''''
# set up UI options

selecttype <- as.list(sort(unique(dfvadata$Description)))
selectgeo <- as.list(c("National",sort(unique(dfvadata$State))))

x <- unlist(selecttype)
names(selecttype) <- x
y <- unlist(selectgeo)
names(selectgeo) <- y

# UI for Veterans detail


shinyUI(pageWithSidebar(
  
  headerPanel("VA Expenditures by Selected State(s) ($'s in 1000's)"),
  
  sidebarPanel(
  
    selectInput("Type", label = h4("Measure: "), 
                choices = selecttype, 
                selected = "AmtperVet"),

    checkboxGroupInput("Geo", 
                       label = h4("Select States"), 
                       choices = selectgeo,
                       selected = "National")
#     br(),
#     
#     helpText(p(("This Shiny app simulates a Web Analytics Dashboard. The objective of 
#                        a dashboard is to display the"),strong("current status of key web metrics"), 
#                ("and arrange it on a single view so the information can be monitored 
#              at a glance.")), 
#              width= 3)
  ),
  
  mainPanel(
    h4(textOutput("caption")),
    tableOutput("dash")
    )
  )
)


