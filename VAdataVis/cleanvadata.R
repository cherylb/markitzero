require(XLConnect)
require(dplyr)
require(reshape)

#### Collect Data
# Notes about the data: most data is pulled from a series of excel data files
# compiled by the US VA and available here: http://www.va.gov/vetdata/Expenditures.asp
# the excel data was in a semi-regular format,which evolved over time
# Missing population data for 2000 was found using the US 2000 Cenus data 
# state codes from data set Decennial/Summary File 3/2000/Summary File 3
###

fromexcel <- function(){
  filepath <- "C:/Users/Cheryl/Documents/GitHub/markitzero/VAdataraw/"
  sheetname <- "State Level Expenditures"
  sheetname2 <- "State Leve Expenditures"
  
  filename <- "GDX_FY10V14.xls"
  fullfile <- paste(filepath,filename, sep = "")
  data <- readWorksheetFromFile(fullfile, sheet = sheetname2, startRow = 6, endRow = 58,
                               startCol = 1, endCol = 11, header = FALSE)
  
  
  year <- rep(2010,nrow(data))
  data$year <- year
  dfall <- data
  ####
  
  filename <- "GDX_FY11.xls"
  fullfile <- paste(filepath,filename, sep = "")
  data = readWorksheetFromFile(fullfile, sheet = sheetname2, startRow = 6, endRow = 58,
                               startCol = 1, endCol = 11, header = FALSE)
  
  year <- rep(2011,nrow(data))
  data$year <- year
  dfall <- rbind(dfall,data)
  ###
  
  filename <- "GDX_FY12_V1.xlsx"
  fullfile <- paste(filepath,filename,sep = "")
  data = readWorksheetFromFile(fullfile, sheet = sheetname, startRow = 6, endRow = 58,
                               startCol = 1, endCol = 11, header = FALSE)
  
  year <- rep(2012,nrow(data))
  data$year <- year
  dfall <- rbind(dfall,data)
  ###
  
  filename <- "GDX_FY13.xlsx"
  fullfile <- paste(filepath,filename,sep ="")
  data = readWorksheetFromFile(fullfile, sheet = sheetname, startRow = 6, endRow = 58,
                               startCol = 1, endCol = 11, header = FALSE)
  
  year <- rep(2013,nrow(data))
  data$year <- year
  dfall <- rbind(dfall,data)
  ###
  filename <- "GDX_FY09_2.xls"
  fullfile <- paste(filepath,filename, sep = "")
  data = readWorksheetFromFile(fullfile, sheet = sheetname2, startRow = 6, endRow = 58,
                               startCol = 1, endCol = 11, header = FALSE)
  
  year <- rep(2009,nrow(data))
  data$year <- year
  dfall <- rbind(dfall,data)
  ###
  
  filename <- "GDX_FY08_V2.xls"
  fullfile <- paste(filepath,filename, sep = "")
  data = readWorksheetFromFile(fullfile, sheet = sheetname2, startRow = 6, endRow = 58,
                               startCol = 1, endCol = 11, header = FALSE)
  
  year <- rep(2008,nrow(data))
  data$year <- year
  dfall <- rbind(dfall,data)
  ###
  
  filename <- "GDX_FY07_Rev_090401.xls"
  fullfile <- paste(filepath,filename, sep = "")
  data = readWorksheetFromFile(fullfile, sheet = sheetname, startRow = 6, endRow = 58,
                               startCol = 1, endCol = 11, header = FALSE)
  
  year <- rep(2007,nrow(data))
  data$year <- year
  dfall <- rbind(dfall,data)
  ###
  
  filename <- "GDX_FY06_Rev_090409.xls"
  fullfile <- paste(filepath,filename, sep = "")
  data = readWorksheetFromFile(fullfile, sheet = sheetname, startRow = 6, endRow = 58,
                               startCol = 1, endCol = 10, header = FALSE)
  
  data$Col11 <- rep(0,nrow(data))
  year <- rep(2006,nrow(data))
  data$year <- year
  dfall <- rbind(dfall,data)
  
  ###
  
  
  ### Year 2000 is missing vet population
  sheetname <- "State Totals"
  
  filename <- "W-GDX-FY05-000-.xls"
  fullfile <- paste(filepath,filename, sep = "")
  data = readWorksheetFromFile(fullfile, sheet = sheetname, startRow = 9, endRow = 60,
                               startCol = 1, endCol = 8, header = FALSE)
  
  data$Col9 <- rep(0,nrow(data)) # missing columns
  data$Col10 <- rep(0,nrow(data))
  data$Col11 <- rep(0,nrow(data))
  
  data <- data[,c(1,2,3,4,7,5,9,8,6,10,11)] #reorder cols
  
  year <- rep(2005,nrow(data))
  data$year <- year
  names(data) <- names(dfall)
  dfall <- rbind(dfall,data)
  
  
  ###
  
  filename <- "GDX-FY04-000-Final.xls"
  fullfile <- paste(filepath,filename, sep = "")
  data = readWorksheetFromFile(fullfile, sheet = sheetname, startRow = 9, endRow = 60,
                               startCol = 1, endCol = 11, header = FALSE)
  
  data <- data[,c(1,2,3,4,7,5,10,8,6,11,9)] #reorder cols
  
  year <- rep(2004,nrow(data))
  data$year <- year
  names(data) <- names(dfall)
  dfall <- rbind(dfall,data)
  ###
  
  sheetname <- "State Totals"
  
  filename <- "GDX-F-000-JAN-VP03-FY2003-re.xls"
  fullfile <- paste(filepath,filename, sep = "")
  data = readWorksheetFromFile(fullfile, sheet = sheetname, startRow = 9, endRow = 60,
                               startCol = 1, endCol = 11, header = FALSE)
  
  data <- data[,c(1,2,3,4,7,5,10,8,6,11,9)] #reorder cols
  
  year <- rep(2003,nrow(data))
  data$year <- year
  names(data) <- names(dfall)
  dfall <- rbind(dfall,data)
  
  ###
  sheetname <- "STATE TOTALS"
  
  filename <- "GDX-Hybrid-FY2002-RPD.xls"
  fullfile <- paste(filepath,filename, sep = "")
  data = readWorksheetFromFile(fullfile, sheet = sheetname, startRow = 9, endRow = 60,
                               startCol = 1, endCol = 11, header = FALSE)
  
  data <- data[,c(1,2,3,4,7,5,10,8,6,11,9)] #reorder cols
  
  year <- rep(2002,nrow(data))
  data$year <- year
  names(data) <- names(dfall)
  dfall <- rbind(dfall,data)
  
  ###
  sheetname <- "STATE TOTALS"
  
  filename <- "WEB-2-GDX-FY2001.xls"
  fullfile <- paste(filepath,filename, sep = "")
  data = readWorksheetFromFile(fullfile, sheet = sheetname, startRow = 9, endRow = 60,
                               startCol = 1, endCol = 11, header = FALSE)
  
  data <- data[,c(1,2,3,4,7,5,10,8,6,11,9)] #reorder cols
  
  year <- rep(2001,nrow(data))
  data$year <- year
  names(data) <- names(dfall)
  dfall <- rbind(dfall,data)
  
  ###
  sheetname <- "STATE TOTALS"
  
  filename <- "GDX-FY2000-NOVETPOP-ALL.xls"
  fullfile <- paste(filepath,filename, sep = "")
  data = readWorksheetFromFile(fullfile, sheet = sheetname, startRow = 9, endRow = 60,
                               startCol = 1, endCol = 11, header = FALSE)
  
  data <- data[,c(1,2,3,4,7,5,10,8,6,11,9)] #reorder cols
  
  year <- rep(2000,nrow(data))
  data$year <- year
  names(data) <- names(dfall)
  dfall <- rbind(dfall,data)
  
  ###
  
  filename <- "GDX99-VETPOP-99-ALL.xls"
  fullfile <- paste(filepath,filename, sep = "")
  data = readWorksheetFromFile(fullfile, sheet = sheetname, startRow = 9, endRow = 60,
                               startCol = 1, endCol = 11, header = FALSE)
  
  data <- data[,c(1,2,3,4,7,5,10,8,6,11,9)]#reorder cols
  
  year <- rep(1999,nrow(data))
  data$year <- year
  names(data) <- names(dfall)
  dfall <- rbind(dfall,data)
  
  dfall <- as.data.frame(dfall)
}

inflation<- function(dfall){
  filepath <- "C:/Users/Cheryl/Documents/GitHub/markitzero/VAdataraw/"
  sheetname <- "BLS"
  
  filename <- "Inflation Rates.xlsx"
  fullfile <- paste(filepath,filename, sep = "")
  data <- readWorksheetFromFile(fullfile, sheet = sheetname, startRow = 1, endRow = 16,
                                startCol = 1, endCol = 2, header = TRUE)
  dfall2 <- merge(x = dfall, y = data, by.x = "Year", by.y = "Year", all.x=TRUE)
  dfall2 <- cbind(dfall2[,1:3], dfall2[,4:13]*dfall2$rate)
  return(dfall2)
}

dfall <- fromexcel()
names(dfall) <- c("State", "NumOfVeterans", "TotalExpense", 
                  "Compensation", "const", "Education","loan",
                  "genopex", "insur", "medcare", "NumofPatients","Year")
dfbackup <- dfall
dfall <- dfall %>% replace(is.na(.), 0)
dfall <- inflation(dfall)
dfall <- dfall %>% replace(is.na(.), 0)

#'''''''''''''''''''''''''''''''''''''''''''''
# Reshape data
dfall$State <- as.character(dfall$State)
dfall$State <- sapply(dfall$State,str_trim)

dfall$MedicalGeneral <- dfall$medcare + dfall$genopex
dfall$OtherExpense <- dfall$insur + dfall$const + dfall$loan
dfall$AmtperVet <- dfall$TotalExpense/dfall$NumOfVeterans

dfall <- dfall[c(1,2,3,4,5,7,12,14,15,16)]

dfvadata <- melt(dfall, id=c("State","Year"))
names <- c("State", "Year", "Description", "Value")
names(dfvadata) <- names
dfvadata <- dfvadata%>%filter(State != 0, Value != 0)

dfvadata$State <- as.character(dfvadata$State)
dfvadata$Description <- as.character(dfvadata$Description)

# Add national total
addtot <- dfvadata%>%group_by(Year, Description) %>% 
  summarise(Value = sum(Value))%>%
  mutate(State = "National")

addtot$Value[addtot$Description=="AmtperVet"] = 
  addtot$Value[addtot$Description =="TotalExpense"]/
  addtot$Value[addtot$Description== "NumOfVeterans"]

dfvadata <- rbind(dfvadata,addtot[c(4,1,2,3)])
dfvadata <- dfvadata%>% replace(is.na(.), 0)



# save externally
write.csv(dfvadata, "markitzero/VA.csv")

