

# load data
# what columns do we  need? 
# Q1 :  Borough, YearBuilt, YearAlter1  YearAlter2, CD, BuiltCode
# Q2 : Total Buidling Floor Area, Building Class, Number of Floors
# Q3 : AssessTot
# Datayear = 2014
library(dplyr)
library(bigvis)
library(ggplot2)

f <- "C:/Users/Cheryl/Documents/pluto/BK.csv"
kols <- c("Borough","YearBuilt", "BuiltCode","CD", "LtdHeight", "BldgArea",
       "NumFloors", "BuiltCode","AssessTot","LtdHeight")

df <- read.csv(file = f)[,kols]
f <- "C:/Users/Cheryl/Documents/pluto/BX.csv"
df <- rbind(df, read.csv(file =f) [,kols])
f <- "C:/Users/Cheryl/Documents/pluto/MN.csv"
df <- rbind(df, read.csv(file =f) [,kols])
f <- "C:/Users/Cheryl/Documents/pluto/QN.csv"
df <- rbind(df, read.csv(file =f) [,kols])
f <- "C:/Users/Cheryl/Documents/pluto/SI.csv"
df <- rbind(df, read.csv(file =f) [,kols])  

#Question 1:  Building constrution
df1 <- df%>%select(YearBuilt, BuiltCode)
df1 <- (df1[complete.cases(df1),])%>%
  filter(YearBuilt != 0, YearBuilt < 2016, YearBuilt > 1850)
#df1a <- df1%>%group_by(Borough, YearBuilt, BuiltCode)%>%summarise(count = n())
df1E <- df1%>%filter(BuiltCode == "E")%>% group_by(YearBuilt,BuiltCode)%>%
  summarise(count = n())
dfyikes <- df1 %>%group_by(YearBuilt,BuiltCode)%>%summarise(count = n())%>%
  filter(count > 22000)
dfyikes2 <- df1 %>%group_by(YearBuilt)%>%summarise(count = n())%>%
  filter(count > 22000)


# looking at data

#(m1 <- qplot(data = df1a, x = YearBuilt, y = count, colour = BuiltCode, facets = ~ BuiltCode))
(m2 <- qplot(data = df1E, x = YearBuilt, y = count))


# BuiltCode = E indicates the year is an estimate for the year it was built, 
# accounts for all of the 16 outlier years
# where buildings > 22000

v <- dfyikes2$YearBuilt
dfbad <- df1[(df1$YearBuilt %in% v),]%>%
  group_by(YearBuilt,BuiltCode)%>%summarise(count=n())
(m3 <- qplot(data = dfbad, x = YearBuilt, y = count, colour = BuiltCode))
df1 <- df1%>%select(YearBuilt)
df2 <- df1%>%filter(!(YearBuilt %in% v))


# what cutoff year do we want, keep 90% of buildings greater, year when cum total > s
dfcalc2 <- df2%>%group_by(YearBuilt)%>%summarise(count = n())
s <- sum(dfcalc2$count)*.1
dfcalc2 <- dfcalc2%>%arrange(YearBuilt)%>%mutate(RunSum = cumsum(count))
t2 <- max((dfcalc2%>%filter(RunSum < s))$YearBuilt)  # this is the line at 10%

dfcalc1 <- df1%>%group_by(YearBuilt)%>%summarise(count = n())
s <- sum(dfcalc1$count)*.1
dfcalc1 <- dfcalc1%>%arrange(YearBuilt)%>%mutate(RunSum = cumsum(count))
t1 <- max((dfcalc1%>%filter(RunSum < s))$YearBuilt)  # this is the line at 10%


# summarise for graph - not needed for this view
#myd <- condense(bin(df2$YearBuilt, 1), summary=("count"))
#myds <- smooth(myd, 50, var=".count", type="robust")

# graph shows cummulative # of buildings by year, with 
# green =adjusted data exluding the years with extreme estimates, exluding out years: <1850 and > 2016
# blue = data only exluding out years: <1850 and > 2016


p <- ggplot()+
  stat_bin(data = df2, binwidth = 10, 
           aes(x = YearBuilt,y=cumsum(..count..)),geom="line",color="green")+
  stat_bin(data = df1, binwidth = 10,
           aes(x = YearBuilt,y=cumsum(..count..)),geom="line",color="blue")+
  scale_y_log10() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(), 
        axis.ticks.y=element_blank(),  panel.border = element_blank(), axis.title.x = element_blank())+
  ylab("log(count)")+
  ggtitle("Number of buildings by year built")
p <- p + geom_vline(xintercept = t1, colour = "blue") +
  geom_vline(xintercept = t2, colour = "green")

yl<- c( max(dfcalc1$RunSum)*1.18,
        median(dfcalc2$RunSum)*.95,
        3000, 2000)
txt <- c("Includes extreme estimate yrs",
         "Adjusted to exclude extreme estimates", 
         paste("> 90% after", t1),
         paste("> 90% after", t2, "(adjusted)"))
xl <- c(1960, 2000, t1+17,  t2+18)

(p <- p + geom_text(data=NULL,
              aes(x=xl[1], y=yl[1], label=txt[1]), color="blue4", size = 3) + 
  geom_text(data=NULL,
              aes(x=xl[2], y=yl[2], label=txt[2]), color="green4", size = 3) + 
  geom_text(data=NULL,
              aes(x=xl[3], y=yl[3], label=txt[3]), color="blue4", size = 3) +
  geom_text(data=NULL,
              aes(x=xl[4], y=yl[4], label=txt[4]), color="green4", size = 3))

####################################


### Question 2 - floors in buildings
dft <- df%>%select(YearBuilt,NumFloors )%>%arrange(-NumFloors)%>%
  filter(YearBuilt != 0, YearBuilt < 2016, YearBuilt > 1850, NumFloors > 0)

dfx <- dft%>%group_by(NumFloors,YearBuilt)%>%summarise(count = n())

dft$floors<-cut(dft$NumFloors, c(0,10,20,30,40,50,60,80,max(dft$NumFloors)))
n <- c("YearBuilt", "NumFloors", "floors")
names(dft) = n
# frequencyt plots
(q <- ggplot(dft)+ 
  geom_freqpoly(size=1, bin=10, aes(x=YearBuilt, colour=floors))+ facet_grid(.~floors)+
  scale_y_log10()+
  theme(panel.background = element_blank(), axis.line = element_blank(), 
        axis.ticks.y=element_blank(),  panel.border = element_blank(), 
        axis.title.x = element_blank())+
  ylab("log(count)")+
  ggtitle("Number of Buildings by Year, by Number of Floors"))

####################################


### Question 3 - Value per floor
df3 <- df%>%select(YearBuilt,NumFloors, AssessTot )%>%
  arrange(-NumFloors)%>%
  filter(YearBuilt != 0, YearBuilt < 2016, YearBuilt > 1850, 
         NumFloors > 0, AssessTot != 0)%>%
  mutate(val = AssessTot/NumFloors)%>%
  filter(val < 200000000)
  # filter out extreme years, zero vals for floors, extreme flor val
  
m <- sum(df3$AssessTot)/nrow(df3)
w <- df3%>%filter(YearBuilt >1940, YearBuilt <1946)
wm <- sum(w$AssessTot)/nrow(w)
df3 <- df3 %>% group_by(YearBuilt)%>%
  summarise(count = n(),val =sum(val)/count, Num = log10(count))

c <- ggplot(data = df3, aes(x = YearBuilt, y = val/1000, fill = Num)) 
c <- c + geom_bar(stat = "identity") + 
  geom_hline(aes(yintercept= wm/1000),colour = "red")+ 
  geom_hline(aes(yintercept = m/1000),colour = "orange")+ 
  ggtitle("Avg Value per Floor by Year")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(), 
        axis.ticks.y=element_blank(),  panel.border = element_blank(), 
        axis.title.x = element_blank())+
  ylab("Avg Val 1000's") + geom_vline(xintercept = 1941, colour = "azure2")+
  geom_vline(xintercept = 1945, colour = "azure2")

yl<- c(250, 500, 1500)
xl <- c(1990, 1995, 1955)
                                            
txt <- c(paste("WWII period avg", round(wm/1000,0)),
         paste("Overall Avg value/floor",round(m/1000, 0)),
         "WWII period")

(c <- c + geom_text(data=NULL,
              aes(x=xl[1], y=yl[1], label=txt[1]), color="red", size = 3) +
  geom_text(data=NULL,
            aes(x=xl[2], y=yl[2], label=txt[2]), color="orange", size = 3) +
  geom_text(data=NULL,
            aes(x=xl[3], y=yl[3], label=txt[3]), color="azure3", size = 3))
  
