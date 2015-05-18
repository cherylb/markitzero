

require(RCurl, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(lubridate, quietly = TRUE)
require(gridExtra, quietly = TRUE)
require(pastecs, quietly = TRUE)
require(scales, quietly = TRUE)
require(ggplot2, quietly = TRUE)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
file <- "https://raw.githubusercontent.com/jlaurito/CUNY_IS608/master/lecture1/data/inc5000_data.csv"
data <- getURL(file)
dfw <- read.csv(text = data)

head(dfw)
summary(dfw)

#-------------------------------------
# Question 1 - total number by state
#-------------------------------------

# Sum by state
dfstate <- dfw%>%group_by(State)%>%
  summarise(totco = n_distinct(Name))%>%
    arrange(desc(totco))


showval <- rep(NA, nrow(dfstate))
showval[seq(1,nrow(dfstate),5)] <- dfstate$totco[seq(1,nrow(dfstate),5)]
dfstate$showval <- showval
dfstate$State <- factor(dfstate$State, levels = dfstate$State[order(dfstate$totco)])

A <- ggplot(data=dfstate, aes(x=State,y=totco)) + 
  geom_bar(stat="identity", fill = "cadetblue", colour = "white")+
  coord_flip() +
  ggtitle("Number of companies by state")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), axis.line = element_blank(), 
               axis.ticks.y=element_blank(),  panel.border = element_blank(),
               axis.title.x = element_blank(),axis.title.y = element_blank()) +
  scale_y_continuous(expand = c(0,0),limits = c(0,max(dfstate$totco)+20)) +
  geom_text(label = dfstate$showval, hjust = -.1, size = 2.5)
print(A)

#-------------------------------------
# Question 2 - Employment by Industry in # 3 State
#-------------------------------------
head(dfstate)
st <- dfstate$State[3]
dfcomp <- dfw[complete.cases(dfw),]

# calculate quantile to exclude outliers
dfQ <- dfcomp%>%filter(State == st)%>%  
  select(State, Industry, Employees) %>%
  group_by(State, Industry)%>%
  summarise(totemp= sum(Employees), count = n(), med = median(Employees),
            IQRe = IQR(Employees), Q1 = quantile(Employees)[2], Q3 = quantile(Employees)[4])%>%
  mutate(lower = Q1 - 1.5 * IQRe, upper = Q3 + 1.5 * IQRe )

# append boundries 
df3 <- dfcomp%>%filter(State == st)%>%  
  select(State, Industry, Employees)
df3$lower <- dfQ$lower[match(df3$Industry, dfQ$Industry)]
df3$upper <- dfQ$upper[match(df3$Industry, dfQ$Industry)]

# Outliers are those with employee count outside lower/upper bounds
dfout <- df3 %>% filter(Employees < lower | Employees > upper)

#calc average excluding outliers
stitle <- paste("Number of Employees by Industry in", st, "State")

dfemp <- df3 %>% filter(Employees >= lower & Employees <= upper) %>%
  group_by(State, Industry)

dfemp$Industry <- factor(dfemp$Industry, levels = dfQ$Industry[order(dfQ$upper)])

B <- ggplot(data = dfemp, aes(x=Industry, y = Employees))+geom_boxplot(fatten = 0, colour = "cadetblue",
          outlier.colour="cadetblue4", outlier.shape=20,outlier.size=2) +
  coord_flip()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                      panel.background = element_blank(), axis.line = element_blank(), 
                      axis.ticks.y=element_blank(),  panel.border = element_blank(),
                      axis.title.x = element_blank(),axis.title.y = element_blank())+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, colour = 'darkcyan') +
  ggtitle(stitle)

print(B)

#-------------------------------------
# Question 3 - Revenue by Employee
#-------------------------------------
dfQ <- dfcomp%>%  
  select(Industry, Employees,Revenue) %>% mutate(rate = Revenue/Employees/1000)%>%
  group_by(Industry)%>%
  summarise(IQRe = IQR(rate), Q1 = quantile(rate)[2], Q3 = quantile(rate)[4])%>%
  mutate(lower = Q1 - 1.5 * IQRe, upper = Q3 + 1.5 * IQRe )

dfrev <- dfcomp%>%
  select(Industry, Employees, Revenue)%>%mutate(rate = Revenue / Employees/1000)%>%
  mutate(Revenue = Revenue / 1000)

dfrev$lower <- dfQ$lower[match(dfrev$Industry, dfQ$Industry)]
dfrev$upper <- dfQ$upper[match(dfrev$Industry, dfQ$Industry)]
#filter outliers
dfrev <- dfrev %>% filter(rate >= lower & rate <= upper)%>%
  arrange(-Revenue)

# arrange factors
dff <- dfrev %>% group_by(Industry) %>% summarise(rate  = mean(rate))%>%
  arrange(-rate)
dff$Industry <- factor(dff$Industry, levels = dff$Industry[order(dff$rate)])

dff<- head(dff,10)

C <- ggplot(data=dff, aes(x=Industry,y = rate)) + 
  geom_bar(stat="identity", fill = "cadetblue", colour = "white")+
 coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(), 
        axis.ticks.y=element_blank(), axis.ticks.x=element_blank(), panel.border = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(hjust = 1),
        axis.title.x = element_blank(),axis.title.y = element_blank()) +
  scale_y_continuous(expand = c(0,0),limits = c(0,max(dff$rate)+50)) +
  geom_text(label = round(dff$rate), hjust = -.1, size = 4)+
  ggtitle(expression(atop("Top 10 Industries Revenue per Employee", atop(italic("$millions"), ""))))
print(C)
                                                                        
