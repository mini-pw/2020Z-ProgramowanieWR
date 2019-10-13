library('lattice')
library('ggplot2')
library(scales)
library(plyr)
library(forcats)
library(dplyr)

Accidents <- read.csv2('Accidents0515.csv',sep=',')
Vehicles <- read.csv2('Vehicles0515.csv', sep=',')
allData <- inner_join(Accidents, Vehicles, by='ď.żAccident_Index')

backup <- allData

temp <- allData
format(as.POSIXct(temp$Time, format="%y/%m/%d %H:%M:%S %Z"), format = '%H')
temp$Time <- as.numeric(temp$Time)

format(as.POSIXct(allData$Time, format="%y/%m/%d %H:%M:%S %Z"), format = '%H:%M')



allData <- allData[(allData$Road_Type != -1 & allData$Road_Type != 9),]

allData <- allData[(allData$Age_of_Driver != -1),]



allData$Road_Type <- mapvalues(allData$Road_Type, 
                               from=c(1,2,3,6,7,12), 
                               to=c("Roundabout","One way street","Dual carriageway","Single carriageway","Slip road","One way street/Slip road"))


##1Histogram

#g1

g1 <- ggplot(allData, aes(Number_of_Casualties)) + geom_histogram(binwidth = 0.5) +
 facet_wrap(~Speed_limit) + xlim(0,5) + ggtitle("Number of Casualties by speed limit")

g1

#L1


  l1 <- histogram(~Number_of_Casualties | factor(Speed_limit), data = allData, xlim = c(0,5),
                  main = 'Number of Casualties by speed limit',type = 'count')
l1

#b1

b1 <- c(hist(allData[allData$Speed_limit == 0,]$Number_of_Casualties),
       hist(allData[allData$Speed_limit == 10,]$Number_of_Casualties),
       hist(allData[allData$Speed_limit == 20,]$Number_of_Casualties),
       hist(allData[allData$Speed_limit == 30,]$Number_of_Casualties),
       hist(allData[allData$Speed_limit == 40,]$Number_of_Casualties),
       hist(allData[allData$Speed_limit == 50,]$Number_of_Casualties),
       hist(allData[allData$Speed_limit == 60,]$Number_of_Casualties),
       hist(allData[allData$Speed_limit == 70,]$Number_of_Casualties))

##2Boxplot



allData[allData$Sex_of_Driver == 1,]$Sex_of_Driver = "Male"
allData[allData$Sex_of_Driver == 2,]$Sex_of_Driver = "Female"
allData  <- allData[allData$Sex_of_Driver == "Male" | allData$Sex_of_Driver == "Female",]

#g2

g2 <- ggplot(data = allData, aes(y=Age_of_Driver, x = factor(Sex_of_Driver))) + geom_boxplot() +
  labs(x = 'Płeć', y='Wiek', title = "Age and sex of drivers that caused accident")
  
g2

#l2

l2 <- bwplot(Age_of_Driver ~ Sex_of_Driver, data = allData)

l2


#b2

b2 <- boxplot(Age_of_Driver ~ Sex_of_Driver, data = allData)


##3Timeplot
library(scales)
format(as.POSIXct(allData$Time, format="'%H:%M'"), format = '%H')
head(allData$Time)


#g3

temp$Time <- temp$Time/60

g3 <- ggplot(data = temp, aes(x=Time)) + geom_density() + scale_y_continuous(breaks = NULL) +
  labs(title = "When most of accidents happen") + xlim(0,24) + xlab("Time in [h]") + scale_x_continuous(breaks = 1:24)

g3

head(backup$Time)

#l3

temp <- allData
format(as.POSIXct(temp$Time, format="%y/%m/%d %H:%M:%S %Z"), format = '%H')
temp$Time <- as.numeric(temp$Time)

l3 <- densityplot(~Time,data=temp,
                  xlab="Time",
                  main="When most of accidents happen",
                  plot.points=FALSE)

l3

#b3

b3 <- density(na.omit(temp$Time))

plot(b3)


##4barplot
allData <- allData[(allData$Road_Type != -1 | allData$Road_Type != 9),]

allData$Road_Type <- mapvalues(allData$Road_Type, 
                               from=c(1,2,3,6,7,12), 
                               to=c("Roundabout","One way street","Dual carriageway","Single carriageway","Slip road","One way street/Slip road"))


##g4



g4 <- ggplot(allData, aes(x=forcats::fct_infreq(Road_Type, ordered=FALSE))) + 
  geom_bar(stat="count", width=.5, fill="tomato3") + 
  labs(title="Accidents by Road type") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))  + xlab("Road Type") + ylab("")

g4


##l4

l4 <- barchart(allData$Road_Type)

l4


##b4


counts <- table(allData$Road_Type)
b4 <- barplot(counts, main="Accidents by Road type",
        xlab="Road Type")


##5ScatterPlot

#g5

g5 <- ggplot(allData[c(1:1000),], aes(x=Age_of_Driver, y=Age_of_Vehicle)) + 
  geom_point(aes(col=Speed_limit, size=Number_of_Casualties)) + labs(title = "Age of Driver, Age of Vehicle and Speed limit by Number of Casulaties")

g5



#l5


l5 <- xyplot(Age_of_Vehicle ~ Age_of_Driver , data = allData[c(1:1000),])

l5


#b5

b5 <- plot(allData[c(1:1000),]$Age_of_Driver, allData[c(1:1000),]$Age_of_Vehicle)


ggplot(data = resultsDataFrame, aes(x = reorder(results.expr, results.time),y = results.time,fill = names)) +
  geom_bar(stat = "identity") + labs(x = "Plot type", y = "Time in [ms] ") + scale_y_continuous(labels = comma) + coord_flip() 

results <- results2[order(results2$expr),]
results$time <- results$time/10^6

results2 <- microbenchmark(unit = "s", times = 1L,
                          g1 = ggplot(allData, aes(Number_of_Casualties)) + geom_histogram(binwidth = 0.5) +
                            facet_wrap(~Speed_limit) + xlim(0,5) + ggtitle("Number of Casualties by speed limit"),
                          l1 = histogram(~Number_of_Casualties | factor(Speed_limit), data = allData, xlim = c(0,5),
                                         main = 'Number of Casualties by speed limit',type = 'count'),
                          b1 = c(hist(allData[allData$Speed_limit == 0,]$Number_of_Casualties),
                                 hist(allData[allData$Speed_limit == 10,]$Number_of_Casualties),
                                 hist(allData[allData$Speed_limit == 20,]$Number_of_Casualties),
                                 hist(allData[allData$Speed_limit == 30,]$Number_of_Casualties),
                                 hist(allData[allData$Speed_limit == 40,]$Number_of_Casualties),
                                 hist(allData[allData$Speed_limit == 50,]$Number_of_Casualties),
                                 hist(allData[allData$Speed_limit == 60,]$Number_of_Casualties),
                                 hist(allData[allData$Speed_limit == 70,]$Number_of_Casualties)),
                          g2 = ggplot(data = allData, aes(y=Age_of_Driver, x = factor(Sex_of_Driver))) + geom_boxplot() +
                            labs(x = 'Płeć', y='Wiek', title = "Age and sex of drivers that caused accident"),
                          l2 = bwplot(Age_of_Driver ~ Sex_of_Driver, data = allData),
                          b2 = boxplot(Age_of_Driver ~ Sex_of_Driver, data = allData),
                          g3 =  ggplot(data = allData, aes(x=Time)) + geom_density() + scale_y_continuous(breaks = NULL) +
                            labs(title = "When most of accidents happen")
                          ,
                          l3 = densityplot(~Time,data=temp,
                                           xlab="Time",
                                           main="When most of accidents happen",
                                           plot.points=FALSE),
                          b3 = density(na.omit(temp$Time)),
                          g4 = ggplot(allData, aes(x=forcats::fct_infreq(Road_Type, ordered=FALSE))) + 
                            geom_bar(stat="count", width=.5, fill="tomato3") + 
                            labs(title="Accidents by Road type") + 
                            theme(axis.text.x = element_text(angle=65, vjust=0.6))  + xlab("Road Type") + ylab(""),
                          l4 = barchart(allData$Road_Type),
                          
                          b4 ={counts = table(allData$Road_Type)
                          barplot(counts, main="Accidents by Road type",
                                  xlab="Road Type")},
                          g5 = ggplot(allData[c(1:1000),], aes(x=Age_of_Driver, y=Age_of_Vehicle)) + 
                            geom_point(aes(col=Speed_limit, size=Number_of_Casualties)) + labs(title = "Age of Driver, Age of Vehicle and Speed limit by Number of Casulaties"),
                          l5 = xyplot(Age_of_Vehicle ~ Age_of_Driver , data = allData[c(1:1000),]),
                          b5 = plot(allData[c(1:1000),]$Age_of_Driver, allData[c(1:1000),]$Age_of_Vehicle)
                          
                          
)
