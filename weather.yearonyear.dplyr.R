melb <- read.csv("data/melbourne.csv", as.is=TRUE, header=TRUE)
library(dplyr)
library(ggplot2)
library(reshape2)

melb <- na.omit(melb)

max.temp1 <- select(melb, Year:Min)%>%
  filter(Year == 2014 )%>%
  group_by(Month)%>%
  summarize(Ave = mean(Max))

barplot(max.temp1$Ave)

max.temp2 <- select(melb, Year:Min)%>%
  filter(Year == 2015 )%>%
  group_by(Month)%>%
  summarize(Ave = mean(Max))

barplot(max.temp2$Ave)

CompYear <- cbind(max.temp1, max.temp2)
colnames(CompYear)[2] <- c("Y2014")
colnames(CompYear)[4] <- c("Y2015")
CompYear <- CompYear[,c(1,2,4)]

CY <- melt(CompYear, id=c("Month"))

ggplot(CY, aes(x = Month, y = value, fill= variable)) + 
  geom_bar(stat = "identity",
           position = "dodge") 
              
