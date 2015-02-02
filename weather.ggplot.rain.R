library(ggplot2)
data1<-read.csv("melbourne.csv", header = TRUE)

#Set month
mth <- 7

newdata <- data1[data1$Month %in% mth,]
aggdata <- aggregate(newdata$Rain ~ newdata$Year, data = newdata, sum, na.rm=TRUE)

newnames1 <- c("Year", "Rain")
colnames(aggdata) <- newnames1

#160 for past 2014 moths, 159 for coming 12014 month
aggdata <- aggdata[2:160,]


ggplot(aggdata, aes(Year,Rain)) + 
  geom_point(size = 3, colour = "lightsteelblue4") + 
  geom_smooth() + 
  ylim(0,200)

