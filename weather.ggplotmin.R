data1<-read.csv("data/melbourne.csv", header = TRUE)
#data1<-read.csv("melbourne.csv", header = TRUE)
#aggregate(data$Max_Temp ~ data$Month, FUN = mean)
#aggregate(cbind(data$Month, data$Max_Temp) ~ data$Year, FUN = mean)

#mth = number, c(vector) or range x:y
mth <- 2
#mth <- c(12,1,2)
par(mfcol = c(1,1))

newdata1 <- data1[data1$Month %in% mth,]
aggdata1 <- aggregate(newdata1$Min ~ newdata1$Year, data = newdata1, mean, na.rm=TRUE)
newnames1 <- c("Year1", "Min1")
colnames(aggdata1) <- newnames1

# For months (159 = 2013)
#aggdata1 <- aggdata1[2:160,]

# For Years
aggdata1 <- aggdata1[2:159,]

# Min Temp pre 2000
mt <- max(aggdata1$Min1[2:146])

library(ggplot2)
#ggplot(aggdata1, aes(Year1,Min1)) + geom_point() + geom_smooth() + ylim(5,15)
ggplot(aggdata1, aes(Year1,Min1)) + 
  geom_point() + 
  geom_smooth() + 
  geom_hline(yintercept = mt, color="red") +
  geom_vline(xintercept = 2015, color="tomato1", linetype = "dashed") +
  geom_vline(xintercept = 2000, color="darkgreen", linetype = "solid") +
  ggtitle("Melbourne's Average Minimum Temperature") + 
  annotate("text", label = "Highest pre-2000 minimum", x = 1900, y = 12.5) + 
  annotate("text", label = "2015", x = 2010, y = 16) + 
  annotate("text", label = "2000", x = 1995, y = 16) + 
  scale_x_continuous("Year",limits=c(1855,2020), breaks=seq(1855,2020, 50)) 
  #scale_y_continuous("Average Temperature, ?C",limits=c(5,17), breaks=seq(5,17,2))

