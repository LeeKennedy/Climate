data1<-read.csv("data/melbourne.csv", header = TRUE)
#data1<-read.csv("melbourne.csv", header = TRUE)
#aggregate(data$Max_Temp ~ data$Month, FUN = mean)
#aggregate(cbind(data$Month, data$Max_Temp) ~ data$Year, FUN = mean)


#mth = number, c(vector) or range x:y
mth <- 2
#mth <- c(12,1,2)
par(mfcol = c(1,1))

newdata2 <- data1[data1$Month %in% mth,]
aggdata2 <- aggregate(newdata2$Max ~ newdata2$Year, data = newdata2, mean, na.rm=TRUE)
newnames2 <- c("Year2", "Max2")
colnames(aggdata2) <- newnames2

# For months (159 = 2013)
aggdata2 <- aggdata2[2:160,]

# For Years
#aggdata2 <- aggdata2[2:159,]

#Max pre 2000 temperature
mt <- max(aggdata2$Max2[2:146])

library(ggplot2)
ggplot(aggdata2, aes(Year2,Max2)) + 
  geom_point() + 
  geom_smooth() + 
  geom_hline(yintercept = mt, color="red") +
  geom_vline(xintercept = 2015, color="tomato1", linetype = "dashed") +
  geom_vline(xintercept = 2000, color="darkgreen", linetype = "solid") +
  ggtitle("Melbourne's Average Temperature") + 
  annotate("text", label = "Highest pre-2000 yearly average", x = 1900, y = 22) + 
  annotate("text", label = "2015", x = 2010, y = 24) + 
  annotate("text", label = "2000", x = 1995, y = 24) + 
  scale_x_continuous("Year",limits=c(1855,2020), breaks=seq(1855,2020, 50)) 
  #scale_y_continuous("Average Temperature, ?C",limits=c(10,25), breaks=seq(10,25, 2))

