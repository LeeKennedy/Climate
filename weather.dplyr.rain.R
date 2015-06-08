library(ggplot2)
library(dplyr)
data1<-read.csv("data/melbourne.csv", header = TRUE)

rain <- select(data1, everything())%>%
        filter(Year %in% 1900:2014)%>%
        #filter(Month %in% c(5))%>%
        group_by(Year)%>%
        summarise(raintot = sum(Rain))
        

ggplot(rain, aes(Year,raintot)) + 
        geom_point(size = 3, colour = "lightsteelblue4") + 
        geom_smooth(method = loess) + 
        ylim(0,1.2*max(na.omit(rain$raintot))) 


rain3 <- select(data1, everything())%>%
        group_by(Year, Month)%>%
        filter(!is.na(Rain))%>%
        summarise (Sum = sum(Rain))

rain4 <- group_by(rain3, Month)%>%
        summarise(Mean = mean(Sum))
rain4

