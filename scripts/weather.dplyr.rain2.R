library(tidyverse)
library(reshape2)

data1<-read.csv("data/melbourne.csv", header = TRUE)

rain <- select(data1, everything())%>%
        filter(Year %in% 1855:2018)%>%
        group_by(Year)%>%
        filter(Rain>0)%>%
        summarise(Days =n())

rain2 <- select(data1, everything())%>%
        filter(Year %in% 1855:2018)%>%
        group_by(Year)%>%
        filter(Rain>0)%>%
        summarise(Amt = mean(Rain))


        

plot <- ggplot(rain, aes(Year,Days)) + 
        geom_point(size = 3, colour = "lightsteelblue4") + 
        geom_smooth(method = loess) + 
        #geom_smooth(method = lm) + 
        #ylim(150,300) +
        theme_bw()

plot <- plot +  labs(title="Number of days with rain in Kerang") +
        theme(plot.title = element_text(size=20, face="bold", vjust=1.5, lineheight=1.2))


plot <- plot + theme(panel.grid.major = element_line(size = 0.5, color = "grey"), axis.line = element_line(size = 0.7, color = "black"))    
plot

plot <- ggplot(rain2, aes(Year,Amt)) + 
        geom_point(size = 3, colour = "lightsteelblue4") + 
        geom_smooth(method = loess) + 
        #ylim(150,300) +
        theme_bw()

plot <- plot +  labs(y= "mm rain per rainy day", title="Average rain on a rainy day in Kerang") +
        theme(plot.title = element_text(size=20, face="bold", vjust=1.5, lineheight=1.2))


plot <- plot + theme(panel.grid.major = element_line(size = 0.5, color = "grey"), axis.line = element_line(size = 0.7, color = "black"))    
plot