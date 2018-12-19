rm(list=ls())
library(tidyverse)
library(reshape2)


data2 <- read.csv("data/melbourne.csv", as.is=TRUE, header=TRUE)
data2 <- na.omit(data2)

temp <- select(data2, everything())%>%
        filter(Month == 12)%>%
        filter(Day == 25)

temp3 <- temp[,c(5,8,9)]

temp2 <- gather(temp3, Type, Temp, -Year )

plot2 <- ggplot(temp2, aes(x = Year,y = Temp, fill = Type)) + 
        geom_point(size=4, shape = 21, col = "black") + 
        geom_smooth(method=loess) + 
        ggtitle("Christmas Temperatures since 1855\n") +
        scale_color_hue(l=45) +
        ylab("degC") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14))

plot2
  
rain_plot2 <- ggplot(temp, aes(x = Year,y = Rain)) + 
        geom_point(size=4, shape = 21, fill = "cornflowerblue", col = "black") + 
        geom_smooth(method=loess) + 
        ggtitle("Christmas Rain since 1855\n") +
        scale_color_hue(l=45) +
        ylab("mm Rain") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              text = element_text(size = 14))

rain_plot2