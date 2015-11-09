
library(dplyr)
library(ggplot2)
library(reshape2)


data2 <- read.csv("data/melbourne.csv", as.is=TRUE, header=TRUE)
data2 <- na.omit(data2)

max.temp <- select(data2, everything())%>%
  filter(Month == 10 )%>%
        #filter(Month %in% c(6,7,8)) %>%
  filter(Year >=1900 & Year <= 2015)%>%
  group_by(Year)%>%
  summarize(MaxT = mean(Max), MinT = mean(Min))

tall.temp <- melt(max.temp, id.vars="Year")

plot <- ggplot(tall.temp, aes(Year,value, col=variable)) + 
        geom_point(size=3) + 
        geom_smooth(method=loess) + 
        ggtitle("Melbourne's Average Temperature") +
        scale_color_hue(l=45) +
        ylab("degC") 

plot <- plot + theme_bw()

plot <- plot + theme(panel.grid.major = element_line(size = 0.5, color = "grey"), axis.line = element_line(size = 0.7, color = "black"))    
plot
  