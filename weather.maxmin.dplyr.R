melb <- read.csv("data/melbourne.csv", as.is=TRUE, header=TRUE)
library(dplyr)
library(ggplot2)
library(reshape2)



melb <- na.omit(melb)

max.temp <- select(melb, everything())%>%
  filter(Month == 5 )%>%
  #filter(Year >= 1954)%>%
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
  