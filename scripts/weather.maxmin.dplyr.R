library('ProjectTemplate')
load.project()

library(reshape2)


data2 <- read.csv("data/kerang.csv", as.is=TRUE, header=TRUE)
data2 <- na.omit(data2)

max.temp <- select(data2, everything())%>%
  #filter(Month == 2)%>%
        filter(Month %in% c(1)) %>%
  #filter(Year >=1900 & Year <= 2016)%>%
  group_by(Year)%>%
  summarize(MaxT = mean(Max), MinT = mean(Min))

tall.temp <- gather(data=max.temp,key=Temp,value=Degree,na.rm=FALSE,MaxT,MinT)

# To tidy up and remove reshape2----------------------------------
#tall.temp2 <- gather(max.temp, Year)

plot <- ggplot(tall.temp, aes(Year,Degree, col=Temp)) + 
        geom_point(size=3) + 
        geom_smooth(method=loess) + 
        ggtitle("Melbourne's Average Temperatures\n") +
        scale_color_hue(l=45) +
        ylab("degC") 

plot <- plot + theme_bw()

plot <- plot + theme(panel.grid.major = element_line(size = 0.5, color = "grey"), axis.line = element_line(size = 0.7, color = "black"))    
plot
  