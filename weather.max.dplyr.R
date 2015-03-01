melb <- read.csv("data/melbourne.csv", as.is=TRUE, header=TRUE)
library("dplyr")
library("ggplot2")

melb <- na.omit(melb)

max.temp <- select(melb, Year:Min)%>%
  #filter(Month == 10 )%>%
  group_by(Year)%>%
  summarize(Ave = mean(Min))

#max temp pre-2000
mt <- max(max.temp$Ave[2:146], na.rm=TRUE)

ggplot(max.temp, aes(Year,Ave)) + 
  geom_point() + 
  geom_smooth() + 
  geom_hline(yintercept = mt, color="red") +
  geom_vline(xintercept = 2000, color="darkgreen", linetype = "dashed") +
  ggtitle("Melbourne's Average Temperature") + 
  annotate("text", label = "Highest pre-2000 yearly average", x = 1900, y = mt*1.05) + 
  annotate("text", label = "2000", x = 1995, y = mt*1.05) + 
  scale_x_continuous("Year",limits=c(1855,2020), breaks=seq(1855,2020, 50)) +
  scale_y_continuous("Average Temperature, ?C",limits=c(0.5*mt,mt*1.1), breaks=seq(0.5*mt,mt*1.1, 2))

