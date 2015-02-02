melb <- read.csv("melbourne.csv", as.is=TRUE, header=TRUE)
library("dplyr")
library("ggplot2")

melb <- na.omit(melb)

max.temp <- select(melb, Year:Min)%>%
  filter(Year > 1953, Month == 12 )%>%
  mutate(diff = Max-Min)%>%
  group_by(Year)%>%
  summarize(diff2 = mean(diff))



ggplot(max.temp, aes(Year,diff2)) + 
  geom_point() + 
  geom_smooth() + 
  geom_vline(xintercept = 2000, color="darkgreen", linetype = "dashed") 
