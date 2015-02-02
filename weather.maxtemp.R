dataw <- read.csv("melbourne.csv", header = TRUE)

limit <- 43

dataw$lim <- dataw$Max >limit

xx <- table(dataw$Year,dataw$lim)


DayCount <- xx[,-1]


plot(DayCount)

