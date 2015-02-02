data1 <- read.csv("melbourne.csv",as.is=TRUE, header=TRUE)
library(dplyr)
data <- na.omit(data1)
bymon <- group_by(data, Month)
avebymon <- summarize(bymon, max.hi = max(Max), min.hi=max(Min), max.lo = min(Max),  min.lo=min(Min), rain.hi=max(Rain), rain.lo=min(Rain))
avebymon
