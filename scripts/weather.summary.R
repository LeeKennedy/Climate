
library(tidyverse)
library(reshape2)



data1 <- read.csv("data/kerang.csv",as.is=TRUE, header=TRUE)

data <- na.omit(data1)

bymon <- group_by(data, Month)%>%
        summarize(max.hi = max(Max), 
                  min.hi=max(Min), 
                  max.lo = min(Max),  
                  min.lo=min(Min), 
                  rain.hi=max(Rain))

bymon
