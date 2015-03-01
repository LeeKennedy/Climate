# libraries to use ----------------------------------------------------------------------
library(lubridate)
library(dplyr)

# Import data ----------------------------------------------------------------------
new.data <- read.csv("data/feb.csv", as.is=TRUE, header=FALSE,skip=7)
new.data <- new.data[,c(2:5)]

data <- read.csv("data/melbourne.csv", as.is=TRUE, header=TRUE)
colnames(data)[2] <- "Station.Code"
j <- nrow(data)
station <- data$Station.Code[j]
p.code <- data$Product.code[j]
new.data$Product.code <- p.code
new.data$Station.Code <- station

# Separate out date ---------------------------------------------------------------
new.data <- new.data %>%
        mutate(Year = year(dmy(V2)),
               Month = month(dmy(V2)),
               Day = day(dmy(V2)))

# Rename & reorder columns --------------------------------------------------------
colnames(new.data)[2] <- "Min"
colnames(new.data)[3] <- "Max"
colnames(new.data)[4] <- "Rain"
new.data <- new.data[, c(5:9,3,2,4)]

# Combine & Save ------------------------------------------------------------------
combined <- rbind(data, new.data)
write.csv(combined, "data/melbourne.csv")
