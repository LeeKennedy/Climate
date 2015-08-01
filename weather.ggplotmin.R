library(ggplot2)
library(dplyr)

data1<-read.csv("data/melbourne.csv", header = TRUE)

input <- select(data1, c(4:6,8)) %>%
        filter(Month == 7) %>%
        filter(Year >= 1855 & Year <= 2015) %>%
        group_by(Year) %>%
        summarise(Mean = mean(Min))

mt <- as.numeric(
        input %>%
                filter (Year <2000) %>%
                summarise(max(Mean, na.rm=TRUE))
)


ggplot(input, aes(Year,Mean)) + 
        geom_point() + 
        geom_smooth(method=loess) + 
        geom_hline(yintercept = mt, color="red") +
        geom_vline(xintercept = 2000, color="darkgreen", linetype = "solid") +
        ggtitle("Melbourne's Average Temperature") + 
        annotate("text", label = "Highest pre-2000 yearly average", x = 1900, y = mt+0.5) +
        annotate("text", label = "2000", x = 1995, y = 14) + 
        scale_x_continuous("Year",limits=c(1855,2020), breaks=seq(1855,2020, 50))
