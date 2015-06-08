library(dplyr)
library(ggplot2)

dataw <- read.csv("data/melbourne.csv", header = TRUE)

input <- dataw %>%
        filter (Max >= 40 ) %>%
        group_by(Year) %>%
        summarise(Count = n())
        
input

plot(input$Count)

plot <- ggplot(input, aes(x=Year, y = Count)) +
        geom_point(size=4, shape=21, colour = "darkgreen") +
        geom_smooth(method=loess)
plot