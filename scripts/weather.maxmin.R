library(tidyverse)

# Calculates the number of days above a given maximum temperature--------------------

dataw <- read.csv("data/melbourne.csv", header = TRUE)

input <- dataw %>%
        group_by(Year) %>%
        filter(Month == 4) %>%
        #filter (Min >= 18 ) %>%
        summarise(Count = n(), Max = max(Min))
        
input

plot <- ggplot(input, aes(x=Year, y = Max)) +
        geom_point(size=4, shape=21, fill = "cornflowerblue", colour = "darkgreen") +
        geom_smooth(method=loess)+
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14))
plot

