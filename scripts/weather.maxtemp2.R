
library(tidyverse)
library(reshape2)


#Extracts the maximum temperature for a month --------------------------------------

dataw <- read.csv("data/melbourne.csv", header = TRUE)

input <- dataw %>%
        filter(Month == 3) %>%
        group_by(Year) %>%
        summarise(Max_month = max(Max))


plot <- ggplot(input, aes(x=Year, y = Max_month)) +
        geom_point(size=4, shape=21, colour = "black", fill = "cornflowerblue") +
        geom_smooth(method=loess) +
        labs(title = "Maximum Temperature", y = "Deg C") +
        geom_hline(yintercept = 29, lty = 2, col ="red") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), text = element_text(size = 14))
plot


#ggsave("temp_september_max.png", device = NULL, width = 10, height = 5, dpi = 100)
