library('ProjectTemplate')
load.project()

#Extracts the maximum temperature for a month --------------------------------------

dataw <- read.csv("data/melbourne.csv", header = TRUE)

input <- dataw %>%
        filter(Month == 9) %>%
        group_by(Year) %>%
        summarise(Max_month = max(Max))
        
input

plot <- ggplot(input, aes(x=Year, y = Max_month)) +
        geom_point(size=4, shape=21, colour = "darkgreen") +
        geom_smooth(method=loess)
plot

