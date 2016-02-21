library('ProjectTemplate')
load.project()

data1<-read.csv("data/kerang.csv", header = TRUE)

input <- select(data1, 4:8) %>%
        #filter(Month == 2) %>%
        filter(Year >= 1855 & Year <= 2014) %>%
        group_by(Year) %>%
        summarise(Mean = mean(Max, na.rm=TRUE))

mt <- as.numeric(
        input %>%
        filter (Year <2000) %>%
        summarise(max(Mean, na.rm=TRUE))
        )

y_min <- min(input$Year)

ggplot(input, aes(Year,Mean)) + 
  geom_point() + 
  geom_smooth(method=loess) + 
  geom_hline(yintercept = mt, color="red") +
  geom_vline(xintercept = 2000, color="darkgreen", linetype = "solid") +
  ggtitle("Melbourne's Average Temperature") + 
  annotate("text", label = "Highest pre-2000 yearly average", x = y_min+10, y = mt+0.25) + 
  annotate("text", label = "2000", x = 1995, y = 24) + 
  scale_x_continuous("Year",limits=c(y_min,2020), breaks=seq(y_min,2020, 50))

