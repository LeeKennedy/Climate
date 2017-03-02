library('ProjectTemplate')
load.project()

data1<-read.csv("data/kerang.csv", header = TRUE)

rain <- select(data1, everything())%>%
        filter(Year %in% 1855:2016)%>%
        filter(Month %in% c(2))%>%
        group_by(Year)%>%
        summarise(raintot = sum(Rain))
        

ggplot(rain, aes(Year,raintot)) + 
        geom_point(size = 3, colour = "lightsteelblue4") + 
        geom_smooth(method = loess) + 
        ylim(0,1.2*max(na.omit(rain$raintot))) +
        theme_bw() +
        labs(title = "Total rainfall in Melbourne (mm)\n", y = "Rainfall, mm") +
        theme(plot.title = element_text(size=22))
ggsave(file.path('graphs', 'rain_melb.pdf'))

rain3 <- select(data1, everything())%>%
        group_by(Year, Month)%>%
        filter(!is.na(Rain))%>%
        summarise (Sum = sum(Rain))

rain4 <- group_by(rain3, Month)%>%
        summarise(Mean = mean(Sum))
rain4

