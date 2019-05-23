#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(here)


#### Functions -----------------------------
outliers <- function (x, b = FALSE) {
xx <- sapply(x, as.numeric)

#xx <- sort(xx)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

yy <- remove_outliers(xx)
ww <- remove_outliers(yy)
zz <- remove_outliers(ww)

diff.out <- data.frame(xx, yy, ww, zz)

if(b == TRUE){
boxplot(diff.out)
}

return(zz)
}

#### Data Input -----------------------------
here()

data_in_m <- read_csv("data/melbourne.csv")
data_in_m$Town <- "Melbourne"

data_in_k <- read_csv("data/kerang.csv")
data_in_k$Town <- "Kerang"


data_in <- rbind(data_in_m, data_in_k)

#### Data Cleaning -----------------------------

yearly <- data_in %>% 
        filter(Year > 1855 & Year < 2019) %>% 
        filter(!Year %in% c(1970,1998)) %>% 
        na.omit() %>% 
        group_by(Town,Year) %>% 
        summarise(n = n(), Max_Ave = mean(Max), Min_Ave = mean(Min))
yearly


#### Visualising Data -----------------------------

yearly_plot <- ggplot(yearly, aes(x = Year, y = Min_Ave, fill = Town)) +
        geom_point(size = 4, shape = 21) +
        geom_smooth(method = "loess")+
        scale_x_continuous(limits = c(1900,2018))+
        labs(title = "Average Yearly Maximum Temperature")+
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14))
yearly_plot

ggsave("Ave_Temps.png", width = 12, height = 6, dpi = 100)
