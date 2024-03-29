#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(LK.Toolbox)
library(here)


#### Functions -----------------------------


#### Data Input -----------------------------
here::here()

data_in_m <- read_csv("data/melbourne.csv")
data_in_m$Town <- "Melbourne"

data_in_k <- read_csv("data/kerang.csv")
data_in_k$Town <- "Kerang"


data_in <- rbind(data_in_m, data_in_k)

#### Data Cleaning -----------------------------

yearly <- data_in %>% 
        filter(Year > 1855 & Year < 2021) %>% 
        filter(!Year %in% c(1970,1998)) %>% 
        na.omit() %>% 
        group_by(Town,Year) %>% 
        summarise(n = n(), Max_Ave = mean(Max), Min_Ave = mean(Min))
yearly


#### Visualising Data -----------------------------

yearly_plot <- ggplot(yearly, aes(x = Year, y = Max_Ave, fill = Town)) +
        geom_point(size = 4, shape = 21) +
        geom_smooth(method = "lm")+
        scale_x_continuous(limits = c(1900,2018))+
        labs(title = "Average Yearly Maximum Temperature", x="", y="deg C")+
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14))
yearly_plot

# ggsave("Ave_Temps.png", width = 12, height = 6, dpi = 100)
