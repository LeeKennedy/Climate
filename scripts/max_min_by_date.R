#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(LK.Toolbox)
library(RColorBrewer)
library(here)


#### Functions -----------------------------


#### Data Input -----------------------------
here()

data_m <- read_csv("data/melbourne.csv", 
                   col_types = cols(Max = col_double(), 
                                    Min = col_double(), Rain = col_double()))
data_m$Location <- "Melbourne"
data_m <- select(data_m,-X1)

data_k <- read_csv("data/kerang.csv", 
                   col_types = cols(Max = col_double(), 
                                    Min = col_double(), Rain = col_double()))
data_k$Location <- "Kerang"
data_k <- select(data_k,-X1)

data_all <- rbind(data_m, data_k)

#### Filter Criteria ---------------------------

## Location = Melbourne or Kerang

Loc <- "Melbourne"

## Time scale

Years <- c(1800:2019)

Months <- c(12)





#### Enter Day in line 54  -----------------------------

data_set <- data_all %>% 
        filter(Location == Loc) %>% 
        filter(Year %in% Years) %>% 
        filter(Month %in% Months) %>% 
        filter(Day == 25)
data_set

data_long <- gather(data_set, Temperature, Value, Max, Min)

#### Visualising Data -----------------------------

data_plot <- ggplot(data_long, aes(x = Year, y = Value, fill = Temperature)) +
        geom_point(size=4, shape=21, col = "black") +
        scale_fill_brewer(palette = "Set1")+
        geom_smooth(method = "loess", se = TRUE, col = "black", lty = 4, alpha = 0.25)+
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14), axis.text.x = element_text(angle = 0, hjust = 1))
data_plot