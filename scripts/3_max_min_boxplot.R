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
here::here()

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
Loc <- "Kerang"

## Time scale

Years <- c(2000:2019)

Months <- c(4)

## - creating date string ------------------------------------

month_list <- paste(month.name[Months], collapse=', ' )

if (length(Months) == 12) {month_list = "Full Year"}
if(setequal(Months,c(6:8)) ==  TRUE) {month_list = "Winter"}
if(setequal(Months,c(3:5)) ==  TRUE) {month_list = "Autumn"}
if(setequal(Months,c(9:11)) ==  TRUE) {month_list = "Spring"}
if(setequal(Months,c(1,2,12)) ==  TRUE) {month_list = "a Calendar Summer"}

M_string <- paste("Data for ", month_list, sep="")

#### Data Cleaning -----------------------------

data_set <- data_all %>% 
        filter(Location == Loc) %>% 
        filter(Year %in% Years) %>% 
        filter(Month %in% Months) %>% 
        group_by(Year) 
data_set

#### Visualising Data -----------------------------

data_plot <- ggplot(data_set, aes(x = Year, y = Min, group = Year)) +
        geom_boxplot(fill = "cornflowerblue") +
        labs(title = "", 
             y = "DegC", 
             x="") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14), axis.text.x = element_text(angle = 0, hjust = 1))
data_plot