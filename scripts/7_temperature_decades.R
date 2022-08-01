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
data_m <- data_m[,-1]

data_k <- read_csv("data/kerang.csv", 
                   col_types = cols(Max = col_double(), 
                                    Min = col_double(), Rain = col_double()))
data_k$Location <- "Kerang"
data_k <- data_k[,-1]

data_all <- rbind(data_m, data_k)

data_all <- data_all[!is.na(data_all$Max), ]

#### Filter Criteria ---------------------------

## Location = Melbourne or Kerang

Loc <- "Melbourne"

## Time scale

Years <- c(1900:2020)

Months <- c(1:3)



#### Data Cleaning -----------------------------

data_all$Decade <- cut(data_all$Year, seq(from = 1860, to = 2020, by = 10), dig.lab = 5)

data_all <- data_all[!is.na(data_all$Decade), ]

data_all <- data_all %>% 
        filter(Month %in% Months)



#### Visualising Total Rainfall Data -----------------------------

data_plot <- ggplot(data_all, aes(x = as.factor(Decade), y = Max)) +
        geom_boxplot() 

data_plot
