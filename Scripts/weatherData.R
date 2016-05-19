library(ggplot2)
library(dplyr)
library(tidyr)
library(xtable)
library(stringr)

SB <- read.csv("southBendWeather.csv")
Nj <- read.csv("northJudsonWeather.csv")

SB<- SB %>% separate(DATE, sep=4, c("year","month"))
SB<- SB %>% separate(month, sep=2, c("month","extra"))
