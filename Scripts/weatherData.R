library(ggplot2)
library(dplyr)
library(tidyr)
library(xtable)
library(stringr)

# data from NOAA in IN. North Judson for precip and South Bend for temp
SB <- read.csv("southBendWeather.csv")
Nj <- read.csv("northJudsonWeather.csv")

# fix the date column into usable month and year column for both
SB<- SB %>% separate(DATE, sep=4, c("year","month"))
SB<- SB %>% separate(month, sep=2, c("month","extra"))
Nj <- Nj %>% separate(DATE, sep=4, c("year","month"))

# interested in 2012 alone and averages from 1893 until 2012
SB <- SB %>% filter(MMXT!="-9999")
twenty12 <- SB %>% filter(year=="2012")
SB1 <- SB%>% filter(year<"2012")

# finding yearly and monthly averages

SB1$monMax <- SB1$MMXT/10
SB1$monMin <- SB1$MMNT/10
SB1$monMean <- SB1$MNTM/10
SB1$precip <-SB1$TPCP/10

SBavg <- SB1 %>% group_by(month) %>% summarize(max=mean(monMax),
                                             maxsd=sd(monMax),
                                             min=mean(monMin),
                                             minsd=sd(monMin),
                                             avg=mean(monMean),
                                             avgsd=sd(monMean),
                                             prec=mean(precip),
                                             precsd=sd(precip))


twenty12 <- twenty12 %>% select(year, month, monMax, monMin, monMean, precip,
                                EMXT, EMNT)                         
twenty08 <- SB1 %>% filter(year=="2008") %>% select(year, month, monMax,
                                                    monMin, monMean, precip,
                                                    EMXT, EMNT) 
betweenProj <- bind_rows(twenty08, twenty12)

ggplot(betweenProj, aes(month, monMax, color=year, shape=year))+
  geom_point(size=2)+
  themeopts+
  scale_color_manual(values = c("black", "gray50"))+
  labs(y="Average Max Daily Temp (C)", x="Month")

ggsave("maxtemp.png",dpi=300)

ggplot(betweenProj, aes(month, monMin, color=year, shape=year))+
  geom_point(size=2)+
  themeopts+
  scale_color_manual(values = c("black", "gray50"))+
  labs(y="Average Min Daily Temp (C)", x="Month")
ggsave("mintemp.png",dpi=300)

ggplot(betweenProj, aes(month, monMean, color=year, shape=year))+
  geom_point(size=2)+
  themeopts+
  scale_color_manual(values = c("black", "gray50"))+
  labs(y="Average Daily Temp (C)", x="Month")
ggsave("avgtemp.png",dpi=300)


ggplot(betweenProj, aes(month, precip, color=year, shape=year))+
  geom_point(size=2)+
  themeopts+
  scale_color_manual(values = c("black", "gray50"))+
  labs(y="Average Monthly Precip (mm)", x="Month")
ggsave("avgprecip.png",dpi=300)


ggplot(betweenProj, aes(month, EMXT/10, color=year, shape=year))+
  geom_point(size=2)+
  themeopts+
  scale_color_manual(values = c("black", "gray50"))+
  labs(y="Max daily Temp (C)", x="Month")
ggsave("maxDaily.png",dpi=300)

ggplot(betweenProj, aes(month, EMNT/10, color=year, shape=year))+
  geom_point(size=2)+
  themeopts+
  scale_color_manual(values = c("black", "gray50"))+
  labs(y="Min Daily Temp (C)", x="Month")
ggsave("minDaily.png",dpi=300)
