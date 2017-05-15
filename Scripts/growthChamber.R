library(ggplot2)
library(nlme)
library(dplyr)
library(tidyr)
library(cowplot)
library(lsmeans)



all <- read.csv("GCdata.csv")

# create a variable for the temp + photoperiod and remove 25/18 and 10 h data
all= all %>% mutate(climate=temp+phoper) %>% filter(climate!=35)

# arrange and rename the climate column
# give all ID as factors
all=all %>% arrange(climate) 
all$climateF = factor(all$climate, labels=c("O-L","H-S","H-L"))
all$species <- factor(all$spp,
                          labels=c("C. stricta", "P. arundinacea"))



# stats 

# transformations for skewed data
all$vcmax<-log10(all$vcmax)
all$jmax<-log10(all$jmax)
all$Nitrogen<-log10(all$Nitrogen)

# dark respration should be postive

all$dr <- all$dr *-1

#

aamb<-lme(aamb ~ species*nTreatment*climateF,
          random =~1|plant_id,
          data=all)
anova(aamb)

lsmeans(aamb, pairwise~climateF, adjust="tukey")
lsmeans(aamb, pairwise~nTreatment, adjust="tukey")
lsmeans(aamb, pairwise~species*nTreatment, adjust="tukey")

print(xtable(lsmeans(aamb, pairwise~climateF, adjust="tukey")$contrasts), type='html')
print(xtable(lsmeans(aamb, pairwise~nTreatment, adjust="tukey")$contrasts), type='html')
print(xtable(lsmeans(aamb, pairwise~species*nTreatment, adjust="tukey")$contrasts), type='html')

anova(d)
lsmeans(d, pairwise~climateF, adjust="tukey")
lsmeans(d, pairwise~species*climateF, adjust="tukey")

print(xtable(lsmeans(d, pairwise~species*climateF, adjust="tukey")$contrasts), type='html')



n <- lme(Nitrogen ~ species*nTreatment*climateF,random =~1|plant_id,
         data=all)
anova(n)

lsmeans(n, pairwise~climateF, adjust="tukey")
lsmeans(n, pairwise~nTreatment, adjust="tukey")
lsmeans(n, pairwise~species*climateF, adjust="tukey")
lsmeans(n, pairwise~nTreatment*climateF, adjust="tukey")

print(xtable(lsmeans(n, pairwise~climateF, adjust="tukey")$contrasts), type='html')
print(xtable(lsmeans(n, pairwise~nTreatment, adjust="tukey")$contrasts), type='html')
print(xtable(lsmeans(n, pairwise~climateF*nTreatment, adjust="tukey")$contrasts), type='html')
print(xtable(lsmeans(n, pairwise~climateF*species, adjust="tukey")$contrasts), type='html')


lsmeans(j, pairwise~species*climateF*nTreatment, adjust="tukey")

v <- lme(vcmax ~ species*nTreatment*climateF,random =~1|plant_id,
         data=all)
anova(v)

lsmeans(v, pairwise~climateF, adjust="tukey")
lsmeans(v, pairwise~nTreatment, adjust="tukey")

print(xtable(lsmeans(v, pairwise~climateF, adjust="tukey")$contrasts), type='html')
print(xtable(lsmeans(v, pairwise~nTreatment, adjust="tukey")$contrasts), type='html')



j <- lme(jmax ~ species*nTreatment*climateF,random =~1|plant_id,
         data=all)
anova(j)
lsmeans(j, pairwise~climateF, adjust="tukey")
lsmeans(j, pairwise~nTreatment, adjust="tukey")

print(xtable(lsmeans(j, pairwise~climateF, adjust="tukey")$contrasts), type='html')
print(xtable(lsmeans(j, pairwise~nTreatment, adjust="tukey")$contrasts), type='html')



q <- lme(netqe ~ species*nTreatment*climateF,random =~1|plant_id,
         data=all)
anova(q)

lsmeans(q, pairwise~climateF, adjust="tukey")
print(xtable(lsmeans(q, pairwise~climateF, adjust="tukey")$contrasts), type='html')





allM <- all %>% group_by(nTreatment, species, nTreatment, climateF) %>%
              summarize(aambSD =sd(aamb),
                  aamb=mean(aamb),
                  ceSD =sd(ce),
                  ce=mean(ce),
                  drSD=sd(dr),
                  dr=mean(dr),
                  netqeSD=sd(netqe),
                  netqe=mean(netqe),
                  PNUESD=sd(pnue),
                  PNUE=mean(pnue),
                  slaSD=sd(SLA),
                  SLA=mean(SLA),
                  jSD=sd(jmax),
                  j=mean(jmax),
                  vcSD=sd(vcmax),
                  vc=mean(vcmax),
                  Nsd=sd(Nitrogen),
                  N=mean(Nitrogen))


o<-c("a","b","c")
t<-c("low","med","high")
order2<-data.frame(order=o,nTreatment=t)
allM<-merge(allM, order2, by=c("nTreatment"))

# figures for growthchamber
# read in themeopts and mulitplot
source("theme-opts.R")


# figures


# fig netqe
qe=ggplot(allM, aes(climateF, netqe, shape=order, fill=order,
                 linetype=order))+
  geom_pointrange(aes(ymin=netqe-netqeSD, ymax=netqe+netqeSD),
                  position=position_dodge(width=0.5), size=1)+
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste("qe"
                          ," (", mu * mol, " CO"[2]  %.% mu * mol, " Ph"^{-1} ,")")))+
  facet_grid(.~species)+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  theme(legend.position="none")+
  panel_border(colour="black")+
  scale_x_discrete(name=NULL,limits=c("O-L","H-L","H-S"))
  

# make multiplot for gc data
# figure for aamb
amb= ggplot(allM, aes(climateF, aamb, shape=order, fill=order,
                 linetype=order))+
  geom_pointrange(aes(ymin=aamb-aambSD, ymax=aamb+aambSD),
                  position=position_dodge(width=0.5), size=1)+
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste(italic(A[amb]),
                          " (", mu * mol %.% m^{-2} %.% s^{-1}, ")")))+
  facet_grid(.~species)+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  theme(legend.position="none")+
  panel_border(colour="black")+
  scale_x_discrete(name=NULL,limits=c("O-L","H-L","H-S"))

dr= ggplot(allM, aes(climateF, dr, shape=order, fill=order,
                 linetype=order))+
  geom_pointrange(aes(ymin=dr-drSD, ymax=dr+drSD),
                  position=position_dodge(width=0.5), size=1)+
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste(italic(R[d]),
                          " (", mu * mol %.% m^{-2} %.% s^{-1}, ")")))+
  facet_grid(.~species)+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  theme(legend.position="none")+
  panel_border(colour="black")+
  scale_x_discrete(name=NULL,limits=c("O-L","H-L","H-S"))


multiplot(qe, dr, amb, cols=2)

# vcmax


vcmax <- ggplot(allM, aes(climateF, vc, shape=order, fill=order,
                 linetype=order))+
  geom_pointrange(aes(ymin=vc-vcSD, ymax=vc+vcSD),
                  position=position_dodge(width=0.5), size=1)+
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  scale_y_continuous(expression(log[10] ~ V[cmax] ~ ( mu * mol %.% m^{-2} %.% s^{-1} ) ) )  +
  facet_grid(.~species)+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  theme(legend.position="none")+
  panel_border(colour="black")+
  scale_x_discrete(name=NULL,limits=c("O-L","H-L","H-S"))


#Jmax

jmax <- ggplot(allM, aes(climateF, j, shape=order, fill=order,
                 linetype=order))+
  geom_pointrange(aes(ymin=j-jSD, ymax=j+jSD),
                  position=position_dodge(width=0.5), size=1)+
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  scale_y_continuous(expression(log[10] ~ J[max] ~ ( mu * mol %.% m^{-2}
                                            %.% s^{-1} ) ) )  +
  facet_grid(.~species)+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  theme(legend.position="none")+
  panel_border(colour="black")+
  scale_x_discrete(name=NULL,limits=c("O-L","H-L","H-S"))


#leaf N
leafN <- ggplot(allM, aes(climateF, N, shape=order, fill=order,
                 linetype=order))+
  geom_pointrange(aes(ymin=N-Nsd, ymax=N+Nsd),
                  position=position_dodge(width=0.5), size=1)+
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste(log[10], " Leaf N"," (",cg %.% g^{-1}, ")")))+
  facet_grid(.~species)+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  theme(legend.position="none")+
  panel_border(colour="black")+
  scale_x_discrete(name=NULL,limits=c("O-L","H-L","H-S"))

multiplot(vcmax, jmax, leafN, cols=2)

