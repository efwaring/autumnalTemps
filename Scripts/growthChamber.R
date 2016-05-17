library(ggplot2)
library(nlme)
library(dplyr)
library(tidyr)
library(cowplot)
library(xtable)


all <- read.csv("GCdata.csv")

# create a variable for the temp + photoperiod and remove 25/18 and 10 h data
all= all %>% mutate(climate=temp+phoper) %>% filter(climate!=35)

# arrange and rename the climate column
# give all ID as factors
all=all %>% arrange(climate) 
all$climateF = factor(all$climate, labels=c("N-L","H-S","H-L"))
all$species <- factor(all$spp,
                          labels=c("C. stricta", "P. arundinacea"))

# add in meosphyll conductance

gm <- read.csv("GM_data.csv")

gm <- gm %>% filter(point=="7") %>% mutate(climate=temp+phoper) %>% 
  filter(climate!=35) %>% select(sample, spp, climate, trt, mesoCond)

gm1<-lme(mesoCond ~ spp*trt*climate,
          random =~1|sample,
          data=gm)
anova(gm1)

gm=gm %>% arrange(climate) 
gm$climateF = factor(gm$climate, labels=c("N-L","H-S","H-L"))
gm$species <- factor(gm$spp,
                      labels=c("C. stricta", "P. arundinacea"))

gmM <- gm %>% group_by(species, climateF, trt) %>% summarize(gmsd=sd(mesoCond),
                                                             gm=mean(mesoCond))

gmM2 <- gm %>% group_by(species) %>% summarize(gmsd=sd(mesoCond),
                                                             gm=mean(mesoCond))


o<-c("a","b","c")
t<-c("low","med","high")
order2<-data.frame(order=o,trt=t)
gmM<-merge(gmM, order2, by="trt")


ggplot(gmM2, aes(climateF, gm, shape=order, fill=order,
                 linetype=order))+
  geom_pointrange(aes(ymin=gm-gmsd, ymax=gm+gmsd),
                  position=position_dodge(width=0.5), size=1)+
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste(italic(g[m]),
                          " (", mmol %.% m^{-2} %.% s^{-1}, ")")))+
  facet_grid(.~species)+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  theme(legend.position="none")+
  panel_border(colour="black")+
  scale_x_discrete(name=NULL,limits=c("N-L","H-L","H-S"))




# stats 

# transformations for skewed data
all$SLA<-log10(all$SLA)
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
c <- lme(ce ~ species*nTreatment*climateF,random =~1|plant_id,
         data=all)
anova(c)
d <- lme(dr ~ species*nTreatment*climateF,random =~1|plant_id,
         data=all)
anova(d)
n <- lme(Nitrogen ~ species*nTreatment*climateF,random =~1|plant_id,
         data=all)
anova(n)
v <- lme(vcmax ~ species*nTreatment*climateF,random =~1|plant_id,
         data=all)
anova(v)
j <- lme(jmax ~ species*nTreatment*climateF,random =~1|plant_id,
         data=all)
anova(j)
p <- lme(pnue ~ species*nTreatment*climateF,random =~1|plant_id,
         data=all)
anova(p)
q <- lme(netqe ~ species*nTreatment*climateF,random =~1|plant_id,
         data=all)
anova(q)



o<-c("a","b","c")
t<-c("low","med","high")
order2<-data.frame(order=o,nTreatment=t)
all<-merge(all, order2, by="nTreatment")



allM <- all %>% group_by(order, species, nTreatment, climateF) %>%
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



# fig for aamb
ggplot(allM, aes(climateF, aamb, shape=order, fill=order,
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
  scale_x_discrete(name=NULL,limits=c("N-L","H-L","H-S"))

ggsave("aamb.png",dpi=600)
  
# fig netqe
ggplot(allM, aes(climateF, netqe, shape=order, fill=order,
                 linetype=order))+
  geom_pointrange(aes(ymin=netqe-netqeSD, ymax=netqe+netqeSD),
                  position=position_dodge(width=0.5), size=1)+
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste("Net qe"," (", mu * mol %.% m^{-2} %.% s^{-1}%.%
                            pfd^{-1}, ")")))+
  facet_grid(.~species)+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  theme(legend.position="none")+
  panel_border(colour="black")+
  scale_x_discrete(name=NULL,limits=c("N-L","H-L","H-S"))
ggsave("netqe.png", dpi=600)

# fig dr
ggplot(allM, aes(climateF, dr, shape=order, fill=order,
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
  scale_x_discrete(name=NULL,limits=c("N-L","H-L","H-S"))
ggsave("dr.png", dpi=600)
#leaf N
ggplot(allM, aes(climateF, N, shape=order, fill=order,
                 linetype=order))+
  geom_pointrange(aes(ymin=N-Nsd, ymax=N+Nsd),
                  position=position_dodge(width=0.5), size=1)+
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste("Leaf N"," (",cg %.% g^{-1}, ")")))+
  facet_grid(.~species)+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  theme(legend.position="none")+
  panel_border(colour="black")+
  scale_x_discrete(name=NULL,limits=c("N-L","H-L","H-S"))

ggsave("leafN.png", dpi=600)

#pnue

ggplot(allM, aes(climateF, PNUE, shape=order, fill=order,
                 linetype=order))+
  geom_pointrange(aes(ymin=PNUE-PNUESD, ymax=PNUE+PNUESD),
                  position=position_dodge(width=0.5), size=1)+
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste(PNUE[]," (", mu * mol %.% g^{-1} %.% s^{-1} , ")")))+
  facet_grid(.~species)+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  theme(legend.position="none")+
  panel_border(colour="black")+
  scale_x_discrete(name=NULL,limits=c("N-L","H-L","H-S"))

ggsave("pnue.png", dpi=600)

# Ce

ggplot(allM, aes(climateF, ce, shape=order, fill=order,
                 linetype=order))+
  geom_pointrange(aes(ymin=ce-ceSD, ymax=ce+ceSD),
                  position=position_dodge(width=0.5), size=1)+
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste(CE[]," (", mu * mol %.% m^{-2} %.% 
                            s^{-1} %.% kPa^{-1}, ")")))+
  facet_grid(.~species)+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  theme(legend.position="none")+
  panel_border(colour="black")+
  scale_x_discrete(name=NULL,limits=c("N-L","H-L","H-S"))

ggsave("ce.png", dpi=600)

# vcmax


ggplot(allM, aes(climateF, vc, shape=order, fill=order,
                 linetype=order))+
  geom_pointrange(aes(ymin=vc-vcSD, ymax=vc+vcSD),
                  position=position_dodge(width=0.5), size=1)+
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste(italic(V[cmax])," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")))+
  facet_grid(.~species)+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  theme(legend.position="none")+
  panel_border(colour="black")+
  scale_x_discrete(name=NULL,limits=c("N-L","H-L","H-S"))

ggsave("vcmax.png", dpi=600)

#Jmax

ggplot(allM, aes(climateF, j, shape=order, fill=order,
                 linetype=order))+
  geom_pointrange(aes(ymin=j-jSD, ymax=j+jSD),
                  position=position_dodge(width=0.5), size=1)+
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste(italic(J[max])," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")))+
  facet_grid(.~species)+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  theme(legend.position="none")+
  panel_border(colour="black")+
  scale_x_discrete(name=NULL,limits=c("N-L","H-L","H-S"))

ggsave("jmax.png", dpi=600)
