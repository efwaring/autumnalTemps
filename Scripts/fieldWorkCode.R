# analysis of the 2012 field data for Waring and Holaday for AJB manuscript

# read in data
plants12 <- read.csv("data2012.csv")

# 2012 naming changs


plants12$species <- factor(plants12$spp,
                           labels=c("C. stricta", "P. arundinacea"))
plants12$monthf <- factor(plants12$month, labels=c("May","July", "October"))

# convert totN from mass base to area base
plants12$Na = plants12$LMA * plants12$totN

# Stats for photosynthetic parameters

amba <-lme(amba ~ spp*month, random=~1|indi, data=plants12, na.action=na.omit)
anova(amba)

v <-lme(vcmax ~ spp*month, random=~1|indi, data=plants12, na.action=na.omit)
anova(v)

j <-lme(jmax ~ spp*month, random=~1|indi, data=plants12, na.action=na.omit)
anova(j)



c <-lme(ce ~ spp*month, random=~1|indi, data=plants12, na.action=na.omit)
anova(c)



# figures


# first select for varibles that had 8 replicates
plants12Fig <-plants12 %>% select(species, monthf, amba, vcmax, jmax, ce)
plants12Fig <- na.omit(plants12Fig)

plantsM <- plants12Fig %>% group_by(monthf, species) %>%
  summarize(asd = sd(amba),
            a = mean(amba),
            v=mean(vcmax),
            vsd=sd(vcmax),
            j=mean(jmax),
            jsd=sd(jmax),
            c=mean(ce),
            csd=sd(ce))


# amb A
Aamb <- ggplot(data=plantsM, aes(monthf, a, color=species, 
                       fill=species)) +
  geom_pointrange(aes(ymin=a-asd,
                      ymax=a+asd), size=0.75)+
  scale_color_manual(name="Species",
                     values = c("black", "gray50"))+
  scale_fill_manual(name="Species",
                    values = c("black", "gray50"))+
  labs(y=expression(paste(italic(A[amb]),
                          " (", mu * mol %.% m^{-2} %.% s^{-1}, ")")))+
  labs(x=NULL)+
  annotate("text", label = "A", x = .6, y = 19.5, size = 12) +
  themeopts+
  theme(legend.position="none")+
  panel_border(colour="black") 
# CE
ce <- ggplot(data=plantsM, aes(monthf, c, color=species, 
                         fill=species)) +
  geom_pointrange(aes(ymin=c-csd,
                      ymax=c+csd), size=0.75)+
  scale_color_manual(name="Species",
                     values = c("black", "gray50"))+
  scale_fill_manual(name="Species",
                    values = c("black", "gray50"))+
  scale_y_continuous(expression(CE ~ ( mu * mol %.% m^{-2} %.% s^{-1} ) ) )  +
  labs(x=NULL)+
  annotate("text", label = "B", x = .6, y = .11, size = 12) +
  themeopts+
  theme(legend.position="none")+
  panel_border(colour="black") 



# jmax
jmax <- ggplot(data=plantsM, aes(monthf, j, color=species, 
                              fill=species)) +
  geom_pointrange(aes(ymin=j-jsd,
                      ymax=j+jsd), size=0.75)+
  scale_color_manual(name="Species",
                     values = c("black", "gray50"))+
  scale_fill_manual(name="Species",
                    values = c("black", "gray50"))+
  scale_y_continuous(expression(J[max] ~ ( mu * mol %.% m^{-2} %.% s^{-1}
                                           ) ) )  +
  labs(x=NULL)+
  annotate("text", label = "C", x = .6, y = 212, size = 12) + 
  themeopts +
  theme(legend.position="none")+
  panel_border(colour="black") 

# vcmax
vcmax <- ggplot(data=plantsM, aes(monthf, v, color=species, 
                         fill=species)) +
  geom_pointrange(aes(ymin=v-vsd,
                      ymax=v+vsd), size=0.75)+
  scale_color_manual(name="Species",
                     values = c("black", "gray50"))+
  scale_fill_manual(name="Species",
                    values = c("black", "gray50"))+
  scale_y_continuous(expression(V[cmax] ~ ( mu * mol %.% m^{-2}
                                            %.% s^{-1} ) ) )  +
  labs(x=NULL)+
  annotate("text", label = "D", x = .6, y = 90, size = 12) +
  themeopts+
  theme(legend.position="none")+
  panel_border(colour="black") 

multiplot(Aamb, jmax, ce, vcmax, cols=2)

# constants from Niinemets et al 1998

vcr = 20.5
jmc = 156
cb = 2.15



# get vcmax, jmax, and chl on mass basis

plants12$vcmaxM <- plants12$LMA * plants12$vcmax
plants12$jmaxM <- plants12$LMA * plants12$jmax
#plants12$chlM <- plants12$LMA * plants12$chl

# from niiements 1997 "A model separating leaf structural and
# physiological effects on carbon gain along light gradients for the 
# shade-tolerant species Acer saccharum".  Also see 
# Oecologia (2007) 153:501–510 by Feng

plants12$PC <- plants12$vcmaxM/(6.25*vcr*plants12$Na)
plants12$PB <- plants12$jmaxM/(8.06*jmc*plants12$Na)
#plants12$PL <- plants12$chlM/(plants12$totN*cb)



# will figure out a quicker way to do this

pc.lme <- lme(PC ~ spp*month, 
              random=~1|indi, data=plants12, na.action=na.omit)
anova(pc.lme)


pb.lme <- lme(PB ~ spp*month, 
              random=~1|indi, data=plants12, na.action=na.omit)
anova(pb.lme)




# reshape proprotion data for figure making.

allP <- plants12 %>% gather("Npartition", "percentN", 35:36, na.rm=T) %>%
  select(indi, place, month, species, Npartition, percentN)

allPM <- allP %>% group_by(month, species, Npartition) %>%
          summarize(proportion = mean(percentN),
               proportionSD = sd(percentN))

allPM$monthf <-factor(allPM$month, labels=c("May","July","August"))

ggplot(data=allPM, aes(monthf, proportion, color=Npartition, 
                       fill=Npartition)) +
  geom_pointrange(aes(ymin=proportion-proportionSD,
                      ymax=proportion+proportionSD), size=0.75)+
  scale_color_manual(name="Allocation Location",
                     values = c("black", "gray50"),
                     labels=c("Bioenergics","Carboxylation"))+
  scale_fill_manual(name="Allocation Location",
                    values = c("black", "gray50"),
                    labels=c("Bioenergics","Carboxylation"))+
  facet_grid(.~species)+
  labs(y="Proportion of Allocated N", x=NULL)+
  themeopts+
  theme(strip.text.x=element_text(face="italic"))

ggsave("PALL_sites_final.pdf")
ggsave("PALL.png", dpi=300)
