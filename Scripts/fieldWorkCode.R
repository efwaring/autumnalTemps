soil12 <- read.csv("lachat2012.csv")
plants12 <- read.csv("data2012.csv")



# constants from Niinemets et al 1998

vcr = 20.5
jmc = 156
cb = 2.15

# convert totN from mass base to area base
plants12$Na = plants12$LMA * plants12$totN

# get vcmax, jmax, and chl on mass basis

plants12$vcmaxM <- plants12$LMA * plants12$vcmax
plants12$jmaxM <- plants12$LMA * plants12$jmax
plants12$chlM <- plants12$LMA * plants12$chl

# from niiements 1997 "A model separating leaf structural and
# physiological effects on carbon gain along light gradients for the 
# shade-tolerant species Acer saccharum".  Also see 
# Oecologia (2007) 153:501–510 by Feng

plants12$PC <- plants12$vcmaxM/(6.25*vcr*plants12$Na)
plants12$PB <- plants12$jmaxM/(8.06*jmc*plants12$Na)
plants12$PL <- plants12$chlM/(plants12$totN*cb)



# will figure out a quicker way to do this
plants12$placef <- factor(plants12$place,
                          labels = 1:2)
plants12$species <- factor(plants12$spp,
                           labels=c("C. stricta", "P. arundinacea"))

pc.lme <- lme(PC ~ spp+month+place+spp:place+spp:month, 
              random=~1|indi, data=plants12, na.action=na.omit)
anova(pc.lme)

ggplot(data=plants12, aes(month, PC, color=species, shape=species)) +
  geom_point(size=3)+
  scale_color_manual(name="species",
                     values = c("black", "gray50"))+
  themeopts
ggsave("PC.pdf")

pb.lme <- lme(PB ~ spp+month+place+spp:place+spp:month, 
              random=~1|indi, data=plants12, na.action=na.omit)
anova(pb.lme)

ggplot(data=plants12, aes(month, PB, color=species, shape=species)) +
  geom_point(size=3)+
  scale_color_manual(name="species",
                     values = c("black", "gray50"))+
  themeopts
ggsave("PB.pdf")

pl.lme <- lme(PL ~ spp+month+place+spp:place+spp:month, 
              random=~1|indi, data=plants12, na.action=na.omit)
anova(pl.lme)

ggplot(data=plants12, aes(month, PL, color=species, shape=species)) +
  geom_point(size=3)+
  scale_color_manual(name="species",
                     values = c("black", "gray50"))+
  themeopts

ggsave("PL.pdf")

# reshape proprotion data for figure making.

allP <- plants12 %>% gather("Npartition", "percentN", 33:35, na.rm=T) %>%
  select(indi, place, month, species, Npartition, percentN)

allPM <- ddply(allP, .(month, species, Npartition, place), summarize,
               proportion = mean(percentN),
               proportionSD = sd(percentN))

allPM$site <- factor(allPM$place, labels=c("Site 1","Site 2"))
allPM$monthf <-factor(allPM$month, labels=c("May","July","August"))

ggplot(data=allPM, aes(monthf, proportion, color=Npartition, 
                       fill=Npartition)) +
  geom_pointrange(aes(ymin=proportion-proportionSD,
                      ymax=proportion+proportionSD), size=0.75)+
  scale_color_manual(name="Allocation Location",
                     values = c("black", "gray50", "blue"),
                     labels=c("Bioenergics","Carboxylation", "Light Harvest"))+
  scale_fill_manual(name="Allocation Location",
                    values = c("black", "gray50", "blue"),
                    labels=c("Bioenergics","Carboxylation", "Light Harvest"))+
  facet_grid(species~site)+
  labs(y="Proportion of Allocated N", x=NULL)+
  themeopts+
  theme(strip.text.y=element_text(face="italic"))

ggsave("PALL_sites_final.pdf")
ggsave("PALL.png", dpi=300)
