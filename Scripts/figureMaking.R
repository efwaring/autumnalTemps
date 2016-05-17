# Figure making for Waring and Holaday growth chamber experiment
library(xtable)
library(ggplot2)
library(plyr)
library(cowplot)  # making labeled grids of ggplot figs.  For future


# figures

textsize <- 16
themeopts <- theme( axis.title.y = element_text(size = textsize, 
                                                angle = 90,vjust=0.3) ,
                    axis.title.x = element_text(size = textsize,
                                                vjust=-0.3),
                    panel.background = element_blank(), 
                    panel.border = element_rect(fill=NA), 
                    axis.text.x = element_text(size=14,color = "black"),
                    axis.text.y = element_text(size=14, color = "black"),
                    legend.title = element_text(size = 14),
                    legend.text = element_text(size = 14), 
                    strip.text.x = element_text(size = textsize), 
                    strip.text.y = element_text(size = textsize), 
                    strip.background = element_blank(),
                    legend.background=element_blank(),
                    legend.key = element_rect(fill = "white"))
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# making curves for figures 1 
ACI<-read.csv("allACI.csv")

o<-c("a","b","c")
t<-c("low","med","high")
order2<-data.frame(order=o,nTreatment=t)



ACI<-merge(ACI, order2, by="nTreatment")



#ACI curve

ACIavg <- ddply(ACI, .(spp, nTreatment, temp, phoper, point, order), summarize,
                photo_sd=sd(photo),
                photo=mean(photo),
                ci_sd=sd(ci),
                ci=mean(ci))

ACIavg$ci <- ACIavg$ci/10
ACIavg$ci_Sd <- ACIavg$ci_sd/10

ACIavg$phoperf <- factor(ACIavg$phoper, 
                         labels = c("10 h", "14 h"))
ACIavg$tempf<-factor(ACIavg$temp,
                     labels=c("25/18 °C", "32/21 °C"))

ACIavg$sppf<-factor(ACIavg$spp,
                    labels=c("C. stricta", "P. arundinacea"))

ACIavg=na.omit(ACIavg)

ggplot(ACIavg, aes(ci, photo, shape=order, linetype=sppf,
                              fill=sppf)) +
  geom_pointrange(aes(ymin=photo-photo_sd, ymax=photo+photo_sd,
                      xmin=ci-ci_sd, xmax=ci+ci_sd), size=1.25) +
  geom_line(size=0.5) +
  xlim(-5,200) +
  ylim(-5,30)+
  labs(x=expression(paste(italic(pCO[2]),(Pa))))+
  labs(y=expression(paste(italic(A)," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")))+
  scale_fill_manual(name="Species",
                     values = c("black", NA)) +
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2))+
  facet_grid(phoperf~tempf) +
  themeopts  +
  theme(legend.position="none")+
  panel_border(colour="black") 

ggsave("WaringandHoladay-figS1.pdf", width=15, height=10)






# Individual varibles

# for figure making
allData<-read.csv("GCdata_2.csv")
allData<-merge(allData, order2, by="nTreatment")
allData$dr <-allData$dr*-1



#### Figure changes from sept 8th meeting



allDataM <- ddply(allData, .(tempf, order, photoperiodf, species, nTreatment), 
                  summarize,
                  aambSD =sd(aamb),
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

# Aamb

ggplot(allDataM, aes(tempf, aamb, shape=order, fill=factor(order),
                    linetype=order))+
  geom_pointrange(aes(ymin=aamb-aambSD, ymax=aamb+aambSD),
                  position=position_dodge(width=0.5), size=1)+
  facet_grid(photoperiodf~species)+  
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste(italic(A[amb])," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")))+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  scale_x_discrete(name="Temperature (°C)") +
  theme(legend.position="none")+
  panel_border(colour="black") 

ggsave("waring-and-holaday-figure-2.pdf")



ggplot(allDataM, aes(tempf, dr, shape=order, linetype=order,
                     fill=factor(order)))+
  geom_pointrange(aes(ymin=dr-drSD, ymax=dr+drSD),
                 position=position_dodge(width=0.5), size=1)+
  facet_grid(photoperiodf~species)+  
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste(italic(R[d])," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")))+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  scale_x_discrete(name="Temperature (°C)") +
  theme(legend.position="none")+
  panel_border(colour="black") 

ggsave("waring-and-holaday-figure-3.pdf")

# net qe



ggplot(allDataM, aes(tempf, netqe, shape=order, linetype=order, 
                     fill=factor(order)))+
  geom_pointrange(aes(ymin=netqe-netqeSD, ymax=netqe+netqeSD),
                  position=position_dodge(width=0.5), size=1)+
  facet_grid(photoperiodf~species)+  
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste("Net qe"," (", mu * mol %.% m^{-2} %.% s^{-1}%.% pfd^{-1}, ")")))+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  scale_x_discrete(name="Temperature (°C)") +
  theme(legend.position="none")+
  panel_border(colour="black")

ggsave("waring-holday-figure-4.pdf")

ggplot(allDataM, aes(tempf, N, shape=order, fill=factor(order),
                     linetype=order))+
  geom_pointrange(aes(ymin=N-Nsd, ymax=N+Nsd),
                  position=position_dodge(width=0.5), size=1)+
  facet_grid(photoperiodf~species)+  
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste("Leaf N"," (", cg %.% g^{-1}, ")")))+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  scale_x_discrete(name="Temperature (°C)") +
  theme(legend.position="none")+
  panel_border(colour="black") 

ggsave ("waring-holday-figure-5.pdf")


# pnue
ggplot(allDataM, aes(tempf, PNUE, shape=order, linetype=order,
                     fill=order))+
  geom_pointrange(aes(ymin=PNUE-PNUESD, ymax=PNUE+PNUESD),
                  position=position_dodge(width=0.5), size=1)+
  facet_grid(photoperiodf~species)+  
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste(PNUE[]," (", mu * mol %.% g^{-1} %.% s^{-1} , ")")))+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  scale_x_discrete(name="Temperature (°C)") +
  theme(legend.position="none")+
  panel_border(colour="black")

ggsave("waring-and-holaday-figure6.pdf")

#  Figure 7


# ce



ce <- ggplot(allDataM, aes(tempf, ce, shape=order,linetype=order,
                     fill=factor(order))) +
  geom_pointrange(aes(ymin=ce-ceSD, ymax=ce+ceSD),
               position=position_dodge(width=0.5), size=1)+
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  facet_grid(photoperiodf~species)+  
  labs(y=expression(paste(CE[]," (", mu * mol %.% m^{-2} %.% s^{-1} %.% kPa^{-1}, ")")))+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  scale_x_discrete(name="Temperature (°C)") +
  theme(legend.position="none")+
  panel_border(colour="black")
ggsave("waring-and-holaday-figure7.pdf")

# vcmax

vc <- ggplot(allDataM, aes(tempf, vc, shape=order, fill=factor(order),
                     linetype=order))+
  geom_pointrange(aes(ymin=vc-vcSD, ymax=vc+vcSD),
                  position=position_dodge(width=0.5), size=1)+
  facet_grid(photoperiodf~species)+  
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste(italic(V[cmax])," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")))+
  themeopts +
  #annotate("text", label = c("B","C","D","F"), x = 0.5, y = 98, size = 10) +
  theme(strip.text.x = element_text(face = "italic")) +
  scale_x_discrete(name="Temperature (°C)") +
  theme(legend.position="none")+
  panel_border(colour="black") 
ggsave("waring-and-holaday-figure8.pdf")


# jmax
jmax <- ggplot(allDataM, aes(tempf, j, shape=order, fill=factor(order),
                     linetype=order))+
  geom_pointrange(aes(ymin=j-jSD, ymax=j+jSD),
                  position=position_dodge(width=0.5), size=1)+
  facet_grid(photoperiodf~species)+  
  scale_fill_manual(values=c("black",NA,"gray50"))+
  scale_shape_manual(values=c(21,22,23))+
  scale_linetype_manual(values=c(1,2,3))+
  labs(y=expression(paste(italic(J[max])," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")))+
  themeopts +
  theme(strip.text.x = element_text(face = "italic")) +
  scale_x_discrete(name="Temperature (°C)") +
  theme(legend.position="none")+
  panel_border(colour="black") 
ggsave("waring-and-holaday-figure9.pdf")


pdf("WaringandHoladay-fig7.pdf", height = 4, width = 15, useDingbats = F)
multiplot(ce, vc, jmax, cols=3)
dev.off()
