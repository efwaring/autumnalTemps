# PCA for growth chamber data

library(ggfortify)

allPCA <-  read.csv("GCdata.csv")
allPCA = allPCA %>% mutate(climate=temp+phoper) %>% filter(climate!=35) %>% 
  select(spp, climate, nTreatment, aamb,dr,netqe,jmax,vcmax,Nitrogen)
allPCA$dr = allPCA$dr * -1


log.allPCA <- log(allPCA[c(4, 5, 6, 7, 8, 9)])
log.allPCA[33,]$dr <-0
log.allPCA[31,]$dr <-0

log.allPCA <- na.omit(log.allPCA)

log.allPCA$Rd <- log.allPCA$dr
log.allPCA$dr<-NULL


pr.allPCA <- prcomp(log.allPCA, scale=T, center=T)
plot(pr.allPCA)
print(pr.allPCA)

cumsum((pr.allPCA$sdev)^2) / sum(pr.allPCA$sdev^2) 


autoplot(pr.allPCA, data = allPCA,  fill='spp', shape="nTreatment",
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size =4) + 
  scale_shape_manual(values=c(23,21,22))+
  scale_fill_manual(values=c("black", NA)) + 
  theme(legend.position="none")

