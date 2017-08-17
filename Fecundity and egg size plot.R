
###Removing any previously existing objects. This is useful if you are debugging or repeatedly running analysis, as it prevents
rm(list=ls())



setwd("E://Dropbox/School/data, analyses/Butterfly comparative project/Spring 2016")  ###Setting drive to use
setwd("C://Users/Owner/Dropbox/School/data, analyses/Butterfly comparative project/Spring 2016")  ###Setting drive to use

###Load packages not included in the base. Sciplot makes some interesting interaction plots. bbmle is Ben Bolker's package that allows some neat stuff like AICctab that I like
library(sciplot)  ###Some interesting plots available. I believe we don't end up using any of these in thise script.
library(bbmle)  
library(ggplot2)  ###Used for plotting here
library(cowplot)
library("gridExtra")


##################################################
##################################################
##########                         ###############
##########   Collected butterflies    ###############
##########                         ###############
##################################################
##################################################






#################
#####        ####
##  Brain Weight  ##
#####        ####
#################

data.reared <- read.csv(file="data for analysis.csv")                   ###Load in data.reared
data.reared <- data.reared[data.reared$Provenance=="Reared",]                         ###Limit data.reared to just lab reared individuals
data.reared <- data.reared[data.reared$Group=="Lifespan",]
data.reared <-data.reared[data.reared$sex!="M",]                                      ###Remove any males
data.reared$Treatment <- data.reared$conTreatment   ###None replaced with control

### analysis limited to certain species
data.reared <- data.reared[data.reared$species=="Danaus_plexippus"|data.reared$species=="Pieris_rapae"|data.reared$species=="Vanessa_atalanta"|data.reared$species=="Colias_interior"|data.reared$species=="Colias_philodice"|data.reared$species=="Papilio_glaucus"|data.reared$species=="Papilio_polyxenes"|data.reared$species=="Pieris_olaracea"|data.reared$species=="Pontia_occidentalis"|data.reared$species=="Pontia_protodice"|data.reared$species=="Satyrodes_eurydice",]
###Reduction to variables I am going to analyze in this section
data.reared<-na.omit(data.frame("ID"=data.reared$ID,"Hindwing.Perimeter"=data.reared$Hindwing.Perimeter,"Egg.Size"=data.reared$Average.egg.perimeter,"Egg.Number"=data.reared$egg_number,"Treatment"=data.reared$Treatment,"species"=data.reared$species))
data.reared <- data.reared[data.reared$species%in%names(table(data.reared$species)[table(data.reared$species)>9]),]
data.reared$Treatment<-factor(data.reared$Treatment)  ###Turning treatment into a factor
data.reared$species<-factor(data.reared$species)  ###Turning species into a factor
data.reared$lHindwing.Perimeter <- log(data.reared$Hindwing.Perimeter) ###Log transforming morphological variable



Pr.data.reared <- data.reared[data.reared$species=="Pieris_rapae",]
Prt <- Pr.data.reared[Pr.data.reared$Treatment=="Treatment",]
Prc <- Pr.data.reared[Pr.data.reared$Treatment=="Control",]

Dp.data.reared <- data.reared[data.reared$species=="Danaus_plexippus",]
Dpt <- Dp.data.reared[Dp.data.reared$Treatment=="Treatment",]
Dpc <- Dp.data.reared[Dp.data.reared$Treatment=="Control",]

summary(lm(Egg.Size~Egg.Number+lHindwing.Perimeter,data=Pr.data.reared[Pr.data.reared$Treatment=="Control",]))
summary(lm(Egg.Size~Egg.Number+lHindwing.Perimeter,data=Pr.data.reared[Pr.data.reared$Treatment=="Treatment",]))

summary(lm(Egg.Size~Egg.Number+lHindwing.Perimeter,data=Dp.data.reared[Dp.data.reared$Treatment=="Control",]))
summary(lm(Egg.Size~Egg.Number+lHindwing.Perimeter,data=Dp.data.reared[Dp.data.reared$Treatment=="Treatment",]))




Pr.egg.life.plot <- ggplot(data=Pr.data.reared,aes(x=Egg.Number,y=Lifespan,color=Treatment,shape=Treatment))+ 
  geom_point(data=Pr.data.reared,size=I(5)) + scale_color_grey(start=0.6,end=0.05)+
  theme(legend.position=c(.9,.9))+
  stat_smooth(aes(x=Egg.Number,y=Lifespan),se=F,method=lm,data=Pr.data.reared[Pr.data.reared$Treatment=="Control",],span=1,color="black",lwd=1,lty=2)+
  stat_smooth(aes(x=Egg.Number,y=Lifespan),se=F,method=lm,data=Pr.data.reared[Pr.data.reared$Treatment=="Treatment",],span=1,color="black",lwd=1,lty=2)+
  xlab("Fecundity (number of eggs)") + ylab("Lifespan (days)")
Pr.egg.life.plot

Dp.egg.life.plot <- ggplot(data=Dp.data.reared,aes(x=Egg.Number,y=Lifespan,color=Treatment,shape=Treatment))+ 
  geom_point(data=Dp.data.reared,size=I(5)) + scale_color_grey(start=0.6,end=0.05)+
  theme(legend.position=c(.90,.90))+
  annotate("text",label="R^2 == 0.418",x=100,y=95,parse=T)+
  stat_smooth(aes(x=Egg.Number,y=Lifespan),se=F,method=lm,data=Dp.data.reared[Dp.data.reared$Treatment=="Control",],span=1,color="black",lwd=1,lty=2)+ 
  stat_smooth(aes(x=Egg.Number,y=Lifespan),se=F,method=lm,data=Dp.data.reared[Dp.data.reared$Treatment=="Treatment",],span=1,color="black",lwd=1,lty=1)+
  xlab("Fecundity (number of eggs)") + ylab("Lifespan (days)")
Dp.egg.life.plot

comb <- plot_grid(Pr.egg.life.plot,Dp.egg.life.plot,align="v",nrow=2,labels=c("A. Pieris rapae","B. Danaus plexippus"))
comb
#save_plot("Lifespan by fecundity.pdf",comb,base_aspect_ratio=1)
ggsave("Lifespan by fecundity.pdf",height=8,width=5, comb)

print(comb)

