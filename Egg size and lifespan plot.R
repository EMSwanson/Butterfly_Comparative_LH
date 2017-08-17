
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

data.reared <- read.csv(file="data for analysis with dates.csv")                   ###Load in data.reared
data.reared <- data.reared[data.reared$Provenance=="Reared",]                         ###Limit data.reared to just lab reared individuals
data.reared <- data.reared[data.reared$Group=="Lifespan",]
data.reared <-data.reared[data.reared$sex!="M",]                                      ###Remove any males
data.reared$Treatment <- data.reared$conTreatment   ###None replaced with control

### analysis limited to certain species
data.reared <- data.reared[data.reared$species=="Danaus_plexippus"|data.reared$species=="Pieris_rapae"|data.reared$species=="Vanessa_atalanta"|data.reared$species=="Colias_interior"|data.reared$species=="Colias_philodice"|data.reared$species=="Papilio_glaucus"|data.reared$species=="Papilio_polyxenes"|data.reared$species=="Pieris_olaracea"|data.reared$species=="Pontia_occidentalis"|data.reared$species=="Pontia_protodice"|data.reared$species=="Satyrodes_eurydice",]
###Reduction to variables I am going to analyze in this section
data.reared<-na.omit(data.frame("ID"=data.reared$ID,"Hindwing.Perimeter"=data.reared$Hindwing.Perimeter,"Lifespan"=data.reared$Lifespan,"Egg.Size"=data.reared$Average.egg.perimeter,"Treatment"=data.reared$Treatment,"species"=data.reared$species, "Year"=data.reared$Year,"Date.Eclosed"=data.reared$Date.Eclosed))
data.reared <- data.reared[data.reared$species%in%names(table(data.reared$species)[table(data.reared$species)>9]),]
data.reared$Treatment<-factor(data.reared$Treatment)  ###Turning treatment into a factor
data.reared$species<-factor(data.reared$species)  ###Turning species into a factor
data.reared$lHindwing.Perimeter <- log(data.reared$Hindwing.Perimeter) ###Log transforming morphological variable
data.reared$Date.Eclosed <- as.Date(data.reared$Date.Eclosed, format="%m/%d")


Pr.data.reared <- data.reared[data.reared$species=="Pieris_rapae",]
Prt <- Pr.data.reared[Pr.data.reared$Treatment=="Treatment",]
Prc <- Pr.data.reared[Pr.data.reared$Treatment=="Control",]

Dp.data.reared <- data.reared[data.reared$species=="Danaus_plexippus",]
Dpt <- Dp.data.reared[Dp.data.reared$Treatment=="Treatment",]
Dpc <- Dp.data.reared[Dp.data.reared$Treatment=="Control",]

summary(glm(Lifespan~Egg.Number+Year+Date.Eclosed,data=Pr.data.reared[Pr.data.reared$Treatment=="Control",],family='poisson'))
summary(glm(Lifespan~Egg.Number+Year+Date.Eclosed,data=Pr.data.reared[Pr.data.reared$Treatment=="Treatment",],family='poisson'))
summary(glm(Lifespan~Egg.Size+Treatment+Year*Treatment+Date.Eclosed*Treatment,data=Pr.data.reared,family='poisson'))


summary(glm(Lifespan~Egg.Number+Date.Eclosed,data=Dp.data.reared[Dp.data.reared$Treatment=="Control",],family="poisson"))
summary(glm(Lifespan~Egg.Number+Date.Eclosed,data=Dp.data.reared[Dp.data.reared$Treatment=="Treatment",],family="poisson"))
summary(glm(Lifespan~Treatment*Date.Eclosed+Egg.Size,data=Dp.data.reared,family="poisson"))




pdf(file="Reproduction lifespan tradeoff with year and treatment for Pieris and Danaus.pdf",useDingbats=F,width=8.5,heigh=5)
par(mfrow=c(1,2))
###PLot for Pieris
Pr.con.glm <- glm(Lifespan~Egg.Number+Year,data=Pr.data.reared[Pr.data.reared$Treatment=="Control",],family='poisson')
Pr.tre.glm <- glm(Lifespan~Egg.Number+Year,data=Pr.data.reared[Pr.data.reared$Treatment=="Treatment",],family='poisson')
e.seq <- c(seq(0,199,by=1),seq(0,199,by=1))
y.seq <- c(rep(2013,200), rep(2014,200))
plot(Pr.data.reared$Egg.Number,Pr.data.reared$Lifespan,col=ifelse(Pr.data.reared$Treatment=="Control","black","grey50"),pch=Pr.data.reared$Year-2013,cex=1.5,lwd=2,xlab="Fecundity",ylab="Lifespan",frame.plot=F,xlim=c(0,200),ylim=c(0,60))
legend("topright",pch=c(0,1),c("2013","2014"),lty=c(1,2),cex=0.8)

new.data <- data.frame(Egg.Number = e.seq, Year=y.seq)
new.predict.con <- predict(Pr.con.glm, newdata=new.data, type="response")
new.predict.tre <- predict(Pr.tre.glm, newdata=new.data, type="response")

lines(new.data[1:200,1],new.predict.con[1:200],col="black",lwd=2)
lines(new.data[201:400,1],new.predict.con[201:400],col="black",lwd=2,lty=2)
lines(new.data[1:200,1],new.predict.tre[1:200],col="grey50",lwd=2)
lines(new.data[201:400,1],new.predict.tre[201:400],col="grey50",lwd=2,lty=2)
text(0,60,"A")

###PLot for Danaus
Dp.con.glm <- glm(Lifespan~Egg.Number,data=Dp.data.reared[Dp.data.reared$Treatment=="Control",],family='poisson')
Dp.tre.glm <- glm(Lifespan~Egg.Number,data=Dp.data.reared[Dp.data.reared$Treatment=="Treatment",],family='poisson')
e.seq <- seq(0,249,by=1)
plot(Dp.data.reared$Egg.Number,Dp.data.reared$Lifespan,col=ifelse(Dp.data.reared$Treatment=="Control","black","grey50"),xlab="Fecundity",ylab="Lifespan",cex=1.5,lwd=2,frame.plot=F,ylim=c(0,120))

new.data <- data.frame(Egg.Number = e.seq)
new.predict.con <- predict(Dp.con.glm, newdata=new.data, type="response")
new.predict.tre <- predict(Dp.tre.glm, newdata=new.data, type="response")

lines(new.data[1:250,1],new.predict.con[1:250],col="black",lwd=2)
lines(new.data[1:250,1],new.predict.tre[1:250],col="grey50",lwd=2)
text(0,120,"B")
legend("topright",col=ifelse(levels(Dp.data.reared$Treatment)=="Control","black","grey50"),c("Control","Treatment"),lwd=2,cex=0.8)

dev.off()








Pr.egg.life.plot <- ggplot(data=Pr.data.reared,aes(x=Egg.Number,y=Lifespan,color=Treatment,shape=Treatment))+ 
  geom_point(data=Pr.data.reared,size=I(5)) + scale_color_grey(start=0.6,end=0.05)+
  theme(legend.position=c(.9,.9))+
  stat_smooth(aes(x=Egg.Number,y=Lifespan),se=F,method=lm,data=Pr.data.reared[Pr.data.reared$Treatment=="Control",],span=1,color="black",lwd=1,lty=2)+
  stat_smooth(aes(x=Egg.Number,y=Lifespan),se=F,method=lm,data=Pr.data.reared[Pr.data.reared$Treatment=="Treatment",],span=1,color="black",lwd=1,lty=2)+
  xlab("Fecundity (number of eggs)") + ylab("Lifespan (days)")+
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

