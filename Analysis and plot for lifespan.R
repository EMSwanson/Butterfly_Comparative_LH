

###Removing any previously existing objects. This is useful if you are debugging or repeatedly running analysis, as it prevents
rm(list=ls())



setwd("E://Dropbox/School/data, analyses/Butterfly comparative project/Spring 2016")  ###Setting drive to use


###Load packages not included in the base. Sciplot makes some interesting interaction plots. bbmle is Ben Bolker's package that allows some neat stuff like AICctab that I like
library(sciplot)  ###Some interesting plots available. I believe we don't end up using any of these in thise script.
library(bbmle)  
library(ggplot2)  ###Used for plotting here
library(cowplot)
library("gridExtra")


##################################################
##################################################
##########                         ###############
##########   Reared lifespan butterflies    ###############
##########                         ###############
##################################################
##################################################







l.data.reared <- read.csv(file="data for analysis with dates.csv")                   ###Load in l.data.reared
l.data.reared <- l.data.reared[l.data.reared$Provenance=="Reared",]                         ###Limit l.data.reared to just lab reared individuals
l.data.reared <-l.data.reared[l.data.reared$sex!="M",]                                      ###Remove any males
l.data.reared$Treatment <- l.data.reared$conTreatment                              ###None replaced with control

### analysis limited to certain species
l.data.reared <- l.data.reared[l.data.reared$Group=="Lifespan",]
l.data.reared <- l.data.reared[l.data.reared$species=="Danaus_plexippus"|l.data.reared$species=="Pieris_rapae"|l.data.reared$species=="Vanessa_atalanta"|l.data.reared$species=="Colias_interior"|l.data.reared$species=="Colias_philodice"|l.data.reared$species=="Papilio_glaucus"|l.data.reared$species=="Papilio_polyxenes"|l.data.reared$species=="Pieris_olaracea"|l.data.reared$species=="Pontia_occidentalis"|l.data.reared$species=="Pontia_protodice"|l.data.reared$species=="Satyrodes_eurydice",]

###Reduction to variables I am going to analyze in this section
l.data.reared<-na.omit(data.frame("ID"=l.data.reared$ID,"Lifespan"=l.data.reared$Lifespan,"Treatment"=l.data.reared$Treatment,"species"=l.data.reared$species,"Year"=l.data.reared$Year,"Date.Eclosed"=l.data.reared$Date.Eclosed,"egg_number"=l.data.reared$egg_number))
l.data.reared <- l.data.reared[l.data.reared$species%in%names(table(l.data.reared$species)[table(l.data.reared$species)>9]),]
l.data.reared$Treatment<-factor(l.data.reared$Treatment)  ###Turning treatment into a factor
l.data.reared$species<-factor(l.data.reared$species)  ###Turning species into a factor
l.data.reared$lHindwing.Perimeter <- log(l.data.reared$Hindwing.Perimeter) ###Log transforming morphological variable
l.data.reared$surv1 <- Surv(l.data.reared$Lifespan)
l.data.reared$Date.Eclosed<- as.Date(l.data.reared$Date.Eclosed,format="%m/%d")


###Splitting up l.data.reared by species for t-tests
Pr.l.data.reared <- l.data.reared[l.data.reared$species=="Pieris_rapae",]
Pr.l.data.reared$IDnums <- as.numeric(sapply(strsplit(as.character(Pr.l.data.reared$ID),split='Pr | P'), "[[", 2))

Dp.l.data.reared <- l.data.reared[l.data.reared$species=="Danaus_plexippus",]
Dp.l.data.reared$IDnums <- as.numeric(sapply(strsplit(as.character(Dp.l.data.reared$ID),split='Dp'), "[[", 2))

Va.l.data.reared <- l.data.reared[l.data.reared$species=="Vanessa_atalanta",]
#Pp.l.data.reared <- l.data.reared[l.data.reared$species=="Papilio_polyxenes",]

###Running glms to compare control and treatment
sum.Pieris.lifespan.2013 <- summary(glm(Lifespan~Treatment+Date.Eclosed,data=Pr.l.data.reared[Pr.l.data.reared$Year==2013,],family='poisson'))
sum.Pieris.lifespan.2014 <- summary(glm(Lifespan~Treatment*Date.Eclosed,data=Pr.l.data.reared[Pr.l.data.reared$Year==2014,],family='poisson'))
sum.Danaus.lifespan <- summary(glm(Lifespan~Treatment*Date.Eclosed,data=Dp.l.data.reared,family='poisson'))
sum.Vanessa.lifespan <- summary(glm(Lifespan~Treatment*Date.Eclosed,data=Va.l.data.reared,family='poisson'))
#summary(glm(Lifespan~Treatment,data=Pp.l.data.reared,family='poisson'))

summary(glm(Lifespan~Treatment*Date.Eclosed+Treatment*Year+egg_number,data=Pr.l.data.reared,family='poisson'))
summary(Dp.glm <- glm(Lifespan~Treatment*Date.Eclosed,data=Dp.l.data.reared,family='poisson'))
summary(glm(Lifespan~Treatment*Date.Eclosed,data=Va.l.data.reared,family='poisson'))







pdf(file="Seasonal changes in lifespan.pdf",useDingbats=F,width=8,height=3)
par(mfrow=c(1,3))

###Plots
###PLot for Pieris
Pr.con.glm <- glm(Lifespan~Date.Eclosed+Year,data=Pr.l.data.reared[Pr.l.data.reared$Treatment=="Control",],family='poisson')
Pr.tre.glm <- glm(Lifespan~Date.Eclosed+Year,data=Pr.l.data.reared[Pr.l.data.reared$Treatment=="Treatment",],family='poisson')
e.seq <- c(seq(as.Date("2016-06-22"),as.Date("2016-09-15"),by=1),seq(as.Date("2016-06-22"),as.Date("2016-09-15"),by=1))
y.seq <- c(rep(2013,86), rep(2014,86))
plot(Pr.l.data.reared$Date.Eclosed,Pr.l.data.reared$Lifespan,col=ifelse(Pr.l.data.reared$Treatment=="Control","black","grey50"),cex=1.5,lwd=2,xlab="Date",ylab="Lifespan",frame.plot=F,cex.lab=1.3,cex.axis=1.3,pch=as.numeric(Pr.l.data.reared$Year)-2013)
legend("topright",pch=c(0,1),c("2013","2014"),lty=c(1,2),cex=0.8)

new.data <- data.frame(Date.Eclosed = e.seq, Year=y.seq)
new.predict.con <- predict(Pr.con.glm, newdata=new.data, type="response")
new.predict.tre <- predict(Pr.tre.glm, newdata=new.data, type="response")

lines(new.data[145:172,1],new.predict.con[59:86],col="black",lwd=2)
lines(new.data[1:33,1],new.predict.con[87:119],col="black",lwd=2,lty=2)
lines(new.data[145:172,1],new.predict.tre[16:43],col="grey50",lwd=2)
lines(new.data[1:33,1],new.predict.tre[87:119],col="grey50",lwd=2,lty=2)




Va.con.glm <- glm(Lifespan~Date.Eclosed,data=Va.l.data.reared[Va.l.data.reared$Treatment=="Control",],family='poisson')
Va.tre.glm <- glm(Lifespan~Date.Eclosed,data=Va.l.data.reared[Va.l.data.reared$Treatment=="Treatment",],family='poisson')
plot(Va.l.data.reared$Date.Eclosed,Va.l.data.reared$Lifespan,col=ifelse(Va.l.data.reared$Treatment=="Control","black","gray50"),xlab="Date",ylab="Lifespan",cex=1.5,lwd=2,cex.lab=1.3,cex.axis=1.3,frame.plot=F)
e.seq <- seq(as.Date("2016-07-31"),as.Date("2016-08-05"),by=1)
new.data <- data.frame(Date.Eclosed = e.seq)
new.predict.con <- predict(Va.con.glm, newdata=new.data, type="response")
new.predict.tre <- predict(Va.tre.glm, newdata=new.data, type="response")
lines(new.data[,1],new.predict.con,col="black",lwd=2,lty=2)
lines(new.data[,1],new.predict.tre,col="grey50",lwd=2,lty=2)
legend("topright",col=ifelse(Pr.l.data.reared$Treatment=="Control","black","grey50"),c("Control","Treatment"),lwd=2,cex=0.8)


Dp.con.glm <- glm(Lifespan~Date.Eclosed,data=Dp.l.data.reared[Dp.l.data.reared$Treatment=="Control",],family='poisson')
Dp.tre.glm <- glm(Lifespan~Date.Eclosed,data=Dp.l.data.reared[Dp.l.data.reared$Treatment=="Treatment",],family='poisson')
plot(Dp.l.data.reared$Date.Eclosed,Dp.l.data.reared$Lifespan,col=ifelse(Dp.l.data.reared$Treatment=="Control","black","gray50"),xlab="Date",ylab="Lifespan",cex=1.5,lwd=2,cex.lab=1.3,cex.axis=1.3,frame.plot=F)
e.seq <- seq(as.Date("2016-07-11"),as.Date("2016-07-22"),by=1)
new.data <- data.frame(Date.Eclosed = e.seq)
new.predict.con <- predict(Dp.con.glm, newdata=new.data, type="response")
new.predict.tre <- predict(Dp.tre.glm, newdata=new.data, type="response")
lines(new.data[,1],new.predict.con,col="black",lwd=2,lty=2)
lines(new.data[,1],new.predict.tre,col="grey50",lwd=2,lty=2)
dev.off()






lm1<-glm(Lifespan~Treatment*Year,data=Pr.l.data.reared,na.action=na.pass,family='poisson')
d<-dredge(lm1)
s1 <- subset(d)
m1 <- model.avg(d)
summary(m1)




write.csv(sum.Pieris.lifespan.2013$coef,"Pr treatment and lifespan 2013.csv")
write.csv(sum.Pieris.lifespan.2014$coef,"Pr treatment and lifespan 2014.csv")
write.csv(sum.Danaus.lifespan$coef,"Dp treatment and lifespan.csv")
write.csv(sum.Vanessa.lifespan$coef,"Va treatment and lifespan.csv")














###Plot for reared lifespan butterflies

give.n <- function(x)  return(c(y = -10, label = length(x)))
l.data.reared$species <- gsub('_', " ", l.data.reared$species)
sig <- factor(c('*',' ','*'))
labs <- data.frame("species" = as.character(levels(factor(l.data.reared$species))),sig,quant=rep(130,3))
dodge <- position_dodge(width=0.75)
g <- ggplot(l.data.reared, aes(x = species,fill = Treatment, y= Lifespan,xlab="Species")) + 
  stat_boxplot(geom='errorbar',coef=0.5)+annotate("text",x=labs$species,y=120,label=labs$sig,size=15)
g.reared.life <- g+geom_boxplot(coef=0.5) +
  xlab("Species") + ylab("Lifespan") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) + 
  stat_summary(fun.data= give.n ,position=dodge,geom='text') +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + theme(legend.position="right") +
  aes(ymin = 0, ymax=130) + geom_blank()
g.reared.life

ggsave("Lifespan plot.pdf",g.reared.life)



