
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
data.reared <-data.reared[data.reared$sex!="M",]                                      ###Remove any males
data.reared$Treatment <- data.reared$conTreatment                              ###None replaced with control

### analysis limited to certain species
data.reared <- data.reared[data.reared$species=="Danaus_plexippus"|data.reared$species=="Pieris_rapae"|data.reared$species=="Vanessa_atalanta"|data.reared$species=="Colias_interior"|data.reared$species=="Colias_philodice"|data.reared$species=="Papilio_glaucus"|data.reared$species=="Papilio_polyxenes"|data.reared$species=="Pieris_olaracea"|data.reared$species=="Pontia_occidentalis"|data.reared$species=="Pontia_protodice"|data.reared$species=="Satyrodes_eurydice",]
###Reduction to variables I am going to analyze in this section
data.reared<-na.omit(data.frame("ID"=data.reared$ID,"Brain.Weight"=data.reared$Brain.Weight,"Egg.Number"=data.reared$egg_number,"Treatment"=data.reared$Treatment,"species"=data.reared$species,"Year"=as.factor(data.reared$Year)))
data.reared <- data.reared[data.reared$species%in%names(table(data.reared$species)[table(data.reared$species)>9]),]
data.reared$Treatment<-factor(data.reared$Treatment)  ###Turning treatment into a factor
data.reared$species<-factor(data.reared$species)  ###Turning species into a factor
data.reared$lHindwing.Perimeter <- log(data.reared$Hindwing.Perimeter) ###Log transforming morphological variable
data.reared$lBrain.Weight <- log (data.reared$Brain.Weight + 1)

plot(data.reared$Egg.Number,data.reared$Brain.Weight,col=as.numeric(data.reared$species),pch=as.numeric(data.reared$Treatment))


Pr.data.reared <- data.reared[data.reared$species=="Pieris_rapae",]
Dp.data.reared <- data.reared[data.reared$species=="Danaus_plexippus",]


summary(lm(lBrain.Weight~Egg.Number*Year+Treatment,data=Pr.data.reared))
summary(lm(lBrain.Weight~Egg.Number+Treatment,data=Dp.data.reared))


egg.n.plot <- ggplot(data=data.reared,aes(x=Egg.Number,y=Brain.Weight,color=Treatment,shape=species))+ 
  geom_point(data=data.reared,size=I(5))
egg.n.plot


+ 
  stat_smooth(aes(x=N.family.level,y=Number.Eggs) ,data=species.data,span=1,color="black",lwd=1)+ 
  geom_abline(intercept=a,slope=b,lty=2,lwd=1.3)+
  xlab("Plant family Nitrogen") + ylab("Number of Eggs")+ 
  annotate("text",label=paste("p = ",p),x=-1,y=2)+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22,face="bold"), legend.position="right")
egg.n.plot


###Plot for reared lifespan butterflies

give.n <- function(x)  return(c(y = 1, label = length(x)))
l.data.reared$species <- gsub('_', " ", l.data.reared$species)
dodge <- position_dodge(width=0.75)
g <- ggplot(l.data.reared, aes(x = species,fill = Treatment, y= Brain.Weight,xlab="Species")) + 
  stat_boxplot(geom='errorbar',coef=0.5)
g.reared.life <- g+geom_boxplot(coef=0.5) +
  xlab("Reared Females At Death") + ylab("Brain Weight") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) + 
  stat_summary(fun.data= give.n ,position=dodge,geom='text') +
  theme(axis.text.x=element_text(angle=45,hjust=1),axis.text.y=element_blank(),axis.title.y=element_blank()) + theme(legend.position="none") +
  aes(ymin = 1, ymax=4) + geom_blank()


###Plot for reared 

give.n <- function(x)  return(c(y = 0, label = length(x)))
data.reared$species <- gsub('_', " ", data.reared$species)
dodge <- position_dodge(width=0.75)
g <- ggplot(data.reared, aes(x = species,fill = Treatment, y= Brain.Weight,xlab="Species")) + 
  stat_boxplot(geom='errorbar',coef=0.5)
g.reared <- g+geom_boxplot(coef=0.5) +
  xlab("Reared Females") + ylab("Brain Weight") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) + 
  stat_summary(fun.data= give.n ,position=dodge,geom='text') +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + theme(legend.position="none") +
  aes(ymin = 0, ymax=0.75) + geom_blank()

###Plot for collected
give.n <- function(x)  return(c(y = 0, label = length(x)))
data.collected$species <- gsub('_', " ", data.collected$species)
dodge <- position_dodge(width=0.75)
g <- ggplot(data.collected, aes(x = species,fill = Treatment, y= Brain.Weight,xlab="Species")) + 
  stat_boxplot(geom='errorbar',coef=0.5)
g.collected <- g+geom_boxplot(coef=0.5) +
  xlab("Collected Females") + ylab("Brain Weight") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) + 
  stat_summary(fun.data = give.n ,position=dodge,geom='text') +
  theme(axis.text.x=element_text(angle=45,hjust=1),axis.text.y=element_blank(),axis.title.y=element_blank()) +
  aes(ymin = 0, ymax=0.75) + geom_blank()


#plot <- plot_grid(g.reared, g.collected, labels=c("A","B",ncol=2,nrow=1))

#pdf(file="Combined Brain Weight plots for reared and collect.pdf",useDingbats=F)


comb.plot<- ggdraw()+
  draw_plot(g.reared,x=0,y=0,width=3.5/5.5,height=1)+
  draw_plot(g.collected,x=.60,y=.05,width=2/5.5,height=.95)+
  draw_plot_label(c("A", "B"), c(.1,.65), c(1, 1), size = 15)

comb.plot

ggsave("Combined Brain Weight plots reared and collected.pdf",comb.plot)

