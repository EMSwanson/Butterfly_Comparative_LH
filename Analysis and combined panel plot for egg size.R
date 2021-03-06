
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
##  Egg Size  ##
#####        ####
#################

data.collected <- read.csv(file="data for analysis with dates.csv")                   ###Load in data.collected
data.collected <- data.collected[data.collected$Provenance=="Collected",]                         ###Limit data.collected to just lab reared individuals
data.collected <-data.collected[data.collected$sex!="M",]                                      ###Remove any males
data.collected$Treatment <- data.collected$conTreatment                              ###None replaced with control

### analysis limited to certain species
data.collected <- data.collected[data.collected$species=="Danaus_plexippus"|data.collected$species=="Pieris_rapae"|data.collected$species=="Vanessa_atalanta"|data.collected$species=="Colias_interior"|data.collected$species=="Colias_philodice"|data.collected$species=="Papilio_glaucus"|data.collected$species=="Papilio_polyxenes"|data.collected$species=="Pieris_olaracea"|data.collected$species=="Pontia_occidentalis"|data.collected$species=="Pontia_protodice"|data.collected$species=="Satyrodes_eurydice",]
###Reduction to variables I am going to analyze in this section
data.collected<-na.omit(data.frame("ID"=data.collected$ID,"Egg.Size"=data.collected$Average.egg.perimeter,"Treatment"=data.collected$Treatment,"species"=data.collected$species,"Hindwing.Perimeter" = data.collected$Hindwing.Perimeter, "Date.Eclosed"=data.collected$Date.Eclosed,"Year"=as.factor(data.collected$Year)))
#data.collected <- data.collected[data.collected$species%in%names(table(data.collected$species)[table(data.collected$species)>9]),]
data.collected$Treatment<-factor(data.collected$Treatment)  ###Turning treatment into a factor
data.collected$species<-factor(data.collected$species)  ###Turning species into a factor
data.collected$lHindwing.Perimeter <- log(data.collected$Hindwing.Perimeter) ###Log transforming morphological variable
data.collected$lEgg.Size <- log(data.collected$Egg.Size) ###Log transforming morphological variable
data.collected$Date.Eclosed<- as.Date(data.collected$Date.Eclosed,format="%m/%d")

###Splitting up data.collected by species for t-tests
Pr.data.collected <- data.collected[data.collected$species=="Pieris_rapae",]
#Dp.data.collected <- data.collected[data.collected$species=="Danaus_plexippus",]
Ci.data.collected <- data.collected[data.collected$species=="Colias_interior",]
#Pg.data.collected <- data.collected[data.collected$species=="Papilio_glaucus",]
Cp.data.collected <- data.collected[data.collected$species=="Colias_philodice",]
#Va.data.collected <- data.collected[data.collected$species=="Vanessa_atalanta",]

###Running glms to compare control and treatment
summary(lm(lEgg.Size~Treatment,data=Pr.data.collected))
summary(lm(lEgg.Size~Treatment+lHindwing.Perimeter,data=Pr.data.collected))
summary(lm(Egg.Size~Treatment,data=Ci.data.collected))
summary(lm(Egg.Size~Treatment,data=Cp.data.collected))
#summary(glm(Egg.Size~Treatment,data=Va.data.collected,family='poisson'))





data.reared <- read.csv(file="data for analysis with dates.csv")                   ###Load in data.reared
data.reared <- data.reared[data.reared$Provenance=="Reared",]                         ###Limit data.reared to just lab reared individuals
data.reared <-data.reared[data.reared$sex!="M",]                                      ###Remove any males
data.reared$Treatment <- data.reared$conTreatment                              ###None replaced with control

### analysis limited to certain species
data.reared <- data.reared[data.reared$Group=="Brain",]
data.reared <- data.reared[data.reared$species=="Danaus_plexippus"|data.reared$species=="Pieris_rapae"|data.reared$species=="Vanessa_atalanta"|data.reared$species=="Colias_interior"|data.reared$species=="Colias_philodice"|data.reared$species=="Papilio_glaucus"|data.reared$species=="Papilio_polyxenes"|data.reared$species=="Pieris_olaracea"|data.reared$species=="Pontia_occidentalis"|data.reared$species=="Pontia_protodice"|data.reared$species=="Satyrodes_eurydice",]

###Reduction to variables I am going to analyze in this section
data.reared<-na.omit(data.frame("ID"=data.reared$ID,"Egg.Size"=data.reared$Average.egg.perimeter,"Treatment"=data.reared$Treatment,"species"=data.reared$species,"Hindwing.Perimeter" = data.reared$Hindwing.Perimeter, "Date.Eclosed"=data.reared$Date.Eclosed,"Year"=as.factor(data.reared$Year)))
data.reared <- data.reared[data.reared$species%in%names(table(data.reared$species)[table(data.reared$species)>9]),]
data.reared$Treatment<-factor(data.reared$Treatment)  ###Turning treatment into a factor
data.reared$species<-factor(data.reared$species)  ###Turning species into a factor
data.reared$lHindwing.Perimeter <- log(data.reared$Hindwing.Perimeter) ###Log transforming morphological variable
data.reared$lEgg.Size <- log(data.reared$Egg.Size) ###Log transforming morphological variable
data.reared$Date.Eclosed<- as.Date(data.reared$Date.Eclosed,format="%m/%d")

###Splitting up data.reared by species for t-tests
Pr.data.reared <- data.reared[data.reared$species=="Pieris_rapae",]
Dp.data.reared <- data.reared[data.reared$species=="Danaus_plexippus",]
#Va.data.reared <- data.reared[data.reared$species=="Vanessa_atalanta",]
#Pp.data.reared <- data.reared[data.reared$species=="Papilio_polyxenes",]

###Running glms to compare control and treatment
summary(lm(lEgg.Size~Treatment,data=Pr.data.reared))
summary(lm(lEgg.Size~Treatment,data=Dp.data.reared))
#summary(glm(Egg.Size~Treatment,data=Va.data.reared,family='poisson'))
#summary(glm(Egg.Size~Treatment,data=Pp.data.reared,family='poisson'))
summary(lm(lEgg.Size~egg_number,data=Pr.data.reared))
summary(lm(lEgg.Size~egg_number,data=Dp.data.reared))










l.data.reared <- read.csv(file="data for analysis with dates.csv")                   ###Load in l.data.reared
l.data.reared <- l.data.reared[l.data.reared$Provenance=="Reared",]                         ###Limit l.data.reared to just lab reared individuals
l.data.reared <-l.data.reared[l.data.reared$sex!="M",]                                      ###Remove any males
l.data.reared$Treatment <- l.data.reared$conTreatment                              ###None replaced with control

### analysis limited to certain species
l.data.reared <- l.data.reared[l.data.reared$Group=="Lifespan",]
l.data.reared <- l.data.reared[l.data.reared$species=="Danaus_plexippus"|l.data.reared$species=="Pieris_rapae"|l.data.reared$species=="Vanessa_atalanta"|l.data.reared$species=="Colias_interior"|l.data.reared$species=="Colias_philodice"|l.data.reared$species=="Papilio_glaucus"|l.data.reared$species=="Papilio_polyxenes"|l.data.reared$species=="Pieris_olaracea"|l.data.reared$species=="Pontia_occidentalis"|l.data.reared$species=="Pontia_protodice"|l.data.reared$species=="Satyrodes_eurydice",]

###Reduction to variables I am going to analyze in this section
l.data.reared<-na.omit(data.frame("ID"=l.data.reared$ID,"Egg.Size"=l.data.reared$Average.egg.perimeter,"egg_number" = l.data.reared$egg_number,"Lifespan"=l.data.reared$Lifespan..days., "Treatment"=l.data.reared$Treatment,"species"=l.data.reared$species,"Year"=as.factor(l.data.reared$Year),"Date.Eclosed"=l.data.reared$Date.Eclosed))
l.data.reared <- l.data.reared[l.data.reared$species%in%names(table(l.data.reared$species)[table(l.data.reared$species)>9]),]
l.data.reared$Treatment<-factor(l.data.reared$Treatment)  ###Turning treatment into a factor
l.data.reared$species<-factor(l.data.reared$species)  ###Turning species into a factor
l.data.reared$lHindwing.Perimeter <- log(l.data.reared$Hindwing.Perimeter) ###Log transforming morphological variable
l.data.reared$Date.Eclosed<- as.Date(l.data.reared$Date.Eclosed,format="%m/%d")
l.data.reared$lEgg.Size <- log(l.data.reared$Egg.Size) ###Log transforming morphological variable


###Splitting up l.data.reared by species for t-tests
Pr.l.data.reared <- l.data.reared[l.data.reared$species=="Pieris_rapae",]
Dp.l.data.reared <- l.data.reared[l.data.reared$species=="Danaus_plexippus",]
#Va.l.data.reared <- l.data.reared[l.data.reared$species=="Vanessa_atalanta",]
#Pp.l.data.reared <- l.data.reared[l.data.reared$species=="Papilio_polyxenes",]

###Running glms to compare control and treatment
summary(lm(Egg.Size~Treatment,data=Pr.l.data.reared))
summary(glm(Lifespan~Treatment*Year+Treatment*Date.Eclosed+Egg.Size+egg_number,data=Pr.l.data.reared,family='poisson'))
summary(glm(Lifespan~Egg.Size+egg_number,data=Pr.l.data.reared,family='poisson'))



summary(glm(Lifespan~Treatment*Date.Eclosed+Egg.Size+egg_number*Treatment,data=Dp.l.data.reared,family='poisson'))
summary(glm(Lifespan~Treatment*Date.Eclosed+Egg.Size*Treatment+egg_number*Treatment,data=Dp.l.data.reared,family='poisson'))
#summary(glm(Egg.Size~Treatment,data=Va.l.data.reared,family='poisson'))
#summary(glm(Egg.Size~Treatment,data=Pp.l.data.reared,family='poisson'))
summary(lm(lEgg.Size~egg_number,data=Pr.l.data.reared))
summary(lm(lEgg.Size~egg_number,data=Dp.l.data.reared))



plot(Dp.l.data.reared$Date.Eclosed,Dp.l.data.reared$Lifespan,col=as.numeric(Dp.l.data.reared$Treatment))














###Plot for reared lifespan butterflies

give.n <- function(x)  return(c(y = 1, label = length(x)))
l.data.reared$species <- gsub('_', " ", l.data.reared$species)
dodge <- position_dodge(width=0.75)
g <- ggplot(l.data.reared, aes(x = species,fill = Treatment, y= Egg.Size,xlab="Species")) + 
  stat_boxplot(geom='errorbar',coef=0.5)
g.reared.life <- g+geom_boxplot(coef=0.5) +
  xlab("Reared Females At Death") + ylab("Egg Size") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) + 
  stat_summary(fun.data= give.n ,position=dodge,geom='text') +
  theme(axis.text.x=element_text(angle=45,hjust=1),axis.text.y=element_blank(),axis.title.y=element_blank()) + theme(legend.position="none") +
  aes(ymin = 1, ymax=4) + geom_blank()


###Plot for reared 

give.n <- function(x)  return(c(y = 2, label = length(x)))
data.reared$species <- gsub('_', " ", data.reared$species)
dodge <- position_dodge(width=0.75)
g <- ggplot(data.reared, aes(x = species,fill = Treatment, y= Egg.Size,xlab="Species")) + 
  stat_boxplot(geom='errorbar',coef=0.5)
g.reared <- g+geom_boxplot(coef=0.5) +
  xlab("Reared Females") + ylab("Egg Size") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) + 
  stat_summary(fun.data= give.n ,position=dodge,geom='text') +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + theme(legend.position="none") +
  aes(ymin = 2, ymax=4) + geom_blank()

###Plot for collected
give.n <- function(x)  return(c(y = 2, label = length(x)))
data.collected$species <- gsub('_', " ", data.collected$species)
dodge <- position_dodge(width=0.75)
g <- ggplot(data.collected, aes(x = species,fill = Treatment, y= Egg.Size,xlab="Species")) + 
  stat_boxplot(geom='errorbar',coef=0.5)
g.collected <- g+geom_boxplot(coef=0.5) +
  xlab("Collected Females") + ylab("Egg Size") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) + 
  stat_summary(fun.data = give.n ,position=dodge,geom='text') +
  theme(axis.text.x=element_text(angle=45,hjust=1),axis.text.y=element_blank(),axis.title.y=element_blank()) +
  aes(ymin = 2, ymax=4) + geom_blank()


#plot <- plot_grid(g.reared, g.collected, labels=c("A","B",ncol=2,nrow=1))

#pdf(file="Combined Egg Size plots for reared and collect.pdf",useDingbats=F)


#comb.plot<- ggdraw()+
#  draw_plot(g.reared,x=0,y=0,width=2.5/6.5,height=1)+
#  draw_plot(g.reared.life,x=0.38,y=0,width=2/6.5,height=1)+
#  draw_plot(g.collected,x=.68,y=.05,width=2/7,height=.95)+
#  draw_plot_label(c("A", "B", "C"), c(.1,.43,.73), c(1, 1, 1), size = 15)
comb.plot <- plot_grid(g.reared,g.reared.life,g.collected,align='h',labels=c("A", "B"),ncol=3,rel_widths=c(2.5,2,2))
comb.plot

ggsave("Combined Egg Size plots reared and collected.pdf",comb.plot)

