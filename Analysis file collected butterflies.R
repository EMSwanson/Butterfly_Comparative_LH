

###Removing any previously existing objects. This is useful if you are debugging or repeatedly running analysis, as it prevents
rm(list=ls())



setwd("C://Users/Eli Swanson/Dropbox/School/Data, analyses/Butterfly comparative project/Spring 2016")  ###Setting drive to use


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
##  Fecundity  ##
#####        ####
#################

data <- read.csv(file="Data for analysis.csv")                   ###Load in data
data <- data[data$Provenance=="Collected",]                         ###Limit data to just lab reared individuals
data <-data[data$sex!="M",]                                      ###Remove any males
data$Treatment <- data$conTreatment                              ###None replaced with control

### analysis limited to certain species
data <- data[data$species=="Danaus_plexippus"|data$species=="Pieris_rapae"|data$species=="Vanessa_atalanta"|data$species=="Colias_interior"|data$species=="Colias_philodice"|data$species=="Papilio_glaucus"|data$species=="Papilio_polyxenes"|data$species=="Pieris_olaracea"|data$species=="Pontia_occidentalis"|data$species=="Pontia_protodice"|data$species=="Satyrodes_eurydice",]
###Reduction to variables I am going to analyze in this section
data<-na.omit(data.frame("ID"=data$ID,"egg_number"=data$egg_number,"Treatment"=data$Treatment,"species"=data$species))
data <- data[data$species%in%names(table(data$species)[table(data$species)>5]),]
data$Treatment<-factor(data$Treatment)  ###Turning treatment into a factor
data$species<-factor(data$species)  ###Turning species into a factor
data$lHindwing.Perimeter <- log(data$Hindwing.Perimeter) ###Log transforming morphological variable

###Splitting up data by species for t-tests
Pr.data <- data[data$species=="Pieris_rapae",]
Dp.data <- data[data$species=="Danaus_plexippus",]
Ci.data <- data[data$species=="Colias_interior",]
Pg.data <- data[data$species=="Papilio_glaucus",]
Cp.data <- data[data$species=="Colias_philodice",]
Va.data <- data[data$species=="Vanessa_atalanta",]

###Running glms to compare control and treatment
summary(glm(egg_number~Treatment,data=Pr.data,family='poisson'))
summary(glm(egg_number~Treatment,data=Dp.data,family='poisson'))
summary(glm(egg_number~Treatment,data=Ci.data,family='poisson'))
summary(glm(egg_number~Treatment,data=Pg.data,family='poisson'))
summary(glm(egg_number~Treatment,data=Cp.data,family='poisson'))
summary(glm(egg_number~Treatment,data=Va.data,family='poisson'))


give.n <- function(x)  return(c(y = 350, label = length(x)))
data$species <- gsub('_', " ", data$species)


dodge <- position_dodge(width=0.75)
g <- ggplot(data, aes(x = species,fill = Treatment, y= egg_number,xlab="Species")) + 
  stat_boxplot(geom='errorbar',coef=0.5)
g.collected <- g+geom_boxplot(coef=0.5) +
  xlab("Species") + ylab("Fecundity") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) + 
  stat_summary(fun.data = give.n ,position=dodge,geom='text') +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  aes(ymin = 0, ymax=350) + geom_blank()



















###ggplot plotting approach
pdf(file="Egg number and treatment collected.pdf",useDingbats=F)  ###Opening a pdf file for writing. set usedingbats to false if you are going to edit
dodge <- position_dodge(width=0.75)
g <- ggplot(data, aes(x = species,fill = Treatment, y= egg_number,xlab="Species")) + 
  stat_boxplot(geom='errorbar',coef=0.5)
g+geom_boxplot(coef=0.5) +
  xlab("Species") + ylab("Fecundity") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) + 
  stat_summary(fun.data = give.n ,position=dodge,geom='text') +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  aes(ymin = 0, ymax=350) + geom_blank()
dev.off()
###This code used to put a dot for the mean, instead of sample size. Can replace later.



#################
#####        ####
###  Egg size  ##
#####        ####
#################

data <- read.csv(file="Data for analysis.csv")
data <- data[data$Provenance=="Reared",]
data <-data[data$sex!="M",]
data$Treatment <- data$conTreatment
data <- data[data$species=="Danaus_plexippus"|data$species=="Pieris_rapae",]
data<-na.omit(data.frame("ID"=data$ID,"Egg.Number"=data$egg_number,"Egg.Size"=data$Average.egg.perimeter,"Year"=data$Year,"Hindwing.Perimeter"=data$Hindwing.Perimeter,"Treatment"=data$Treatment,"species"=data$species,'LegLength'=data$Leg.Length.1))
data <- data[data$species%in%names(table(data$species)[table(data$species)>5]),]
data$Treatment<-factor(data$Treatment)
data$species<-factor(data$species)
data$lEgg.Size <- log(data$Egg.Size)
data$lHindwing.Perimeter <- log(data$Hindwing.Perimeter) ###Log transforming morphological variable

data$relEgg.Size <- data$lEgg.Size/data$lHindwing.Perimeter

Pr.data <- data[data$species=="Pieris_rapae",]
Dp.data <- data[data$species=="Danaus_plexippus",]

summary(lm(lEgg.Size~Treatment,data=Pr.data))
summary(lm(lEgg.Size~Treatment,data=Dp.data))

pdf(file="Egg size and treatment reared.pdf",useDingbats=F)  ###Opening a pdf file for writing. set usedingbats to false if you are going to edit
dodge <- position_dodge(width=0.75)
g <- ggplot(data, aes(x = species,fill = Treatment, y= lEgg.Size,xlab="Species")) + stat_boxplot(geom='errorbar',coef=0.5)
g+geom_boxplot(coef=0.5) +
  xlab("Species") + ylab("Egg Size") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
dev.off()

summary(lm(lEgg.Size~Treatment+lHindwing.Perimeter,data=Pr.data))
summary(lm(lEgg.Size~Treatment+lHindwing.Perimeter,data=Dp.data))

pdf(file=" Relative Egg size and treatment reared.pdf",useDingbats=F)  ###Opening a pdf file for writing. set usedingbats to false if you are going to edit
dodge <- position_dodge(width=0.75)
g <- ggplot(data, aes(x = species,fill = Treatment, y= relEgg.Size,xlab="Species")) + stat_boxplot(geom='errorbar',coef=0.5)
g+geom_boxplot(coef=0.5) +
  xlab("Species") + ylab("Egg Size") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
dev.off()




#################
#####        ####
###  Lifespan  ##
#####        ####
#################

data <- read.csv(file="Data for analysis.csv")
data <- data[data$Provenance=="Reared",]
data <-data[data$sex!="M",]
data$Treatment <- data$conTreatment
data <- data[data$species=="Danaus_plexippus"|data$species=="Pieris_rapae"|data$species=="Vanessa_atalanta"|data$species=="Colias_interior"|data$species=="Colias_philodice"|data$species=="Papilio_glaucus"|data$species=="Papilio_polyxenes"|data$species=="Pieris_olaracea"|data$species=="Pontia_occidentalis"|data$species=="Pontia_protodice"|data$species=="Satyrodes_eurydice",]
data<-na.omit(data.frame("ID"=data$ID,"Lifespan"=data$Lifespan,"Treatment"=data$Treatment,"species"=data$species))
data <- data[data$species%in%names(table(data$species)[table(data$species)>4]),]
data$Treatment<-factor(data$Treatment)
data$species<-factor(data$species)

###Splitting up data by species for t-tests
Pr.data <- data[data$species=="Pieris_rapae",]
Dp.data <- data[data$species=="Danaus_plexippus",]
Va.data <- data[data$species=="Vanessa_atalanta",]

###Running glms to compare control and treatment
summary(glm(Lifespan~Treatment,data=Pr.data,family='poisson'))
summary(glm(Lifespan~Treatment,data=Dp.data,family='poisson'))
summary(glm(Lifespan~Treatment,data=Va.data,family='poisson'))

pdf(file="Lifespan and treatment reared.pdf",useDingbats=F)  ###Opening a pdf file for writing. set usedingbats to false if you are going to edit
dodge <- position_dodge(width=0.75)
g <- ggplot(data, aes(x = species,fill = Treatment, y= Lifespan,xlab="Species")) + stat_boxplot(geom='errorbar',coef=0.5)
g+geom_boxplot(coef=0.5) +
  xlab("Species") + ylab("Lifespan") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
dev.off()





#################
#####        ####
##Brain Weight ##
#####        ####
#################

data <- read.csv(file="Data for analysis.csv")
data <- data[data$Provenance=="Reared",]
data$Treatment <- data$conTreatment
data <- na.omit(data.frame("ID"=data$ID,"Brain.Weight"=data$Brain.Weight,"Hindwing.Perimeter"=data$Hindwing.Perimeter,"Treatment"=data$Treatment,"species"=data$species))
data <- data[data$species%in%names(table(data$species)[table(data$species)>4]),]
data$Treatment<-factor(data$Treatment)
data$species<-factor(data$species)
data$lBrain.Weight <- log(data$Brain.Weight+1)
data$lHindwing.Perimeter <- log(data$Hindwing.Perimeter)
data$relBrain <- data$Brain.Weight/data$Hindwing.Perimeter

Pr.data <- data[data$species=="Pieris_rapae",]
Dp.data <- data[data$species=="Danaus_plexippus",]
Va.data <- data[data$species=="Vanessa_atalanta",]

summary(lm(lBrain.Weight~Treatment+lHindwing.Perimeter,data=Pr.data))
summary(lm(lBrain.Weight~Treatment+lHindwing.Perimeter,data=Dp.data))
summary(lm(lBrain.Weight~Treatment+lHindwing.Perimeter,data=Va.data))



pdf(file="Brain and treatment reared.pdf",useDingbats=F)  ###Opening a pdf file for writing. set usedingbats to false if you are going to edit
dodge <- position_dodge(width=0.75)
g <- ggplot(data, aes(x = species,fill = Treatment, y= lBrain.Weight,xlab="Species")) + stat_boxplot(geom='errorbar',coef=0.5)
g+geom_boxplot(coef=0.5) +
  xlab("Species") + ylab("Brain Weight") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
dev.off()



pdf(file="relBrain and treatment reared.pdf",useDingbats=F)  ###Opening a pdf file for writing. set usedingbats to false if you are going to edit
dodge <- position_dodge(width=0.75)
g <- ggplot(data, aes(x = species,fill = Treatment, y= relBrain,xlab="Species")) + stat_boxplot(geom='errorbar',coef=0.5)
g+geom_boxplot(coef=0.5) +
  xlab("Species") + ylab("Relative Brain Weight") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
dev.off()








#################
#####        ####
##  Leg Length ##
#####        ####
#################

data <- read.csv(file="Data for analysis.csv")
data <- data[data$Provenance=="Collected",]
data$Treatment <- data$conTreatment
data <- data[data$species=="Danaus_plexippus"|data$species=="Pieris_rapae"|data$species=="Vanessa_atalanta"|data$species=="Colias_interior"|data$species=="Colias_philodice"|data$species=="Papilio_glaucus"|data$species=="Papilio_polyxenes"|data$species=="Pieris_olaracea"|data$species=="Pontia_occidentalis"|data$species=="Pontia_protodice"|data$species=="Satyrodes_eurydice",]
data<-na.omit(data.frame("ID"=data$ID,"Leg.Length"=data$Leg.Length.1,"Hindwing.Perimeter"=data$Hindwing.Perimeter,"Treatment"=data$Treatment,"species"=data$species))
data <- data[data$species%in%names(table(data$species)[table(data$species)>4]),]
data$species <- factor(data$species)
data$Treatment<-factor(data$Treatment)
data$species<-factor(data$species)
data$lLeg.Length <- log(data$Leg.Length)
data$lHindwing.Perimeter <- log(data$Hindwing.Perimeter)

Pr.data <- data[data$species=="Pieris_rapae",]
Dp.data <- data[data$species=="Danaus_plexippus",]
Va.data <- data[data$species=="Vanessa_atalanta",]
Pg.data <- data[data$species=="Papilio_glaucus",]
Ci.data <- data[data$species=="Colias_interior",]
Cp.data <- data[data$species=="Colias_philodice",]
Po.data <- data[data$species=="Pontia_occidentalis",]

summary(lm(lLeg.Length~Treatment,data=Pr.data))
summary(lm(lLeg.Length~Treatment,data=Dp.data))
summary(lm(lLeg.Length~Treatment,data=Va.data))
summary(lm(lLeg.Length~Treatment,data=Pg.data))
summary(lm(lLeg.Length~Treatment,data=Ci.data))
summary(lm(lLeg.Length~Treatment,data=Cp.data))
summary(lm(lLeg.Length~Treatment,data=Po.data))

pdf(file="Leg Length and treatment reared.pdf",useDingbats=F)  ###Opening a pdf file for writing. set usedingbats to false if you are going to edit
dodge <- position_dodge(width=0.75)
g <- ggplot(data, aes(x = species,fill = Treatment, y= Leg.Length,xlab="Species")) + stat_boxplot(geom='errorbar',coef=0.5)
g+geom_boxplot(coef=0.5) +
  xlab("Species") + ylab("Leg Length") +
  stat_summary(fun.y=mean,geom="point",position=dodge,size=3,shape=1) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
dev.off()











