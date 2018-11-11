
###Removing any previously existing objects. This is useful if you are debugging or repeatedly running analysis, as it prevents
rm(list=ls())


###Adding commentary 2
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
  
  data.collected <- read.csv(file="data for analysis.csv")                   ###Load in data.collected
  data.collected <- data.collected[data.collected$Provenance=="Collected",]                         ###Limit data.collected to just lab reared individuals
  data.collected <-data.collected[data.collected$sex!="M",]                                      ###Remove any males
  data.collected$Treatment <- data.collected$conTreatment                              ###None replaced with control
  
  ### analysis limited to certain species
  data.collected <- data.collected[data.collected$species=="Danaus_plexippus"|data.collected$species=="Pieris_rapae"|data.collected$species=="Vanessa_atalanta"|data.collected$species=="Colias_interior"|data.collected$species=="Colias_philodice"|data.collected$species=="Papilio_glaucus"|data.collected$species=="Papilio_polyxenes"|data.collected$species=="Pieris_olaracea"|data.collected$species=="Pontia_occidentalis"|data.collected$species=="Pontia_protodice"|data.collected$species=="Satyrodes_eurydice",]
  ###Reduction to variables I am going to analyze in this section
  data.collected<-na.omit(data.frame("ID"=data.collected$ID,"Brain.Weight"=data.collected$Brain.Weight, "Treatment"=data.collected$Treatment,"Hindwing.Perimeter" = data.collected$Hindwing.Perimeter, "species"=data.collected$species))
  #data.collected <- data.collected[data.collected$species%in%names(table(data.collected$species)[table(data.collected$species)>9]),]
  data.collected$Treatment<-factor(data.collected$Treatment)  ###Turning treatment into a factor
  data.collected$species<-factor(data.collected$species)  ###Turning species into a factor
  data.collected$lHindwing.Perimeter <- log(data.collected$Hindwing.Perimeter) ###Log transforming morphological variable
  data.collected$lBrain.Weight <- log(data.collected$Brain.Weight) ###Log transforming morphological variable
  
  ###Splitting up data.collected by species for t-tests
  Pr.data.collected <- data.collected[data.collected$species=="Pieris_rapae",]
  Dp.data.collected <- data.collected[data.collected$species=="Danaus_plexippus",]
  Ci.data.collected <- data.collected[data.collected$species=="Colias_interior",]
  Pg.data.collected <- data.collected[data.collected$species=="Papilio_glaucus",]
  Cp.data.collected <- data.collected[data.collected$species=="Colias_philodice",]
  Va.data.collected <- data.collected[data.collected$species=="Vanessa_atalanta",]
  
  ###Running glms to compare control and treatment
  summary(lm(lBrain.Weight~Treatment,data=Pr.data.collected))
  summary(lm(lBrain.Weight~Treatment,data=Dp.data.collected))
  summary(lm(lBrain.Weight~Treatment,data=Ci.data.collected))
  summary(lm(lBrain.Weight~Treatment,data=Cp.data.collected))
  summary(lm(lBrain.Weight~Treatment,data=Pg.data.collected))
  summary(lm(lBrain.Weight~Treatment,data=Va.data.collected))
  

  summary(lm(lHindwing.Perimeter~Treatment,data=Pr.data.collected))
  summary(lm(lHindwing.Perimeter~Treatment,data=Dp.data.collected))
  summary(lm(lHindwing.Perimeter~Treatment,data=Ci.data.collected))
  summary(lm(lHindwing.Perimeter~Treatment,data=Cp.data.collected))
  summary(lm(lHindwing.Perimeter~Treatment,data=Pg.data.collected))
  summary(lm(lHindwing.Perimeter~Treatment,data=Va.data.collected))
  
  
  
  


data.reared <- read.csv(file="data for analysis with dates.csv")                   ###Load in data.reared
data.reared <- data.reared[data.reared$Provenance=="Reared",]                         ###Limit data.reared to just lab reared individuals
data.reared <-data.reared[data.reared$sex!="M",]                                      ###Remove any males
data.reared$Treatment <- data.reared$conTreatment                              ###None replaced with control

### analysis limited to certain species
data.reared <- data.reared[data.reared$Group=="Brain",]
data.reared <- data.reared[data.reared$species=="Danaus_plexippus"|data.reared$species=="Pieris_rapae"|data.reared$species=="Vanessa_atalanta"|data.reared$species=="Colias_interior"|data.reared$species=="Colias_philodice"|data.reared$species=="Papilio_glaucus"|data.reared$species=="Papilio_polyxenes"|data.reared$species=="Pieris_olaracea"|data.reared$species=="Pontia_occidentalis"|data.reared$species=="Pontia_protodice"|data.reared$species=="Satyrodes_eurydice",]

###Reduction to variables I am going to analyze in this section
data.reared<-na.omit(data.frame("ID"=data.reared$ID,"Year" = as.factor(data.reared$Year), "Hindwing.Perimeter"=data.reared$Hindwing.Perimeter, "Brain.Weight"=data.reared$Brain.Weight,"Treatment"=data.reared$Treatment,"species"=data.reared$species))
data.reared <- data.reared[data.reared$species%in%names(table(data.reared$species)[table(data.reared$species)>9]),]
data.reared$Treatment<-factor(data.reared$Treatment)  ###Turning treatment into a factor
data.reared$species<-factor(data.reared$species)  ###Turning species into a factor
data.reared$lBrain.Weight <- log(data.reared$Brain.Weight+1)
data.reared$lHindwing.Perimeter <- log(data.reared$Hindwing.Perimeter) ###Log transforming morphological variable
data.reared$Date.Eclosed<- as.Date(data.reared$Date.Eclosed,format="%m/%d")



###Splitting up data.reared by species for t-tests
Pr.data.reared <- data.reared[data.reared$species=="Pieris_rapae",]
Dp.data.reared <- data.reared[data.reared$species=="Danaus_plexippus",]
Va.data.reared <- data.reared[data.reared$species=="Vanessa_atalanta",]
Pp.data.reared <- data.reared[data.reared$species=="Papilio_polyxenes",]

###Absolute brain size
summary(lm(lBrain.Weight~Treatment+Year,data=Pr.data.reared))
summary(lm(lBrain.Weight~Treatment,data=Dp.data.reared))
summary(lm(lBrain.Weight~Treatment,data=Va.data.reared))

###Relative brain size
summary(lm(lBrain.Weight~Treatment+Year+lHindwing.Perimeter,data=Pr.data.reared))
summary(lm(lBrain.Weight~Treatment+lHindwing.Perimeter,data=Dp.data.reared))
summary(lm(lBrain.Weight~Treatment+lHindwing.Perimeter,data=Va.data.reared))
summary(lm(Brain.Weight~Treatment,data=Pp.data.reared))
        
        
summary(lm(lHindwing.Perimeter~Treatment,data=Pr.data.reared))
summary(lm(lHindwing.Perimeter~Treatment,data=Dp.data.reared))
summary(lm(lHindwing.Perimeter~Treatment,data=Pg.data.collected))
summary(lm(lHindwing.Perimeter~Treatment,data=Va.data.reared))
        
        
        
        
        
        
        
        
        
        


###Plot for reared 
#sig <- factor(c('*','*','*'))
#labs <- data.frame("species" = as.character(levels(factor(data.collected$species))),sig,quant=rep(250,3))
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


#comb.plot<- ggdraw()+
#  draw_plot(g.reared,x=0,y=0,width=3.5/5.5,height=1)+
#  draw_plot(g.collected,x=.60,y=.05,width=2/5.5,height=.95)+
#  draw_plot_label(c("A", "B"), c(.1,.65), c(1, 1), size = 15)


comb.plot <- plot_grid(g.reared,g.collected,align='h',labels=c("A", "B"),ncol=2,rel_widths=c(4,2.5))
comb.plot

ggsave("Combined Brain Weight plots reared and collected.pdf",comb.plot)

