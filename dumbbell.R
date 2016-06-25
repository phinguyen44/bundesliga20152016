library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggalt)

dataset <- read.csv('bundesliga1516slim.csv')
head(dataset,18)
str(dataset)

#slim data sets
dataset$Goaldiff14 <- ifelse(is.na(dataset$Goaldiff14==T),0,dataset$Goaldiff14)
dataset.gd <- select(dataset, Club_f, Matchday, Goaldiff, Goaldiff14, Points) %>% filter(Matchday == 34) %>% mutate(Diff = Goaldiff-Goaldiff14)
dataset.gd$Diff <- ifelse(is.na(dataset.gd$Diff==T),0,dataset.gd$Diff)
dataset.gd

#asterisk to teams that were in 2. Bundesliga in prior season (Darmstadt, Ingolstadt)
dataset.gd$Club_f <- ifelse(dataset.gd$Club_f=='SV Darmstadt 98','SV Darmstadt 98*',as.character(dataset.gd$Club_f))
dataset.gd$Club_f <- ifelse(dataset.gd$Club_f=='FC Ingolstadt 04','FC Ingolstadt 04*',as.character(dataset.gd$Club_f))

#arrange data by descending goaldifferential in 2015-2016 season
dataset.gd <- arrange(dataset.gd, Goaldiff)
dataset.gd$Club_f <- factor(dataset.gd$Club_f, levels=dataset.gd$Club_f)

#set up plot
p <- ggplot(data = dataset.gd)
p <- p + 
  #add plot titles and subtitles
  labs(x=NULL, y=NULL, title='Difference in Goal Differential, 2015-2016 to 2014-2015 Season', subtitle='Ordered by 2015-2016 Season Goal Differential', caption='* = Teams promoted to 1. Bundesliga for 2015-2016 season have goal differentials = 0 for 2014-2015 season') + 
  geom_segment(aes(y=Club_f, yend=Club_f, x=min(Goaldiff)-5, xend=max(Goaldiff)+5), color="#B2B2B2", size=0.15) + 
  #separate to create fill for dumbbell
  geom_dumbbell(aes(x=Goaldiff14,xend=Goaldiff,y=Club_f), point.size.l=10, point.colour.l="#B2B2B2") +
  geom_dumbbell(aes(x=Goaldiff14,xend=Goaldiff,y=Club_f),size=2, color="#B2B2B2", point.size.l=6, point.size.r=10, point.colour.l="white", point.colour.r="#649EFC") + 
  #clean up plot
  scale_y_discrete(expand=c(0.07,0)) + 
  theme_pander() + 
  theme(panel.grid = element_blank(), axis.line=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_text(size=16),axis.text.x=element_blank(),plot.title=element_text(face="bold"), plot.subtitle=element_text(face="italic", size=9, margin=margin(b=12)), plot.caption=element_text(size=7, margin=margin(t=12)), plot.margin=unit(rep(10, 4), "pt"))

#add text
p <- p + geom_text(aes(x=Goaldiff14, y=Club_f, label=Goaldiff14), color="#B2B2B2", alpha=0.7, size=4, vjust=2.5) +
  geom_text(aes(x=Goaldiff, y=Club_f, label=Goaldiff), color="#649EFC", size=4, vjust=2.5)

#add pseudo-legends
p <- p + geom_text(data=filter(dataset.gd, Club_f=='FC Bayern Muenchen'),aes(x=Goaldiff, y=Club_f, label='2015-2016'), color="#649EFC", size=4, vjust=-2, hjust=-0, fontface="bold") + 
  geom_text(data=filter(dataset.gd, Club_f=='FC Bayern Muenchen'),aes(x=Goaldiff14, y=Club_f, label='2014-2015'), color="#B2B2B2", size=4, vjust=-2, hjust=1, fontface="bold")

#add columns to show differences
p <- p + geom_rect(aes(xmin=max(Goaldiff)+10, xmax=max(Goaldiff)+20, ymin=-Inf, ymax=Inf), fill="#EFEFE3") + 
  geom_text(aes(label=Diff, y=Club_f, x=max(Goaldiff)+15,color=Diff,order=Goaldiff), fontface="bold", size=4) + 
  geom_text(data=filter(dataset.gd, Club_f=='FC Bayern Muenchen'),aes(x=max(Goaldiff)+15,y=Club_f,label='Diff'),size=4,vjust=-2,fontface='bold', color='#4D4D4D') + 
  scale_color_gradientn(colors=c('#F57670','#649EFC')) + 
  theme(legend.position='none')

#add legend for last and this year
p
