library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggalt)

dataset <- read.csv('bundesliga1516slim.csv')
head(dataset,18)
str(dataset)

#slim data sets
dataset.rank <- select(dataset, Club_f, Matchday, Goaldiff, Points) %>% filter(Matchday == 34)
dataset.rank

#convert to rank order
dataset.rank <- mutate(dataset.rank, GD.Rank=min_rank(desc(Goaldiff)))
dataset.rank <- arrange(dataset.rank, desc(Points))
#manually since order is hard to do compute programmatically without a full data set
dataset.rank$Points.Rank <- c(1,2,3,4,5,6,7,8,9,10,11,12,14,13,15,16,17,18)
dataset.rank <- select(dataset.rank, Club_f,Points.Rank,GD.Rank)
dataset.rank$Diff <- dataset.rank$Points.Rank - dataset.rank$GD.Rank

#arrange
dataset.rank <- arrange(dataset.rank, desc(Points.Rank))
dataset.rank$Club_f <- factor(dataset.rank$Club_f, levels=dataset.rank$Club_f)
str(dataset.rank)

#create dumbbell plots
p <- ggplot(data = dataset.rank, aes(y=Club_f))
p <- p + geom_segment(aes(yend=Club_f, x=0, xend=19), color="#B2B2B2", size=0.15) + 
  geom_dumbbell(aes(x=Points.Rank,xend=GD.Rank), point.size.r=7, point.size.l=10, point.colour.l="#B2B2B2") +
  #clean up plot
  scale_y_discrete(expand=c(0.07,0)) + 
  theme_pander() +
  theme(panel.grid = element_blank(), axis.line=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_text(size=16),axis.text.x=element_blank(),plot.title=element_text(face="bold"), plot.subtitle=element_text(face="italic", size=9, margin=margin(b=12)), plot.caption=element_text(size=7, margin=margin(t=12)), plot.margin=unit(rep(10, 4), "pt"))

p <- p + geom_text(data=filter(dataset.rank, Club_f=='FC Bayern Muenchen'),aes(x=Points.Rank, label='Rank(Points)'), color="#B2B2B2", size=3, vjust=-2, hjust=-0.1, fontface="bold") + 
  geom_text(data=filter(dataset.rank, Club_f=='FC Bayern Muenchen'),aes(x=GD.Rank, y=Club_f, label='Rank(GoalDiff)'), size=3, vjust=-2, hjust=1, fontface="bold")

#add columns to show differences
dataset.rank$Colors <- ifelse(dataset.rank$Diff==0,'black',ifelse(dataset.rank$Diff>0,'blue','red'))
Colors <- c('red','black','blue')

p <- p + geom_rect(aes(xmin=21, xmax=23, ymin=-Inf, ymax=Inf), fill="#EFEFE3") + 
  geom_text(aes(label=Diff, y=Club_f, x=22, color=dataset.rank$Colors), fontface="bold", size=4) + 
  scale_color_manual(values=c('black','blue','red')) +
  geom_text(data=filter(dataset.rank, Club_f=='FC Bayern Muenchen'),aes(x=22,y=Club_f,label='Diff'),size=4,vjust=-2.5,fontface='bold', color='#4D4D4D') + 
  theme(legend.position='none')
p
