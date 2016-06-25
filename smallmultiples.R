library(dplyr)
library(ggplot2)
library(ggthemes)
#add plot titles and subtitles?

dataset <- read.csv('bundesliga1516.csv')
head(dataset,18)
str(dataset)

#slim dataset
dataset <- select(dataset,Club,Matchday,Goaldiff,Points)

facetgroup <- filter(dataset, Matchday == max(dataset$Matchday)) %>% arrange(desc(Goaldiff))
dataset$Club_f <- factor(dataset$Club, levels=facetgroup$Club)

mins <- group_by(dataset, Club) %>% slice(which.min(Goaldiff))
maxs <- group_by(dataset, Club) %>% slice(which.max(Goaldiff))
ends <- group_by(dataset, Club) %>% filter(Matchday == max(Matchday))

p <- ggplot(dataset, aes(x=Matchday, y=Goaldiff))
p + 
  geom_area(fill='#999999') + 
  geom_line(size=0.7) + 
  xlim(c(0,37)) + 
  facet_wrap(~Club_f, nrow = 3, ncol = 6) + 
  geom_point(data = mins, color = '#F57670', size = 4) +
  geom_point(data = maxs, color = '#649EFC', size = 4) +
  geom_point(data = ends, size = 2) +
  geom_text(data = ends, aes(label = Goaldiff), size = 4, hjust = 0, nudge_x = 0.9) +
  geom_text(data = mins, aes(label = Goaldiff), size = 4, vjust = -1) +
  geom_text(data = maxs, aes(label = Goaldiff), size = 4, vjust = 2.5) +
  theme_pander() + 
  theme(panel.background = element_rect(fill = "#E6E6E6"), panel.grid = element_blank(), axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

#manipulate only the teams that have both pos/neg values. take out teams with negative min values, take out teams with positive
minvals <- dataset %>% group_by(Club) %>% summarize(mins=min(Goaldiff))
maxvals <- dataset %>% group_by(Club) %>% summarize(maxes=max(Goaldiff))
total <- left_join(minvals,maxvals,by='Club')
slimset <- total %>% filter(mins*maxes<0)

teams <- slimset$Club
teams <- unique(teams)

addall <- data.frame()

#Creates intermediate dummy x-intercepts at 0 crossings
for (n in 1:length(teams)) {
  teamgroup <- filter(dataset,Club==teams[n])
  rx <- do.call("rbind",sapply(1:(nrow(teamgroup)-1), function(i){
    f <- lm(Matchday~Goaldiff, teamgroup[i:(i+1),])
    if (f$qr$rank < 2) return(NULL)
    r <- predict(f, newdata=data.frame(Goaldiff=0))
    if(teamgroup[i,]$Matchday < r & r < teamgroup[i+1,]$Matchday)
      return(data.frame(Matchday=r,Goaldiff=0))
    else return(NULL)
  }))

  rx$Club=teams[n]
  rx$Club_f=teams[n]
  
  addall <- bind_rows(addall, rx)
}

addall[,c(3,1,2,4)]

#final product
completeset <- bind_rows(dataset,addall)
completeset <- arrange(completeset, Club, Matchday)

#add previous year for comparison
dataset14 <- read.csv('bundesliga1415.csv')
dataset14 <- select(dataset14,team, matchday, goal.differential)
names(dataset14) <- c('Club', 'Matchday', 'Goaldiff14')
head(dataset14)

clubs14 <- as.character(unique(dataset14$Club))
clubs <- as.character(unique(completeset$Club))
#check differences in data set
setdiff(clubs,clubs14)

dataset14$Club <- as.character(dataset14$Club)
dataset14$Club[dataset14$Club=='Bayer Leverkusen'] <- 'Bayer 04 Leverkusen'
dataset14$Club[dataset14$Club=='Bayern Munich'] <- 'FC Bayern Muenchen'
dataset14$Club[dataset14$Club=='Schalke 04'] <- 'FC Schalke 04'
dataset14$Club[dataset14$Club=='Hertha BSC'] <- 'Hertha Berlin'
  
dataset14$Club <- as.factor(dataset14$Club)

#merge datasets
completeset2 <- left_join(completeset, dataset14, by=c('Club', 'Matchday'))
completeslim <- filter(completeset2, is.na(Goaldiff14)==FALSE)
ends2 <- group_by(completeslim, Club) %>% filter(Matchday == max(Matchday))

#final product
p4 <- ggplot(completeset2, aes(x=Matchday, y=Goaldiff))
p4 +
  geom_line(size=0.8) + 
  geom_area(data=filter(completeset2, Goaldiff<=0), fill="#F57670") +
  geom_area(data=filter(completeset2, Goaldiff>=0), fill="#649EFC") +
  geom_line(data=completeslim, aes(y=Goaldiff14), color='#545454', linetype='longdash') + 
  xlim(c(0,37)) + 
  facet_wrap(~Club_f, nrow = 3, ncol = 6) + 
  geom_point(data = mins, size = 2) +
  geom_point(data = maxs, size = 2) +
  geom_point(data = ends, size = 2) +
  geom_point(data = ends2, aes(y=Goaldiff14), size = 1, color = '#545454') + 
  geom_text(data = ends, aes(label = Goaldiff), size = 4, hjust = 0, nudge_x = 0.9) +
  geom_text(data = mins, aes(label = Goaldiff), size = 4, vjust = -1) +
  geom_text(data = maxs, aes(label = Goaldiff), size = 4, vjust = 2.5) +
  theme_pander() + 
  theme(panel.background = element_rect(fill = "#E6E6E6"), panel.grid = element_blank(), axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

# haven't got this to work (prevent overlap): geom_text(data = ends2, aes(y=Goaldiff14, label=Goaldiff14), size = 4, color = '#545454', hjust = 0, nudge_x = 1, check_overlap = TRUE) + 

#save slim data set
write.csv(completeset2, file='bundesliga1516slim.csv',row.names=F)
