#Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

#Reading in IPUMS data
a <- read_csv('Lab 4.csv')

#Create vector of age category labels
agecats <- '0-9'
for (i in 1:7) {
  agecats <- c(agecats,paste(i,'0-',i,9,sep=''))
}
agecats <- c(agecats,'80+')

#assigning variables to sex
b <- a %>% mutate(Sex=factor(SEX,labels=c('Male','Female')))

#separate ages by 0-9, 10-19, etc.
c <- b %>% mutate(Age=ifelse(AGE>=80,8,floor(AGE/10)))

#label with agecats labels to age
d <- c %>% mutate(Age=factor(Age,labels=agecats))

#separating out first and second generation immigrants
e <- d %>% mutate(Gen=ifelse(420<BPL & BPL<429,'First Generation',
                             ifelse((420<MBPL & MBPL<429 | 420<FBPL & FBPL<429) & (001<BPL & BPL<099), 'Second Generation',
                                    'Neither')))

#exclude data from Hawaii and Alaska
f <- e %>% filter(YEAR>=1960 | !(STATEFIP %in% c(2,15)))

#filtering out immigrants not from Western Europe
g <- f %>% filter(Gen != 'Neither')

#account for sample line response in 1940
g2 <- g %>% mutate(Weight=ifelse(YEAR==1940 & Gen=='Second Generation',
                                 SLWT,PERWT))

#Group by Age, Sex, Generation, and Year
h <- g2 %>% group_by(Age,Sex,Gen,YEAR) %>% summarise(Number=sum(Weight))

#flip male to other side of the graph
h2 <- h %>% mutate(Number=ifelse(Sex=='Male',-1 *Number,Number))

#make population period graph. 
#set correct number of pixels
png('population_pyramid_final.png',height=500,width=2000)
#set x and y axis
ggplot(data=h2,aes(x=Age,y=Number,fill=Sex)) +
#setting the values of the bar in the graph
  geom_bar(data=h2[h2$Sex=='Male',], stat='identity') +
  geom_bar(data=h2[h2$Sex=='Female',],stat='identity') +
#flipping the graph around so the y axis is horizontal
  coord_flip() +
#separating out/faceting the graphs by generation and year
  facet_grid(Gen~.~YEAR) +
#View h2 to determine range, set range based on number of immigrants per year
  scale_y_continuous(breaks=c(-100000,-50000,0,50000,100000),
                     labels=c('10','5','0','5','10')) +
#labeling the graph
  labs(y='Population in Tens of Thousands',title='Population Pyramids for Western European Immigrants and their Children') +
#setting the color palette of the graph
    scale_fill_brewer(palette='Set1',guide=guide_legend(reverse=TRUE))+
#setting "Sex" label above the legend
  guides(fill=guide_legend(title='Sex',title.position='top')) +
#setting background colors to black and white
  theme_bw() + theme(legend.position='bottom') 
dev.off()