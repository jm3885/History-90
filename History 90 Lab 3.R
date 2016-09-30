#running packages
library(dplyr)
library(readr)
library(ggplot2)
library(RColorBrewer)

#reading data downloaded from IPUMS
a <- read_csv('data.csv')

#filtering out Alaska and Hawaii
a <- a%>%filter(!(STATEFIP %in% c(2,15)))

#filtering out ages X<15 or X>65
a <- a%>%filter(AGE>=15&AGE<=65)

#check to see if it worked
a%>%filter(AGE<15 | AGE>65 | STATEFIP %in% c(2,15))

#variables in my data frame: YEAR, SEX, RACE, OCCUPATION, PERWT

#Recoding race variable
RaceData <- a %>% mutate (Race=factor(ifelse(RACE==1,1,
                                             ifelse(RACE==2,2,
                                                    ifelse(RACE==3,3,4))),
                                      labels=c('White','Black','Native American', 'Asian')))     
table(RaceData$Race)

#create industry label by recoding OCC1950 (How do I organize these if there are codes all over the place?)
OccupationData <- RaceData %>% mutate (Occupation=factor(ifelse(OCC1950<100,1,
                                                                ifelse(OCC1950>979,2,
                                                                       ifelse(OCC1950<500 & OCC1950>200,3, 
                                                                              ifelse(OCC1950<200 & OCC1950>100 | OCC1950>800 & OCC1950<900,4,
                                                                                     ifelse(OCC1950>500 & OCC1950<700 | OCC1950>900 & OCC1950<979,5,6))))),
                                                         labels=c('professional','none','managerial/clerical/sales','farmers','craftsmen/operatives/laborers','service')))

#testing that we have the industries we think we have
table (OccupationData$Occupation)

#Recoding sex
SexData <- OccupationData %>% mutate(Sex = ifelse(SEX==1,'male','female'))

#select variables needed for graphing
b<-SexData %>% select (YEAR,PERWT,Sex,Race,Occupation)

#For the first graph, I want to group by YEAR, Sex, and Race
f1 <- b %>% group_by(Race, YEAR, Sex) %>% summarise(Number=sum(PERWT))

#For the second graph, I want to group by YEAR,Sex,Race,and Occupation.
f2 <- b %>% group_by (Race, YEAR, Sex, Occupation) %>% summarise (Number=sum(PERWT))

#Graphing Figure 1
ggplot(data=f1,aes(x=YEAR,y=Number,fill=Sex)) +
  geom_bar(stat='identity') +
  labs(x='Year',y='Number',fill='Sex',title='2.Population Aged 15 - 65 by Race, Year, and Sex, 1870-1920') +
  scale_y_continuous(label=scales::comma) +
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='Set2', guide=guide_legend(reverse=TRUE)) +
  facet_wrap(~Race,ncol=2, scales='free_y') +
  theme_bw()

#Same thing for Occupation
ggplot(data=f2,aes(x=YEAR,y=Number,fill=Occupation))+
  geom_bar(stat='identity',position='fill')+
  labs(x='Year',y='Percent of Population',fill ='Occupation',title='4. Occupation of Persons Aged 15 - 65 by Sex, Race, and Year, 1870-1920')+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='Set1') +
  facet_grid(Sex~.~Race) +
  theme_bw() + theme(legend.position='bottom')
