library(dplyr)
library(plyr)
library(readr)
library("lubridate")
library("chron")
folder="C:/Users/Dominique Njinkeu/OneDrive - afrtsd.com/Project/Zips"

setwd(folder)
Accidents0514 <- read_csv("Accidents0514.csv")
Casualties0514 <- read_csv("Casualties0514.csv")
Vehicles0514 <- read_csv("Vehicles0514.csv")

#What fraction of accidents occur in urban areas? 
#Report the answer in decimal form.

urb<-select(Accidents0514,Urban_or_Rural_Area)%>%filter(Urban_or_Rural_Area==1)%>%dplyr::summarise(total_urban=n())

tot<-select(Accidents0514,Urban_or_Rural_Area)%>%   dplyr::summarise(total=n())

fraction=urb$total_urban/tot$total
fraction



#When is the most dangerous time to drive? 
#Find the hour of the day that has the highest
#occurance of fatal accidents, normalized by the total
#number of accidents that occured in
#that hour. For your answer, submit the corresponding frequency of fatal
#accidents to all
#accidents in that hour. Note: round accident times down. For example, if an accident occured
#at 23:55 it occured in hour 23.

Accidents0514<-Accidents0514 %>% mutate(dateT=as.Date(Date,format='%d%m%y'),hourT=hour(hm(Time)))


most_danger<-  group_by(filter(Accidents0514,Accident_Severity==max(Accident_Severity)),hourT)%>%
        dplyr::summarise(total_fatal_hrs=n())%>%
        arrange(desc(total_fatal_hrs))%>%
        left_join(group_by(Accidents0514,hourT) %>% 
                    dplyr::summarise(tot_hrs=n())%>% 
                    arrange(desc(tot_hrs))) %>%
                mutate(occ=total_fatal_hrs/tot_hrs)%>%
        arrange(desc(occ))%>%filter(occ==max(occ))

#most dangerous hour
most_danger$hourT

#There appears to be a linear trend in the number of 
#accidents that occur each year. What is
#that trend? Return the slope in units of 
#increased number of accidents per year.

accTs<-Accidents0514 %>% mutate(dateT=dmy(Date))%>%
  mutate(year=year(dateT))%>%
  group_by(year)%>%dplyr::summarise(tot_by_year=n())%>%
  select(tot_by_year,year)%>% 
  mutate(change=(log(tot_by_year/lag(tot_by_year,1))))%>%
  select(year,change)

plot(x=accTs$year,y=accTs$change)
reg<-lm(accTs$change~accTs$year)
abline(reg,col="red")

#the coefficient of log regression is percent change per year
#percent change is used as unit of increased number of accident
reg$coefficients[2]




#Do accidents in high-speed-limit areas have more casualties? 
#Compute the Pearson
#correlation coefficient between the speed limit and the ratio of the number of casualties to
#accidents for each speed limit. Bin the data by speed limit.
names(Casualties0514)[1]<-"index"
names(Accidents0514)[1]<-"index"
names(Vehicles0514)[1]<-"index"


casualties_per_accident<-select(Accidents0514,index,Number_of_Casualties,Speed_limit)

cor(casualties_per_accident$Number_of_Casualties,casualties_per_accident$Speed_limit,method = "pearson")

                    

plot(x=casualties_per_accident$Number_of_Casualties,y=casualties_per_accident$Speed_limit)

#How many times more likely are you to be in an accident where you skid, jackknife, or
#overturn (as opposed to an accident where you don't) when it's raining or snowing compared
#to nice weather with no high winds? Ignore accidents where the weather is unknown or
#missing.




skid_dummy<-Vehicles0514%>%mutate(skid_dummy=as.numeric(Skidding_and_Overturning>0))%>%select(index,skid_dummy)
dataforskidlikelihood<-Accidents0514%>% 
  #filtering situations where we have no rain,snow or nice weather with no high winds
  filter(!Weather_Conditions %in% c(9,4,7,8))%>%
  #creating dummy for nice and bad weather
  mutate(rain_or_snow_dummy=as.numeric(Weather_Conditions>1)) %>%
  select(index,rain_or_snow_dummy)%>%
  inner_join(skid_dummy,by=c("index"="index")) 

skid_when_itsrain<-glm(dataforskidlikelihood$skid_dummy~dataforskidlikelihood$rain_or_snow_dummy,family = binomial(link="logit"))

#we see statistical significance
summary(skid_when_itsrain)

#likelihood of skidding when it rains:
exp(skid_when_itsrain$coefficients[2])

# How many times more likely are accidents involving male car drivers to be fatal compared to
#accidents involving female car drivers? The answer should be the ratio of fatality rates of
#males to females. Ignore all accidents where the driver wasn't driving a car."""
casula_sex=Casualties0514 %>% filter(!Casualty_Class %in% c(2,3),!Sex_of_Casualty==-1) %>%
   select(index,Sex_of_Casualty)%>%
  inner_join( select(Accidents0514,index,Accident_Severity),by=c("index"="index"))%>%
  mutate(fatal_or_no_dummy=as.numeric(Accident_Severity>2))



fatal_sex<- glm(casula_sex$fatal_or_no_dummy~casula_sex$Sex_of_Casualty,family = binomial(link="logit"))
summary(fatal_sex)
# we see statistical significance
# the odds of being in fatal accident
exp(fatal_sex$coefficients[2])





#We can use the accident locations to estimate the areas of the police districts. Represent
#each as an ellipse with semi-axes given by a single standard deviation of the longitude and
#latitude. What is the area, in square kilometers, of the largest district measured in this
#manner?
          
          

#How fast do the number of car accidents drop off with age? Only consider car drivers who
#are legally allowed to drive in the UK (17 years or older). Find the rate at which the number of
#accidents exponentially decays with age. Age is measured in years. Assume that the number
#of accidents is exponentially distributed with age for driver's over the age of 17.

decay<-select(Casualties0514,index,Age_of_Casualty,Casualty_Class)%>%filter(!Casualty_Class %in% c(2,3),Age_of_Casualty>=17)%>%group_by(Age_of_Casualty)%>%
                                                        dplyr::summarise(Accidents_by_age=n())
                                                              
plot(x=decay$Age_of_Casualty,y=decay$Accidents_by_age)

decay.model<-lm(log(decay$Accidents_by_age)~decay$Age_of_Casualty)


summary(decay.model)
#explains 76.47% of variance
p<-exp(predict(decay.model,list(decay$Age_of_Casualty)))
plot(p)

#the accident decay rate is?
exp(decay.model$coefficients[2])
