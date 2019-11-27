
bp_tbl<-read.csv(file.choose(), stringsAsFactors = FALSE)


str(bp_tbl)

library(tidyverse)
library(sqldf)
library("RODBC")
#QUESTION 1

#A. Convert BPAlerts to BPStatus

names(bp_tbl)

#keeps all the same column names but changes BPAlerts
names(bp_tbl)<- c("ID", "SystolicValue","Diastolicvalue","BPStatus","ObservedTime")


sample_n(bp_tbl,10) #print 10 random rows

#B. 

sqldf("select distinct BPStatus, count(ID) from bp_tbl
      group by BPStatus")
#there are 7 null values in this column, so an ifelse() statement might overlook that
#we should leae null values as they are and be sure that they don't get incorrectly coded to 1 or 0

str(bp_tbl$BPStatus)

#create a new column called "Dichotomus" with a 1 or 0 indicating 
#whether BP is controlled or uncontrolled. 

bp_tbl$Dichotomus[bp_tbl$BPStatus=="Hypo1"]<-1
bp_tbl$Dichotomus[bp_tbl$BPStatus=="Normal"]<-1
bp_tbl$Dichotomus[bp_tbl$BPStatus=="Hypo2"]<-0
bp_tbl$Dichotomus[bp_tbl$BPStatus=="HTN1"]<-0
bp_tbl$Dichotomus[bp_tbl$BPStatus=="HTN2"]<-0
bp_tbl$Dichotomus[bp_tbl$BPStatus=="HTN3"]<-0

sample_n(bp_tbl,10) #print 10 random rows


#C.. 
myconn<-odbcConnect("dartmouth","sganesh","sganesh@qbs181")


demographics<-sqlQuery(myconn,"select * from Demographics")

#merged table
bp_demographics<-
sqldf("select A.*, B.* from bp_tbl A
      inner join demographics B
      on A.ID=B.contactid")

sample_n(bp_demographics,10) #print 10 random rows


#D.

sqldf("select count(ID) from bp_demographics")
sqldf("select count(ID) from bp_dem_1d")


str(bp_demographics$tri_imaginecareenrollmentemailsentdate)

library(lubridate)


#tri_imaginecareenrollmentemailsentdate and tri_enrollmentcompletedate are not in date format
bp_demographics$tri_imaginecareenrollmentemailsentdate<-mdy(bp_demographics$tri_imaginecareenrollmentemailsentdate)

bp_demographics$tri_enrollmentcompletedate<-mdy(bp_demographics$tri_enrollmentcompletedate)


#table that shows the number of readings per ID
summarized_observations<- 
sqldf("select ID, count(ID) as num_readings
      from bp_demographics
      group by ID")

#certain IDs have MANY observations, and certain IDs have very few. 
#The idea of applying a "12 week interval" is to filter 


#the earliest enrollment date is on 5-24-2016. We create a 12 week interval based on this min value, 
#such that all observations fall into a 12 week interval FROM this start date. 
min(bp_demographics$tri_imaginecareenrollmentemailsentdate, na.rm=TRUE) 
max(bp_demographics$tri_imaginecareenrollmentemailsentdate, na.rm=TRUE)


min(bp_demographics$tri_imaginecareenrollmentemailsentdate, na.rm=TRUE) + weeks(12)

#create a new column called ObservedDate

bp_demographics2$ObservedDate<- as.Date("1900-01-01") + bp_demographics2$ObservedTime

#make a table to show the aggregated values based on a 12 week interval for each patient
bp_dem_1d<-
  bp_demographics%>%
  group_by(ID) %>%
  filter(ObservedDate<=min(ObservedDate+weeks(12)))%>%
  summarize (avg_systolic = mean(SystolicValue,na.rm=TRUE), avg_diastolic=mean(Diastolicvalue)) %>%
  mutate(avg_BPStatus = ifelse((avg_systolic<120 & avg_diastolic<80), "Normal",
                               ifelse((avg_systolic>120 & avg_systolic<129 & avg_diastolic<80), "Hypo-1",
                                      ifelse((avg_systolic>130&avg_systolic<139) | (avg_diastolic>80&avg_diastolic<89),"HTN1",
                                             ifelse((avg_systolic>140 & avg_systolic<180) | (avg_diastolic>90 & avg_diastolic<120),"HTN2","HTN3")))))%>%
  mutate(avg_score = ifelse(avg_BPStatus=="Hypo-1"|avg_BPStatus=="Normal",1,0))



as.Date("2016-03-07") + weeks(12)


sqldf("select count (distinct ID) from bp_demographics")
#there are 143 distinct IDs in our table, and 143 observations in our aggregated 
#table for question 1D. No data was lost!

sample_n(bp_dem_1d,10) #print 10 random rows


#E  

bp_dem_1e<-
  bp_demographics %>%
  group_by(ID) %>%
  filter(ObservedDate<=min(ObservedDate+weeks(12)))%>%
  arrange(ObservedTime, .by_group=TRUE) %>%
  mutate(row_id=row_number())%>%
  filter(row_id==1|row_id==max(row_id))

sample_n(bp_dem_1e,10) #print 10 random rows
#with this table, we can now see the first observation and the final observation for each patient ID in the 12
#week interval. I first filtered to get our 12 week interval, then arranged EACH individual group of IDs in order
#of increasing time. I then provided each row with a distinct row_id to identify an earlier observation from a prior. 
#My final dataset only shows the first and final observation for each ID in the 12 week interval. 




#F

#Create a column called "change" that puts a 1 if the change in Dichotomus value 
#goes from 0 to 1, and a 0 if there is no such change. 

bp_dem_1f<- bp_demographics2 %>%
  group_by(ID) %>%
  filter(ObservedDate<=min(ObservedDate+weeks(12)))%>%
  arrange(ObservedTime, .by_group=TRUE) %>%
  mutate(row_id=row_number())%>%
  filter(row_id==1|row_id==max(row_id))%>%
  summarize(change=ifelse(Dichotomus[1]==0 & Dichotomus[2]==1,1,0))%>%
  summarize(total=sum(change,na.rm=TRUE))

#table to filter and see what IDs switch from 0 to 1. 
q1ftest<-bp_dem_1e %>%
  group_by(ID) %>%
  filter(Dichotomus[1]==0 & Dichotomus[2]==1)


#QUESTION 2

#**Done in SQL environment**


# --- QUESTION 3---
library(tidyr)
library(dplyr)

demographics<-sqlQuery(myconn,"select * from Demographics")
conditions<-sqlQuery(myconn,"select * from Conditions")
textmessages<-sqlQuery(myconn,"select * from TextMessages")

#step 1: recode column names to enable the join

names(conditions)[names(conditions) == 'tri_patientid'] <- 'contactid'
names(textmessages)[names(textmessages) == 'tri_contactId'] <- 'contactid'



demographics$contactid<-as.character(demographics$contactid)
conditions$contactid<-as.character(conditions$contactid)
textmessages$contactid<-as.character(textmessages$contactid)

str(demographics)
str(conditions)
str(textmessages)

#step 2: join the three tables


text_cond<-inner_join(textmessages,conditions, by = "contactid")

dem_text_cond<- inner_join(demographics,text_cond, by="contactid")

#Step 3: convert textSentDate to date format

dem_text_cond$TextSentDate<-as.Date(dem_text_cond$TextSentDate,format="%m/%d/%y")



#Step 4: narrow to one observation per date


#Resulting table has one observation PER ID that lists the latest date that a text was sent for each condition. 

filtered_tbl<-
  dem_text_cond %>% 
  group_by(contactid,tri_name) %>%
  slice(which.max(TextSentDate)) %>%
  spread(key="tri_name", value="TextSentDate")

sample_n(filtered_tbl,10) #print 10 random rows
