
PhoneCall<-sqlQuery(myconn,"select * from PhoneCall")

phoneCall_encounter<-sqlQuery(myconn,"select * from PhoneCall_Encounter")

phonecall_encounter_merged <- sqldf("select A.*,B.* from PhoneCall A
                                    inner join
                                    phoneCall_encounter B
                                    on A.tri_customerIDEntityReference=B.CustomerId")

phonecall_encounter_merged$EnrollmentGroup <- 
  ifelse(phonecall_encounter_merged$EncounterCode == 125060000,"Clinical Alert",
  ifelse (phonecall_encounter_merged$EncounterCode == 125060001,"Health Coaching",
  ifelse (phonecall_encounter_merged$EncounterCode == 125060002, "Technical Question",
  ifelse (phonecall_encounter_merged$EncounterCode == 125060003, "Administrative",
  ifelse (phonecall_encounter_merged$EncounterCode == 125060004, "Other", "Lack of engagement")))))

head(phonecall_encounter_merged, n=10)

sqldf ("select count (CustomerId),EnrollmentGroup
       from phonecall_encounter_merged as Record_Count
       group by EnrollmentGroup")

phoneCall_encounter<-sqlQuery(myconn,"select * from PhoneCall_Encounter")

CallDuration<-sqlQuery(myconn,"select * from CallDuration")

PC_Encounter_duration<-sqldf("select A.*,B.* from phoneCall_encounter A
                             inner join CallDuration B
                             on A.CustomerId = B.tri_CustomerIDEntityReference")

head(PC_Encounter_duration, n=10)

inbound_outbound<-
sqldf ("select count (CustomerID) as Count_callType, callType
       from PC_Encounter_duration
       group by CallType")
inbound_outbound$Type[inbound_outbound$CallType==1] <-"Inbound"
inbound_outbound$Type[inbound_outbound$CallType==2] <-"Outbound"

inbound_outbound
# Counts: Inbound(1) = 9782, Outbound (2) = 832

call_outcomes<- sqldf ("select count (CustomerID) as Count_callOutcome, callOutcome
       from PC_Encounter_duration
       group by CallOutcome")


call_outcomes$outcome[call_outcomes$CallOutcome==1] <-"No Response"
call_outcomes$outcome[call_outcomes$CallOutcome==2] <-"Left Voice Mail"
call_outcomes$outcome[call_outcomes$CallOutcome==3] <-"Successful"

#Counts: No Responce (1) = 5201, Left Voice Mail (2) = 4739, successful (3) = 764


#find the average call duration by enrollment group
#use the table we created in question 1, phonecall_encounter_merged
sqldf("select avg(CallDuration) as avg_duration,EnrollmentGroup
      from phonecall_encounter_merged
      group by EnrollmentGroup")

demographics <- sqlQuery(myconn,"select * from Demographics")

conditions <- sqlQuery(myconn,"select * from Conditions")


dem_conditions<-sqldf("select A.*,B.* from demographics A
                      inner join conditions B
                      on A.contactid = B.tri_patientid")

text_messages<-sqlQuery(myconn,"select * from TextMessages")


dem_cond_text<- sqldf("select A.*,B.* from dem_conditions A
                      inner join text_messages B
                      on A.contactid = B.tri_contactId")

head(dem_cond_text,n=10)

dem_cond_text$TextSentDate<-as.Date(dem_cond_text$TextSentDate,format="%m/%d/%y")


dem_cond_text$week_no<-strftime(dem_cond_text$TextSentDate, format='%V')

head(dem_cond_text,n=10)

texts_per_week<- sqldf("select count(contactid) as texts_per_week, 
                      SenderName,week_no
                      from dem_cond_text
                      group by week_no,SenderName")

library(tidyverse)

ggplot(data=texts_per_week, aes(x=week_no, y=texts_per_week,
                                color=SenderName,group=SenderName)) + 
  geom_line(size=1)+
  theme_minimal()+ 
  labs(x='Week Number',y='Texts per Week', title= "Texts per Week by Sender Name",
       fill="Sender Name")

texts_by_condition<-sqldf("select count(contactid)as txts_per_week,tri_name, week_no
                    from dem_cond_text
                    group by tri_name,week_no")

ggplot(data=texts_by_condition,aes(x=week_no,y=txts_per_week,
      color=tri_name,group=tri_name))+
  geom_line(size=1)+
  theme_minimal()+
  labs(x="Week Number",y="Texts per Week",title="Texts per Week by Condition",
       color="Condition")
