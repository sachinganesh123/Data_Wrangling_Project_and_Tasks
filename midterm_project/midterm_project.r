install.packages("SASxport")
library(SASxport)
library(sqldf)
library('RODBC')
library(tidyverse)

health_tbl<- read.xport(file.choose())
#
is.data.frame(health_tbl)

str(health_tbl)

install.packages("sjlabelled")
library(sjlabelled)

health_tbl<-remove_all_labels(health_tbl)

str(health_tbl)
#all labels are removed!

#now we can run sql queries
sqldf("select count(SEQN),DIQ010
      from health_tbl
      group by DIQ010")

#Rename Columns
names(health_tbl)<-c("Seqn",
                     "Have you been told you have diabetes",
                     "Age when first told you had diabetes",
                     "Ever told you have prediabetes?",
                     "Ever told have health risk for diabetes",
                     "Feel could be at risk for diabetes?",
                     "Family history",
                     "Overweight",
                     "Age",
                     "Poor Diet",
                     "Race",
                     "Had a baby weighed over 9lbs. at birth",
                     "Lack of physical activity",
                     "High blood pressure",
                     "High blood sugar",
                     "High cholesterol",
                     "Hypoglycemic",
                     "Extreme hunger",
                     "Tingling/numbness in hands or feet",
                     "Blurred Vision",
                     "Increased fatigue",
                     "Anyone could be at risk",
                     "Doctor Warning",
                     "Other,specify",
                     "Gestational diabetes",
                     "Frequent Urination",
                     "Thirst",
                     "Craving for sweet/eating a lot of sugar",
                     "Medication",
                     "Polycystic ovarian syndrome",
                     "Had blood tested past three years",
                     "Taking insulin now",
                     "How long taking insulin",
                     "Unit of measure (month/year)",
                      "Taking diabetic pills to lower blood sugar",
                     "How long ago saw a diabetes specialist",
                     "Is there one Doctor you see for diabetes",
                     "Past year how many times seen doctor?",
                     "How often check blood for glucose/sugar",
                     "Unit of measure (day/week/month/year)",
                     "Past year Doctor checked for A1C",
                     "What was your last A1C level",
                     "What does Dr say A1C should be",
                     "What was your recent SBP",
                     "What was your recent DBP",
                     "What does Dr say SBP should be",
                     "What does Dr say DBP should be",
                     "What was most recent LDL number",
                     "What does Dr say LDL should be",
                     "Past year times Dr checked feet for sores",
                     "How often do you check your feet",
                     "Unit of measure feet (day/week/month/year)",
                     "Last time had pupils dilated for exam",
                     "Diabetes affected eyes/had retinopathy")
             
names(health_tbl)<-gsub(" ","_",names(health_tbl))

#replace all question marks with nothing; causes errors in sqldf
#**quetion marks are a special character in R

names(health_tbl)<-gsub("\\?", "",names(health_tbl))
#first instance (for insulin)
health_tbl$`Unit_of_measure_(month/year)`<- 
  ifelse(health_tbl$`Unit_of_measure_(month/year)` == 1, "Months","Years")

#second instance (for blood glucose)
health_tbl$`Unit_of_measure_(day/week/month/year)`<-
  ifelse(health_tbl$`Unit_of_measure_(day/week/month/year)`==1,"Per day",
         ifelse(health_tbl$`Unit_of_measure_(day/week/month/year)`==2,"Per week",
                ifelse(health_tbl$`Unit_of_measure_(day/week/month/year)`==3,"Per month","Per year")))

#third instance (how often checking feet)
health_tbl$`Unit_of_measure_feet_(day/week/month/year)`<-
  ifelse(health_tbl$`Unit_of_measure_feet_(day/week/month/year)`==1, "Per day",
         ifelse (health_tbl$`Unit_of_measure_feet_(day/week/month/year)`==2, "Per week",
                 ifelse (health_tbl$`Unit_of_measure_feet_(day/week/month/year)`==3,"Per month",
                         "Per year")))
health_tbl$How_long_taking_insulin<-
  paste(health_tbl$How_long_taking_insulin,health_tbl$`Unit_of_measure_(month/year)`)

health_tbl$`How_often_check_blood_for_glucose/sugar`<-
  paste(health_tbl$`How_often_check_blood_for_glucose/sugar`,health_tbl$`Unit_of_measure_(day/week/month/year)`)

health_tbl$How_often_do_you_check_your_feet<-
  paste(health_tbl$How_often_do_you_check_your_feet,health_tbl$`Unit_of_measure_feet_(day/week/month/year)`)

#remove units column; now it's not needed anymore

health_tbl$`Unit_of_measure_(month/year)`<-NULL
health_tbl$`Unit_of_measure_(day/week/month/year)`<-NULL
health_tbl$`Unit_of_measure_feet_(day/week/month/year)`<-NULL
```


Step 3: Finally, we must replace the "NA NA" values in the merged column with just NA. Right now, the "NA NA" is of character type, but it is important to recode them to missing values at the moment, since we do not know how that could affect our analysis. 
```{r}
#Replace 'NA NA' merges with just NA

health_tbl$How_long_taking_insulin<-gsub('NA NA', NA,health_tbl$How_long_taking_insulin)

health_tbl$`How_often_check_blood_for_glucose/sugar`<-gsub('NA NA', NA,health_tbl$`How_often_check_blood_for_glucose/sugar`)

health_tbl$How_often_do_you_check_your_feet<-gsub('NA NA', NA,health_tbl$How_often_do_you_check_your_feet)


head(health_tbl$`How_often_check_blood_for_glucose/sugar`)


health_tbl$How_long_taking_insulin<-gsub("0 NA", "0", health_tbl$How_long_taking_insulin)

health_tbl$`How_often_check_blood_for_glucose/sugar`<-gsub("0 NA", "0", health_tbl$`How_often_check_blood_for_glucose/sugar`)

health_tbl$How_often_do_you_check_your_feet<-gsub("0 NA", "0", health_tbl$How_often_do_you_check_your_feet)


#recode values, then convert to factor
health_tbl$`Have_you_been_told_you_have_diabetes`<-
  ifelse(health_tbl$`Have_you_been_told_you_have_diabetes`==1,"Yes",
         ifelse (health_tbl$`Have_you_been_told_you_have_diabetes`==2,"No",
                 ifelse(health_tbl$`Have_you_been_told_you_have_diabetes`==3, "Borderline",
                        ifelse(health_tbl$`Have_you_been_told_you_have_diabetes`==7,"Refused",
                               "Don't know"))))

health_tbl$`Have_you_been_told_you_have_diabetes`<-as.factor(health_tbl$`Have_you_been_told_you_have_diabetes`)
#ever told at health risk for diabetes
health_tbl$Ever_told_you_have_prediabetes<-
  ifelse(health_tbl$Ever_told_you_have_prediabetes==1,"Yes",
         ifelse (health_tbl$Ever_told_you_have_prediabetes==2,"No",
                 ifelse(health_tbl$Ever_told_you_have_prediabetes==3, "Borderline",
                        ifelse(health_tbl$Ever_told_you_have_prediabetes==7,"Refused",
                               "Don't know"))))

health_tbl$Ever_told_you_have_prediabetes<-as.factor(health_tbl$Ever_told_you_have_prediabetes)
str(health_tbl$Ever_told_you_have_prediabetes)

#ever told have health risk for diabetes

health_tbl$Ever_told_have_health_risk_for_diabetes<-
  ifelse(health_tbl$Ever_told_have_health_risk_for_diabetes==1,"Yes",
         ifelse (health_tbl$Ever_told_have_health_risk_for_diabetes==2,"No",
                 ifelse(health_tbl$Ever_told_have_health_risk_for_diabetes==3, "Borderline",
                        ifelse(health_tbl$Ever_told_have_health_risk_for_diabetes==7,"Refused",
                               "Don't know"))))
health_tbl$Ever_told_have_health_risk_for_diabetes<-as.factor(health_tbl$Ever_told_have_health_risk_for_diabetes)
str(health_tbl$Ever_told_have_health_risk_for_diabetes)

#feel could be at risk for diabetes
health_tbl$Feel_could_be_at_risk_for_diabetes<-
  ifelse(health_tbl$Feel_could_be_at_risk_for_diabetes==1,"Yes",
         ifelse (health_tbl$Feel_could_be_at_risk_for_diabetes==2,"No",
                 ifelse(health_tbl$Feel_could_be_at_risk_for_diabetes==3, "Borderline",
                        ifelse(health_tbl$Feel_could_be_at_risk_for_diabetes==7,"Refused",
                               "Don't know"))))
health_tbl$Feel_could_be_at_risk_for_diabetes<-as.factor(health_tbl$Feel_could_be_at_risk_for_diabetes)
str(health_tbl$Feel_could_be_at_risk_for_diabetes)

health_tbl$Had_blood_tested_past_three_years<-
  ifelse(health_tbl$Had_blood_tested_past_three_years==1,"Yes",
         ifelse (health_tbl$Had_blood_tested_past_three_years==2,"No",
                 ifelse(health_tbl$Had_blood_tested_past_three_years==7,"Refused","Don't know")))

health_tbl$Had_blood_tested_past_three_years<-as.factor(health_tbl$Had_blood_tested_past_three_years)

str(health_tbl$Had_blood_tested_past_three_years)
#taking insulin now

health_tbl$Taking_insulin_now<-
  ifelse(health_tbl$Taking_insulin_now==1,"Yes",
         ifelse (health_tbl$Taking_insulin_now==2,"No",
                 ifelse(health_tbl$Taking_insulin_now==7, "Refused","Don't know")))

health_tbl$Taking_insulin_now<-as.factor(health_tbl$Taking_insulin_now)
str(health_tbl$Taking_insulin_now)

#taking diabetic pills to lower bp
health_tbl$Taking_diabetic_pills_to_lower_blood_sugar<-
  ifelse(health_tbl$Taking_diabetic_pills_to_lower_blood_sugar==1,"Yes",
         ifelse (health_tbl$Taking_diabetic_pills_to_lower_blood_sugar==2,"No",
                 ifelse(health_tbl$Taking_diabetic_pills_to_lower_blood_sugar==7, "Refused",
                        "Don't know")))

health_tbl$Taking_diabetic_pills_to_lower_blood_sugar<-as.factor(health_tbl$Taking_diabetic_pills_to_lower_blood_sugar)

str(health_tbl$Taking_diabetic_pills_to_lower_blood_sugar)

#is there 1 doctor you see for diabetes?
health_tbl$Is_there_one_Doctor_you_see_for_diabetes<-
  ifelse(health_tbl$Is_there_one_Doctor_you_see_for_diabetes==1,"Yes",
         ifelse (health_tbl$Is_there_one_Doctor_you_see_for_diabetes==2,"No",
                 ifelse(health_tbl$Is_there_one_Doctor_you_see_for_diabetes==7, "Refused",
                        "Don't know")))

health_tbl$Is_there_one_Doctor_you_see_for_diabetes<-as.factor(health_tbl$Is_there_one_Doctor_you_see_for_diabetes)

str(health_tbl$Is_there_one_Doctor_you_see_for_diabetes)
#past year dr checked for A1C

health_tbl$Past_year_Doctor_checked_for_A1C<-
  ifelse(health_tbl$Past_year_Doctor_checked_for_A1C==1,"Yes",
         ifelse (health_tbl$Past_year_Doctor_checked_for_A1C==2,"No",
                 ifelse(health_tbl$Past_year_Doctor_checked_for_A1C==7, "Refused",
                        "Don't know")))

health_tbl$Past_year_Doctor_checked_for_A1C<-as.factor(health_tbl$Past_year_Doctor_checked_for_A1C)

str(health_tbl$Past_year_Doctor_checked_for_A1C)
#diabetes affected eyes/retinopathy

health_tbl$`Diabetes_affected_eyes/had_retinopathy`<-
  ifelse(health_tbl$`Diabetes_affected_eyes/had_retinopathy`==1,"Yes",
         ifelse (health_tbl$`Diabetes_affected_eyes/had_retinopathy`==2,"No",
                 ifelse(health_tbl$`Diabetes_affected_eyes/had_retinopathy`==7, "Refused",
                        "Don't know")))

health_tbl$`Diabetes_affected_eyes/had_retinopathy`<-as.factor(health_tbl$`Diabetes_affected_eyes/had_retinopathy`)

str(health_tbl$`Diabetes_affected_eyes/had_retinopathy`)

health_tbl$How_long_ago_saw_a_diabetes_specialist<-
  ifelse(health_tbl$How_long_ago_saw_a_diabetes_specialist==1,"<1 Year",
         ifelse(health_tbl$How_long_ago_saw_a_diabetes_specialist==2, "1-2 Years",
                ifelse(health_tbl$How_long_ago_saw_a_diabetes_specialist==3,"2-5 Years",
                       ifelse(health_tbl$How_long_ago_saw_a_diabetes_specialist==4,">5 Years",
ifelse(health_tbl$How_long_ago_saw_a_diabetes_specialist==5,"Never",
                          ifelse(health_tbl$How_long_ago_saw_a_diabetes_specialist==7,"Refused","Don't Know"))))))



health_tbl$How_long_ago_saw_a_diabetes_specialist<-as.factor(health_tbl$How_long_ago_saw_a_diabetes_specialist)

str(health_tbl$How_long_ago_saw_a_diabetes_specialist)

#last time had pupils dilated for exam

health_tbl$Last_time_had_pupils_dilated_for_exam<-
  ifelse(health_tbl$Last_time_had_pupils_dilated_for_exam==1,"<1 month",
         ifelse(health_tbl$Last_time_had_pupils_dilated_for_exam==2,"1-12 months",
                ifelse(health_tbl$Last_time_had_pupils_dilated_for_exam==3,"13-24 months",
                    ifelse(health_tbl$Last_time_had_pupils_dilated_for_exam==4,">2 years",
                       ifelse(health_tbl$Last_time_had_pupils_dilated_for_exam==5,"Never",
                        ifelse(health_tbl$Last_time_had_pupils_dilated_for_exam==7,"Refused","Don't Know"))))))



health_tbl$Last_time_had_pupils_dilated_for_exam<-as.factor(health_tbl$Last_time_had_pupils_dilated_for_exam)

str(health_tbl$Last_time_had_pupils_dilated_for_exam)
health_tbl$What_does_Dr_say_A1C_should_be<-
  ifelse(health_tbl$What_does_Dr_say_A1C_should_be==1,"<6",
         ifelse(health_tbl$What_does_Dr_say_A1C_should_be==2,"<7",
                ifelse(health_tbl$What_does_Dr_say_A1C_should_be==3,"<8",
                       ifelse(health_tbl$What_does_Dr_say_A1C_should_be==4,"<9",
                              ifelse(health_tbl$What_does_Dr_say_A1C_should_be==5,"<10",
                                     ifelse(health_tbl$What_does_Dr_say_A1C_should_be==6,"Provider did not specify goal",
                                            ifelse(health_tbl$What_does_Dr_say_A1C_should_be==77,"Refused","Don't Know")))))))

health_tbl$What_does_Dr_say_A1C_should_be<-as.factor(health_tbl$What_does_Dr_say_A1C_should_be)

library(sqldf)

sqldf("select count(Seqn),Age_when_first_told_you_had_diabetes from health_tbl
      where Age_when_first_told_you_had_diabetes between 10 and 33")
#we can't just replace all 10's-33's in the dataset with 1's. There are 111 age
#values between these numbers, so we need to work specifically with this subset of the data

#STEPS:

#I created a subset of the original table, which includes just the columns that I am working with
health_subset<-subset(health_tbl, 
                      select=Family_history:Polycystic_ovarian_syndrome)


#replace all NA values with 0
health_subset[is.na(health_subset)]<-0

#replace all digits from 10-33 with 1
health_subset[health_subset>=10 & health_subset<=33]<-1

#finally, replace all of the rows in the original table with our cleaned subset
health_tbl[,7:30]<-health_subset

head(health_tbl)
library(mltools)
library(data.table)
#make a copy  of our table thus far, which will be the one hot encoded version; need to convert from data frame to data table format for one_hot() function to work

#ealth_tbl2<-as.data.table(health_tbl)

str(health_tbl2)

health_tbl2<-one_hot(as.data.table(health_tbl),sparsifyNAs=TRUE, naCols=TRUE,dropCols=TRUE,
                                   dropUnusedLevels=FALSE)
#replace 777

health_tbl2$Age_when_first_told_you_had_diabetes[health_tbl2$Age_when_first_told_you_had_diabetes==777]<-NA

#replace 999
health_tbl2$Age_when_first_told_you_had_diabetes[health_tbl2$Age_when_first_told_you_had_diabetes==999]<-NA

health_tbl2$Age_when_first_told_you_had_diabetes[is.na(health_tbl2$Age_when_first_told_you_had_diabetes)]<-mean(health_tbl2$Age_when_first_told_you_had_diabetes,na.rm=TRUE)


health_tbl2$Age_when_first_told_you_had_diabetes
str(health_tbl2$`How_often_check_blood_for_glucose/sugar`)

health_tbl2$`How_often_check_blood_for_glucose/sugar`[is.na(health_tbl2$`How_often_check_blood_for_glucose/sugar`)]<-"Missing"

head(health_tbl2$`How_often_check_blood_for_glucose/sugar`)
health_tbl2$Past_year_how_many_times_seen_doctor[is.na(health_tbl2$Past_year_how_many_times_seen_doctor)]<-0

health_tbl2$How_long_taking_insulin[is.na(health_tbl2$How_long_taking_insulin)]<-"Missing"

head(health_tbl2$How_long_taking_insulin)
library(sqldf)

sqldf("select count(Seqn), Have_you_been_told_you_have_diabetes
      from health_tbl
      group by Have_you_been_told_you_have_diabetes")

sqldf("select count(Seqn), Have_you_been_told_you_have_diabetes_No
      from health_tbl2
      group by Have_you_been_told_you_have_diabetes_No")

sqldf("select count(Seqn), Have_you_been_told_you_have_diabetes_Yes
      from health_tbl2
      group by Have_you_been_told_you_have_diabetes_Yes")

sqldf("select count(Seqn), Have_you_been_told_you_have_diabetes_Borderline
      from health_tbl2
      group by Have_you_been_told_you_have_diabetes_Borderline")

sqldf("select count(Seqn), Ever_told_you_have_prediabetes
      from health_tbl
      group by Ever_told_you_have_prediabetes ")


sqldf("select count(Seqn), Ever_told_you_have_prediabetes_Yes
      from health_tbl2
      group by Ever_told_you_have_prediabetes_Yes")

sqldf("select count(Seqn), Ever_told_you_have_prediabetes_No
      from health_tbl2
      group by Ever_told_you_have_prediabetes_No")
sqldf("select count(Seqn), Had_blood_tested_past_three_years
      from health_tbl
      group by Had_blood_tested_past_three_years ")


sqldf("select count(Seqn), Had_blood_tested_past_three_years_Yes
      from health_tbl2
      group by Had_blood_tested_past_three_years_Yes")

sqldf("select count(Seqn), Had_blood_tested_past_three_years_No
      from health_tbl2
      group by Had_blood_tested_past_three_years_No")
sqldf("select count(Seqn), Had_blood_tested_past_three_years_Refused
      from health_tbl2
      group by Had_blood_tested_past_three_years_Refused")
sqldf("select count(Seqn), Taking_insulin_now
      from health_tbl
      group by Taking_insulin_now ")

sqldf("select count(Seqn), Taking_insulin_now_Yes
      from health_tbl2
      group by Taking_insulin_now_Yes")

sqldf("select count(Seqn), Taking_insulin_now_No
      from health_tbl2
      group by Taking_insulin_now_No")

sqldf("select count(Seqn), Taking_insulin_now_Refused
      from health_tbl2
      group by Taking_insulin_now_Refused")

sqldf("select count(Seqn), Taking_diabetic_pills_to_lower_blood_sugar
      from health_tbl
      group by Taking_diabetic_pills_to_lower_blood_sugar ")

sqldf("select count(Seqn), Taking_diabetic_pills_to_lower_blood_sugar_Yes
      from health_tbl2
      group by Taking_diabetic_pills_to_lower_blood_sugar_Yes")

sqldf("select count(Seqn), Taking_diabetic_pills_to_lower_blood_sugar_No
      from health_tbl2
      group by Taking_diabetic_pills_to_lower_blood_sugar_No")
sqldf("select count(Seqn), Taking_diabetic_pills_to_lower_blood_sugar_Refused
      from health_tbl2
      group by Taking_diabetic_pills_to_lower_blood_sugar_Refused")

sqldf("select count(Seqn), How_long_ago_saw_a_diabetes_specialist
      from health_tbl
      group by How_long_ago_saw_a_diabetes_specialist ")
sqldf("select count(Seqn), Is_there_one_doctor_you_see_for_diabetes
      from health_tbl
      group by Is_there_one_doctor_you_see_for_diabetes ")

#0 count in "refused" and "don't know" column

sqldf("select count(Seqn), Is_there_one_doctor_you_see_for_diabetes_No
      from health_tbl2
      group by Is_there_one_doctor_you_see_for_diabetes_No")

sqldf("select count(Seqn), Is_there_one_doctor_you_see_for_diabetes_Yes
      from health_tbl2
      group by Is_there_one_doctor_you_see_for_diabetes_Yes")
sqldf("select count(Seqn), What_does_Dr_say_A1C_should_be
      from health_tbl
      group by What_does_Dr_say_A1C_should_be ")
sqldf("select count(seqn), Thirst from health_tbl2 group by Thirst")
