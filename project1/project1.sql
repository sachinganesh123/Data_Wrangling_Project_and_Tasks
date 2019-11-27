
select * from sganesh.DemographicsHW1

execute sp_rename 'sganesh.demographicsHW1.contactid', 'Contact','COLUMN'
go
execute sp_rename 'sganesh.demographicsHW1.gendercode', 'Gender','COLUMN'
go
execute sp_rename 'sganesh.demographicsHW1.tri_age', 'Age','COLUMN'
go
execute sp_rename 'sganesh.demographicsHW1.parentcustomeridname', 'ParentCustomerID','COLUMN'
go
execute sp_rename 'sganesh.demographicsHW1.tri_imaginecareenrollmentstatus', 'ImagineCareEnrollmentStatus','COLUMN'
go
execute sp_rename 'sganesh.demographicsHW1.address1_stateorprovince', 'State','COLUMN'
go
execute sp_rename 'sganesh.demographicsHW1.tri_imaginecareenrollmentemailsentdate', 'EmailSentdate','COLUMN'
go
execute sp_rename 'sganesh.demographicsHW1.tri_enrollmentcompletedate', 'EnrollmentCompletedate','COLUMN'
go
execute sp_rename 'sganesh.demographicsHW1.gender', 'GenderCode1','COLUMN'
go
execute sp_rename 'sganesh.demographicsHW1.Gender_Name', 'GenderName','COLUMN'
go


--time in days to complete enrollment
select * from sganesh.DemographicsHW1

alter table sganesh.demographicsHW1
add TimeToComplete int

alter table sganesh.demographicsHW1
alter column TimeToComplete int


update sganesh.DemographicsHW1
set TimeToComplete = DATEDIFF (DAY,try_convert(date, EmailSentDate, 101), TRY_CONVERT(date,EnrollmentCompleteDate,101))

select TimeToComplete from sganesh.DemographicsHW1

-- QUESTION 2 --

select * from sganesh.DemographicsHW1

alter table sganesh.DemographicsHw1
add EnrollmentStatus nvarchar 

alter table sganesh.DemographicsHW1
alter column EnrollmentStatus nvarchar(255)

alter table sganesh.DemographicsHW1
alter column ImagineCareEnrollmentStatus nvarchar

update sganesh.DemographicsHW1
set EnrollmentStatus = 'Complete' 
where ImagineCareEnrollmentStatus = 167410011

update sganesh.DemographicsHW1
set EnrollmentStatus = 'Email sent' 
where ImagineCareEnrollmentStatus = 167410001


update sganesh.DemographicsHW1
set EnrollmentStatus = 'Non Responder' 
where ImagineCareEnrollmentStatus = 167410004

update sganesh.DemographicsHW1
set EnrollmentStatus = 'Facilitated Enrollment' 
where ImagineCareEnrollmentStatus = 167410005

update sganesh.DemographicsHW1
set EnrollmentStatus = 'Incomplete Enrollments' 
where ImagineCareEnrollmentStatus = 167410002

update sganesh.DemographicsHW1
set EnrollmentStatus = 'Opted Out' 
where ImagineCareEnrollmentStatus = 167410003

update sganesh.DemographicsHW1
set EnrollmentStatus = 'Unprocessed' 
where ImagineCareEnrollmentStatus = 167410000

update sganesh.DemographicsHW1
set EnrollmentStatus = 'Second Email Sent' 
where ImagineCareEnrollmentStatus = 167410006

select * from sganesh.DemographicsHW1

--QUESTION 3 --

alter table sganesh.DemographicsHW1
add Sex nvarchar(255)

update sganesh.DemographicsHW1
set Sex='Female' 
where Gender ='2'

update sganesh.DemographicsHW1
set Sex='Male' 
where Gender ='1'

update sganesh.DemographicsHW1
set Sex='other' 
where Gender ='167410000'

update sganesh.DemographicsHW1
set Sex='Unknown' 
where Gender ='Null'

select * from sganesh.DemographicsHW1

-- QUESTION 4 -- 

alter table sganesh.DemographicsHW1
add AgeGroup nvarchar(255)

update sganesh.DemographicsHW1
set AgeGroup= '0-25'
where Age between 0 and 25

update sganesh.DemographicsHW1
set AgeGroup= '26-50'
where Age between 26 and 50

update sganesh.DemographicsHW1
set AgeGroup= '51-75'
where Age between 51 and 75

update sganesh.DemographicsHW1
set AgeGroup= '76-100'
where Age between 76 and 100


-- Print Random Rows -- 
select * from sganesh.DemographicsHW1

select top 20 * from sganesh.DemographicsHW1
order by newid()

#
