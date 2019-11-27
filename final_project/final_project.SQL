
-- Make a copy of each table --
select * into sganesh.DemographicsFinal from Demographics 

select* into sganesh.ConditionsFinal from Conditions

select * into sganesh.TextMessagesFinal from TextMessages

-- test to be sure tables exist --
select * from sganesh.DemographicsFinal
select * from sganesh.ConditionsFinal
select * from sganesh.TextMessagesFinal



--convert text sent time into date format --

alter table sganesh.TextMessagesFinal
alter column TextSentDate date


-- Final table : 4 nested select statements to join, and pivot the result--

select * from
(select  A.*, AA.* from sganesh.DemographicsFinal A
inner join
	(select B.*, BB.* from sganesh.ConditionsFinal B
	inner join
		(select max(TextSentDate) as latest_text,tri_contactId
		from sganesh.TextMessagesFinal
		group by tri_contactId)BB
	on B.tri_patientid=BB.tri_contactId)AA
on A.contactid=AA.tri_patientid)FINAL
PIVOT (MAX(latest_text) for tri_name in ([Activity Monitoring],[Congestive Heart Failure],[Hypertension],[COPD],[Diabetes])) AS pivotedtbl

select count(distinct tri_contactId) from sganesh.TextMessagesFinal

