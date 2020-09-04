File(s): 

	2015_District_Data_EducationOutcomes.csv



Used Column(s): 

	School District, GEOID, INDEX_Attendance, INDEX_ISS, INDEX_OOS, INDEX_Expulsion, INDEX_Grad, INDEX_Overall



Maps to make:
	Education Outcomes (index map using INDEX_Overall data)
	Absenteeism (INDEX_Attendance)
	In-School Suspensions (INDEX_ISS)
	Out-of-School Suspensions (INDEX_OOS)
	Expulsions (INDEX_Expulsion)
	Cohort Graduation (INDEX_Grad)

Table columns:
	School District
	County
	Rank Overall
	Quintile
	Absenteeism Rate
	In-school Suspension Rate (%)
	Out-of-school Suspension Rate (%)
	Expulsion Rate (%)
	4 Year Graduation Rate (%)

Hover box:
	Education Outcomes
		School District
		County
		Overall Ranking
	Absenteeism
		School District
		County
		Absenteeism Rate
	In-School Suspensions 
		School District
		County
		In-school Suspension Rate (%)
	Out-of-School Suspensions
		School District
		County
		Out-of-school Suspension Rate (%)
	Expulsions
		School District
		County
		Expulsion Rate 
	Cohort Graduation 
		School District
		County
		4 Year Graduation Rate (%)


Notes:
	Generally speaking, 1 is good, 5 is bad. 




Column Description:

	GEOID:
		Distrcit GEO_ID that matches with District shapefile GEOID for leaflet maps

	School District: 
		School district name
	
	INDEX_Attendance:
		Index to show regular attendance (1=lower than average attendance, 5=higher than average attendance, 1-5 quintiles)

	INDEX_ISS
		Index of in school suspensions (1=lower than average ISS, 5=higher than average ISS, 1-5 quintiles)

	INDEX_OOS
		Index of out of school suspensions (1=lower than average out of school suspension, 5=higher than average out of school suspension, 1-5 quintiles)

	INDEX_Expulsion
		Index of expulsions (1=lower than average expulsions 5=higher than average expulsions, 1-5 quintiles)

	INDEX_Grad
		Index of cohort graduation rate of the 2015-2019 cohorts (1=higher than average graduation rates 5=lower than average graduation rates, 1-5 quintiles)

	INDEX_Overall:
		The overall index, values range from 1-5 (where 5=the bottom quintile/worst, and 1= top quintile/best)