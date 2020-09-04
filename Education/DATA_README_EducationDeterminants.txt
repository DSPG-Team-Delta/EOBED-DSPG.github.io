File(s): 

	2015_District_Data_EducationDeterminants.csv



Used Column(s): 

	School District, GEOID, INDEX_Attendance, INDEX_ISS, INDEX_OOS, INDEX_Expulsion, INDEX_Grad, INDEX_Overall



Maps to make:
	Determinants of Educational Quality (index map using INDEX_Overall data)
	Student to Teacher Ratio (INDEX_STR)
	Student to Counselor Ratio (INDEX_STC)
	Share of New Teachers (INDEX_Experience)
	Teacher Salary (as a share of students) (INDEX_TeacherSalary)
	Number of Title I schools (INDEX_Title1)

Table Columns:
	School District
	County
	Rank Overall
	Quintile
	Student-to-teacher ratio
	Student-to-Counselor ratio
	% 1st and 2nd Year Teachers
	Teacher Salary (pupil)
	Number of Title 1 Schools


Hover box:
	Determinants of Educational Quality
		School District
		County
		Rank Overall
	Teacher to Student Ratio
		School District
		County
		Ratio
	Counselor to Student Ratio
		School District
		County
		Ratio
	Share of new teachers
		School District
		County
		% 1st and 2nd Year Teachers
	Teacher Salary (as a share of students)
		School District
		County
		Teacher Salary (pupil)
	Number of Title I schools
		School District
		County
		Number of Title 1 Schools

	

Notes:
	Generally speaking, 1 is good, 5 is bad. 




Column Description:


	GEOID:
		Distrcit GEO_ID that matches with District shapefile GEOID for leaflet maps

	School District: 
		School district name
	
	INDEX_STR
		Index of teacher to student ratio (1=higher than average teacher to student ratio, 5=lower than average str, 1-5 quintiles)

	INDEX_STC
		Index of counselor to studnet ratio (1=higher than average counselors to student ratio, 5=lower than average stc, 1-5 quintiles)

	INDEX_Experience
		Index of teacher experience (1=lower than average new teachers, 5=higher than average new teachers, 1-5 quintiles)

	INDEX_TeacherSalary
		Index of teacher salary by number of children (1=higher than average salary per pupil, 5=lower than average attendance, 1-5 quintiles)

	INDEX_Title1
		Index of Title 1 funding (1=higher than average Title I schools, 5=lower than average Titel I schools, 1-5 quintiles)

	INDEX_Overall:
		The overall index, values range from 1-5 (where 5=the bottom quintile, and 1= top quintile)