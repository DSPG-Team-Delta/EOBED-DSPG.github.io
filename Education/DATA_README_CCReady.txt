File(s): 

	2015_District_Data_CCReady.csv



Used Column(s): 

	School District, GEOID,INDEX_Overall



Maps to make:
	College and Career Readiness(INDEX_Overall data)
	AP Classes (AP_index)
	Gifted and Talented Classes (Gifted_index)
	Dual Enrollment/Credit Recovery	(Dual_index)
	IB Classes (IB_index)
	Calculus 1 (Calc_index)
	Chemistry (Chem_index)
	Physics (Physics_index)
		

Table Columns:
	School District
	County
	Overall Ranking	
	Quintile
	School that offers AP classes?
	School that offers gifted and talented education?
	School that offers dual enrollment/credit recovery?
	School that offers IB curriculum?
	School offers Calculus 1?
	School offers Chemistry?
	School offers Physics?

Hoverbox:
	College and Career Readiness
		School District
		County
		Overall Ranking		
	AP Classes
		School District
		County
		School that offers AP classes?		
	Gifted and Talented Classes
		School District
		County	
		School that offers gifted and talented education?
	Dual Enrollment/Credit Recovery
		School District
		County	
		School that offers dual enrollment/credit recovery?	
	IB Classes
		School District
		County	
		School that offers IB curriculum?
	Calculus 1
		School District
		County	
		School offers Calculus 1?
	Chemistry
		School District
		County	
		School offers Chemistry?
	Physics
		School District
		County	
		School offers Physics?

Notes:
	Generally speaking, 1 is good, 5 is bad. We are not going to create submaps for this because all the variable are binary and the submaps aren't very informative. 




Column Description:

	GEOID:
		Distrcit GEO_ID that matches with District shapefile GEOID for leaflet maps

	School District: 
		School district name				

	INDEX_Overall:
		The overall index, values range from 1-5 (where 5=the bottom quintile, and 1= top quintile)
