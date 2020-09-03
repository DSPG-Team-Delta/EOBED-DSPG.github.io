File(s): 

	2018_Tract_Data_HealthQual.csv



Used Column(s): 

	Geographic Area Name, id_Fix, INDEX_Overall, INDEX_qual, INDEX_readminrate, INDEX_LifeExpect, INDEX_InfantMort


Maps to make:
	Heathcare Outcomes and Quality (index map using INDEX_Overall data)
	Care and quality ratings (INDEX_qual)
`	30 day readmission rate (INDEX_readminrate)
	Life expectancy (INDEX_LifeExpect)
	Infant mortality (INDEX_InfantMort)
	

Notes:
	Generally speaking, 1 is good, 5 is bad. 




Column Description:

	id_Fix: 
		Usable part of GEO_ID for leaflet maps

	Geographic Area Name: 
		County, Tract, or State name (ie Malheur County, `Census Tract 201, Canyon County, Idaho`, etc.)

	INDEX_Overall:
		The overall index, values range from 1-5 (where 5=the bottom quintile/worst, and 1= top quintile/best)