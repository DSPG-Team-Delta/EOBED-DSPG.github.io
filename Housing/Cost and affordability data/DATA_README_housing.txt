File(s): 

	20xx_County_Data_Housing.csv, 20xx_Tract_Data_Housing.csv



Used Column(s): 

	Geographic Area Name, id_Fix, Rent, HOC, MedianincHH, RN_income, median_inc_rent, median_inc_mort, RN_income_rent, RN_income_mort, MOE_Rent, MOE_HOC, MOE_MedianHH




Column Description:

	id_Fix: 
		Usable part of GEO_ID for leaflet maps

	Geographic Area Name: 
		County, Tract, or State name (ie Malheur County, `Census Tract 201, Canyon County, Idaho`, etc.)

	Rent:
		Median renter-occupied annual housing cost (monthly x 12). 

	MOE_Rent:
		Median renter-occupied annual housing cost margin of error

	HOC: 
		Median owner-occupied annual housing cost (monthly x 12). Includes mortgage payments, rent 
		payments, condominium and other fees, real estate taxes, and premiums for home owners insurance. 
		Also, for mobile homes, installment loan payments, site rent, license and registration fees, and 
		personal property taxes.

	MOE_HOC:
		Median owner-occupied annual housing cost margin of error

	MedianincHH:
		Household income in the past 12 months (in 20xx inflation-adjusted dollars)

	MOE_MedianHH:
		MedianincHH margin of error

	RN_income:
		Annual wage for registered nurse in the relevant region (BLS data)

	median_inc_rent:
		Share of income spent on rent. =Rent/MedianincHH

	median_inc_mort:
		Share of income spent on homeownership. =HOC/MedianincHH

	RN_income_rent:
		Share of income spent on rent. =Rent/RN_income

	RN_income_mort:
		Share of income spent on rent. =HOC/RN_income

	