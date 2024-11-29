**** Housing ****

***state-trend***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_housing.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_HPI S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_housing_trend.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_housing_trend, replace
putexcel A1 = matrix(V), names
putexcel close


***state-year***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_housing.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

egen state_year = group(id_state time)
tabulate state_year, generate(d_state_year)

acreg GR_HPI S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec d_state_year*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_housing_stateyear.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_housing_stateyear, replace
putexcel A1 = matrix(V), names
putexcel close
