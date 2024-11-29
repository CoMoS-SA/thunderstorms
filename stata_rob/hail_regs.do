****Hail*****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_hail_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_TOT_TOT S3 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_hail.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_hail, replace
putexcel A1 = matrix(V), names
putexcel close


****income*****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_hail_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_INCOME_PC S3 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_hail_income.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_hail_income, replace
putexcel A1 = matrix(V), names
putexcel close

