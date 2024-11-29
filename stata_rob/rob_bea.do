**** Rob bea ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_bea.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_wage_pe_bea S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_bea.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_bea, replace
putexcel A1 = matrix(V), names
putexcel close
