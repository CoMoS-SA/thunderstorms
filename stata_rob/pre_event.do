****pre_event*****


clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 lead2 lead3 lead4 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_pre_event.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_pre_event, replace
putexcel A1 = matrix(V), names
putexcel close



****Income *****


clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 lead2 lead3 lead4 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_pre_event.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_pre_event, replace
putexcel A1 = matrix(V), names
putexcel close
