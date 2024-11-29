****Altlag*****

**** 12 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

xtset id_area time
gen lag11 = L11.S4
gen lag12 = L12.S4


acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 lag11 lag12 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_altlag_12.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_altlag_12, replace
putexcel A1 = matrix(V), names
putexcel close


**** 8 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_altlag_8.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_altlag_8, replace
putexcel A1 = matrix(V), names
putexcel close











****Income*****

**** 12 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

xtset id_area time
gen lag11 = L11.S4
gen lag12 = L12.S4


acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 lag11 lag12 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_altlag_12.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_altlag_12, replace
putexcel A1 = matrix(V), names
putexcel close


**** 8 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_altlag_8.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_altlag_8, replace
putexcel A1 = matrix(V), names
putexcel close
