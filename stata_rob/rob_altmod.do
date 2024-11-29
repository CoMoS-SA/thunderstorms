****Altmod*****

**** 1 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_altmod_1.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_altmod_1, replace
putexcel A1 = matrix(V), names
putexcel close


**** 2 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec tmp_sq prec_sq state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_altmod_2.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_altmod_2, replace
putexcel A1 = matrix(V), names
putexcel close


**** 3 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec tmp_sq prec_sq GR_AAE_TOT_TOT state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_altmod_3.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_altmod_3, replace
putexcel A1 = matrix(V), names
putexcel close


**** 4 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec tmp_sq prec_sq GR_AAE_TOT_TOT GR_POPULATION state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_altmod_4.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_altmod_4, replace
putexcel A1 = matrix(V), names
putexcel close


**** 5 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

xtset id_area time
gen lag_ST_ACT_POP = L.ST_ACT_POP

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec tmp_sq prec_sq GR_AAE_TOT_TOT GR_POPULATION lag_ST_ACT_POP state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_altmod_5.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_altmod_5, replace
putexcel A1 = matrix(V), names
putexcel close


**** 6 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

xtset id_area time
gen lag_ST_ACT_POP = L.ST_ACT_POP
gen lag_ST_AAP_TOT_TOT = L.ST_AAP_TOT_TOT

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec tmp_sq prec_sq GR_AAE_TOT_TOT GR_POPULATION lag_ST_ACT_POP lag_ST_AAP_TOT_TOT state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_altmod_6.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_altmod_6, replace
putexcel A1 = matrix(V), names
putexcel close




















****Income*****

**** 1 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_altmod_1.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_altmod_1, replace
putexcel A1 = matrix(V), names
putexcel close


**** 2 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec tmp_sq prec_sq state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_altmod_2.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_altmod_2, replace
putexcel A1 = matrix(V), names
putexcel close


**** 3 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec tmp_sq prec_sq GR_AAE_TOT_TOT state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_altmod_3.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_altmod_3, replace
putexcel A1 = matrix(V), names
putexcel close


**** 4 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec tmp_sq prec_sq GR_AAE_TOT_TOT GR_POPULATION state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_altmod_4.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_altmod_4, replace
putexcel A1 = matrix(V), names
putexcel close


**** 5 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

xtset id_area time
gen lag_ST_ACT_POP = L.ST_ACT_POP

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec tmp_sq prec_sq GR_AAE_TOT_TOT GR_POPULATION lag_ST_ACT_POP state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_altmod_5.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_altmod_5, replace
putexcel A1 = matrix(V), names
putexcel close


**** 6 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

xtset id_area time
gen lag_ST_ACT_POP = L.ST_ACT_POP
gen lag_ST_INCOME_PC = L.ST_INCOME_PC

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec tmp_sq prec_sq GR_AAE_TOT_TOT GR_POPULATION lag_ST_ACT_POP lag_ST_INCOME_PC state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_altmod_6.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_altmod_6, replace
putexcel A1 = matrix(V), names
putexcel close









