**** SLX ****

**** d50 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_slx_d50.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 x_1 x1_lag1 x1_lag2 x1_lag3 x1_lag4 x1_lag5 x1_lag6 x1_lag7 x1_lag8 x1_lag9 x1_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_slx_d50.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_slx_d50, replace
putexcel A1 = matrix(V), names
putexcel close


**** d100 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_slx_d100.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 x_1 x1_lag1 x1_lag2 x1_lag3 x1_lag4 x1_lag5 x1_lag6 x1_lag7 x1_lag8 x1_lag9 x1_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_slx_d100.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_slx_d100, replace
putexcel A1 = matrix(V), names
putexcel close


**** d50_sq ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_slx_d50_sq.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 x_1 x1_lag1 x1_lag2 x1_lag3 x1_lag4 x1_lag5 x1_lag6 x1_lag7 x1_lag8 x1_lag9 x1_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_slx_d50_sq.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_slx_d50_sq, replace
putexcel A1 = matrix(V), names
putexcel close










**** Income ****

**** d50 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_slx_d50.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 x_1 x1_lag1 x1_lag2 x1_lag3 x1_lag4 x1_lag5 x1_lag6 x1_lag7 x1_lag8 x1_lag9 x1_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_slx_d50.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_slx_d50, replace
putexcel A1 = matrix(V), names
putexcel close


**** d100 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_slx_d100.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 x_1 x1_lag1 x1_lag2 x1_lag3 x1_lag4 x1_lag5 x1_lag6 x1_lag7 x1_lag8 x1_lag9 x1_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_slx_d100.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_slx_d100, replace
putexcel A1 = matrix(V), names
putexcel close


**** d50_sq ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_slx_d50_sq.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 x_1 x1_lag1 x1_lag2 x1_lag3 x1_lag4 x1_lag5 x1_lag6 x1_lag7 x1_lag8 x1_lag9 x1_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_slx_d50_sq.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_slx_d50_sq, replace
putexcel A1 = matrix(V), names
putexcel close
