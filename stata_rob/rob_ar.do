****Altar*****

**** 1 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 GR_AAP_TOT_TOT_lag1 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_ar_1.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_ar_1, replace
putexcel A1 = matrix(V), names


**** 2 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 GR_AAP_TOT_TOT_lag1 GR_AAP_TOT_TOT_lag2 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_ar_2.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_ar_2, replace
putexcel A1 = matrix(V), names


**** 3 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 GR_AAP_TOT_TOT_lag1 GR_AAP_TOT_TOT_lag2 GR_AAP_TOT_TOT_lag3 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_ar_3.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_ar_3, replace
putexcel A1 = matrix(V), names


**** 4 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 GR_AAP_TOT_TOT_lag1 GR_AAP_TOT_TOT_lag2 GR_AAP_TOT_TOT_lag3 GR_AAP_TOT_TOT_lag4 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_ar_4.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_ar_4, replace
putexcel A1 = matrix(V), names














****Income*****

**** 1 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 GR_INCOME_PC_lag1 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_ar_1.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_ar_1, replace
putexcel A1 = matrix(V), names


**** 2 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 GR_INCOME_PC_lag1 GR_INCOME_PC_lag2 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_ar_2.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_ar_2, replace
putexcel A1 = matrix(V), names


**** 3 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 GR_INCOME_PC_lag1 GR_INCOME_PC_lag2 GR_INCOME_PC_lag3 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_ar_3.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_ar_3, replace
putexcel A1 = matrix(V), names


**** 4 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 GR_INCOME_PC_lag1 GR_INCOME_PC_lag2 GR_INCOME_PC_lag3 GR_INCOME_PC_lag4 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_ar_4.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_ar_4, replace
putexcel A1 = matrix(V), names

