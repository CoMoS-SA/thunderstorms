**** Alt time ****

***d96***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_d96.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_alttime_d96.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_alttime_d96, replace
putexcel A1 = matrix(V), names
putexcel close

***d03***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_d03.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_alttime_d03.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_alttime_d03, replace
putexcel A1 = matrix(V), names
putexcel close


***d08***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_d08.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_alttime_d08.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_alttime_d08, replace
putexcel A1 = matrix(V), names
putexcel close


***d13***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_d13.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_alttime_d13.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_alttime_d13, replace
putexcel A1 = matrix(V), names
putexcel close


















**** Income ****

***d96***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_d96.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_alttime_d96.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_alttime_d96, replace
putexcel A1 = matrix(V), names
putexcel close

***d03***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_d03.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_alttime_d03.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_alttime_d03, replace
putexcel A1 = matrix(V), names
putexcel close


***d08***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_d08.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_alttime_d08.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_alttime_d08, replace
putexcel A1 = matrix(V), names
putexcel close


***d13***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_d13.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_alttime_d13.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_alttime_d13, replace
putexcel A1 = matrix(V), names
putexcel close

