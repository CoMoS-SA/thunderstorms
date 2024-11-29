**** Repeated events ****

***dp***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_re_dt_p.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_re_dt_p.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_re_dt_p, replace
putexcel A1 = matrix(V), names
putexcel close



***wm***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_re_dt_wm.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_re_dt_wm.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_re_dt_wm, replace
putexcel A1 = matrix(V), names
putexcel close




***wm***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_re_dt_wmo.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_re_dt_wmo.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_re_dt_wmo, replace
putexcel A1 = matrix(V), names
putexcel close




***wt***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_re_dt_wt.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_re_dt_wt.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_re_dt_wt, replace
putexcel A1 = matrix(V), names
putexcel close




















**** Income ****

***dp***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_re_dt_p.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_re_dt_p.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_re_dt_p, replace
putexcel A1 = matrix(V), names
putexcel close



***wm***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_re_dt_wm.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_re_dt_wm.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_re_dt_wm, replace
putexcel A1 = matrix(V), names
putexcel close




***wm***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_re_dt_wmo.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_re_dt_wmo.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_re_dt_wmo, replace
putexcel A1 = matrix(V), names
putexcel close




***wt***

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_re_dt_wt.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_re_dt_wt.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_re_dt_wt, replace
putexcel A1 = matrix(V), names
putexcel close
