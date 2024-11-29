****** EMPLOYMENT ******

***Natmin****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_sectors_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAE_PRIV_NATMIN S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_e_natmin.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_e_natmin, replace
putexcel A1 = matrix(V), names
putexcel close


***Constr****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_sectors_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAE_PRIV_CONSTR S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_e_constr.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_e_constr, replace
putexcel A1 = matrix(V), names
putexcel close



***Manuf****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_sectors_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAE_PRIV_MANUF S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_e_manuf.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_e_manuf, replace
putexcel A1 = matrix(V), names
putexcel close



***Serv****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_sectors_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAE_PRIV_SERV S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_e_serv.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_e_serv, replace
putexcel A1 = matrix(V), names
putexcel close







****** Wages ******

***Natmin****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_sectors_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_PRIV_NATMIN S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_natmin.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_natmin, replace
putexcel A1 = matrix(V), names
putexcel close


***Constr****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_sectors_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_PRIV_CONSTR S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_constr.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_constr, replace
putexcel A1 = matrix(V), names
putexcel close



***Manuf****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_sectors_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_PRIV_MANUF S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_manuf.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_manuf, replace
putexcel A1 = matrix(V), names
putexcel close



***Serv****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_sectors_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_PRIV_SERV S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_serv.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_serv, replace
putexcel A1 = matrix(V), names
putexcel close
