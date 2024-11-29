
******************************
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


*acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec, id(id_area) time(time) pfe1(id_area) pfe2(time)

*acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) pfe1(id_area) pfe2(time)



*Conley 50 no lag

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(0) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_conley50_nolag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_conley50_nolag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley 100 no lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(0) spatial latitude(latitude) longitude(longitude) dist(100) pfe1(id_area) pfe2(time)

esttab using "results_conley100_nolag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_conley100_nolag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley solo lag 29

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(29) spatial latitude(latitude) longitude(longitude) dist(0) pfe1(id_area) pfe2(time)

esttab using "results_conley_only29lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_conley_only29lag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley 50 1 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_conley50_1lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_conley50_1lag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley 100 1 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(100) pfe1(id_area) pfe2(time)

esttab using "results_conley100_1lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_conley100_1lag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley 50 2 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(2) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_conley50_2lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_conley50_2lag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley 100 2 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(2) spatial latitude(latitude) longitude(longitude) dist(100) pfe1(id_area) pfe2(time)

esttab using "results_conley100_2lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_conley100_2lag, replace
putexcel A1 = matrix(V), names
putexcel close










*NEW

*Conley 50 5 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(5) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_conley50_5lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_conley50_5lag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley 100 5 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(5) spatial latitude(latitude) longitude(longitude) dist(100) pfe1(id_area) pfe2(time)

esttab using "results_conley100_5lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_conley100_5lag, replace
putexcel A1 = matrix(V), names
putexcel close







*Conley 150 1 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(150) pfe1(id_area) pfe2(time)

esttab using "results_conley150_1lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_conley150_1lag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley 150 2 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(2) spatial latitude(latitude) longitude(longitude) dist(150) pfe1(id_area) pfe2(time)

esttab using "results_conley150_2lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_conley150_2lag, replace
putexcel A1 = matrix(V), names
putexcel close


**********************************************
*INCOME


clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


*Conley 50 no lag

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(0) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "income_results_conley50_nolag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set income_vcov_conley50_nolag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley 100 no lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(0) spatial latitude(latitude) longitude(longitude) dist(100) pfe1(id_area) pfe2(time)

esttab using "income_results_conley100_nolag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set income_vcov_conley100_nolag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley solo lag 29

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(29) spatial latitude(latitude) longitude(longitude) dist(0) pfe1(id_area) pfe2(time)

esttab using "income_results_conley_only29lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set income_vcov_conley_only29lag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley 50 1 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "income_results_conley50_1lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set income_vcov_conley50_1lag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley 100 1 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(100) pfe1(id_area) pfe2(time)

esttab using "income_results_conley100_1lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set income_vcov_conley100_1lag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley 50 2 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(2) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "income_results_conley50_2lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set income_vcov_conley50_2lag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley 100 2 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(2) spatial latitude(latitude) longitude(longitude) dist(100) pfe1(id_area) pfe2(time)

esttab using "income_results_conley100_2lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set income_vcov_conley100_2lag, replace
putexcel A1 = matrix(V), names
putexcel close

















*NEW

*Conley 50 5 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(5) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "income_results_conley50_5lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set income_vcov_conley50_5lag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley 100 5 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(5) spatial latitude(latitude) longitude(longitude) dist(100) pfe1(id_area) pfe2(time)

esttab using "income_results_conley100_5lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set income_vcov_conley100_5lag, replace
putexcel A1 = matrix(V), names
putexcel close







*Conley 150 1 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(150) pfe1(id_area) pfe2(time)

esttab using "income_results_conley150_1lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set income_vcov_conley150_1lag, replace
putexcel A1 = matrix(V), names
putexcel close


*Conley 150 2 lag

clear all
use "panel_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(2) spatial latitude(latitude) longitude(longitude) dist(150) pfe1(id_area) pfe2(time)

esttab using "income_results_conley150_2lag.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set income_vcov_conley150_2lag, replace
putexcel A1 = matrix(V), names
putexcel close

