****Altexp*****

**** S1 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S1 S1_lag1 S1_lag2 S1_lag3 S1_lag4 S1_lag5 S1_lag6 S1_lag7 S1_lag8 S1_lag9 S1_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_altexp_s1.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_altexp_s1, replace
putexcel A1 = matrix(V), names


**** S2 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S2 S2_lag1 S2_lag2 S2_lag3 S2_lag4 S2_lag5 S2_lag6 S2_lag7 S2_lag8 S2_lag9 S2_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_altexp_s2.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_altexp_s2, replace
putexcel A1 = matrix(V), names


**** S3 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_AAP_TOT_TOT S3 S3_lag1 S3_lag2 S3_lag3 S3_lag4 S3_lag5 S3_lag6 S3_lag7 S3_lag8 S3_lag9 S3_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_w_altexp_s3.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_w_altexp_s3, replace
putexcel A1 = matrix(V), names













****Income*****

**** S1 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S1 S1_lag1 S1_lag2 S1_lag3 S1_lag4 S1_lag5 S1_lag6 S1_lag7 S1_lag8 S1_lag9 S1_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_altexp_s1.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_altexp_s1, replace
putexcel A1 = matrix(V), names


**** S2 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S2 S2_lag1 S2_lag2 S2_lag3 S2_lag4 S2_lag5 S2_lag6 S2_lag7 S2_lag8 S2_lag9 S2_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_altexp_s2.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_altexp_s2, replace
putexcel A1 = matrix(V), names


**** S3 ****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

acreg GR_INCOME_PC S3 S3_lag1 S3_lag2 S3_lag3 S3_lag4 S3_lag5 S3_lag6 S3_lag7 S3_lag8 S3_lag9 S3_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_i_altexp_s3.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_i_altexp_s3, replace
putexcel A1 = matrix(V), names


