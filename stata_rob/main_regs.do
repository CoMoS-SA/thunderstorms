****Risk level*****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


gen S4_tmm1 = S4 * (tmm_risk == 1)
gen S4_tmm2 = S4 * (tmm_risk == 2)
gen S4_tmm3 = S4 * (tmm_risk == 3)

forval i = 1/10 {
    gen lag`i'_tmm1 = lag`i' * (tmm_risk == 1)
    gen lag`i'_tmm2 = lag`i' * (tmm_risk == 2)
    gen lag`i'_tmm3 = lag`i' * (tmm_risk == 3)
}

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 S4_tmm2 S4_tmm3 lag1_tmm2 lag1_tmm3 lag2_tmm2 lag2_tmm3 lag3_tmm2 lag3_tmm3 lag4_tmm2 lag4_tmm3 lag5_tmm2 lag5_tmm3 lag6_tmm2 lag6_tmm3 lag7_tmm2 lag7_tmm3 lag8_tmm2 lag8_tmm3 lag9_tmm2 lag9_tmm3 lag10_tmm2 lag10_tmm3 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_risk.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_risk, replace
putexcel A1 = matrix(V), names
putexcel close



****Rich level*****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


gen S4_tmm1 = S4 * (tmm_rich == 1)
gen S4_tmm2 = S4 * (tmm_rich == 2)
gen S4_tmm3 = S4 * (tmm_rich == 3)

forval i = 1/10 {
    gen lag`i'_tmm1 = lag`i' * (tmm_rich == 1)
    gen lag`i'_tmm2 = lag`i' * (tmm_rich == 2)
    gen lag`i'_tmm3 = lag`i' * (tmm_rich == 3)
}

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 S4_tmm2 S4_tmm3 lag1_tmm2 lag1_tmm3 lag2_tmm2 lag2_tmm3 lag3_tmm2 lag3_tmm3 lag4_tmm2 lag4_tmm3 lag5_tmm2 lag5_tmm3 lag6_tmm2 lag6_tmm3 lag7_tmm2 lag7_tmm3 lag8_tmm2 lag8_tmm3 lag9_tmm2 lag9_tmm3 lag10_tmm2 lag10_tmm3 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_rich.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_rich, replace
putexcel A1 = matrix(V), names
putexcel close

****Disaster SD *****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

* Creazione delle variabili di interazione
gen S4_dmm = S4 * dmm_IS_D

* Utilizza un ciclo per creare le variabili di interazione tra i lag e dmm
forval i = 1/10 {
    gen lag`i'_dmm_lag`i' = lag`i' * dmm_IS_D_lag`i'
}

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 S4_dmm lag1_dmm_lag1 lag2_dmm_lag2 lag3_dmm_lag3 lag4_dmm_lag4 lag5_dmm_lag5 lag6_dmm_lag6 lag7_dmm_lag7 lag8_dmm_lag8 lag9_dmm_lag9 lag10_dmm_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_isd.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_isd, replace
putexcel A1 = matrix(V), names
putexcel close

****Disaster SD2 *****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

* Creazione delle variabili di interazione
gen S4_dmm = S4 * dmm_IS_D2

* Utilizza un ciclo per creare le variabili di interazione tra i lag e dmm
forval i = 1/10 {
    gen lag`i'_dmm_lag`i' = lag`i' * dmm_IS_D2_lag`i'
}

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 S4_dmm lag1_dmm_lag1 lag2_dmm_lag2 lag3_dmm_lag3 lag4_dmm_lag4 lag5_dmm_lag5 lag6_dmm_lag6 lag7_dmm_lag7 lag8_dmm_lag8 lag9_dmm_lag9 lag10_dmm_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_isd2.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_isd2, replace
putexcel A1 = matrix(V), names
putexcel close


****Disaster SD3 *****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

* Creazione delle variabili di interazione
gen S4_dmm = S4 * dmm_IS_D3

* Utilizza un ciclo per creare le variabili di interazione tra i lag e dmm
forval i = 1/10 {
    gen lag`i'_dmm_lag`i' = lag`i' * dmm_IS_D3_lag`i'
}

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 S4_dmm lag1_dmm_lag1 lag2_dmm_lag2 lag3_dmm_lag3 lag4_dmm_lag4 lag5_dmm_lag5 lag6_dmm_lag6 lag7_dmm_lag7 lag8_dmm_lag8 lag9_dmm_lag9 lag10_dmm_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_isd3.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_isd3, replace
putexcel A1 = matrix(V), names
putexcel close




****Disaster SD external *****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

* Creazione delle variabili di interazione
gen S4_dmm = S4 * dmm_IS_D

* Utilizza un ciclo per creare le variabili di interazione tra i lag e dmm
forval i = 1/10 {
    gen lag`i'_dmm_lag`i' = lag`i' * dmm_IS_D_lag`i'
}

acreg GR_AAP_TOT_TOT S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 S4_dmm lag1_dmm_lag1 lag2_dmm_lag2 lag3_dmm_lag3 lag4_dmm_lag4 lag5_dmm_lag5 lag6_dmm_lag6 lag7_dmm_lag7 lag8_dmm_lag8 lag9_dmm_lag9 lag10_dmm_lag10 dmm_DISASTER_YEAR_EXT dmm_DISASTER_YEAR_EXT_lag1 dmm_DISASTER_YEAR_EXT_lag2 dmm_DISASTER_YEAR_EXT_lag3 dmm_DISASTER_YEAR_EXT_lag4 dmm_DISASTER_YEAR_EXT_lag5 dmm_DISASTER_YEAR_EXT_lag6 dmm_DISASTER_YEAR_EXT_lag7 dmm_DISASTER_YEAR_EXT_lag8 dmm_DISASTER_YEAR_EXT_lag9 dmm_DISASTER_YEAR_EXT_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_isd_ext.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_isd_ext, replace
putexcel A1 = matrix(V), names
putexcel close







***** INCOME *******







****Risk level*****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


gen S4_tmm1 = S4 * (tmm_risk == 1)
gen S4_tmm2 = S4 * (tmm_risk == 2)
gen S4_tmm3 = S4 * (tmm_risk == 3)

forval i = 1/10 {
    gen lag`i'_tmm1 = lag`i' * (tmm_risk == 1)
    gen lag`i'_tmm2 = lag`i' * (tmm_risk == 2)
    gen lag`i'_tmm3 = lag`i' * (tmm_risk == 3)
}

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 S4_tmm2 S4_tmm3 lag1_tmm2 lag1_tmm3 lag2_tmm2 lag2_tmm3 lag3_tmm2 lag3_tmm3 lag4_tmm2 lag4_tmm3 lag5_tmm2 lag5_tmm3 lag6_tmm2 lag6_tmm3 lag7_tmm2 lag7_tmm3 lag8_tmm2 lag8_tmm3 lag9_tmm2 lag9_tmm3 lag10_tmm2 lag10_tmm3 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_risk_income.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_risk_income, replace
putexcel A1 = matrix(V), names
putexcel close



****Rich level*****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)


gen S4_tmm1 = S4 * (tmm_rich == 1)
gen S4_tmm2 = S4 * (tmm_rich == 2)
gen S4_tmm3 = S4 * (tmm_rich == 3)

forval i = 1/10 {
    gen lag`i'_tmm1 = lag`i' * (tmm_rich == 1)
    gen lag`i'_tmm2 = lag`i' * (tmm_rich == 2)
    gen lag`i'_tmm3 = lag`i' * (tmm_rich == 3)
}

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 S4_tmm2 S4_tmm3 lag1_tmm2 lag1_tmm3 lag2_tmm2 lag2_tmm3 lag3_tmm2 lag3_tmm3 lag4_tmm2 lag4_tmm3 lag5_tmm2 lag5_tmm3 lag6_tmm2 lag6_tmm3 lag7_tmm2 lag7_tmm3 lag8_tmm2 lag8_tmm3 lag9_tmm2 lag9_tmm3 lag10_tmm2 lag10_tmm3 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_rich_income.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_rich_income, replace
putexcel A1 = matrix(V), names
putexcel close

****Disaster SD *****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

* Creazione delle variabili di interazione
gen S4_dmm = S4 * dmm_IS_D

* Utilizza un ciclo per creare le variabili di interazione tra i lag e dmm
forval i = 1/10 {
    gen lag`i'_dmm_lag`i' = lag`i' * dmm_IS_D_lag`i'
}

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 S4_dmm lag1_dmm_lag1 lag2_dmm_lag2 lag3_dmm_lag3 lag4_dmm_lag4 lag5_dmm_lag5 lag6_dmm_lag6 lag7_dmm_lag7 lag8_dmm_lag8 lag9_dmm_lag9 lag10_dmm_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_isd_income.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_isd_income, replace
putexcel A1 = matrix(V), names
putexcel close

****Disaster SD2 *****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

* Creazione delle variabili di interazione
gen S4_dmm = S4 * dmm_IS_D2

* Utilizza un ciclo per creare le variabili di interazione tra i lag e dmm
forval i = 1/10 {
    gen lag`i'_dmm_lag`i' = lag`i' * dmm_IS_D2_lag`i'
}

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 S4_dmm lag1_dmm_lag1 lag2_dmm_lag2 lag3_dmm_lag3 lag4_dmm_lag4 lag5_dmm_lag5 lag6_dmm_lag6 lag7_dmm_lag7 lag8_dmm_lag8 lag9_dmm_lag9 lag10_dmm_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_isd2_income.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_isd2_income, replace
putexcel A1 = matrix(V), names
putexcel close


****Disaster SD3 *****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

* Creazione delle variabili di interazione
gen S4_dmm = S4 * dmm_IS_D3

* Utilizza un ciclo per creare le variabili di interazione tra i lag e dmm
forval i = 1/10 {
    gen lag`i'_dmm_lag`i' = lag`i' * dmm_IS_D3_lag`i'
}

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 S4_dmm lag1_dmm_lag1 lag2_dmm_lag2 lag3_dmm_lag3 lag4_dmm_lag4 lag5_dmm_lag5 lag6_dmm_lag6 lag7_dmm_lag7 lag8_dmm_lag8 lag9_dmm_lag9 lag10_dmm_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_isd3_income.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_isd3_income, replace
putexcel A1 = matrix(V), names
putexcel close




****Disaster SD external *****

clear all
cd "/Users/matteo/Desktop/strm_review/stata_rob"

use "panel_rob_dta.dta", clear
encode Area, generate(id_area)
encode STATE, generate(id_state)

* Creazione delle variabili di interazione
gen S4_dmm = S4 * dmm_IS_D

* Utilizza un ciclo per creare le variabili di interazione tra i lag e dmm
forval i = 1/10 {
    gen lag`i'_dmm_lag`i' = lag`i' * dmm_IS_D_lag`i'
}

acreg GR_INCOME_PC S4 lag1 lag2 lag3 lag4 lag5 lag6 lag7 lag8 lag9 lag10 S4_dmm lag1_dmm_lag1 lag2_dmm_lag2 lag3_dmm_lag3 lag4_dmm_lag4 lag5_dmm_lag5 lag6_dmm_lag6 lag7_dmm_lag7 lag8_dmm_lag8 lag9_dmm_lag9 lag10_dmm_lag10 dmm_DISASTER_YEAR_EXT dmm_DISASTER_YEAR_EXT_lag1 dmm_DISASTER_YEAR_EXT_lag2 dmm_DISASTER_YEAR_EXT_lag3 dmm_DISASTER_YEAR_EXT_lag4 dmm_DISASTER_YEAR_EXT_lag5 dmm_DISASTER_YEAR_EXT_lag6 dmm_DISASTER_YEAR_EXT_lag7 dmm_DISASTER_YEAR_EXT_lag8 dmm_DISASTER_YEAR_EXT_lag9 dmm_DISASTER_YEAR_EXT_lag10 tmp prec state_trend_*, id(id_area) time(time) lagcut(1) spatial latitude(latitude) longitude(longitude) dist(50) pfe1(id_area) pfe2(time)

esttab using "results_isd_ext_income.csv", se replace

matrix V=get(VCE)
matrix list V
putexcel set vcov_isd_ext_income, replace
putexcel A1 = matrix(V), names
putexcel close














