
clear all

global repo "/Users/christianbaehr/Box Sync/westbank_inpii"

import delimited "$repo/ProcessedData/merged_ab.csv", clear

* replace NA values and coerce numeric vars to numeric
foreach var of varlist _all {
	capture confirm string variable `var'
	if !_rc {
		replace `var' = "." if `var' == "NA"
		destring `var', replace
	}
}

encode village_name, gen(village_code)

teffects psmatch (q7001) (treatment male age education dist_to_city)
teffects psmatch (q7001) (treatment male age education married muslim urban rural unemployed own_a_car)
teffects psmatch (q7002) (treatment male age education married muslim urban rural unemployed own_a_car)


* teffects psmatch (q7001) (treatment male age education married muslim urban rural unemployed population dmsp_pretrend viirs2012max dist_to_city)

loc y_val "q7001 q7002 q707 q101 q102 q2011 q2013 q511 q513 q5162"

loc covars "male age education married muslim population dist_to_city dmsp_pretrend viirs2012max"

* loc covars "male age education married muslim population dist_to_city"

* reg q7001 c.treatment#c.wave4 wave4 `covars_1' i.village_code, cluster(village_code)
* outreg2 using "/Users/christianbaehr/Desktop/test.doc", replace noni nocons addtext("Village FEs", Y) keep(c.treatment#c.wave4 wave4 `covars_1')

foreach i of local y_val {
	
	reg `i' c.treatment#c.wave4 wave4, cluster(village_code)
	outreg2 using "$repo/Results/`i'.doc", replace tex noni nocons addtext("Village FEs", N)
	
	reg `i' c.treatment#c.wave4 wave4 `covars' , cluster(village_code)
	outreg2 using "$repo/Results/`i'.doc", append tex noni nocons addtext("Village FEs", N)
		
	reg `i' c.treatment#c.wave4 wave4 i.village_code `covars', cluster(village_code)
	outreg2 using "$repo/Results/`i'.doc", append tex noni nocons addtext("Village FEs", Y) keep(c.treatment#c.wave4 wave4 `covars')
	
	rm "$repo/Results/`i'.txt"
}

***************************************************

*loc y_val "q7001 q7002 q707 q101 q102 q2011 q2013 q511 q513 q5162"
*loc covars_1 "male age education married muslim population dist_to_city dmsp_pretrend viirs2012max"
*loc covars_2 "male age education married muslim"




foreach i of local y_val {

	reg `i' c.wave4#c.earlytreat wave4, cluster(village_code)
	outreg2 using "$repo/Results/`i'_earlytrt.doc", replace tex noni nocons addtext("Village FEs", N)
	
	reg `i' c.wave4#c.earlytreat wave4 `covars', cluster(village_code)
	outreg2 using "$repo/Results/`i'_earlytrt.doc", append tex noni nocons addtext("Village FEs", N)
	
	reg `i' c.wave4#c.earlytreat wave4 i.village_code `covars', cluster(village_code)
	outreg2 using "$repo/Results/`i'_earlytrt.doc", append tex noni nocons addtext("Village FEs", Y) keep(c.wave4#c.earlytreat wave4 `covars')

	rm "$repo/Results/`i'_earlytrt.txt"

}

* reg q7001 c.wave4#c.earlytreat wave4 population dist_to_city dmsp_pretrend viirs2012max i.village_code, cluster(village_code)
* reg q7001 c.wave4#c.earlytreat wave4 i.village_code population dist_to_city dmsp_pretrend viirs2012max, cluster(village_code)
* reghdfe q7001 c.wave4#c.earlytreat wave4 population dist_to_city dmsp_pretrend viirs2012max, cluster(village_code) absorb(village_code)

























