*-----------------------------------------------------------------------------
*Programming : Jehu Mette
*For: Assignment 3
*Spring 2020
*-----------------------------------------------------------------------------

break


*Generate Table 2

use  chem_patents_maindataset.dta, clear


*creating postTWEA 
gen postTWEA=1 if grntyr>=1919
replace postTWEA=0 if grntyr<1919

*creating preTWEA
gen preTWEA=1 if grntyr<=1919
replace preTWEA=0 if grntyr>=1919



*Labelling the variables

label variable treat "Subclass has at least one license "

label variable count_for " Number of patents by foreign inventors "

label variable count_cl "Number of licenses "

label variable count_cl_2 " Number of licenses squared "

label variable year_conf "Remaining lifetime of licensed patents "

label variable year_conf_2 " Remaining lifetime of licensed patents squared (x100) "

*Table 2

*(Column 1)
reghdfe count_usa treat count_for, absorb(grntyr class_id) cluster(class_id)
outreg2 using trade_a3_q1.tex, dec(3) label 

*(Column 2)
reghdfe count_usa treat, absorb(grntyr class_id) cluster(class_id)
outreg2 using trade_a3_q1.tex, dec(3) label append
*(Column 3)
reghdfe count_usa count_cl count_cl_2 count_for, absorb(grntyr class_id) cluster(class_id)
outreg2 using trade_a3_q1.tex, dec(3) label append
*(Column 4)
reghdfe count_usa count_cl count_for, absorb(grntyr class_id) cluster(class_id)
outreg2 using trade_a3_q1.tex, dec(3) label append
*(Column 5)
reghdfe count_usa count_cl, absorb(grntyr class_id) cluster(class_id)
outreg2 using trade_a3_q1.tex, dec(3) label append
*(Column 6)
reghdfe count_usa year_conf year_conf_2 count_for, absorb(grntyr class_id) cluster(class_id)
outreg2 using trade_a3_q1.tex, dec(3) label append
*(Column 7)
reghdfe count_usa year_conf count_for, absorb(grntyr class_id) cluster(class_id)
outreg2 using trade_a3_q1.tex, dec(3) label append
*(Column 8)
reghdfe count_usa year_conf, absorb(grntyr class_id) cluster(class_id)
outreg2 using trade_a3_q1.tex, dec(3) label append



*Table 3


*(Column 1)
reghdfe count_usa count_cl_itt count_for, absorb(grntyr class_id) cluster(class_id)
outreg2 using trade_a3_q1_3.tex, dec(3) label 
*(Column 2)
reghdfe count_usa count_cl_itt, absorb(grntyr class_id) cluster(class_id)
outreg2 using trade_a3_q1_3.tex, dec(3) label append
*(Column 3)
reghdfe count_usa year_conf_itt count_for, absorb(grntyr class_id) cluster(class_id)
outreg2 using trade_a3_q1_3.tex, dec(3) label append
*(Column 4)
reghdfe count_usa year_conf_itt, absorb(grntyr class_id) cluster(class_id)
outreg2 using trade_a3_q1_3.tex, dec(3) label append


*Table 4

*(Column 1)
reghdfe count_cl count_cl_itt, absorb(grntyr class_id) cluster(class_id)
predict count_clhat
outreg2 using trade_a3_q1_4.tex, dec(3) label 

*(Column 2)
reghdfe count_cl year_conf_itt, absorb(grntyr class_id) cluster(class_id)
predict count_clhat2
outreg2 using trade_a3_q1_4.tex, dec(3) label append

*(Column 3)
reghdfe count_usa count_clhat, absorb(grntyr class_id) cluster(class_id)
outreg2 using trade_a3_q1_4.tex, dec(3) label append

*(Column 4)
reghdfe count_usa count_clhat2, absorb(grntyr class_id) cluster(class_id)
outreg2 using trade_a3_q1_4.tex, dec(3) label append





	
	 
*For Figure 4
use chem_patents_maindataset.dta,clear

forvalues x=1875/1939 {
gen td_`x'=0
qui replace td_`x'=1 if grntyr==`x'
}

forvalues x=1875/1939 {
     gen untreat_`x'=1 if licensed_class==0 & grntyr==`x'
     replace untreat_`x'=0 if untreat_`x'==.
     gen treat_`x'= 1 if licensed_class==1 & grntyr==`x'
	 qui replace treat_`x'=0 if treat_`x'==.
	 }
	 drop *treat_1900

reghdfe count_usa treat_* untreat_*, absorb(class_id ) cluster(class_id)
regsave treat_* untreat_* using fig4, tstat pval ci replace
use fig4.dta, clear

gen year=substr(var, strpos(var, "_")+1,4)



destring year, replace
drop if year>1919
drop stderr tstat pval N r2
gen treat=1 if substr(var, 1,2)=="tr"
replace treat=0 if treat==.
drop var
reshape wide coef ci_lower ci_upper, i(year) j(treat)

twoway (line coef1 year , lcolor(edkblue) legend(off)) (line ci_lower1 year, lpattern(shortdash) lcolor(eltblue)) (line ci_upper1 year, lpattern(shortdash) lcolor(eltblue)) (line coef0 year , lcolor(red) legend(off)) (line ci_lower0 year, lpattern(shortdash) lcolor(red)) (line ci_upper0 year, lpattern(shortdash) lcolor(red)) ,ytitle("Coefficient for year dummies")

graph export "C:\Users\User\Documents\ECON 890\figure4.png", as(png) name("Graph")



* Figure 6

use chem_patents_maindataset.dta, clear

forvalues x=1876/1939 {
gen td_`x'=0
qui replace td_`x'=1 if grntyr==`x'
}

foreach var in treat count_cl year_conf {
forvalues x=1919/1939 {
     gen `var'_`x'=  `var' if grntyr==`x'
	 qui replace `var'_`x'=0 if grntyr!=`x'
	 }
	 }
	 
reghdfe count_usa treat_* count_for, absorb (class_id grntyr) cluster(class_id) 
regsave treat_* using fig6, tstat pval ci replace
use fig6.dta, clear
gen year=substr(var, strpos(var, "_")+1,4)
destring year, replace
drop if year<1919
drop stderr tstat pval N r2
gen treat=1 if substr(var, 1,2)=="tr"
replace treat=0 if treat==.
drop var
reshape wide coef ci_lower ci_upper, i(year) j(treat)
twoway (line coef1 year , lcolor(edkblue) legend(off)) (line ci_lower1 year, lpattern(shortdash) lcolor(eltblue)) (line ci_upper1 year, lpattern(shortdash) lcolor(eltblue)) ,ytitle("Annual Treatment Effect")

graph export "C:\Users\User\Documents\ECON 890\figure6.png", as(png) name("Graph")

*For Figure 7

use chem_patents_maindataset.dta, clear
forvalues x=1876/1939 {
gen td_`x'=0
qui replace td_`x'=1 if grntyr==`x'
}

foreach var in treat count_cl year_conf {
forvalues x=1919/1939 {
     gen `var'_`x'=  `var' if grntyr==`x'
	 qui replace `var'_`x'=0 if grntyr!=`x'
	 }
	 }
	 
reghdfe count_usa count_cl_1919-count_cl_1939 count_for, absorb (class_id grntyr) cluster(class_id)
regsave count_cl_1919-count_cl_1939 using fig7, tstat pval ci replace
use fig7.dta, clear
gen year=substr(var, strpos(var, "cl_")+3,4)
destring year, replace
drop stderr tstat pval N r2
gen count_cl=1 if substr(var, 1,2)=="co"
replace count_cl=0 if count_cl==.
drop var
reshape wide coef ci_lower ci_upper, i(year) j(count_cl)
twoway (line coef1 year , lcolor(edkblue) legend(off)) (line ci_lower1 year, lpattern(shortdash) lcolor(eltblue)) (line ci_upper1 year, lpattern(shortdash) lcolor(eltblue)) ,ytitle("Annual Treatment Effect")

graph export "C:\Users\User\Documents\ECON 890\figure7.png", as(png) name("Graph")

*For Figure 8
use chem_patents_maindataset.dta, clear
forvalues x=1876/1939 {
gen td_`x'=0
qui replace td_`x'=1 if grntyr==`x'
}

foreach var in treat count_cl year_conf {
forvalues x=1919/1939 {
     gen `var'_`x'=  `var' if grntyr==`x'
	 qui replace `var'_`x'=0 if grntyr!=`x'
	 }
	 }
	 
reghdfe count_usa year_conf_1919-year_conf_1939 count_for, absorb (class_id grntyr) cluster(class_id)
regsave year_conf_1919-year_conf_1939 using fig8, tstat pval ci replace
use fig8.dta, clear
gen year=substr(var, strpos(var, "nf_")+3,4)
destring year, replace
drop stderr tstat pval N r2
gen year_conf=1 if substr(var, 1,2)=="ye"
replace year_conf=0 if year_conf==.
drop var
reshape wide coef ci_lower ci_upper, i(year) j(year_conf)
twoway (line coef1 year , lcolor(edkblue) legend(off)) (line ci_lower1 year, lpattern(shortdash) lcolor(eltblue)) (line ci_upper1 year, lpattern(shortdash) lcolor(eltblue)) ,ytitle("Annual Treatment Effect")

graph export "C:\Users\User\Documents\ECON 890\figure8.png", as(png) name("Graph")

*For Figure 9
use "fig10.dta",clear
reghdfe y usa_treat_td1919-usa_treat_td1939 usa_td* usa_treat treat_td* usa td_*, absorb(class_id) cluster(class_id)
regsave usa_treat_td1919-usa_treat_td1939 using fig9, tstat pval ci replace
use fig9.dta, clear
gen year=substr(var, strpos(var, "d")+1,4)
destring year, replace
drop stderr tstat pval N r2
gen usa_treat=1 if substr(var, 1,2)=="us"
replace usa_treat=0 if usa_treat==.
drop var
reshape wide coef ci_lower ci_upper, i(year) j(usa_treat)
twoway (line coef1 year , lcolor(edkblue) legend(off)) (line ci_lower1 year, lpattern(shortdash) lcolor(eltblue)) (line ci_upper1 year, lpattern(shortdash) lcolor(eltblue)) ,ytitle("Annual Treatment Effect")

graph export "C:\Users\User\Documents\ECON 890\figure9.png", as(png) name("Graph")


*For Figure 10
use chem_patents_maindataset.dta, clear
forvalues x=1876/1939 {
gen td_`x'=0
qui replace td_`x'=1 if grntyr==`x'
}

foreach var in treat count_cl year_conf {
forvalues x=1919/1939 {
     gen `var'_`x'=  `var' if grntyr==`x'
	 qui replace `var'_`x'=0 if grntyr!=`x'
	 }
	 }
reghdfe count_france treat_* td*, absorb(class_id) cluster(class_id)

*save the coefficients as variables in fig10a.dta
*regsave treat_* td* using fig10a, tstat pval ci replace

use fig10a.dta, clear
gen year=substr(var, strpos(var, "_")+1,4)
destring year, replace
drop if year<1919
drop stderr tstat pval N r2
gen treat=1 if substr(var, 1,2)=="tr"
replace treat=0 if treat==.
drop var
reshape wide coef ci_lower ci_upper, i(year) j(treat)
twoway (line coef1 year , lcolor(edkblue) legend(off)) (line ci_lower1 year, lpattern(shortdash) lcolor(eltblue)) (line ci_upper1 year, lpattern(shortdash) lcolor(eltblue)) ///
(line coef0 year , lcolor(red) legend(off)) (line ci_lower0 year, lpattern(shortdash) lcolor(red)) (line ci_upper0 year, lpattern(shortdash) lcolor(red)) ,ytitle("Coefficient for year dummies")
graph export "C:\Users\User\Documents\ECON 890\figure10.png", as(png) name("Graph")
