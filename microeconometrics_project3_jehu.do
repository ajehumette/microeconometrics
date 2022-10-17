*----------------------------------------------------------------------------
*Programming: Jehu
*Purpose: Homework 3 Microeconometrics
*---------------------------------------------------------------------------


use "C:\Users\User\Downloads\slave_trade_QJE.dta"


*Labelling


label variable ln_export_area "ln(export/area)"
label variable abs_latitude"Distance from equator "
label variable longitude " Longitude "
label variable rain_min "Lowest Monthly Rainfall "
label variable humid_max  "Avg Max humidity "
label variable low_temp "Avg min temperature "
label variable ln_coastline_area  "ln(coastline/area) "
label variable island_dum" Island indicator "
label variable islam " Percent Islamic "
label variable legor_fr " French Legal Origin"
label variable ln_avg_gold_pop  " ln(gold prod/pop)"
label variable ln_avg_all_diamonds_pop" ln(diamond prod/pop) "




global IV "atlantic_distance_minimum indian_distance_minimum saharan_distance_minimum red_sea_distance_minimum"
global full_controls "abs_latitude longitude rain_min humid_max low_temp ln_coastline_area island_dum islam legor_fr region_n ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop"
global colony_fe "colony1 colony2 colony3 colony4 colony5 colony6 colony7"


*Q1 a. 
ivreg2 ln_maddison_pcgdp2000 (ln_export_area=$IV) $full_controls $colony_fe
outreg2 using mx_a3_q1a.tex, dec(3) label

*Q1 b. 
test _b[ln_export_area]=0


*Q1 c. 
program gmm_ivreg

	version 16

    syntax varlist [if] , at(name) rhs(varlist) depvar(varlist)

    tempvar m
    quietly gen double `m' = 0 `if'
    local i 1
    foreach var of varlist `rhs' {
        quietly replace `m' = `m' + `var'*`at'[1,`i'] `if'
        local `++i'
    }
    quietly replace `m' = `m' + `at'[1,`i'] `if'    // constant

    quietly replace `varlist' = `depvar' - `m' `if'
end

*Running gmm program
gmm gmm_ivreg, nequations(1) nparameters(22) instruments($IV $full_controls $colony_fe) depvar(ln_maddison_pcgdp2000) rhs(ln_export_area $full_controls $colony_fe) onestep

*Q1
* d)
gmm gmm_ivreg, nequations(1) nparameters(22) instruments($IV $full_controls $colony_fe) depvar(ln_maddison_pcgdp2000) rhs(ln_export_area $full_controls $colony_fe) 


*Q1
* e)
ivregress gmm ln_maddison_pcgdp2000 (ln_export_area=$IV) $full_controls $colony_fe 



ivregress 2sls ln_maddison_pcgdp2000 (ln_export_area=$IV) $full_controls $colony_fe 








*Q1 f)

ivreg2 ln_maddison_pcgdp2000 (ln_export_area=$IV) $full_controls $colony_fe, cue
outreg2 using mx_a3_q1f.tex, dec(3) label


*Q1 g) 
ivregress 2sls ln_maddison_pcgdp2000 (ln_export_area=$IV) $full_controls $colony_fe
estimate store reg1

ivregress gmm ln_maddison_pcgdp2000 (ln_export_area=$IV) $full_controls $colony_fe 
estimate store reg2

hausman reg2 reg1, sigmamore force

gen a=1-chi2(1, 0.9876)

sum a

*cannot do chow test for iv/2sls
*suest reg1 reg2



*a or the pvalue is 0.32, cannot reject the nulll that reg2 and reg1 are the same. 

*test ln_export in reg2 and reg1
*could use the difference=0.333, divide by the SE=0.08849
*t-statistic: 0.333/0.088=3.78
*this means we should reject the null that ln_export in reg2 is the same as ln_export in reg1. 



*Q2

clear

set seed 10001
set obs 100
scalar b0 = 1
scalar b1 = 2
scalar b2 = 1
scalar b3 = 0.5

scalar a0 = 1
scalar a1 = 1
scalar a2 = 1
scalar a3 = 0.5

* Generate regressors
gen x1 = invnorm(uniform())
gen x2 = 2+3*invnorm(uniform()) 
gen x3 = 1+3*invnorm(uniform()) 
gen u1 = invnorm(uniform())
gen u2= invnorm(uniform())

* Generate y
gen y=b0+b1*x1+b2*x2+b3*x3+u1 if x1>=0
replace y=a0+a1*x1+a2*x2+a3*x3+u2 if y==.| y<0



*2.a
reg y x1 x2 x3
test x1=1
*Manually i)
matrix bfull = e(b)
matrix b =bfull'
matrix vfull = e(V)
matrix R = (1,0,0,0)
matrix r = (1)
matrix h=R*b-r
matrix Wald = h'*syminv(R*vfull*R')*h
matrix list h
matrix list R
matrix list Wald
scalar WaldB = Wald[1,1]
scalar W3=WaldB/1
di W3



test x2=1
*Manually ii)

matrix bfull = e(b)
matrix b =bfull'
matrix vfull = e(V)
matrix R = (0,1,0,0)
matrix r = (1)
matrix h=R*b-r
matrix Wald = h'*syminv(R*vfull*R')*h
matrix list h
matrix list R
matrix list Wald
scalar WaldB = Wald[1,1]
scalar W3=WaldB/1
di W3

test x1=x2
*Manually iii)
matrix drop h R Wald
matrix h = (bfull[1,1]/bfull[1,2] - 1)
matrix R = (1/bfull[1,2], -bfull[1,1]/(bfull[1,2]^2), 0, 0)
matrix Wald = h'*syminv(R*vfull*R')*h
matrix list h
matrix list R
matrix list Wald
scalar W3=Wald[1,1]/1
di W3

testnl (_b[x1]^2=_b[x2]) (_b[x1]=_b[x3])
*Manually iv)
matrix drop h R Wald
matrix h = (bfull[1,1]/bfull[1,3] - 1\ bfull[1,1]^2/bfull[1,2]-1)
*dh/dbfull=[dh/db2, dh/db3, dh/db4, dh/db1]
matrix R = (1/bfull[1,3], 0, -bfull[1,1]/(bfull[1,3]^2), 0\ 2*bfull[1,1]/bfull[1,2], -bfull[1,1]^2/(bfull[1,2]^2),0,0) // what is R in nonlinear
matrix Wald = h'*syminv(R*vfull*R')*h
matrix list h
matrix list R
matrix list Wald
scalar W3=Wald[1,1]/1
di W3




*b. Using the likelihood ratio test
*i)
reg y x1 x2 x3
estimates store unrestricted
scalar llunrest = e(ll)
constraint define 1 x1=1
cnsreg y x1 x2 x3, constraints(1)
estimates store restricted
scalar llrest = e(ll)
scalar LR = -2*(llrest-llunrest)
scalar p=1-chi2(1, LR)
di "LR " LR
di "p-value " p

*ii)

reg y x1 x2 x3
estimates store unrestricted
scalar llunrest = e(ll)
constraint define 1 x2=1
cnsreg y x1 x2 x3, constraints(1)
estimates store restricted
scalar llrest = e(ll)
scalar LR = -2*(llrest-llunrest)
scalar p=1-chi2(1, LR)
di "LR " LR
di "p-value " p


*iii)
reg y x1 x2 x3
estimates store unrestricted
scalar llunrest = e(ll)
constraint define 1 x1=x2
cnsreg y x1 x2 x3, constraints(1)
estimates store restricted
scalar llrest = e(ll)
scalar LR = -2*(llrest-llunrest)
scalar p=1-chi2(1, LR)
di "LR " LR
di "p-value " p


*iv)

nl (y={b0} + {b1}*x1 + {sqrt(b1)}*x2+{b1}*x3)
scalar llrest=e(ll)
reg y x1 x2 x3
scalar llunrest = e(ll)
scalar LR = -2*(llrest-llunrest)
di "LR " LR
scalar p=1-chi2(2, LR)
di "p-value " p

*c)
reg y x1 x2 x3 if x1>=0
estimate store reg1

reg y x1 x2 x3 if x1<0
estimate store reg2

suest reg1 reg2

test [reg1_mean]x1=[reg2_mean]x1


d)

gen x1_b=x1 if x1<0
replace x1_b=0 if x1>=0

reg y x1 x1_b x2 x3

test x1=x1+x1_b


