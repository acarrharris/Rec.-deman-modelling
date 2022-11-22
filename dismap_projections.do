



clear 
set obs 2
gen year=2019 if _n==1
replace year=2060 if year==.

tsset year
tsfill, full

tempfile new
save `new', replace

cd "C:\Users\andrew.carr-harris\Desktop\Git\Rec.-deman-modelling"
import excel using "pct_biomass_region_dismap.xlsx", clear first 
/*
tsset year
tsfill, full

su south if inlist(year, 2016, 2018)
replace south=`r(mean)' if year==2017

su nj if inlist(year, 2016, 2018)
replace nj=`r(mean)' if year==2017

su north if inlist(year, 2016, 2018)
replace north=`r(mean)' if year==2017
*/
append using `new'

reg south_pct_biomass year if year>=1999
predict south_pct_biomass_predict, xb
*twoway scatter south_pct_biomass  year  || line south_pct_biomass_predict year if year>=1999


reg north_pct_biomass year if year>=1999
predict north_pct_biomass_predict, xb
*twoway scatter north_pct_biomass  year  || line north_pct_biomass_predict year if year>=1999


reg nj_pct_biomass year if year>=1999
predict nj_pct_biomass_predict, xb
*twoway scatter nj_pct_biomass  year  || line nj_pct_biomass_predict year if year>=1999
/*
global graphoptions graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)

twoway(scatter north_pct_biomass year, connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
		   (scatter north_pct_biomass_predict year if year>=1999 , connect(direct) lcol(blue)   lwidth(medthick)  lpat(dash) msymbol(i))  ///
		   (scatter nj_pct_biomass year, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i))  ///
		   (scatter nj_pct_biomass_predict year if year>=1999 , connect(direct) lcol(red)   lwidth(medthick)  lpat(dash) msymbol(i)) ///
		   (scatter south_pct_biomass year, connect(direct) lcol(green)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
		   (scatter south_pct_biomass_predict year if year>=1999 , connect(direct) lcol(green)   lwidth(medthick)  lpat(dash) msymbol(i)  $graphoptions ///
			xtitle("") xlab(1974(5)2060, labsize(small) angle(45)) ytitle("Percent of total biomass", size(medsmall)) ylabel(, angle(horizontal) labsize(small))  title("Observed and predicted percent of total" "interpolated fluke biomass by region ", size(medsmall)) ///
		   legend(lab(1 "Observed ME-NY") lab(2 "Predicted ME-NY") ///
					  lab(3 "Observed NJ") lab(4 "Predicted NJ")  ///
					  lab(5 "Observed DE-NC") lab(6 "Predicted DE-NC")  cols(2) size(small) region(lcolor(none)) ) ) 
*/



replace  south_pct_biomass_predict=south_pct_biomass_predict/100
replace  north_pct_biomass_predict=north_pct_biomass_predict/100
replace  nj_pct_biomass_predict=nj_pct_biomass_predict/100		

egen sum=rowtotal(south_pct_biomass_predict north_pct_biomass_predict nj_pct_biomass_predict)


/*
local vars south nj north
foreach v of local vars{
	su `v'_pct_biomass_predict if year==2019
	gen norm_pred_`v'= `v'_pct_biomass_predict/`r(mean)'
}
*/

rename south_pct_biomass_predict predict_south
rename nj_pct_biomass_predict predict_nj
rename north_pct_biomass_predict predict_north


keep year predict*  
duplicates drop 
reshape long  predict_, i(year) j(reg) string

expand 4 if reg=="north"
expand 4 if reg=="south"
sort year reg predict_

bysort year reg: gen tab=_n
gen state="NJ" if reg=="nj"
replace state="MA" if reg=="north" & tab==1
replace state="RI" if reg=="north" & tab==2
replace state="CT" if reg=="north" & tab==3
replace state="NY" if reg=="north" & tab==4
replace state="DE" if reg=="south" & tab==1
replace state="MD" if reg=="south" & tab==2
replace state="VA" if reg=="south" & tab==3
replace state="NC" if reg=="south" & tab==4
drop tab reg

rename predict prediction
sort year state
replace pred=pred if state=="NJ"
replace pred=pred/4 if state!="NJ"

keep if year>=2015
export excel using "projected_biomass_prop_by_state.xlsx", replace firstrow(var)

tempfile base_props
save `base_props', replace 


global fluke_catch
forv y=2019(1)2060{

u `base_props', clear 

keep if year==`y'

tempfile base_props2
save `base_props2', replace  

cd "C:\Users\andrew.carr-harris\Desktop\Git\Rec.-deman-modelling"
import excel using "numbers_at_length_new.xlsx", clear first 

drop prediction year 
merge m:1 state using `base_props2', nogen

replace pred=pred if state=="NJ"
replace pred=pred/4 if state!="NJ"

gen N_l_prop_new=pred*N_l 

tabstat N_l_prop, stat(sum) by(state) format(%12.0gc)
tabstat N_l_prop_new, stat(sum) by(state) format(%12.0gc)

preserve 
collapse (mean) prediction, by(state l_in)
tempfile new
save `new', replace
restore 

collapse (sum) C_l N_l_prop N_l_prop_new, by(l_in state year) 
merge 1:1 state l_in using `new', nogen

order state l_in C_l N_l_prop prediction N_l_prop_new


gen selectivity= C_l/N_l_prop
gen C_l_new_adj= selectivity*N_l_prop_new

ds state year, not
format `r(varlist)' %13.0gc


tempfile fluke_catch`y'
save `fluke_catch`y'', replace
global fluke_catch "$fluke_catch "`fluke_catch`y''" " 
}

clear
dsconcat $fluke_catch

tabstat C_l_new_adj, stat(sum) by(year)  format(%13.0gc)
tabstat N_l_prop_new, stat(sum) by(year)  format(%13.0gc)


collapse (sum)  C_l C_l_new_adj N_l_prop N_l_prop_new, by(state year) 
gen biomass_adj_scalar=C_l_new_adj/C_l
gen catch_check=C_l*biomass_adj_scalar
format  catch_check %13.0gc

keep state year biomass_adj_scalar
replace biomass_adj_scalar=1 if year==2019

sort year state
cd "C:\Users\andrew.carr-harris\Desktop\Git\Rec.-deman-modelling"
export excel using "biomass_adj_scalar.xlsx", replace firstrow(variables)


format C_l_new_adj %13.0gc
format C_l_new %13.0gc

gen catch_exp_factor_pop=C_l_new_adj/C_l_new



gen base_ratio1= C_l_new/N_l_prop

gen C_l_new_adj=base_ratio*N_l_prop_new

gen ratio=N_l_prop_new/N_l_prop

gen C_l_new_adj=N_l_prop_new*E*q
tabstat C_l, stat(sum) by(state) format(%12.0gc)
tabstat C_l_new, stat(sum) by(state) format(%12.0gc)
tabstat C_l_new_adj, stat(sum) by(state) format(%12.0gc)

gen reg="SO" if inlist(state, "DE", "MD", "VA", "NC")
replace reg=" NO"  if inlist(state, "MA", "RI", "CT", "NY")
replace reg=" NJ"  if inlist(state, "NJ")

tabstat C_l, stat(sum) by(reg) format(%12.0gc)
tabstat N_l_prop, stat(sum) by(reg) format(%13.0gc)
tabstat N_l_prop_new, stat(sum) by(reg) format(%13.0gc)

tabstat N_l_prop_new, stat(sum) by(reg) format(%13.0gc)

egen sum_C_l_new_state=sum(C_l_new), by(state)
gen C_l_new_adj_state=ratio*sum_C_l_new_state
gen diff=C_l_new_adj_state-sum_C_l_new_state

distinct l_in 
gen C_l_new_adj1= C_l_new+(diff/`r(ndistinct)' )



gen extra fish=
gen C_l_new_adj=C_l_new*ratio
 


drop sum*
drop ratio
gen C_l_new_adj=N_l_prop_new*E*q






drop sum_C_l-C_l_new_prop_adj
/*
replace prediction=.6 if reg=="NO"
replace prediction=.2 if reg=="NJ"
replace prediction=.2 if reg=="SO"
*/
egen sum_C_l_base=sum(C_l), by(reg)

egen sum_C_l_reg=sum(C_l_new), by(reg)
egen sum_C_l=sum(C_l_new)
gen predicted_regional_catch=sum_C_l*prediction


format predicted_regional_catch sum_C_l_reg sum_C_l sum_C_l_base %12.0gc

gen N_l_new_prop=N_l*prediction
format N_l_new_prop %12.0gc


su N_l_prop
return list
su N_l_new_prop
return list

gen C_l_new_adj=N_l_new_prop*E*q
tabstat C_l_new_adj, stat(sum) by(state) format(%12.0gc)

tabstat C_l_new, stat(sum) by(state) format(%12.0gc)
tabstat C_l, stat(sum) by(state) format(%12.0gc)


tabstat C_l_new_adj, stat(sum) by(state) format(%12.0gc)

sort state l_
gen N_l_new=N_l*stand
gen C_l_new_adj= N_l_new*q*E

gen C_l_new_adj=C_l_new*stand

su N_l
local base=`r(sum)'
su N_l_new
di `base'/`r(sum)'

su C_l
local base=`r(sum)'
su C_l_new_adj
di `base'/`r(sum)'


tabstat N_l, stat(sum) by(state) format(%12.0gc)
tabstat N_l_new, stat(sum) by(state) format(%12.0gc)

tabstat C_l, stat(sum) by(state) format(%12.0gc)
tabstat C_l_new_adj, stat(sum) by(state) format(%12.0gc)


bysort state: egen sum_N_l_new=sum(N_l_new)
format sum_N_l_new %12.0gc

bysort state: egen sum_N_l_base=sum(N_l)
format sum_N_l_base %12.0gc


*gen correction_factor= sum_N_l_base/sum_N_l_new
*replace N_l_new=N_l_new*correction

*gen C_l_new_adj=N_l_new*q*E
gen C_l_new_adj=C_l_new*stand


gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="NJ"  if inlist(state, "NJ")
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")


tabstat C_l_new, stat(sum) by(state) format(%12.0gc)
tabstat C_l_new_adj, stat(sum) by(state) format(%12.0gc)

bysort region: egen sum_C_l_base=sum(C_l_new)
format sum_C_l_base %12.0gc

bysort region: egen sum_C_l_new=sum(C_l_new_adj)
format sum_C_l_new %12.0gc

gen correction_factor= sum_C_l_base/sum_C_l_new
replace C_l_new_adj=C_l_new_adj*correction_factor




tabstat C_l_new, stat(sum) by(state) format(%12.0gc)
tabstat C_l_new_adj, stat(sum) by(state) format(%12.0gc)





gen C_l_new_adj=C_l_new*stand_pred


gen reg="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace reg="NJ" if state=="NJ"
replace reg="SO" if  inlist(state, "DE", "MD", "VA", "NC")

tabstat C_l, stat(sum) by(reg) format(%12.0gc)
tabstat C_l_new, stat(sum) by(reg) format(%12.0gc)
tabstat C_l_new_adj, stat(sum) by(reg) format(%12.0gc)


