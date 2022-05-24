
clear 
set obs 2
gen year=2020 if _n==1
replace year=2060 if year==.

tsset year
tsfill, full

tempfile new
save `new', replace

cd "C:\Users\andrew.carr-harris\Desktop\Git\Rec.-deman-modelling"
import excel using "pct_biomass_region_dismap.xlsx", clear first 

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

gen N_l_2019=154821261
gen south_N_l=N_l*south_pct_biomass_predict
gen nj_N_l=N_l*nj_pct_biomass_predict
gen north_N_l=N_l*north_pct_biomass_predict

format south_N_l nj_N_l north_N_l %13.0gc

egen sum_N_l=rowtotal(south_N_l nj_N_l north_N_l) 
format  sum_N_l N_l %13.0gc


local vars south nj north
foreach v of local vars{
	su `v'_N_l if year==2019
	gen normalize_pred_`v'= `v'_N_l/`r(mean)'
}

egen norm_rowtot=rowtotal(normalize_pred_south normalize_pred_nj normalize_pred_north)



local vars south nj north
foreach v of local vars{
gen abs_

}
format stand_pred_south stand_pred_nj stand_pred_north %13.0gc
egen rowtot=rowtotal(stand_pred_south stand_pred_nj stand_pred_north)
format rowtot %13.0gc

local vars south nj north
foreach v of local vars{
	su stand_pred_`v' if year==2019
	gen stand_pred_`v'_new=  stand_pred_`v'/`r(mean)'
}



local vars south nj north
foreach v of local vars{
	su `v'_pct_biomass_predict if year==2019
	gen stand_pred_`v'= `r(mean)'*N_l
}
format stand_pred_south stand_pred_nj stand_pred_north %13.0gc
egen rowtot1=rowtotal(stand_pred_south_new stand_pred_nj_new stand_pred_north_new)
format rowtot %13.0gc

egen sum =

local vars south nj north
foreach v of local vars{
	su `v'_pct_biomass_predict if year==2019
	gen stand_pred_`v'= `v'_pct_biomass_predict/`r(mean)'
}


egen sumperc=rowtotal(south_pct_biomass_predict north_pct_biomass_predict nj_pct_biomass_predict)



/*				  
replace  south_pct_biomass_predict=south_pct_biomass_predict/100
replace  north_pct_biomass_predict=north_pct_biomass_predict/100
replace  nj_pct_biomass_predict=nj_pct_biomass_predict/100			

replace south_pct_biomass_predict=round(south_pct_biomass_predict, .001)
replace north_pct_biomass_predict=round(north_pct_biomass_predict, .001)
replace nj_pct_biomass_predict=round(nj_pct_biomass_predict, .001)
*/

egen sum_predict=rowtotal(south_pct_biomass_predict nj_pct_biomass_predict north_pct_biomass_predict)
drop sum*
rename south_pct_biomass_predict predict_south
rename nj_pct_biomass_predict predict_nj
rename north_pct_biomass_predict predict_north


keep year stand*  
reshape long  stand_pred_, i(year) j(reg) string

expand 4 if reg=="north"
expand 4 if reg=="south"
sort year reg stand

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

rename stand pred_pct_biomass_stand
export excel using "projected_biomass_by_region.xlsx", replace firstrow(var)


import excel using "numbers_at_length_new.xlsx", clear first 
tabstat C_l, stat(sum) by(state) format(%12.0gc)
tabstat C_l_new, stat(sum) by(state) format(%12.0gc)
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


