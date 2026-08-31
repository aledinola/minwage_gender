version 18.0

* Build the annual prepared file used by Figure 16 and the annual moments.
use "${work}/panel_annual_analysis.dta", clear
merge m:1 syear using "${work}/cpi.dta"
drop _merge
keep if ${civilian} & ${noschool} & ${nonretired}

sort pid syear
generate flag_csse = (empcs==1 | empse==1) if lfs4<=3
generate flag_noncsse = empcs==0 & empse==0 if lfs4<=3
by pid: egen flag_evercsse = max(flag_csse)
by pid: egen flag_evernoncsse = max(flag_noncsse)
generate flag_emp = lfs4<=3
by pid: egen flag_everemp = max(flag_emp)

keep if ${age}
drop if flag_evernoncsse==0
drop if flag_everemp==0

generate gender = 1+female
generate hours_type = .
replace hours_type = 1 if lfs4==3
replace hours_type = 2 if lfs4==2
replace hours_type = 3 if lfs4==1
replace hours_type = 0 if lfs4==4
label define hours_type 0 "NE (0)" 1 "ME (1)" 2 "PT (2)" 3 "FT (3)", replace
label values hours_type hours_type
generate hours_actual = pgtatzeit if pgtatzeit>0
generate hours_agreed = pgvebzeit if pgvebzeit>0

foreach age in 3 6 12 {
    capture drop has_kids_under`age'
    capture drop has_kids_above`age'
    generate has_kids_under`age' = N_kids_under`age'>0
    generate has_kids_above`age' = has_kids_under`age'==0 & N_kids_all>0
    capture drop childstate_age`age'
    generate childstate_age`age' = 1
    replace childstate_age`age' = 2 if has_kids_under`age'==1
    replace childstate_age`age' = 3 if has_kids_above`age'==1
}

compress
save "${work}/panel_annual_moments_temp.dta", replace

* Build the monthly prepared file used by labor-force transitions and tax data.
use "${work}/panel_monthly.dta", clear
generate syear = year(dofm(tmonth))
drop if inlist(spelltyp_lfs1,6,8,9)
merge m:1 pid syear using "${work}/panel_annual_moments_temp.dta"
keep if _merge==3
drop _merge hours_type
keep if empcs==0 & empse==0 & ${edu} & mar!=2
tsset pid tmonth

generate hours_type = 0 if inlist(spelltyp_lfs1,5,6,10,12)
replace hours_type = 3 if spelltyp_lfs1==1
replace hours_type = 2 if spelltyp_lfs1==3
replace hours_type = 1 if inlist(spelltyp_lfs1,15,2)
drop if missing(hours_type)

compress
save "${work}/panel_monthly_mom.dta", replace
