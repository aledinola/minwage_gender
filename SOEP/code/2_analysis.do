version 18.0

* Build the annual analysis file used by Tables 1-2 and the moment preparation.
use "${work}/panel_annual.dta", clear

* Four-state labor-force status.
generate lfs = .
replace lfs = 1 if pgemplst==1
replace lfs = 2 if inlist(pgemplst,2,3)
replace lfs = 3 if pgemplst==4
replace lfs = 4 if inlist(pglfs,4,6,8,9,10)
replace lfs = 5 if pglfs==1
replace lfs = 3 if missing(lfs) & pglabgro>0 & !missing(pglabgro)
replace lfs = 5 if missing(lfs)
generate lfs4 = lfs
replace lfs4 = 4 if lfs==5

* Civil-service and self-employment indicators used for the moment sample.
generate empcs = inrange(pgstib,600,699)
generate empse = inrange(pgstib,400,499)

* Marital status, including the paper-consistent reclassification.
generate mar = .
replace mar = 1 if inlist(pgfamstd,1,2)
replace mar = 2 if pgfamstd>2 & !missing(pgfamstd) & pgpartnr>0 & !missing(pgpartnr)
replace mar = 3 if missing(mar)
sort pid pgpartnr
by pid pgpartnr: egen evermarried = max(mar==1) if pgpartnr>0 & !missing(pgpartnr)
sort pid syear
generate mar_orig = mar
by pid: replace mar = 1 if mar==2 & evermarried==1 & N_kids_all>0 & !missing(N_kids_all)

* Spousal-income state for Table 2.
generate netinc = pglabnet
replace netinc = 0 if lfs4==4 | netinc<0 | missing(netinc)
generate med_netinc_m = .
generate med_netinc_f = .
forvalues y = 2007/2018 {
    forvalues s = 1/2 {
        quietly summarize netinc [aweight=phrf] if syear==`y' & ${age} & ${noschool} & ${civilian} & ${nonretired} & sex==`s', detail
        local med_s`s'_y`y' = r(p50)
    }
    replace med_netinc_m = `med_s1_y`y'' if syear==`y'
    replace med_netinc_f = `med_s2_y`y'' if syear==`y'
}

generate mar_spinc = .
replace mar_spinc = 1 if sex==2 & mar==1 & pglabnet_partner>med_netinc_m & !missing(pglabnet_partner)
replace mar_spinc = 2 if sex==2 & mar==1 & pglabnet_partner<=med_netinc_m
replace mar_spinc = 3 if sex==2 & mar==2 & pglabnet_partner>med_netinc_m & !missing(pglabnet_partner)
replace mar_spinc = 4 if sex==2 & mar==2 & pglabnet_partner<=med_netinc_m
replace mar_spinc = 5 if sex==2 & mar==3
replace mar_spinc = 1 if sex==1 & mar==1 & pglabnet_partner>med_netinc_f & !missing(pglabnet_partner)
replace mar_spinc = 2 if sex==1 & mar==1 & pglabnet_partner<=med_netinc_f
replace mar_spinc = 3 if sex==1 & mar==2 & pglabnet_partner>med_netinc_f & !missing(pglabnet_partner)
replace mar_spinc = 4 if sex==1 & mar==2 & pglabnet_partner<=med_netinc_f
replace mar_spinc = 5 if sex==1 & mar==3
drop med_netinc_m med_netinc_f netinc

generate mar_spinc_sim = 1 if mar_spinc==1
replace mar_spinc_sim = 2 if mar_spinc==2
replace mar_spinc_sim = 3 if inrange(mar_spinc,3,5)

* Education, demographics, and child controls.
generate edu = .
replace edu = 1 if inrange(pgbilzeit,1,9)
replace edu = 2 if inrange(pgbilzeit,10,12)
replace edu = 3 if pgbilzeit>12 & !missing(pgbilzeit)
replace age = . if age<0
generate agesq = age^2
generate has_kids = N_kids_all>0
generate has_kids_under3 = N_kids_under3>0
generate has_kids_under6 = N_kids_under6>0
generate female = sex==2

label define lfs4 1 "FT" 2 "PT" 3 "ME" 4 "NE"
label define edu 1 "9 Years or Less" 2 "10-12 Years" 3 "Over 12 Years"
label define mar_spinc_sim 1 "Married, High-Earning partner" 2 "Married, Low-Earning partner" 3 "Unmarried"
label define mar_spinc 1 "Married, High-Earning partner" 2 "Married, Low-Earning partner" 3 "Cohabiting, high-earning partner" 4 "Cohabiting, low-earning partner" 5 "Single", replace
label define kids 0 "No kids" 1 "Kids present"
label define mar 1 "married" 2 "cohabiting" 3 "single", replace
label values lfs4 lfs4
label values edu edu
label values mar_spinc_sim mar_spinc_sim
label values mar_spinc mar_spinc
label values has_kids_under3 kids
label values has_kids_under6 kids
label values has_kids kids
label values mar mar

compress
save "${work}/panel_annual_analysis.dta", replace
