version 18.0

* Build the cleaned CNEF file used by Figure 13 and the spousal-income moments.
local suffix = 13
foreach wave in bd be {
    use "${soep_orig}/`wave'pequiv.dta", clear
    rename d11102ll gender
    rename d11101`suffix' age
    rename d11104`suffix' marital
    rename d11105`suffix' rel
    rename e11101`suffix' emphours
    rename e11102`suffix' empstatus
    rename e11103`suffix' emplevel
    rename i11101`suffix' pregov_inc
    rename i11102`suffix' postgov_inc
    rename i11103`suffix' lab_inc
    rename i11107`suffix' pub_trans
    rename i11109`suffix' tot_tax
    rename i11110`suffix' ind_lab_inc
    rename i11111`suffix' fed_tax
    rename i11112`suffix' ss_tax
    rename i11113`suffix' postgov_inc_sim
    rename i11114`suffix' tot_tax_sim
    rename w11102`suffix' hh_weight
    rename d11107`suffix' numch_all
    rename h11103`suffix' numch_0_1
    rename h11104`suffix' numch_2_4
    rename h11105`suffix' numch_5_7
    rename h11106`suffix' numch_8_10
    rename h11107`suffix' numch_11_12
    rename h11108`suffix' numch_13_15
    rename h11109`suffix' numch_16_18
    rename iunby`suffix' unemp_ben
    rename iunay`suffix' unemp_as
    rename imaty`suffix' mat_ben
    rename chspt`suffix' ch_ben
    rename alg2`suffix' unemp_ben2

    keep hid pid syear gender age marital rel emphours empstatus emplevel pregov_inc postgov_inc lab_inc pub_trans tot_tax ind_lab_inc fed_tax ss_tax postgov_inc_sim tot_tax_sim hh_weight numch_* unemp_ben unemp_as mat_ben ch_ben unemp_ben2
    if `suffix'>13 {
        append using "${work}/tax_estim.dta"
    }
    compress
    save "${work}/tax_estim.dta", replace
    local suffix = `suffix'+1
}

* Add annual non-employment duration and education from the monthly panel.
use "${work}/panel_monthly_mom.dta", clear
keep pid syear tmonth hours_type edu
generate nonemp = hours_type==0
bysort pid syear: egen nonemp_months = total(nonemp)
keep pid syear nonemp_months edu
duplicates drop
merge 1:1 syear pid using "${work}/tax_estim.dta"
keep if _merge==3
drop _merge
save "${work}/tax_estim.dta", replace

* Keep valid heads/spouses and construct household and child states.
keep if inlist(rel,1,2)
drop if gender==-1
bysort syear hid: generate hhsize = _N
drop if hhsize>2
drop if hhsize==2 & marital!=1
generate married = marital==1

generate chst_age6 = .
replace chst_age6 = 0 if numch_all==0
replace chst_age6 = 1 if (numch_0_1>0 & !missing(numch_0_1)) | (numch_2_4>0 & !missing(numch_2_4)) | (numch_5_7>0 & !missing(numch_5_7))
replace chst_age6 = 2 if numch_all>0 & missing(chst_age6)

generate chst_age12 = .
replace chst_age12 = 0 if numch_all==0
replace chst_age12 = 1 if (numch_0_1>0 & !missing(numch_0_1)) | (numch_2_4>0 & !missing(numch_2_4)) | (numch_5_7>0 & !missing(numch_5_7)) | (numch_8_10>0 & !missing(numch_8_10)) | (numch_11_12>0 & !missing(numch_11_12))
replace chst_age12 = 2 if numch_all>0 & missing(chst_age12)

foreach var in emphours unemp_ben unemp_ben2 unemp_as mat_ben pregov_inc postgov_inc postgov_inc_sim ss_tax {
    replace `var' = 0 if `var'<0
}

compress
save "${work}/tax_estim_clean.dta", replace
