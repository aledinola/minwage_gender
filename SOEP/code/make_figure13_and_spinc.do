version 18.0

use "${work}/tax_estim_clean.dta", clear
keep if ${edu}
sort syear hid gender
foreach var in age empstatus emplevel unemp_ben mat_ben ind_lab_inc {
    generate `var'_sp = .
    by syear hid: replace `var'_sp = `var'[2] if marital==1 & hhsize==2 & _n==1
}
bysort syear hid: drop if _n>1
generate hhage = age if married==0
replace hhage = (age+age_sp)/2 if married==1
keep if hhage>=25 & hhage<55

generate netinc = .
replace netinc = postgov_inc-unemp_ben-unemp_ben_sp-unemp_ben2-mat_ben-mat_ben_sp if married==1
replace netinc = postgov_inc-unemp_ben-unemp_ben2-mat_ben if married==0
generate groinc = pregov_inc-ss_tax
generate lnet = log(netinc) if netinc>0
generate lgro = log(groinc) if groinc>0

forvalues m = 0/1 {
    graph twoway ///
        lfitci lnet lgro if married==`m' & chst_age6==0 & lgro>=6 & lgro<=15, est(robust) ciplot(rline) lp("l") lw(medthick) alp("l") alw(medium) || ///
        lfitci lnet lgro if married==`m' & chst_age6==1 & lgro>=6 & lgro<=15, est(robust) ciplot(rline) lpattern("-") lw(medthick) alp("l") alw(medium) || ///
        lfitci lnet lgro if married==`m' & chst_age6==2 & lgro>=6 & lgro<=15, est(robust) ciplot(rline) lpattern("-.") lw(medthick) alp("l") alw(medium) ||, ///
        scale(1.4) yscale(range(6.5 12.5)) xscale(range(6 14)) ///
        name(tax_mar`m', replace) ytitle("Log of Net Household Annual Income") ///
        xtitle("Log of Gross Household Annual Income") ///
        legend(order(1 "95% CI" 2 "No child" 4 "Young children" 6 "Grown children") position(5) ring(0))

    local panel = char(97+`m')
    graph export "${paper_outputs}/Figure13`panel'.eps", name(tax_mar`m') replace
    graph export "${paper_outputs}/Figure13`panel'.png", name(tax_mar`m') replace
}

use "${work}/tax_estim_clean.dta", clear
keep if married==1 & ${edu}
sort syear hid gender
generate age_sp = .
by syear hid: replace age_sp = age[3-_n] if marital==1 & hhsize==2
generate hhage = (age+age_sp)/2 if married==1
keep if hhage>=25 & hhage<55
replace ind_lab_inc = 0 if ind_lab_inc<0 | ind_lab_inc>.
replace unemp_ben = 0 if unemp_ben<0 | unemp_ben>.
generate all_inc = ind_lab_inc+unemp_ben
cumul all_inc [aweight=hh_weight] if married==1 & empstatus==1, generate(inc_cdf)
generate inc_type = ceil(inc_cdf*5)
tabulate gender inc_type [aweight=hh_weight], row nofreq matcell(inc_dist)

file open spdist using "${tmp}/SOEP_spinc_dist.txt", write text replace
forvalues g_sp = 1/2 {
    local g = 3-`g_sp'
    forvalues l = 1/5 {
        scalar spfrac = inc_dist[`g_sp',`l'] / (inc_dist[`g_sp',1]+inc_dist[`g_sp',2]+inc_dist[`g_sp',3]+inc_dist[`g_sp',4]+inc_dist[`g_sp',5])
        local spstate = `l'+1
        file write spdist "`g' `spstate' " %21.0g (spfrac) _n
    }
}
file close spdist

_pctile all_inc [aweight=hh_weight] if married==1 & empstatus==1, percentiles(10 30 50 70 90)
file open spvec using "${tmp}/SOEP_spinc_vec.txt", write text replace
forvalues l = 1/5 {
    local spstate = `l'+1
    file write spvec "`spstate' " %21.0g (r(r`l')) _n
}
file close spvec
