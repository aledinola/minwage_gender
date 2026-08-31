version 18.0

use "${work}/panel_annual_moments_temp.dta", clear
keep if empcs==0 & empse==0 & ${edu} & mar!=2

generate dwage_gross = pglabgro/365*12/cpi*100
generate hwage_gross = dwage_gross*7/hours_agreed
generate iphrf = round(phrf*100)
local euro = uchar(8364)

forvalues h = 1/3 {
    twoway ///
        histogram hwage_gross [fweight=iphrf] if hwage_gross<=40 & hours_type==`h' & ${time}, width(1) fcolor(none) lcolor(black) || ///
        histogram hwage_gross [fweight=iphrf] if hwage_gross<=40 & hours_type==`h' & ${time_post}, color(blue%30) width(1) ///
        name(hwage_hist_`h', replace) xline(8.5) ///
        legend(order(1 "Before" 2 "After") size(large) position(2) ring(0)) ///
        ytitle("Density (width = 1)", size(large)) ///
        xtitle("Hourly Wage (in 2015 `euro')", size(large)) ///
        xlabel(, labsize(large)) ylabel(, labsize(large))

    local panel = char(96+`h')
    graph export "${paper_outputs}/Figure16`panel'.eps", name(hwage_hist_`h') replace
    graph export "${paper_outputs}/Figure16`panel'.png", name(hwage_hist_`h') replace
}

capture program drop wpct_hw
program define wpct_hw, rclass
    _pctile hwage_gross [pweight=phrf], percentiles(5 10 25 50 75 90 95)
    scalar p5 = r(r1)
    scalar p10 = r(r2)
    scalar p25 = r(r3)
    scalar p50 = r(r4)
    scalar p75 = r(r5)
    scalar p90 = r(r6)
    scalar p95 = r(r7)
end

forvalues g = 1/2 {
    forvalues h = 1/3 {
        preserve
            keep if hours_type==`h' & gender==`g' & ${time}
            quietly bootstrap p5=p5 p10=p10 p25=p25 p50=p50 p75=p75 p90=p90 p95=p95, reps(500) seed(12345): wpct_hw
            matrix wage_g`g'_h`h' = r(table)
        restore
    }
}

file open hwage using "${tmp}/SOEP_hwage.txt", write text replace
local percentiles "5 10 25 50 75 90 95"
forvalues g = 1/2 {
    forvalues h = 1/3 {
        forvalues q = 1/7 {
            local pct : word `q' of `percentiles'
            file write hwage "`g' `h' `pct' " %21.0g (wage_g`g'_h`h'[1,`q']) " " %21.0g (wage_g`g'_h`h'[2,`q']) _n
        }
    }
}
file close hwage

use "${work}/panel_annual_moments_temp.dta", clear
keep if empcs==0 & empse==0 & ${edu} & mar!=2
tabulate hours_type, generate(dhours)

forvalues c = 1/3 {
    forvalues g = 1/2 {
        quietly mean dhours* if childstate_age6==`c' & gender==`g' & ${time} [aweight=phrf], vce(cluster pid)
        matrix emp_g`g'_c`c' = r(table)
    }
}

file open empdist using "${tmp}/SOEP_empdist_gc.txt", write text replace
forvalues c = 1/3 {
    forvalues g = 1/2 {
        forvalues h = 0/3 {
            local col = `h'+1
            file write empdist "`g' `c' `h' " %21.0g (emp_g`g'_c`c'[1,`col']) " " %21.0g (emp_g`g'_c`c'[2,`col']) _n
        }
    }
}
file close empdist

quietly tabulate mar [aweight=phrf] if ${time}, matcell(mar_counts)
scalar frac_mar = mar_counts[1,1]/(mar_counts[1,1]+mar_counts[2,1])
file open marriage using "${tmp}/SOEP_mar.txt", write text replace
file write marriage "married " %21.0g (frac_mar) _n
file close marriage

sort pid syear
by pid: generate childstate_age6_t1 = childstate_age6[_n+1] if syear+1==syear[_n+1]
quietly tabulate childstate_age6 childstate_age6_t1 [aweight=phrf] if mar==1 & childstate_age6_t1>=childstate_age6 & ${time}, row matcell(child_m)
forvalues g = 1/2 {
    quietly tabulate childstate_age6 childstate_age6_t1 [aweight=phrf] if mar==3 & gender==`g' & childstate_age6_t1>=childstate_age6 & ${time}, row matcell(child_s`g')
}

file open child using "${tmp}/SOEP_child.txt", write text replace
scalar child12 = child_m[1,2]/(child_m[1,1]+child_m[1,2])
scalar child23 = child_m[2,3]/(child_m[2,2]+child_m[2,3])
file write child "married " %21.0g (child12) " " %21.0g (child23) _n
forvalues g = 1/2 {
    scalar child12 = child_s`g'[1,2]/(child_s`g'[1,1]+child_s`g'[1,2])
    scalar child23 = child_s`g'[2,3]/(child_s`g'[2,2]+child_s`g'[2,3])
    file write child "single_gender`g' " %21.0g (child12) " " %21.0g (child23) _n
}
file close child

use "${work}/panel_monthly_mom.dta", clear
tsset pid tmonth
sort pid tmonth
generate hours_type_t0 = L.hours_type
tabulate hours_type, generate(dhours)

file open lfstran using "${tmp}/SOEP_lfstran.txt", write text replace
forvalues l0 = 0/3 {
    quietly mean dhours* if hours_type_t0==`l0' & ${time} [aweight=phrf], vce(cluster pid)
    matrix transitions = r(table)
    forvalues l1 = 0/3 {
        local col = `l1'+1
        file write lfstran "`l0' `l1' " %21.0g (transitions[1,`col']) " " %21.0g (transitions[2,`col']) _n
    }
}
file close lfstran

