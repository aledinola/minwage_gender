version 18.0

import delimited using "${other}/minwage_minijob_policy.csv", clear varnames(1) stringcols(_all)
generate effective_day = daily(effective_date, "YMD")
format effective_day %tdCCYY-NN-DD
generate syear = year(effective_day)
destring minwage_nominal minijob_cap_nominal, replace

merge m:1 syear using "${work}/cpi.dta", keep(master match) nogen keepusing(cpi)
generate minwage_real_2015 = minwage_nominal * 100 / cpi
generate minijob_cap_real_2015 = minijob_cap_nominal * 100 / cpi
sort effective_day

twoway ///
    (connected minwage_nominal effective_day, yaxis(1) connect(J) ///
        msymbol(circle) msize(medlarge) mcolor(navy) lcolor(navy) lwidth(medthick)) ///
    (connected minijob_cap_nominal effective_day, yaxis(2) connect(J) ///
        msymbol(square) msize(medlarge) mcolor(maroon) lcolor(maroon) lwidth(medthick)), ///
    title("") ///
    xtitle("Calendar time", size(large)) ///
    ytitle("Minimum wage (EUR/hour)", axis(1) size(large)) ///
    ytitle("Minijob cap (EUR/month)", axis(2) size(large)) ///
    xscale(range(`=td(01jan2015)' `=td(01jan2025)')) ///
    xlabel(`=td(01jan2015)' `=td(01jan2016)' `=td(01jan2017)' `=td(01jan2018)' `=td(01jan2019)' `=td(01jan2020)' `=td(01jan2021)' `=td(01jan2022)' `=td(01jan2023)' `=td(01jan2024)' `=td(01jan2025)', format(%tdCCYY) angle(45) labsize(large)) ///
    ylabel(8(1)13, axis(1) angle(horizontal) labsize(large)) ///
    ylabel(440(20)560, axis(2) angle(horizontal) labsize(large)) ///
    legend(order(1 "Minimum wage" 2 "Minijob cap") rows(1) position(6) size(medlarge)) ///
    graphregion(color(white)) plotregion(color(white)) scale(1.15) ///
    name(Figure12a, replace)

graph export "${paper_outputs}/Figure12a.eps", name(Figure12a) replace
graph export "${paper_outputs}/Figure12a.png", name(Figure12a) replace width(2400)

keep if !missing(cpi)
sort effective_day

twoway ///
    (connected minwage_real_2015 effective_day, yaxis(1) connect(J) ///
        msymbol(circle) msize(medlarge) mcolor(navy) lcolor(navy) lwidth(medthick)) ///
    (connected minijob_cap_real_2015 effective_day, yaxis(2) connect(J) ///
        msymbol(square) msize(medlarge) mcolor(maroon) lcolor(maroon) lwidth(medthick)), ///
    title("") ///
    xtitle("Calendar time", size(large)) ///
    ytitle("Minimum wage (2015 EUR/hour)", axis(1) size(large)) ///
    ytitle("Minijob cap (2015 EUR/month)", axis(2) size(large)) ///
    xscale(range(`=td(01jan2015)' `=td(01jan2025)')) ///
    xlabel(`=td(01jan2015)' `=td(01jan2016)' `=td(01jan2017)' `=td(01jan2018)' `=td(01jan2019)' `=td(01jan2020)' `=td(01jan2021)' `=td(01jan2022)' `=td(01jan2023)' `=td(01jan2024)' `=td(01jan2025)', format(%tdCCYY) angle(45) labsize(large)) ///
    ylabel(8(1)13, axis(1) angle(horizontal) labsize(large)) ///
    ylabel(360(40)520, axis(2) angle(horizontal) labsize(large)) ///
    legend(order(1 "Minimum wage" 2 "Minijob cap") rows(1) position(6) size(medlarge)) ///
    graphregion(color(white)) plotregion(color(white)) scale(1.15) ///
    name(Figure12b, replace)

graph export "${paper_outputs}/Figure12b.eps", name(Figure12b) replace
graph export "${paper_outputs}/Figure12b.png", name(Figure12b) replace width(2400)

