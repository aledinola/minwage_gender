version 18.0
clear
set more off

use "${tmp}/regional_analysis.dta", clear
sort amrnr year
xtset amrnr year
replace kaitz = kaitz / 100

eststo clear
foreach outcome in me pt ft {
    foreach group in male female {
        xtreg l`outcome'_`group' ib2014.year##c.kaitz lwapop_`group' ///
            if inrange(year, 2012, 2019), fe vce(cluster amrnr) cformat(%9.0g)
        estadd scalar r2_within = e(r2_w)
        estadd scalar r2_between = e(r2_b)
        estadd scalar r2_overall = e(r2_o)
        scalar clusters = round(e(N_clust))
        estadd scalar clusters
        eststo `outcome'_`group'_r
        margins, dydx(kaitz) at(year=(2012 2013 2014 2015 2016 2017 2018 2019)) ///
            saving("${tmp}/l`outcome'_r_`group'.dta", replace)
    }
}

esttab me_male_r me_female_r pt_male_r pt_female_r ft_male_r ft_female_r ///
    using "${paper_outputs}/Table8.tex", replace label ///
    star(* 0.10 ** 0.05 *** 0.01) b(%9.3f) se(%9.3f) ///
    keep(*.kaitz lwapop_*) drop(2014.year#c.kaitz) ///
    order(*.kaitz lwapop_*) ///
    mtitles("ME Male" "ME Female" "PT Male" "PT Female" "FT Male" "FT Female") ///
    stats(N clusters r2_within r2_between r2_overall, ///
        labels("Observations" "Clusters" "R$^2$ (within)" "R$^2$ (between)" "R$^2$ (overall)") ///
        fmt(0 0 3 3 3))

local men_ME = -0.112
local men_PT = -0.060
local men_FT = 0.011
local women_ME = -0.059
local women_PT = 0.027
local women_FT = 0.009

cd "${tmp}"

combomarginsplot lme_r_male, xline(2014.5) xdim(year) ///
    yline(0, lcolor(gs8) lwidth(thin) lpattern(solid)) ///
    yline(`men_ME', lcolor(red) lwidth(thin) lpattern(solid)) ///
    text(`men_ME' 2018 "Model", place(nw) color(red)) ///
    name(Figure1a, replace) ytitle("Effect of Kaitz on log employment") ///
    graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
    plot1opts(lwidth(medthick)) ci1opts(lwidth(medthick)) title("") ///
    scale(1.5) yticks(-0.3(0.1)0.2) ylabels(-0.3(0.1)0.2)
graph export "${paper_outputs}/Figure1a.eps", name(Figure1a) replace
graph export "${paper_outputs}/Figure1a.png", name(Figure1a) width(1061) height(636) replace

combomarginsplot lme_r_female, xline(2014.5) xdim(year) ///
    yline(0, lcolor(gs8) lwidth(thin) lpattern(solid)) ///
    yline(`women_ME', lcolor(red) lwidth(thin) lpattern(solid)) ///
    text(`women_ME' 2018 "Model", place(nw) color(red)) ///
    name(Figure1b, replace) ytitle("Effect of Kaitz on log employment") ///
    graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
    plot1opts(lwidth(medthick)) ci1opts(lwidth(medthick)) title("") ///
    scale(1.5) yticks(-0.3(0.1)0.2) ylabels(-0.3(0.1)0.2)
graph export "${paper_outputs}/Figure1b.eps", name(Figure1b) replace
graph export "${paper_outputs}/Figure1b.png", name(Figure1b) width(1061) height(636) replace

combomarginsplot lpt_r_male, xline(2014.5) xdim(year) ///
    yline(0, lcolor(gs8) lwidth(thin) lpattern(solid)) ///
    yline(`men_PT', lcolor(red) lwidth(thin) lpattern(solid)) ///
    text(`men_PT' 2018 "Model", place(nw) color(red)) ///
    name(Figure1c, replace) ytitle("Effect of Kaitz on log employment") ///
    graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
    plot1opts(lwidth(medthick)) ci1opts(lwidth(medthick)) title("") ///
    scale(1.5) yticks(-1(0.5)0.5) ylabels(-1(0.5)0.5)
graph export "${paper_outputs}/Figure1c.eps", name(Figure1c) replace
graph export "${paper_outputs}/Figure1c.png", name(Figure1c) width(1061) height(636) replace

combomarginsplot lpt_r_female, xline(2014.5) xdim(year) ///
    yline(0, lcolor(gs8) lwidth(thin) lpattern(solid)) ///
    yline(`women_PT', lcolor(red) lwidth(thin) lpattern(solid)) ///
    text(`women_PT' 2018 "Model", place(nw) color(red)) ///
    name(Figure1d, replace) ytitle("Effect of Kaitz on log employment") ///
    graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
    plot1opts(lwidth(medthick)) ci1opts(lwidth(medthick)) title("") ///
    scale(1.5) yticks(-1(0.5)0.5) ylabels(-1(0.5)0.5)
graph export "${paper_outputs}/Figure1d.eps", name(Figure1d) replace
graph export "${paper_outputs}/Figure1d.png", name(Figure1d) width(1061) height(636) replace

combomarginsplot lft_r_male, xline(2014.5) xdim(year) ///
    yline(0, lcolor(gs8) lwidth(thin) lpattern(solid)) ///
    yline(`men_FT', lcolor(red) lwidth(thin) lpattern(solid)) ///
    text(`men_FT' 2018 "Model", place(nw) color(red)) ///
    name(Figure1e, replace) ytitle("Effect of Kaitz on log employment") ///
    graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
    plot1opts(lwidth(medthick)) ci1opts(lwidth(medthick)) title("") ///
    scale(1.5) yticks(-0.6(0.2)0.2) ylabels(-0.6(0.2)0.2)
graph export "${paper_outputs}/Figure1e.eps", name(Figure1e) replace
graph export "${paper_outputs}/Figure1e.png", name(Figure1e) width(1061) height(636) replace

combomarginsplot lft_r_female, xline(2014.5) xdim(year) ///
    yline(0, lcolor(gs8) lwidth(thin) lpattern(solid)) ///
    yline(`women_FT', lcolor(red) lwidth(thin) lpattern(solid)) ///
    text(`women_FT' 2018 "Model", place(nw) color(red)) ///
    name(Figure1f, replace) ytitle("Effect of Kaitz on log employment") ///
    graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
    plot1opts(lwidth(medthick)) ci1opts(lwidth(medthick)) title("") ///
    scale(1.5) yticks(-0.6(0.2)0.2) ylabels(-0.6(0.2)0.2)
graph export "${paper_outputs}/Figure1f.eps", name(Figure1f) replace
graph export "${paper_outputs}/Figure1f.png", name(Figure1f) width(1061) height(636) replace

graph close _all
