version 18.0
clear
set more off

import delimited using "${raw}/arbeitsmarktregionen-2021.csv", ///
    delimiter(";") varnames(1) clear
rename kreise2021kennziffer kreise
rename kreise2021name kreise_name_2021
rename arbeitsmarktregionen2021kennziff amrnr_2021
rename arbeitsmarktregionen2021name amr_2021
isid kreise
merge 1:1 kreise using "${raw}/county_kaitz_crosswalk.dta", ///
    keepusing(kaitz_amr pop_kreise_2015)
keep if _merge == 3
drop _merge
isid kreise

sort amrnr_2021 kreise
generate kaitz_pop = kaitz_amr * pop_kreise_2015
by amrnr_2021: egen kaitz_pop_total = total(kaitz_pop)
by amrnr_2021: egen pop_total = total(pop_kreise_2015)
generate kaitz_amr_2021 = kaitz_pop_total / pop_total
drop kaitz_pop kaitz_pop_total pop_total
compress

keep amrnr_2021 amr_2021 kaitz_amr_2021
duplicates drop
rename amrnr_2021 amrnr
rename amr_2021 amr
rename kaitz_amr_2021 kaitz
isid amrnr
save "${tmp}/kaitz_amr_2021.dta", replace

import delimited using "${raw}/inkar_2021.csv", ///
    delimiter(";") varnames(1) encoding(utf8) clear
keep if bereich == "Absolutzahlen" | ///
    bereich == "Arbeitslosigkeit" | ///
    bereich == "Beschäftigung und Erwerbstätigkeit" | ///
    bereich == "Bevölkerung" | ///
    bereich == "Wirtschaft"
keep if raumbezug == "Arbeitsmarktregionen"

generate byte indicator = .
replace indicator = 1 if indikator == "Anteil Minijobs (Frauen)"
replace indicator = 2 if indikator == "Anteil Minijobs (ausschließlich) an den Beschäftigungsverhältnissen"
replace indicator = 3 if indikator == "Beschäftigtenquote"
replace indicator = 4 if indikator == "Beschäftigtenquote Frauen"
replace indicator = 5 if indikator == "Beschäftigtenquote Männer"
replace indicator = 6 if indikator == "Erwerbsfähige Bevölkerung (15 bis unter 65 Jahre)"
replace indicator = 7 if indikator == "Frauenanteil"
replace indicator = 8 if indikator == "Teilzeitbeschäftige Frauen"
replace indicator = 9 if indikator == "Teilzeitbeschäftigte"
drop if missing(indicator)

rename kennziffer amrnr
rename name amr
rename wert value
rename zeitbezug year
label variable year ""
replace value = subinstr(value, ",", ".", .)
destring amrnr year value, replace
keep amrnr amr year indicator value
isid amrnr year indicator
reshape wide value, i(amrnr year) j(indicator)

rename value1 me_share_female
rename value2 me_rate
rename value3 empr
rename value4 empr_female
rename value5 empr_male
rename value6 wapop
rename value7 pop_share_female
rename value8 ptr_female
rename value9 ptr

merge m:1 amrnr using "${tmp}/kaitz_amr_2021.dta"
drop _merge
isid amrnr year

rename wapop wapop_total
generate wapop_female = wapop_total * (pop_share_female / 100)
generate wapop_male = wapop_total * (1 - pop_share_female / 100)

generate emp_total = (empr / 100) * wapop_total
generate emp_male = (empr_male / 100) * wapop_male
generate emp_female = (empr_female / 100) * wapop_female

generate me_total = (me_rate / 100) * emp_total
generate me_female = me_total * me_share_female / 100
generate me_male = me_total - me_female

generate pt_total = (ptr / 100) * emp_total
generate pt_female = (ptr_female / 100) * emp_female
generate pt_male = pt_total - pt_female
replace pt_male = . if pt_male < 1

foreach group in total male female {
    generate ft_`group' = emp_`group' - pt_`group' - me_`group'
}

foreach var in me_total me_female me_male pt_total pt_female pt_male ft_total ft_female ft_male emp_total emp_female emp_male wapop_total wapop_male wapop_female {
    generate l`var' = log(`var')
}

sort amrnr year
compress
save "${tmp}/regional_analysis.dta", replace
