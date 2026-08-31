version 18.0

* Build the annual panel, monthly panel, and CPI data used downstream.
*************************************************************************
*	1. Time invariant variables - output file: "demog_pid.dta"
*************************************************************************

* Time invariant variables: sex, birthmonth
use sex gebjahr gebmonat pid hid syear using "${soep_orig}/ppathl.dta", clear
* Change label language
label language EN
sort pid syear

* some birth month is missing for some, set it to 7
replace gebjahr =. if gebjahr<1800
replace gebmonat=. if gebjahr==.
replace gebmonat =7 if (gebmonat<1 | gebmonat>12)

by pid: keep if _n==1
drop syear
* create single variable to indicate birth year-month
gen birth_ym=ym(gebjahr,gebmonat)
drop gebjahr gebmonat
compress
save "${work}/demog_pid.dta", replace


*************************************************************************
*	2. Time varying variables from panel data - output file: panel_annual.dta
*************************************************************************


* Variables from pgen.dta
use pid hid syear pgpartnr pgfamstd pgmonth pgbilzeit pglfs pgemplst ///
 pglabgro pglabnet pgsndjob pgnace pgegp88 pgbetr pgtatzeit pgvebzeit pgstib ///
 using "${soep_orig}/pgen.dta", clear
save "${work}/panel_annual_raw.dta", replace

use "${work}/panel_annual_raw.dta", clear
* gen variable for partner's income
use pid syear pglabgro pglabnet pgemplst pglfs pgstib using "${work}/panel_annual_raw.dta", clear
ren pid pgpartnr
ren pglabgro pglabgro_partner
ren pglabnet pglabnet_partner
ren pgemplst pgemplst_partner
ren pglfs pglfs_partner
ren pgstib pgstib_partner

merge 1:m pgpartnr syear using "${work}/panel_annual_raw.dta"
drop if _merge==1
drop _merge 
save "${work}/panel_annual_raw.dta", replace

* merge with bula (federal land)
* bula is only available in hbrutto, which contains the identifier hid (no pid)
use hid syear bula_v2 using "${soep_orig}/hbrutto.dta", clear
ren bula_v2 bula
merge 1:m hid syear using "${work}/panel_annual_raw.dta"
drop if _merge==1
drop _merge
save "${work}/panel_annual_raw.dta", replace

* merge with sample region and weight
use pid syear sampreg pbleib phrf using "${soep_orig}/ppathl.dta", clear
merge 1:1 pid syear using "${work}/panel_annual_raw.dta"
* About 1/3 observations in ppathl do not appear in pgen. Drop these
drop if _merge != 3
drop _merge
save "${work}/panel_annual_raw.dta", replace

*************************************************************************
*	3. Time varying variables from cs data - output file: births_pid_t.dta
*************************************************************************

* Generate panel data: each row is a pid-month indicating number of new births
use "${soep_orig}/biobirth.dta", clear
capture drop pid
rename persnr pid
keep pid kidgeb* kidmon* 
reshape long kidgeb kidmon,i(pid) j(kidnr) string
* Half of the kids with known kidgeb (birthyear of kid) have missing kidmon (birth month of kid). 
* If kidgeb (birthyear of kid) is known but kidmon (birth month of kid) is unknown, assume birth month is Jan (1)
replace kidmon=1 if kidgeb>1850 & kidgeb<. & kidmon<0
* drop if kidgeb and kidmon are both missing
drop if kidgeb<0 & kidmon<0
* gen t_month for calendar month
gen tmonth=ym(kidgeb,kidmon)
sort pid tmonth
* accounting for multiple births 
gen birth=1
by pid tmonth: egen N_births=total(birth)
by pid tmonth: keep if _n==1
keep pid tmonth N_births
format %tm tmonth
compress
save "${work}/births_pid_t.dta", replace


*************************************************************************
*	4. Time varying variables from spell data - output file: 
*************************************************************************

* Define programs in the Censor_Programs do file. 
run "${code}/Censor_Programs.do" 


*	4.1 Employment data: artkalen.dta
* Change spell data to monthly panel data 
 
use "${soep_orig}/artkalen.dta", clear
 
* persnr and pid are identical, drop persnr
drop persnr cid hhnr

* change spell data to monthly panel data
sort pid begin end spelltyp 
 gen keep_spells = 0
 * Values of Spelltyp to keep
global spells 		"1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 99"		

 
foreach val of global spells {
	replace keep_spells = 1 if spelltyp == `val'
}
keep if keep_spells == 1
 
censor_unfold zensor
 
* all endings of spells must be equal to the beginning of the following
bysort pid (begin end): replace end = end + 1 if end > 0 

gen duration = end - begin 
*if end or begin of a spell is unknown or missing, the duration of a spell is set as zero month and the spell will not be expanded
replace duration = 0 if end < 0 | begin < 0
expand duration
bysort pid spellnr (begin end): replace begin = begin[_n-1] + 1 if begin[_n-1] != .
 
replace end = begin + 1

drop duration keep_spells end
order pid begin spellnr spelltyp
compress

* Adjusting censor variables
bysort pid spellnr (begin spellnr): replace right = 0 if _n != _N & _N > 1
bysort pid spellnr (begin spellnr): replace left = 0 if _n > 1 & _N > 1
sort pid begin spellnr spelltyp

censor_contract org_censor zensor
drop org_censor 

* Rename variables
ren spelltyp spelltyp_lfs
ren spellnr spellnr_lfs
ren zensor censor_lfs
* gen month variable tmonth (jan 1960 as base month)
gen tmonth = begin + 23*12 -1
format tmonth %tm
drop begin

* Dealing with multiple activities 
 
sort pid tmonth spelltyp_lfs spellnr_lfs

* Keep the top two activities (ranked according to spelltyp) 
by pid tmonth: drop if _n>2
* Reshape so that each row is a pid-month
by pid tmonth: gen actnr = _n
reshape wide spelltyp_lfs spellnr_lfs censor_lfs, i(pid tmonth) j(actnr) 
compress


save "${work}/spelldata.dta", replace



*	4.2 relationship data: biocouplm.dta

* Define programs in the Censor_Programs do file. 
run "${code}/Censor_Programs.do" 


use "${soep_orig}/biocouplm.dta", clear
keep pid begin end spelltyp spellnr censor 

* change spell data to monthly panel data
sort pid begin end spelltyp 
 gen keep_spells = 0
  * Values of Spelltyp to keep
global spells 		"1 2 3 4 5 6 7 8 98 99"		

foreach val of global spells {
	replace keep_spells = 1 if spelltyp == `val'
}
keep if keep_spells == 1
 
censor_unfold censor
 
* all endings of spells must be equal to the beginning of the following
bysort pid (begin end): replace end = end + 1 if end > 0 

gen duration = end - begin 
*if end or begin of a spell is unknown or missing, the duration of a spell is set as zero month and the spell will not be expanded
replace duration = 0 if end < 0 | begin < 0
expand duration
bysort pid spellnr (begin end): replace begin = begin[_n-1] + 1 if begin[_n-1] != .
 
replace end = begin + 1

drop duration keep_spells end
order pid begin spellnr spelltyp
compress

* Adjusting censor variables
bysort pid spellnr (begin spellnr): replace right = 0 if _n != _N & _N > 1
bysort pid spellnr (begin spellnr): replace left = 0 if _n > 1 & _N > 1
sort pid begin spellnr spelltyp

censor_contract org_censor censor
drop org_censor 
 
 
* Rename variables 
ren spelltyp spelltyp_relation
ren spellnr spellnr_relation
ren censor censor_relation

* gen month variable tmonth (jan 1960 as base month)
gen tmonth = begin + 23*12 -1
format tmonth %tm
drop begin

* Dealing with multiple relations 
 
sort pid tmonth spelltyp_relation spellnr_relation

* Keep the top one relationship status (ranked according to spelltyp) 
by pid tmonth: drop if _n>1
compress

* merge with spelldata.dta
merge 1:1 pid tmonth using "${work}/spelldata.dta"
drop _merge 
compress

* redefine variable labels for spell types
capture label drop spelltyp
capture label drop spelltyp_lfs
capture label drop spelltyp_relation

label define spelltyp_lfs 1 "[1] Full-Time Employment" ///
	2 "[2] Short Work Hrs" 3 "[3] Part-Time Employment / Marginally Employed" ///
	4 "[4] Vocational Training" 5 "[5] Registered Unemployment" ///
	6 "[6] Retired" 7 "[7] Maternity Leave" 8 "[8] School, College" ///
	9 "[9] Military, Community Service" 10 "[10] Housewife, Husband" ///
	11 "[11] Second Job" 12 "[12] Other" 13 "[13] First Job Training, Apprenticeship" ///
	14 "[14] Continuing Education, Retraining" 15 "[15] Minijob (up to 400 Euro)" ///
	99 "[99] Gap"
label define spelltyp_relation 1 "[1] married, spouse in household" ///
	2 "[2] married, spouse not in household" 3 "[3] coupled, partner in household" ///
	4 "[4] coupled, partner not in household" 5 "[5] single" ///
	6 "[6] married / reg. same-sex p., separated" 7 "[7] reg. same-sex p.,partner in household" ///
	8 "[8] reg. same-sex p.,partner not in household" ///
	98 "[98] unknown" 	99 "[99] unit nonresponse"	
label values spelltyp_lfs* spelltyp_lfs
label values spelltyp_relation spelltyp_relation
save "${work}/spelldata.dta", replace

*************************************************************************
*	5. Combining everything in a monthly and an annual panel. 
*		Output files: ${work}/panel_monthly.dta, ${work}/panel_annual.dta
*************************************************************************
* 5.1 Combine spelldata.dta with demog_pid.dta
use "${work}/demog_pid.dta", clear
merge 1:m pid using "${work}/spelldata.dta"
drop if _merge==1
drop _merge
* calculate age (in years)
gen age =floor((tmonth-birth_ym+1)/12)
compress
drop birth_ym 
save "${work}/panel_monthly.dta", replace


* 5.2 Enrich panel_annual.dta with info from demog_pid.dta and spelldata.dta
* 5.2.1 panel_annual, demog_pid, and hourly wage
use "${work}/demog_pid.dta", clear
merge 1:m pid using "${work}/panel_annual_raw.dta"
drop if _merge==1
drop _merge
* calculate age (in years)
gen tmonth = ym(syear, pgmonth)
gen age =floor((tmonth-birth_ym+1)/12)

drop birth_ym 


* save 
compress
save "${work}/panel_annual.dta", replace



* 5.2.3  spelldata.dta and births_pid_t.dta
use pid tmonth spelltyp_lfs1 using "${work}/spelldata.dta", clear
merge 1:1 pid tmonth using "${work}/panel_annual.dta"
ren _merge _merge_spell
merge 1:1 pid tmonth using "${work}/births_pid_t.dta"
drop _merge

* Calculate cumulative number of kids
sort pid tmonth
replace N_births=0 if N_births==.
* - All kids
capture drop N_kids_all
by pid: gen N_kids_all = sum(N_births)

* - Kids under age 6, 12, 18
* birth month (calendar month) of each kid, up to four kids)
forvalues n= 1/12 {
	capture drop bmonth_kid`n'
	gen bmonth_kid`n' = .
}

sort pid N_kids_all tmonth
* Birth month of first kid
by pid N_kids_all: replace bmonth_kid1=tmonth if _n==1 & N_kids_all>=1 & bmonth_kid1==.
capture drop bmonth
by pid: egen bmonth = min(bmonth_kid1)
replace bmonth_kid1 = bmonth
* birth months of subsequent kids
forvalues n=2/12 {
	local l = `n'-1
	by pid N_kids_all: replace bmonth_kid`n'=tmonth if _n==1 & N_kids_all>=`n' & bmonth_kid`l'<. & bmonth_kid`n'==.
	capture drop bmonth
	by pid: egen bmonth = min(bmonth_kid`n')
	replace bmonth_kid`n' = bmonth
}
drop bmonth
* age of each kid (if alive)
forvalues n=1/12 {
	capture drop mage_kid`n'
	gen mage_kid`n' = tmonth-bmonth_kid`n'+1 if tmonth-bmonth_kid`n'>=0
	foreach val in 3 6 12 {
		capture drop under`val'_kid`n'
		gen under`val'_kid`n' = 0
		replace under`val'_kid`n' = 1 if mage_kid`n' <=`val'*12
	}
}
* total number of kids under 6 and 12
foreach val in 3 6 12 {
		capture drop N_kids_under`val'
		egen N_kids_under`val' = rowtotal(under`val'_kid*)
}
drop under*kid* mage_kid* bmonth_kid* N_births 
keep if _merge_spell==2 | _merge_spell==3
drop _merge_spell
compress


compress
save "${work}/panel_annual.dta", replace



*************************************************************************
*	6. CPI (output: $work/cpi.dta)
*************************************************************************
import delimited using "${other}/cpi.csv", delimiter(";") clear
ren v1 syear
ren v2 cpi
ren v3 cpi_ch
* Make 2015 = 100
quietly sum cpi if syear == 2015
local cpi_2015 = r(mean)
replace cpi = cpi/`cpi_2015'*100
* labels
label var cpi "CPI, 2015=100"
label var cpi_ch "Annual change in cpi"
* save file
compress 
save "${work}/cpi.dta", replace
