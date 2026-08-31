version 18.0
clear all
set more off

args replication_root
if trim(`"`replication_root'"')=="" {
    display as error "Supply the absolute Replication_minwage_v2 path as the first argument."
    exit 198
}

global replication_root `"`replication_root'"'
global soep "${replication_root}/SOEP"
global code "${soep}/code"
global work "${soep}/work"
global other "${soep}/other"
global logs "${soep}/logs"
global tmp "${soep}/tmp"
global raw "${soep}/raw"
global soep_orig "${raw}"
global paper_outputs "${replication_root}/paper_outputs"

global age "age>=25 & age<55"
global civilian "pglfs!=5 & pgemplst!=6"
global noschool "pglfs!=3"
global nonretired "pglfs!=2"
global edu "(edu==1 | edu==2)"
global syear "syear>=2005"
global time "syear>=2013 & syear<=2014"
global time_post "syear==2016"

capture mkdir "${logs}"
capture mkdir "${tmp}"
capture mkdir "${raw}"

capture log close

foreach raw_file in ppathl.dta pgen.dta hbrutto.dta biobirth.dta artkalen.dta biocouplm.dta bdpequiv.dta bepequiv.dta {
    capture confirm file "${raw}/`raw_file'"
    if _rc {
        display as error "Missing restricted raw input: ${raw}/`raw_file'"
        display as error "See SOEP/README.md for the required SOEP-Core v36 and SOEP-CNEF files."
        exit 601
    }
}

log using "${logs}/replication.log", text replace name(runlog)
timer clear
timer on 1

foreach moment in SOEP_empdist_gc SOEP_hwage SOEP_lfstran SOEP_mar SOEP_child SOEP_spinc_dist SOEP_spinc_vec {
    capture erase "${tmp}/`moment'.txt"
}

display as text "Cleaning raw SOEP data and building annual/monthly panels"
do "${code}/1_crdata.do"

display as text "Building annual analysis data"
do "${code}/2_analysis.do"

display as text "Building annual and monthly moment data"
do "${code}/3_moments.do"

display as text "Building cleaned CNEF tax data"
do "${code}/4_tax.do"

foreach construction_file in demog_pid panel_annual_raw births_pid_t spelldata panel_monthly panel_annual tax_estim {
    capture erase "${work}/`construction_file'.dta"
}

display as text "Creating Table 1"
do "${code}/make_table1.do"

display as text "Creating Table 2"
do "${code}/make_table2.do"

display as text "Creating Figure 12"
do "${code}/make_figure12.do"

display as text "Creating Figure 16 and active annual/monthly moments"
do "${code}/make_figure16_and_moments.do"

display as text "Creating Figure 13 and active spousal-income moments"
do "${code}/make_figure13_and_spinc.do"

display as text "Removing transient moment files"
foreach moment in SOEP_empdist_gc SOEP_hwage SOEP_lfstran SOEP_mar SOEP_child SOEP_spinc_dist SOEP_spinc_vec {
    erase "${tmp}/`moment'.txt"
}

timer off 1
timer list 1
display as result "SOEP replication completed successfully."
log close runlog
