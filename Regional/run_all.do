version 18.0
clear all
set more off

args replication_root
if trim(`"`replication_root'"')=="" {
    display as error "Supply the absolute Replication_minwage_v2 path as the first argument."
    exit 198
}

global replication_root `"`replication_root'"'
global regional "${replication_root}/Regional"
global code "${regional}/code"
global raw "${regional}/raw"
global tmp "${regional}/tmp"
global logs "${regional}/logs"
global paper_outputs "${replication_root}/paper_outputs"

capture mkdir "${raw}"
capture mkdir "${tmp}"
capture mkdir "${logs}"

foreach raw_file in county_kaitz_crosswalk.dta arbeitsmarktregionen-2021.csv inkar_2021.csv {
    capture confirm file "${raw}/`raw_file'"
    if _rc {
        display as error "Missing Regional raw input: ${raw}/`raw_file'"
        display as error "See Regional/README.md for input requirements."
        exit 601
    }
}

capture which esttab
if _rc {
    display as text "Installing the required estout package from SSC"
    ssc install estout
}
capture which estadd
if _rc {
    display as error "The estout installation did not provide estadd."
    exit 499
}

capture which combomarginsplot
if _rc {
    display as text "Installing the required combomarginsplot package from SSC"
    ssc install combomarginsplot
}

capture log close _all
log using "${logs}/replication.log", text replace name(runlog)
timer clear
timer on 1

display as text "Constructing the Regional analysis dataset from raw inputs"
capture noisily do "${code}/1_build_regional_data.do"
local build_rc = _rc
if `build_rc' {
    display as error "Regional data construction failed with return code `build_rc'."
    log close runlog
    exit `build_rc'
}

display as text "Creating Table 8 and Figure 1"
capture noisily do "${code}/2_make_outputs.do"
local output_rc = _rc
if `output_rc' {
    display as error "Regional output generation failed with return code `output_rc'."
    log close runlog
    exit `output_rc'
}

foreach transient in kaitz_amr_2021 regional_analysis lme_r_male lme_r_female lpt_r_male lpt_r_female lft_r_male lft_r_female {
    capture erase "${tmp}/`transient'.dta"
}

timer off 1
timer list 1
display as result "Regional replication completed successfully."
log close runlog
