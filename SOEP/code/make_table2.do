version 18.0

use "${work}/panel_annual_analysis.dta", clear
keep if ${age} & ${civilian} & ${noschool} & ${nonretired} & ${edu} & ${syear} & mar!=2
xtset pid syear

forvalues g = 0/1 {
    quietly mlogit lfs4 age agesq ib3.mar_spinc_sim N_kids_under6 N_kids_all i.edu i.syear i.bula [pweight=phrf] if female==`g', b(4) vce(cluster pid)
    scalar table2_N_`g' = e(N)
    scalar table2_r2_`g' = e(r2_p)
    preserve
        keep if e(sample)
        bysort pid: keep if _n==1
        count
        scalar table2_clusters_`g' = r(N)
    restore

    quietly margins, dydx(ib3.mar_spinc_sim N_kids_under6 N_kids_all i.edu) post
    matrix table2_b = e(b)
    matrix table2_V = e(V)

    local positions "1 5 13 17 25"
    forvalues r = 1/5 {
        local start : word `r' of `positions'
        forvalues o = 1/4 {
            local p = `start' + `o' - 1
            scalar table2_bval = table2_b[1,`p']
            scalar table2_seval = sqrt(table2_V[`p',`p'])
            scalar table2_pval = 2*normal(-abs(table2_bval/table2_seval))

            local table2_b_`g'_`r'_`o' : display %9.3f table2_bval
            local table2_se_`g'_`r'_`o' : display %9.3f table2_seval
            local table2_b_`g'_`r'_`o' = trim("`table2_b_`g'_`r'_`o''")
            local table2_se_`g'_`r'_`o' = trim("`table2_se_`g'_`r'_`o''")

            local table2_star_`g'_`r'_`o' ""
            if table2_pval < 0.10 local table2_star_`g'_`r'_`o' "*"
            if table2_pval < 0.05 local table2_star_`g'_`r'_`o' "**"
            if table2_pval < 0.01 local table2_star_`g'_`r'_`o' "***"
        }
    }
}

forvalues g = 0/1 {
    local table2_Ntxt_`g' : display %12.0fc table2_N_`g'
    local table2_Ctxt_`g' : display %12.0fc table2_clusters_`g'
    local table2_Rtxt_`g' : display %5.3f table2_r2_`g'
    local table2_Ntxt_`g' = trim("`table2_Ntxt_`g''")
    local table2_Ctxt_`g' = trim("`table2_Ctxt_`g''")
    local table2_Rtxt_`g' = trim("`table2_Rtxt_`g''")
}

local bs = char(92)
local br "`bs'`bs'"
local dlr = char(36)

file open table2 using "${paper_outputs}/Table2.tex", write text replace
file write table2 "`bs'begin{center}" _n
forvalues g = 0/1 {
    if `g'==0 local panel "A. Male"
    if `g'==1 local panel "B. Female"

    file write table2 "`bs'scalebox{0.9}{%" _n
    file write table2 "`bs'begin{tabular}{lcccc}" _n
    file write table2 "`bs'multicolumn{5}{c}{`bs'textbf{`panel'}} `br'" _n
    file write table2 "`bs'hline`bs'hline" _n
    file write table2 "& FT        & PT         & ME          & NE        `br'" _n
    file write table2 "`bs'hline" _n
    file write table2 "`bs'multicolumn{5}{l}{`bs'textit{Marital / Spousal Income (ref: Single)}} `br'" _n

    forvalues r = 1/5 {
        if `r'==1 local rowlabel "Married, high-earning spouse"
        if `r'==2 local rowlabel "Married, low-earning spouse"
        if `r'==3 local rowlabel "No. children under 6"
        if `r'==4 local rowlabel "No. children"
        if `r'==5 local rowlabel "10-12 years"

        if `r'==3 file write table2 _n "`bs'multicolumn{5}{l}{`bs'textit{Children}} `br'" _n
        if `r'==5 file write table2 _n "`bs'multicolumn{5}{l}{`bs'textit{Education (ref: `dlr'`bs'leq`dlr'9 years)}} `br'" _n

        file write table2 "`rowlabel'" _n
        forvalues o = 1/4 {
            local cell "`table2_b_`g'_`r'_`o''"
            local stars "`table2_star_`g'_`r'_`o''"
            if "`stars'"!="" local cell "`cell'`bs'sym{`stars'}"
            file write table2 "& `cell' "
        }
        file write table2 "`br'" _n
        forvalues o = 1/4 {
            file write table2 "& (`table2_se_`g'_`r'_`o'')  "
        }
        if `r'==4 file write table2 "`br'[1em]" _n
        else file write table2 "`br'" _n
    }

    file write table2 "`bs'hline" _n
    file write table2 "Observations        & `bs'multicolumn{4}{c}{`table2_Ntxt_`g''} `br'" _n
    file write table2 "Clusters        & `bs'multicolumn{4}{c}{`table2_Ctxt_`g''} `br'" _n
    file write table2 "Pseudo " (char(36)) "R^{2}" (char(36)) "      & `bs'multicolumn{4}{c}{`table2_Rtxt_`g''} `br'" _n
    file write table2 "`bs'hline`bs'hline" _n
    file write table2 "`bs'multicolumn{5}{l}{`bs'footnotesize Standard errors in parentheses} `br'" _n
    file write table2 "`bs'multicolumn{5}{l}{`bs'footnotesize `bs'sym{*} `dlr'p<0.10`dlr', `bs'sym{**} `dlr'p<0.05`dlr', `bs'sym{***} `dlr'p<0.01`dlr'} `br'" _n
    file write table2 "`bs'end{tabular}}" _n
    if `g'==0 file write table2 _n "`bs'vspace{0.5cm}" _n _n
}
file write table2 "`bs'end{center}" _n
file close table2
