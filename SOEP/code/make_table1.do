version 18.0

use "${work}/panel_annual_analysis.dta", clear
keep if ${age} & ${civilian} & ${noschool} & ${nonretired}

tabulate female lfs4 [aweight=phrf] if syear>=2006, row matcell(table1_counts)

forvalues g = 1/2 {
    scalar table1_total = 0
    forvalues h = 1/4 {
        scalar table1_total = table1_total + table1_counts[`g',`h']
    }
    forvalues h = 1/4 {
        scalar table1_pct = 100 * table1_counts[`g',`h'] / table1_total
        local displayed_pct : display %9.2f table1_pct
        scalar table1_share = real(trim("`displayed_pct'")) / 100
        local table1_`g'_`h' : display %6.3f (table1_share+1e-10)
        local table1_`g'_`h' = trim("`table1_`g'_`h''")
    }
}

local bs = char(92)
local br "`bs'`bs'"
file open table1 using "${paper_outputs}/Table1.tex", write text replace
file write table1 "`bs'begin{tabular}{lcc}" _n
file write table1 "`bs'hline " _n
file write table1 " & {Men} & {Women}`bs'tabularnewline" _n
file write table1 "`bs'hline " _n
file write table1 "Full-time & `table1_1_1' & `table1_2_1' `bs'tabularnewline" _n
file write table1 "Part-time & `table1_1_2' &  `table1_2_2' `bs'tabularnewline" _n
file write table1 "Marginal emp. & `table1_1_3' &  `table1_2_3'`bs'tabularnewline" _n
file write table1 "Non-emp. & `table1_1_4' &  `table1_2_4' `bs'tabularnewline" _n
file write table1 "`bs'hline " _n
file write table1 "`bs'end{tabular}" _n
file close table1
