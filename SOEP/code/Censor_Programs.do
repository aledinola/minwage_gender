version 18.0

* Censor helpers required for the ARTKALEN and BIOCOUPLM spell files.
capture program drop censor_unfold
program define censor_unfold

local censor "`1'"
capture confirm variable `censor'

if _rc == 0 {
	qui label list `censor'
	global soep_dataset = cond(r(max)==14,"biocoupl",cond(r(max)==9,"artkalen","wrong"))
	if "$soep_dataset" == "wrong" {
		di in red "Coding of censor variable not identified for BIOCOUPLM or ARTKALEN"
		exit
	}
	
	*********************************************
	***BIOMARSM/Y / BIOCOUPLM/Y***
	*********************************************
	if "$soep_dataset" == "biocoupl" {
		quietly: gen left = .
		quietly: gen right = .
		quietly: replace left = 0 if inlist(`censor',0,3,4,5,6)
		quietly: replace left = 1 if inlist(`censor',1,7,8,9,10)
		quietly: replace left = 2 if inlist(`censor',2,11,12,13,14)
		quietly: replace left = -1 if `censor' < 0 | `censor' == 98 | `censor' == 99
		quietly: replace right = 0 if inlist(`censor',0,1,2)
		quietly: replace right = 1 if inlist(`censor',3,7,11)
		quietly: replace right = 2 if inlist(`censor',4,8,12)
		quietly: replace right = 3 if inlist(`censor',5,9,13)
		quietly: replace right = 4 if inlist(`censor',6,10,14)
		quietly: replace right = -1 if `censor' <  0 | `censor' == 98 | `censor' == 99
		clonevar org_censor = `censor'
		quietly: drop `censor'
	}
	
	
	*********************************************
	*ARTKALEN/PBIOSPE/SOZKALEN*
	*********************************************
	if "$soep_dataset" == "artkalen" {
		quietly: gen left = .
		quietly: replace left = 0 if inlist(`censor',1,2,3)
		quietly: replace left = 1 if inlist(`censor',4,5,6)
		quietly: replace left = 2 if inlist(`censor',7,8,9)
		quietly: replace left = -1 if `censor' <= 0
		quietly: gen right = .
		quietly: replace right = 0 if inlist(`censor',1,4,7)
		quietly: replace right = 1 if inlist(`censor',2,5,8)
		quietly: replace right = 2 if inlist(`censor',3,6,9)
		quietly: replace right = -1 if `censor' <= 0
		clonevar org_censor = `censor'
		quietly: drop `censor'
	}
	
}
else {
	di in red "Censor variable `censor' not found"
}
end


 
capture program drop censor_contract
program define censor_contract

local org_censor = "`1'"
local censor = "`2'"
capture confirm variable left right
if _rc == 0 {

	global censorlab: variable label `org_censor'
	global censorvlab : value label `org_censor'

	qui label list $censorvlab
	global soep_dataset = cond(r(max)==14,"biocoupl",cond(r(max)==9,"artkalen","wrong"))
	if "$soep_dataset" == "wrong" {
		di in red "Coding of `org_censor' variable not identified for BIOCOUPLM or ARTKALEN"
		exit
	}

	*********************************************
	***BIOMARSM/Y / BIOCOUPLM/Y***
	*********************************************
	
	if "$soep_dataset" == "biocoupl" {
		quietly: gen `censor' = .
		quietly: replace `censor' = -2 if left == -1 | right == -1
		quietly: replace `censor' = 0 if left == 0 & right == 0
		quietly: replace `censor' = 1 if left == 1 & right == 0
		quietly: replace `censor' = 2 if left == 2 & right == 0
		quietly: replace `censor' = 3 if left == 0 & right == 1
		quietly: replace `censor' = 4 if left == 0 & right == 2
		quietly: replace `censor' = 5 if left == 0 & right == 3
		quietly: replace `censor' = 6 if left == 0 & right == 4
		quietly: replace `censor' = 7 if left == 1 & right == 1
		quietly: replace `censor' = 8 if left == 1 & right == 2
		quietly: replace `censor' = 9 if left == 1 & right == 3
		quietly: replace `censor' = 10 if left == 1 & right == 4
		quietly: replace `censor' = 11 if left == 2 & right == 1
		quietly: replace `censor' = 12 if left == 2 & right == 2
		quietly: replace `censor' = 13 if left == 2 & right == 3
		quietly: replace `censor' = 14 if left == 2 & right == 4
		quietly: drop left
		quietly: drop right
	}
	
	*********************************************
	*ARTKALEN/PBIOSPE/SOZKALEN*
	*********************************************
	if "$soep_dataset" == "artkalen" {
		quietly: gen `censor' = .
		quietly: replace `censor' = -2 if left == -1 | right == -1
		quietly: replace `censor' = 1 if left == 0 & right == 0
		quietly: replace `censor' = 2 if left == 0 & right == 1
		quietly: replace `censor' = 3 if left == 0 & right == 2
		quietly: replace `censor' = 4 if left == 1 & right == 0
		quietly: replace `censor' = 5 if left == 1 & right == 1
		quietly: replace `censor' = 6 if left == 1 & right == 2
		quietly: replace `censor' = 7 if left == 2 & right == 0
		quietly: replace `censor' = 8 if left == 2 & right == 1
		quietly: replace `censor' = 9 if left == 2 & right == 2
		drop left
		drop right
	}
	
	lab var `censor' "${censorlab}"
	lab val `censor' "${censorvlab}"
}
else {
	di in red "Variables left and right not found. Please use censor_unfold first!"
}
end

 

