cd "E:\2011 Fall\Labor and Population Economics\Problem Sets\PS2"

local nblock = 5
local level = 0.01

local fcount = 0

set more off

set matsize 11000

cap log close PS2
log using PS2, replace text name(PS2)

cap which estout
if _rc ssc install estout
cap which savesome
if _rc ssc install savesome

set scheme s1mono
graph set eps logo off
graph set eps preview off
graph set eps orientation portrait
graph set eps mag 100

use smoking2.dta, replace
// outsheet using smoking2.txt, nolabel replace

la define tobla 0 "Non-smokers" 1 "Smokers"
la values tobacco tobla

rename mblack	dmblack
rename mhispan	dmhispan
rename motherr	dmotherr

drop if dmblack+dmhispan+dmotherr>1

gen dmwhite		= ~(dmblack | dmhispan | dmotherr)
gen dfwhite		= ~(fblack | fhispan | fotherr)
gen dmage2		= dmage^2
gen dmage3		= dmage^3
gen dmage4		= dmage^4
gen dmeduc2		= dmeduc^2
gen disllb2		= disllb^2


la var dmage	"Mother Age"
la var dmage2	"Mother Age$^2$"
la var dmeduc	"Mother Education"
la var dmeduc2	"Mother Education$^2$"
la var dmar		"Mother Unmarried"
la var dfage	"Father Age"
la var dfeduc	"Father Education"
la var dfwhite	"Father White"
la var alcohol	"Mother Drank"
la var nprevist	"Prenatal Visits"
la var deadkids	"Previous Deaths"
la var diabete	"Mother Diabetic"
la var anemia	"Mother Anemic"
la var tobacco	"Mother Smoked"
la var dmwhite	"Mother White"
la var dmblack	"Mother Black"
la var dmhispan	"Mother Hispanic"
la var dmotherr	"Mother Other Race"
la var drink	"Num. Drinks"
la var dlivord	"Birth Order"
la var disllb	"Mo. Since Last"
la var disllb2	"Mo. Since Last$^2$"
la var preterm	"Prev. Birth Pre-Term"
la var pre4000	"Prev. Birth $>4\text{kg}$"
la var plural	"Twins or Greater"
la var phyper	"Hypertension"

eststo r_bw_t: reg dbirwt tobacco, vce(robust)

local mtvars dmage dmeduc dmwhite dmar dfage dfeduc dfwhite alcohol nprevist ///
	deadkids diabete anemia
qui eststo mt_not: estpost summarize `mtvars' if tobacco==0
qui eststo mt_tob: estpost summarize `mtvars' if tobacco==1
qui eststo mt_ttl: estpost summarize `mtvars' if tobacco~=.
qui eststo tt_tob: estpost ttest `mtvars', by(tobacco)
qui estadd mat mean=e(b)
qui estadd mat sd=e(se)

esttab mt_not mt_tob mt_ttl tt_tob using TEXDocs/tab1.tex, replace ///
	cells(mean(fmt(%9.3f) star) sd(fmt(%9.3f) par)) ///
	stats(N, fmt(%9.0g) layout("\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{N}")) ///
	nodepvar ///
	nonumber ///
	collabels(none) ///
	mtitles("Non-Smokers" "Smokers" "Total" "Difference") ///
	label ///
	legend ///
	note("Mean values; standard errors in parentheses.") ///
	title("Descriptive Statistics\label{tab:groupm}") ///
	substitute(none) ///
	gaps ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

eststo r_bw_tc: reg dbirwt tobacco dmage dmage2 dmeduc dmeduc2 dmar ///
	dmblack dmhispan dmotherr alcohol nprevist disllb preterm pre4000 ///
	plural phyper diabete, vce(robust)

egen category = group(dmar dmblack dmhispan dmotherr)
egen dmrace = group(dmblack dmhispan dmotherr)
xi: eststo r_bw_tmvm: reg dbirwt ///
	tobacco ///
	alcohol ///
	i.category ///
	i.dmrace|preterm ///
	i.category|pre4000 ///
	i.category|plural ///
	i.category|phyper ///
	i.category|diabete ///
	///
	i.category|dmage ///
	i.category|dmage2 ///
	i.category|dmage3 ///
	i.category|dmage4 ///
	i.category|dmeduc ///
	i.category|dmeduc2 ///
	i.category|nprevist ///
	i.category|disllb ///
	i.category|disllb2, vce(robust)

// Regression Table for:
//   dbirwt on tobacco 
//   dbirwt on tobacco and covariates
//   dbirwt on tobacco and covariates, interactions, and 2nd order terms
esttab r_bw_t r_bw_tc r_bw_tmvm using TEXDocs/reg1.tex, replace ///
	order(_cons tobacco dmage dmage2 dmage3 dmage4 dmeduc dmeduc2 dmar dmblack ///
		dmhispan dmotherr alcohol nprevist disllb disllb2 preterm pre4000 ///
		plural phyper diabete) ///
	varlabels(_cons "Constant") ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	indicate("Int. and $2^{\text{nd}}$ Order"= _I* *3 *4 disllb2, ///
		labels("\multicolumn{1}{c}{\ensuremath{\text{Yes}}}" ///
		"\multicolumn{1}{c}{\ensuremath{\text{No}}}")) ///
	nomtitles ///
	label ///
	collabels(none) ///
	legend ///
	addnotes("Dependent variable is infant birthweight." ///
		"Robust standard errors in parentheses.") ///
	title("OLS Estimates of Birthweight Equations\label{reg:ols}") ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

// proceed with pscore estimation 
capture drop pscore
capture drop block
capture drop block_inf
capture drop block_sup
// estimate the propensity score
xi: logit tobacco ///
	i.category ///
	alcohol ///
	i.dmrace|preterm ///
	i.category|pre4000 ///
	i.category|plural ///
	i.category|phyper ///
	i.category|diabete ///
	///
	i.category|dmage ///
	i.category|dmage2 ///
	i.category|dmage3 ///
	i.category|dmage4 ///
	i.category|dmeduc ///
	i.category|dmeduc2 ///
	i.category|nprevist ///
	i.category|disllb ///
	i.category|disllb2 ///

predict double pscore
la var pscore "Propensity Score"
sum pscore, detail

// divide the data into blocks
gen id = _n // store obs. ids so we can restore the original order later
xtile block = pscore, nq(`nblock')
by block, sort: egen block_inf = min(pscore)
by block, sort: egen block_sup = max(pscore)
la var block "Block No."
la var block_inf "Block Infimum"
la var block_sup "Block Supremum"

// subdivide blocks until pscores are balanced
tab block tobacco
local iblock = 1
while `iblock'<=`nblock' {
	di "block `iblock' of `nblock'..."
	// count the number of treated/contols in this block
	qui count if block == `iblock' & tobacco==0
	local ncontrol = r(N)
	qui count if block == `iblock' & tobacco==1
	local ntreated = r(N)
	// skip blocks with no treatments or no controls in them
	if `ncontrol' == 0 | `ntreated' == 0 {
		local iblock = `iblock'+1
	}
	else {
		local ivar = 1
		local nvar = 0
		local fail_count = 0
		// test for difference in mean values
		qui ttest pscore if block==`iblock', by(tobacco)
		if r(p)<`level' {
			local fail_count = `fail_count'+1
		}
		// if pscores are different, split the block
		if `fail_count'>0 {
			// shift the remaining blocks up
			qui replace block = block+1 if block>`iblock' & block!=.
			local nblock = `nblock'+1
			// get the block split point
			sum block_inf if block==`iblock', meanonly
			local temp_inf = r(mean)
			sum block_sup if block==`iblock', meanonly
			local temp_sup = r(mean)
			local split = (`temp_inf' + `temp_sup')/2
			// split the block
			di "splitting block `iblock'"
			qui replace block = block+1 if block==`iblock' & pscore>=`split' ///
				& pscore<=block_sup
			qui replace block_sup = `split' if block==`iblock'
			qui replace block_inf = `split' if block==`iblock'+1
		} // end if
		else {
			// pscores are not different, iterate to the next block
			local iblock = `iblock'+1
		} // end else
	} // end else
} // end while
tab block tobacco

// check that the covariates are balanced
local total_fail = 0
local iblock = 1
while `iblock'<=`nblock' {
	di "block `iblock' of `nblock'..."
	// count the number of treated/contols in this block
	qui count if block == `iblock' & tobacco==0
	local ncontrol = r(N)
	qui count if block == `iblock' & tobacco==1
	local ntreated = r(N)
	// skip blocks with no treatments or no controls in them
	if `ncontrol' == 0 | `ntreated' == 0 {
		local iblock = `iblock'+1
	}
	else {
		local ivar = 1
		local nvar = 0
		local fail_count = 0
		// test for difference in mean values
		foreach var in tobacco dmage dmeduc dmar dmblack dmhispan ///
			dmotherr alcohol nprevist disllb preterm ///
			pre4000 plural phyper diabete {
			qui ttest `var' if block==`iblock', by(tobacco)
			if r(p)<`level' {
				di "`var' fail in block `iblock'!"
				local fail_count = `fail_count'+1
			}
			local nvar = `nvar'+1
		} // end foreach
		// if 10% or more mean values are different, fail
		if (`fail_count')/`nvar'>=.10 {
			local total_fail = `total_fail'+1
		} // end if
		// iterate to the next block
		local iblock = `iblock'+1
	} // end else
} // end while
di "`total_fail' (" round(`total_fail'/`nblock'*100) ///
		"%) blocks were unbalanced"
// append the pscore data to the file on disk
preserve
sort id
keep pscore block block_inf block_sup
merge 1:1 _n using smoking2.dta, nogen
save smoking2.dta, replace
restore

// graph the pscore boxplot
local fcount = `fcount'+1
graph box pscore, over(tobacco) ///
	title(Propensity Score Box Plot) legend(off) ///
	name(fig`fcount', replace) saving(Figures\fig`fcount'.gph, replace)
graph export Figures\fig`fcount'.eps, replace

// tabulate the blocks
estpost tab block tobacco
esttab . using TEXDocs/tab2.tex, replace ///
	cell(b(fmt(g)) count(fmt(g) par keep(Total))) ///
	collabels(none) unstack noobs nonumber nomtitle  ///
	eqlabels(, lhs("Block No.")) ///
	varlabels(, blist(Total "{hline @width}{break}")) ///
	title("Propensity Score Blocks\label{tab:blocks}") ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

// regression-adjust using the pscore
local pscore_mean = sum(pscore)/sum(pscore~=.)
disp `pscore_mean'
gen tobacco_pscore = tobacco*(pscore-`pscore_mean')
la var tobacco_pscore "\ensuremath{\text{Mother Smoked}\cdot\text{P-Score}}"
eststo r_bw_tp: reg dbirwt tobacco pscore tobacco_pscore, vce(robust)

// stratify the sample and estimate TOT
drop if pscore == .
// calculate the total number of treatments
egen nT = total(tobacco)
// claulate number of treatments and controls in each block
egen block_nT = total(tobacco), by(block)
egen block_nC = total(~tobacco), by(block)
// not calculate TOT from the stratified sample
gen block_unwtTOT = tobacco*dbirwt/block_nT-(~tobacco)*dbirwt/block_nC
gen block_wt = block_nT/nT
preserve
collapse (sum) block_unwtTOT (first) block_wt, by(block)
gen TOT = block_unwtTOT*block_wt
collapse (sum) TOT
disp "TOT=" TOT
restore

// low birthweight
gen lowbw = (dbirwt<2500)
// ATE and TOT probability weights
gen ate_weight = (~tobacco)*1/(1-pscore)+tobacco*1/pscore
gen tot_weight = (~tobacco)*pscore/(1-pscore)+tobacco

foreach var in dbirwt lowbw {
	if "`var'"=="dbirwt" {
		local yvar = "Mean birth weight, by cell"
		local gtitle = "Mean Birthweight vs. Propensity Score"
	}
	else {
		local yvar = "Fraction low birthweight, by cell"
		local gtitle = "Fraction Low Birthweight vs. Propensity Score"
	}

	// regress using the pscores as weights
	eststo r_bw_tpw_ate_`var': reg `var' tobacco [pw=ate_weight], vce(robust)
	eststo r_bw_tpw_tot_`var': reg `var' tobacco [pw=tot_weight], vce(robust)

	if "`var'"=="dbirwt" {
		// check reasonableness of reweighing procedure
		capture drop block200
		capture drop block200_nT
		capture drop block200_nC
		capture drop block200_fracT
		xtile block200 = pscore, nq(200)
		egen block200_nT = total(tobacco), by(block200)
		egen block200_nC = total(~tobacco), by(block200)
		gen block200_fracT = block200_nT/(block200_nT+block200_nC)
		preserve
		collapse (mean) pscore (first) block200_fracT, by(block200)
		local fcount = `fcount'+1
		twoway scatter pscore block200_fracT || line block200_fracT block200_fracT || , ///
			title(Propensity Score vs. Actual Fraction Smoker) ///
			xtitle("Actual fraction smoker, by cell") ///
			ytitle("Estimated p-score, by cell") ///
			legend(on label(1 "Estimated p-score") label(2 "45-degree line")) ///
			name(fig`fcount', replace) saving(Figures\fig`fcount'.gph, replace)
		graph export Figures\fig`fcount'.eps, replace
		restore
		// estimate TOT using actual fraction of smokers in each block
		capture drop tot_weight200
		gen tot_weight200 = (~tobacco)*block200_fracT/(1-block200_fracT)+tobacco
		eststo r_bw_tpw_tot200_`var': reg `var' tobacco [pw=tot_weight200], vce(robust)
	}

	// compare "non-parametric" means
	preserve
	keep if tobacco==1
	capture drop block100
	xtile block100 = pscore, nq(100)
	collapse (mean) `var' pscore (first) tobacco, by(block100)
	save smokers, replace
	restore
	preserve
	keep if tobacco==0
	capture drop block100
	xtile block100 = pscore, nq(100)
	collapse (mean) `var' pscore (first) tobacco, by(block100)
	append using smokers
	local fcount = `fcount'+1
	twoway scatter `var' pscore if tobacco==0 || scatter `var' pscore if tobacco==1 || , ///
		title("`gtitle'") ///
		xtitle("Estimated p-score, by cell") ///
		ytitle("`yvar'") ///
		legend(on label(1 "Non-smokers") label(2 "Smokers")) ///
		name(fig`fcount', replace) saving(Figures\fig`fcount'.gph, replace)
	graph export Figures\fig`fcount'.eps, replace
	erase smokers.dta
	restore
	preserve
	capture drop block200
	xtile block200 = pscore, nq(200)
	collapse (mean) `var' pscore (first) tobacco, by(block200)
	local fcount = `fcount'+1
	scatter `var' pscore, ///
		title("`gtitle'") ///
		xtitle("Estimated p-score, by cell") ///
		ytitle("`yvar'") ///
		name(fig`fcount', replace) saving(Figures\fig`fcount'.gph, replace)
	graph export Figures\fig`fcount'.eps, replace
	restore
} // end for

// effect of smoking on infant death
eststo r_d_t: reg death tobacco, vce(robust)
eststo r_d_tc: reg death tobacco dmage dmage2 dmeduc dmeduc2 dmar ///
	dmblack dmhispan dmotherr alcohol nprevist disllb preterm pre4000 ///
	plural phyper diabete, vce(robust)
eststo r_d_tpw_ate: reg death tobacco [pw=ate_weight], vce(robust)
eststo r_d_tpw_tot: reg death tobacco [pw=tot_weight], vce(robust)

// smoking lifecycle
preserve
egen ageblock = group(dmage)
collapse (sum) ageblock_nT=tobacco ageblock_nC=~tobacco (first) dmage ///
	(count) ageblock_n=dmage, by(ageblock)
gen ageblock_fracT = ageblock_nT/ageblock_n
local fcount = `fcount'+1
twoway scatter ageblock_n dmage || scatter ageblock_fracT dmage, yaxis(2) || , ///
		title("Sample Size and Smoking Rate vs. Age") ///
		xtitle("Age") ///
		ytitle("Sample Size", axis(1)) ///
		ytitle("Smoking Rate", axis(2)) ///
		legend(on label(1 "Sample Size") label(2 "Smoking Rate")) ///
		name(fig`fcount', replace) saving(Figures\fig`fcount'.gph, replace)
	graph export Figures\fig`fcount'.eps, replace
restore
preserve
egen agetgroup = group(dmage tobacco)
collapse (mean) dbirwt (first) dmage tobacco, by(agetgroup)
local fcount = `fcount'+1
twoway scatter dbirwt dmage if tobacco==0 || scatter dbirwt dmage if tobacco==1|| , ///
		title("Average Birthweight vs. Age") ///
		xtitle("Age") ///
		ytitle("Birthweight") ///
		legend(on label(1 "Non-smokers") label(2 "Smokers")) ///
		name(fig`fcount', replace) saving(Figures\fig`fcount'.gph, replace)
	graph export Figures\fig`fcount'.eps, replace
restore

// Regression Table for:
//   dbirwt on tobacco and pscore 
//   dbirwt on tobacco, weighted by pscore ate
//   dbirwt on tobacco, weighted by pscore tot
//   dbirwt on tobacco, weighted by fractional tot
esttab r_bw_tp r_bw_tpw_ate_dbirwt r_bw_tpw_tot_dbirwt r_bw_tpw_tot200_dbirwt ///
	using TEXDocs/reg2.tex, replace ///
	order(_cons tobacco pscore tobacco_pscore) ///
	varlabels(_cons "Constant") ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	mtitles("OLS" "WLS (ATE)" "WLS (TOT)" "WLS (TOT$^{'}$)") ///
	label ///
	collabels(none) ///
	legend ///
	addnotes("Dependent variable is infant birthweight." ///
		"Robust standard errors in parentheses.") ///
	title("OLS and WLS Estimates of Birthweight Equations\label{reg:wls}") ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

// Regression Table for:
//   lowbw on tobacco, weighted by pscore ate
//   lowbw on tobacco, weighted by pscore tot
esttab r_bw_tpw_ate_lowbw r_bw_tpw_tot_lowbw ///
	using TEXDocs/reg3.tex, replace ///
	order(_cons tobacco) ///
	varlabels(_cons "Constant") ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	mtitles("WLS (ATE)" "WLS (TOT)") ///
	label ///
	collabels(none) ///
	legend ///
	addnotes("Dependent variable is indicator for low birthweight." ///
		"Robust standard errors in parentheses.") ///
	title("WLS Estimates of Low Birthweight Equations\label{reg:wlslbw}") ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

// Regression Table for:
//   death on tobacco 
//   death on tobacco and covariates
//   death on tobacco, weighted by pscore ate
//   death on tobacco, weighted by pscore tot
esttab r_d_t r_d_tc r_d_tpw_ate r_d_tpw_tot using TEXDocs/reg4.tex, replace ///
	order(_cons tobacco dmage dmage2 dmeduc dmeduc2 dmar dmblack ///
		dmhispan dmotherr alcohol nprevist disllb preterm pre4000 ///
		plural phyper diabete) ///
	varlabels(_cons "Constant") ///
	cells(b(star fmt(%9.6f)) se(par fmt(%9.6f))) ///
	stats(r2 F N, fmt(%9.6f %9.6f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	mtitles("OLS" "OLS" "WLS (ATE)" "WLS (TOT)") ///
	label ///
	collabels(none) ///
	legend ///
	addnotes("Dependent variable is indicator for infant death." ///
		"Robust standard errors in parentheses.") ///
	title("OLS and WLS Estimates of Infant Death Equations\label{reg:death}") ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

capture log close PS2
