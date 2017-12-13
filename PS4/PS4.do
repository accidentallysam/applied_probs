cd "E:\2011 Fall\Labor and Population Economics\Problem Sets\PS4"

set more off

local fcount = 0
local tcount = 0
local rcount = 0

set matsize 11000

cap log close PS4
log using PS4, replace text name(PS4)

cap which estout
if _rc ssc install estout

set scheme s1mono
graph set eps logo off
graph set eps preview off
graph set eps orientation portrait
graph set eps mag 100

use poll7080.dta, replace
// outsheet using poll7080.txt, nolabel replace

// re-scale some variables
gen dgtsp100	= dgtsp*1/100
gen dunemp100	= dunemp*100
gen dmnfcg10	= dmnfcg*10

gen dhouse = house80-house70

// select regressors
local varlist dlhouse dgtsp100 ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
	vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80
keep `varlist'  dgtsp tsp75 tsp7576 house80 house70 dunemp100 dmnfcg10 dhouse
// generate polynomials and interactions
tokenize `varlist'
loc i = 2 // start with the first control variable
loc j = 1
loc k = 1
while "``++i''"!="" {
	// quadratic term
	qui gen x`k' = ``i''^2
	la var x`k' "``i''^2"
	loc k = `k'+1
	// cubic term
	qui gen x`k' = ``i''^3
	la var x`k' "``i''^3"
	loc k = `k'+1
	// interactions
	loc j = `i'
	while "``++j''"!="" {
		qui gen x`k'=``i''*``j''
		la var x`k' "``i''*``j''"
		loc k = `k'+1
	}
}

la var dhouse		"Housing value"
la var dgtsp		"Mean TSPs"
la var dgtsp100		"Mean TSPs ($\times 1/100$)"
la var dincome		"Income per capita (1982--84 dollars)"
la var dunemp100	"Unemployment rate ($\times 100$)"
la var dmnfcg10		"\% employment in manufacturing ($\times 10$)"
la var ddens		"Population density"
la var durban		"\% urban ($\times 10$)"
la var blt1080		"\% houses build in last 10 years"
la var tsp7576		"TSPs nonattainment in 1975 or 1976"

/// Part (a)
eststo reg_a1: reg dlhouse dgtsp100, vce(robust)
eststo reg_a2: reg dlhouse dgtsp100 ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
	vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80, vce(robust)
eststo reg_a3: reg dlhouse dgtsp100 ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
	vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80 x*, vce(robust)

xtile tspcat1 = dgtsp, nq(2)
replace tspcat1 = 1-mod(tspcat1, 2)

qui eststo tt_fd: estpost ttest dhouse dgtsp dincome dunemp100 dmnfcg10 ddens durban blt1080, by(tspcat1)
qui estadd mat mean=e(b)
qui estadd mat sd=e(se)

/// Part (b)
gen tspcat2 = 1-tsp7576

qui eststo tt_iv: estpost ttest dhouse dgtsp dincome dunemp100 dmnfcg10 ddens durban blt1080, by(tspcat2)
qui estadd mat mean=e(b)
qui estadd mat sd=e(se)

/// Part (c)

// first stage, 1975/76
eststo reg_c1fs: reg dgtsp tsp7576, vce(robust)
predict dgtsp_c1hat, xb
replace dgtsp_c1hat = dgtsp_c1hat*1/100
eststo reg_c2fs: reg dgtsp tsp7576 ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
	vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80, vce(robust)
predict dgtsp_c2hat, xb
replace dgtsp_c2hat = dgtsp_c2hat*1/100
eststo reg_c3fs: reg dgtsp tsp7576 ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
	vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80 x*, vce(robust)
predict dgtsp_c3hat, xb
replace dgtsp_c3hat = dgtsp_c3hat*1/100

// reduced form, 1975/76
eststo reg_c1rf: reg dlhouse tsp7576, vce(robust)
eststo reg_c2rf: reg dlhouse tsp7576 ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
	vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80, vce(robust)
eststo reg_c3rf: reg dlhouse tsp7576 ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
	vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80 x*, vce(robust)

// 2SLS, 1975/76
eststo reg_c1iv: reg dlhouse dgtsp_c1hat, vce(robust)
eststo reg_c2iv: reg dlhouse dgtsp_c2hat ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
	vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80, vce(robust)
eststo reg_c3iv: reg dlhouse dgtsp_c3hat ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
	vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80 x*, vce(robust)

// first stage, 1975
eststo reg_c1fs75: reg dgtsp tsp75, vce(robust)
predict dgtsp_c1hat75, xb
replace dgtsp_c1hat75 = dgtsp_c1hat75*1/100
eststo reg_c2fs75: reg dgtsp tsp75 ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
	vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80, vce(robust)
predict dgtsp_c2hat75, xb
replace dgtsp_c2hat75 = dgtsp_c2hat75*1/100
eststo reg_c3fs75: reg dgtsp tsp75 ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
	vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80 x*, vce(robust)
predict dgtsp_c3hat75, xb
replace dgtsp_c3hat75 = dgtsp_c3hat75*1/100

// 2SLS, 1975
eststo reg_c1iv75: reg dlhouse dgtsp_c1hat75, vce(robust)
eststo reg_c2iv75: reg dlhouse dgtsp_c2hat75 ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
	vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80, vce(robust)
eststo reg_c3iv75: reg dlhouse dgtsp_c3hat75 ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
	vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80 x*, vce(robust)

/// Part (f)

capture program drop f1
program define f1
	version 11.1
	reg dgtsp tsp7576, vce(robust)

	tempvar dgtsp_f1hat
	tempvar resid_f1hat
	tempvar dgtspXresid_f1hat
	predict `dgtsp_f1hat', xb
	predict `resid_f1hat', resid
	replace dgtsp_f1hat = `dgtsp_f1hat'*1/100
	replace resid_f1hat = `resid_f1hat'*1/100
	replace dgtspXresid_f1hat = dgtsp100*resid_f1hat
	
	reg dlhouse dgtsp100 resid_f1hat dgtspXresid_f1hat, vce(robust)
end

capture program drop f2
program define f2
	version 11.1
	reg dgtsp tsp7576 ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
		vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80, vce(robust)

	tempvar dgtsp_f2hat
	tempvar resid_f2hat
	tempvar dgtspXresid_f2hat
	predict `dgtsp_f2hat', xb
	predict `resid_f2hat', resid
	replace dgtsp_f2hat = `dgtsp_f2hat'*1/100
	replace resid_f2hat = `resid_f2hat'*1/100
	replace dgtspXresid_f2hat = dgtsp100*resid_f2hat
	
	reg dlhouse dgtsp100 resid_f2hat dgtspXresid_f2hat ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
		vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80, vce(robust)
end

capture program drop f3
program define f3
	version 11.1
	reg dgtsp tsp7576 ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
		vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80 x*, vce(robust)

	tempvar dgtsp_f3hat
	tempvar resid_f3hat
	tempvar dgtspXresid_f3hat
	predict `dgtsp_f3hat', xb
	predict `resid_f3hat', resid
	replace dgtsp_f3hat = `dgtsp_f3hat'*1/100
	replace resid_f3hat = `resid_f3hat'*1/100
	replace dgtspXresid_f3hat = dgtsp100*resid_f3hat
	
	reg dlhouse dgtsp100 resid_f3hat dgtspXresid_f3hat ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
		vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80 x*, vce(robust)
end

// Garen 2SLS, 1975/76
timer clear 1
timer on 1
gen dgtsp_f1hat = 0
gen resid_f1hat = 0
gen dgtspXresid_f1hat = 0
eststo reg_f1iv: bootstrap _b, reps(1000): f1
timer off 1
timer list 1

timer clear 2
timer on 2
gen dgtsp_f2hat = 0
gen resid_f2hat = 0
gen dgtspXresid_f2hat = 0
eststo reg_f2iv: bootstrap, reps(1000): f2
timer off 2
timer list 2

timer clear 3
timer on 3
gen dgtsp_f3hat = 0
gen resid_f3hat = 0
gen dgtspXresid_f3hat = 0
eststo reg_f3iv: bootstrap, reps(1000): f3
timer off 3
timer list 3

/// Export tables
esttab reg_a1 reg_a2 reg_a3 using TEXDocs/reg`++rcount'.tex, replace ///
	drop(_cons*) ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	indicate("Controls"= ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
		vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80 x*, ///
		labels("\multicolumn{1}{c}{\ensuremath{\text{Yes}}}" ///
		"\multicolumn{1}{c}{\ensuremath{\text{No}}}")) ///
	nomtitles ///
	label ///
	collabels(none) ///
	legend ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	addnote("Equation (3) includes quadratics, cubics, and interactions of the controls." ///
		"Huber-White standard errors in parentheses.") ///
	title("First-Difference Estimates of the Effect of TSPs Pollution on Log Housing Values\label{reg:fd}") ///
	substitute(none) ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

esttab reg_c1fs reg_c2fs reg_c3fs using TEXDocs/reg`++rcount'.tex, replace ///
	drop(_cons*) ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	indicate("Controls"= ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
		vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80 x*, ///
		labels("\multicolumn{1}{c}{\ensuremath{\text{Yes}}}" ///
		"\multicolumn{1}{c}{\ensuremath{\text{No}}}")) ///
	nomtitles ///
	label ///
	collabels(none) ///
	legend ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	addnote("Equation (3) includes quadratics, cubics, and interactions of the controls." ///
		"Huber-White standard errors in parentheses.") ///
	title("Estimates of the Impact of Mid-Decade TSPs Nonattainment on 1970--80 Changes in TSPs Pollution\label{reg:fs}") ///
	substitute(none) ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

esttab reg_c1rf reg_c2rf reg_c3rf using TEXDocs/reg`++rcount'.tex, replace ///
	drop(_cons*) ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	indicate("Controls"= ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
		vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80 x*, ///
		labels("\multicolumn{1}{c}{\ensuremath{\text{Yes}}}" ///
		"\multicolumn{1}{c}{\ensuremath{\text{No}}}")) ///
	nomtitles ///
	label ///
	collabels(none) ///
	legend ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	addnote("Equation (3) includes quadratics, cubics, and interactions of the controls." ///
		"Huber-White standard errors in parentheses.") ///
	title("Estimates of the Impact of Mid-Decade TSPs Nonattainment on 1970--80 Changes in Log Housing Values\label{reg:rf}") ///
	substitute(none) ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

esttab reg_c1iv reg_c2iv reg_c3iv using TEXDocs/reg`++rcount'.tex, replace ///
	drop(_cons*) ///
	rename(dgtsp_c1hat dgtsp100 dgtsp_c2hat dgtsp100 dgtsp_c3hat dgtsp100) ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	indicate("Controls"= ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
		vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80 x*, ///
		labels("\multicolumn{1}{c}{\ensuremath{\text{Yes}}}" ///
		"\multicolumn{1}{c}{\ensuremath{\text{No}}}")) ///
	nomtitles ///
	label ///
	collabels(none) ///
	legend ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	addnote("Equation (3) includes quadratics, cubics, and interactions of the controls." ///
		"Changes in TSPs instrumented for with 1975/76 TSPs nonattaiment status." ///
		"Huber-White standard errors in parentheses.") ///
	title("2SLS Estimates of the Effect of 1970--80 Changes in TSPs Pollution on Changes in Log Housing Values, Using TSPs Nonattainment in 1975 or 1976 as an Instrument\label{reg:iv7576}") ///
	substitute(none) ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

esttab reg_c1iv75 reg_c2iv75 reg_c3iv75 using TEXDocs/reg`++rcount'.tex, replace ///
	drop(_cons*) ///
	rename(dgtsp_c1hat75 dgtsp100 dgtsp_c2hat75 dgtsp100 dgtsp_c3hat75 dgtsp100) ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	indicate("Controls"= ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
		vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80 x*, ///
		labels("\multicolumn{1}{c}{\ensuremath{\text{Yes}}}" ///
		"\multicolumn{1}{c}{\ensuremath{\text{No}}}")) ///
	nomtitles ///
	label ///
	collabels(none) ///
	legend ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	addnote("Equation (3) includes quadratics, cubics, and interactions of the controls." ///
		"Changes in TSPs instrumented for with 1975 TSPs nonattaiment status." ///
		"Huber-White standard errors in parentheses.") ///
	title("2SLS Estimates of the Effect of 1970--80 Changes in TSPs Pollution on Changes in Log Housing Values, Using TSPs Nonattainment in 1975 Only as an Instrument\label{reg:iv75}") ///
	substitute(none) ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

esttab reg_f1iv reg_f2iv reg_f3iv using TEXDocs/reg`++rcount'.tex, replace ///
	drop(_cons*) ///
	rename(resid_f1hat resid dgtspXresid_f1hat dgtspXresid ///
		resid_f2hat resid dgtspXresid_f2hat dgtspXresid ///
		resid_f3hat resid dgtspXresid_f3hat dgtspXresid) ///
	varlabels(resid "\ensuremath{v_{i}} (first-stage residual) (\ensuremath{\times 1/100})" ///
		dgtspXresid "\ensuremath{v_{i} \times \text{mean TSPs}}~(\ensuremath{\times 1/10000})") ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	indicate("Controls"= ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty vacant70 ///
		vacant80 vacrnt70 downer dplumb drevenue dtaxprop depend deduc dhghwy dwelfr dhlth blt1080 blt2080 bltold80 x*, ///
		labels("\multicolumn{1}{c}{\ensuremath{\text{Yes}}}" ///
		"\multicolumn{1}{c}{\ensuremath{\text{No}}}")) ///
	nomtitles ///
	label ///
	collabels(none) ///
	legend ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	addnote("Equation (3) includes quadratics, cubics, and interactions of the controls." ///
		"Changes in TSPs instrumented for with 1975/76 TSPs nonattaiment status." ///
		"Bootstrap standard errors in parentheses (1000 reps.).") ///
	title("Control Function Estimates of the Capitalization of 1970--80 Changes in TSPs Pollution, with Correction for Selectivity Bias Due to Random Coefficients\label{reg:iv7576cf}") ///
	substitute(none) ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

esttab tt_fd tt_iv using TEXDocs/tab`++tcount'.tex, replace ///
	cells(mean(fmt(%9.3f) star) sd(fmt(%9.3f) par)) ///
	stats(N, fmt(%9.0g) layout("\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{N}")) ///
	nodepvar ///
	/*nonumber*/ ///
	collabels(none) ///
	mtitles("\parbox{1.5in}{\centering First Difference 1980--1970}" "\parbox{1.5in}{\centering TSPs Nonattainment in 1975 or 1976}") ///
	label ///
	legend ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	addnote("Mean values; standard errors in parentheses.") ///
	title("Differences in Sample Means Between Groups of Counties, Defined by TSPs Levels or Nonattainment Status\label{tab:groupm}") ///
	substitute(none) ///
	gaps ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

capture log close PS4
