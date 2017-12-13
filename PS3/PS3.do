cd "E:\2011 Fall\Labor and Population Economics\Problem Sets\PS3"

set more off

local fcount = 0
local rcount = 0

// set matsize 11000

cap log close PS3
log using PS3, replace text name(PS3)

cap which estout
if _rc ssc install estout

set scheme s1mono
graph set eps logo off
graph set eps preview off
graph set eps orientation portrait
graph set eps mag 100

use p900_area2.dta, replace
// outsheet using p900_area2.txt, nolabel replace

gen x88r2 = x88^2
gen x88r3 = x88^3

gen ncas88r2 = ncas88^2

gen x88ncas88 = x88*ncas88

la var p90 "P-900 status"
la var rule2_area2 "1988 average score relative to cutoff"
la var dx90 "1988-1990 gain score"
la var dx92 "1988-1992 gain score"

/// Part (a)
eststo reg_dx90p90: reg dx90 p90, vce(robust)
eststo reg_dx92p90: reg dx92 p90, vce(robust)

/// Part (d)
local fcount = `fcount'+1
lowess p90 rule2_area2, mean noweight bw(0.05) ///
	lineop(lwidth(medthick)) ///
	title("") ///
	legend(on ///
		label(2 "Smoothed status")) ///
	name(fig`fcount', replace) saving(Figures\fig`fcount'.gph, replace)
graph export Figures\fig`fcount'.eps, replace

/// Part (f)
sort rule2_area2

lowess dx90 rule2_area2 if ivrule2_area2, mean noweight bw(0.3) gen(dx90se) nograph
lowess dx90 rule2_area2 if ~ivrule2_area2, mean noweight bw(0.1) gen(dx90si) nograph

local fcount = `fcount'+1
twoway line dx90se rule2_area2, lwidth(thick) || ///
	line dx90si rule2_area2, lwidth(thick) ||, ///
		title("") ///
		ytitle("Smoothed 1990 gain score") ///
		legend(on ///
			label(1 "Eligible") ///
				label(2 "Ineligible")) ///
		name(fig`fcount', replace) saving(Figures\fig`fcount'.gph, replace)
graph export Figures\fig`fcount'.eps, replace

lowess dx92 rule2_area2 if ivrule2_area2, mean noweight bw(0.3) gen(dx92se) nograph
lowess dx92 rule2_area2 if ~ivrule2_area2, mean noweight bw(0.1) gen(dx92si) nograph

local fcount = `fcount'+1
twoway line dx92se rule2_area2, lwidth(thick) || ///
	line dx92si rule2_area2, lwidth(thick) ||, ///
		title("") ///
		ytitle("Smoothed 1992 gain score") ///
		legend(on ///
			label(1 "Eligible") ///
				label(2 "Ineligible")) ///
		name(fig`fcount', replace) saving(Figures\fig`fcount'.gph, replace)
graph export Figures\fig`fcount'.eps, replace

/// Part (g)
gen within7 = rule2_area2^2<=49
gen within3 = rule2_area2^2<=9

foreach i in 90 92 {
	/***
	 * control function 1
	 */
	// first stage
	reg p90 ivrule2_area2, vce(robust)
	mat b_1stage_c1 = e(b), e(r2)
	mat se_1stage_c1 = (_se[ivrule2_area2],_se[_cons])
	local k = colsof(se_1stage_c1)
	matrix p_1stage_c1 = J(1,`k',0)
	forvalues j=1/`k' {
		mat p_1stage_c1[1,`j']=tprob(e(N)-`k', b_1stage_c1[1,`j']/se_1stage_c1[1,`j'])
	}
	mat list p_1stage_c1
	mat colnames b_1stage_c1  = ivrule2_area2_1stage _cons_1stage r2_1stage
	qui capture drop p90hat
	qui predict p90hat
	// reduced form
	reg dx`i' ivrule2_area2, vce(robust)
	mat b_redfrm_c1 = e(b), e(r2)
	mat se_redfrm_c1 = (_se[ivrule2_area2],_se[_cons])
	local k = colsof(se_redfrm_c1)
	matrix p_redfrm_c1 = J(1,`k',0)
	forvalues j=1/`k' {
		mat p_redfrm_c1[1,`j']=tprob(e(N)-`k', b_redfrm_c1[1,`j']/se_redfrm_c1[1,`j'])
	}
	mat list p_redfrm_c1
	mat colnames b_redfrm_c1  = ivrule2_area2_redfrm _cons_redfrm r2_redfrm
	// second stage
	eststo reg_iv`i'_c1: reg dx`i' p90hat, vce(robust)
	mat b_2stage_c1 = e(b), e(r2)
	mat colnames b_2stage_c1  = p90hat _cons r2
	mat se_2stage_c1 = (_se[p90hat],_se[_cons])
	mat C1b =  b_1stage_c1, b_redfrm_c1, b_2stage_c1
	estadd mat b_stack = C1b
	mat C1se =  se_1stage_c1, se_redfrm_c1, se_2stage_c1
	mat colnames C1se = ivrule2_area2_1stage _cons_1stage ivrule2_area2_redfrm _cons_redfrm p90hat _cons
	estadd mat se_stack = C1se
	local k = colsof(se_2stage_c1)
	matrix p_2stage_c1 = J(1,`k',0)
	forvalues j=1/`k' {
		mat p_2stage_c1[1,`j']=tprob(e(N)-`k', b_2stage_c1[1,`j']/se_2stage_c1[1,`j'])
	}
	mat list p_2stage_c1
	mat C1p = p_1stage_c1, p_redfrm_c1, p_2stage_c1
	mat colnames C1p = ivrule2_area2_1stage _cons_1stage ivrule2_area2_redfrm _cons_redfrm p90hat _cons
	estadd mat p_stack = C1p

	/***
	 * control function 2
	 */
	// first stage
	reg p90 ivrule2_area2 x88 x88r2 x88r3, vce(robust)
	mat b_1stage_c2 = e(b), e(r2)
	mat se_1stage_c2 = (_se[ivrule2_area2],_se[x88],_se[x88r2],_se[x88r3],_se[_cons])
	local k = colsof(se_1stage_c2)
	matrix p_1stage_c2 = J(1,`k',0)
	forvalues j=1/`k' {
		mat p_1stage_c2[1,`j']=tprob(e(N)-`k', b_1stage_c2[1,`j']/se_1stage_c2[1,`j'])
	}
	mat list p_1stage_c2
	mat colnames b_1stage_c2 = ivrule2_area2_1stage x88_1stage x88r2_1stage x88r3_1stage _cons_1stage r2_1stage
	qui capture drop p90hat
	qui predict p90hat
	// reduced form
	reg dx`i' ivrule2_area2 x88 x88r2 x88r3, vce(robust)
	mat b_redfrm_c2 = e(b), e(r2)
	mat se_redfrm_c2 = (_se[ivrule2_area2],_se[x88],_se[x88r2],_se[x88r3],_se[_cons])
	local k = colsof(se_redfrm_c2)
	matrix p_redfrm_c2 = J(1,`k',0)
	forvalues j=1/`k' {
		mat p_redfrm_c2[1,`j']=tprob(e(N)-`k', b_redfrm_c2[1,`j']/se_redfrm_c2[1,`j'])
	}
	mat list p_redfrm_c2
	mat colnames b_redfrm_c2 = ivrule2_area2_redfrm x88_redfrm x88r2_redfrm x88r3_redfrm _cons_redfrm r2_redfrm

	// second stage
	eststo reg_iv`i'_c2: reg dx`i' p90hat x88 x88r2 x88r3, vce(robust)
	mat b_2stage_c2 = e(b), e(r2)
	mat colnames b_2stage_c2  = p90hat x88 x88r2 x88r3 _cons r2
	mat se_2stage_c2 = (_se[p90hat],_se[x88],_se[x88r2],_se[x88r3],_se[_cons])
	mat C2b =  b_1stage_c2, b_redfrm_c2, b_2stage_c2
	estadd mat b_stack = C2b
	mat C2se =  se_1stage_c2, se_redfrm_c2, se_2stage_c2
	mat colnames C2se = ivrule2_area2_1stage x88_1stage x88r2_1stage x88r3_1stage _cons_1stage ///
		ivrule2_area2_redfrm x88_redfrm x88r2_redfrm x88r3_redfrm _cons_redfrm ///
		p90hat x88 x88r2 x88r3 _cons
	estadd mat se_stack = C2se
	local k = colsof(se_2stage_c2)
	matrix p_2stage_c2 = J(1,`k',0)
	forvalues j=1/`k' {
		mat p_2stage_c2[1,`j']=tprob(e(N)-`k', b_2stage_c2[1,`j']/se_2stage_c2[1,`j'])
	}
	mat list p_2stage_c2
	mat C2p = p_1stage_c2, p_redfrm_c2, p_2stage_c2
	mat colnames C2p = ivrule2_area2_1stage x88_1stage x88r2_1stage x88r3_1stage _cons_1stage ///
		ivrule2_area2_redfrm x88_redfrm x88r2_redfrm x88r3_redfrm _cons_redfrm ///
		p90hat x88 x88r2 x88r3 _cons
	estadd mat p_stack = C2p

	/***
	 * control function 3
	 */
	// first stage
	reg p90 ivrule2_area2 x88 x88r2 x88r3 ncas88 ncas88r2 x88ncas88, vce(robust)
	mat b_1stage_c3 = e(b), e(r2)
	mat se_1stage_c3 = (_se[ivrule2_area2],_se[x88],_se[x88r2],_se[x88r3],_se[ncas88],_se[ncas88r2],_se[x88ncas88],_se[_cons])
	local k = colsof(se_1stage_c3)
	matrix p_1stage_c3 = J(1,`k',0)
	forvalues j=1/`k' {
		mat p_1stage_c3[1,`j']=tprob(e(N)-`k', b_1stage_c3[1,`j']/se_1stage_c3[1,`j'])
	}
	mat list p_1stage_c3
	mat colnames b_1stage_c3 = ivrule2_area2_1stage x88_1stage x88r2_1stage x88r3_1stage ncas88_1stage ncas88r2_1stage x88ncas88_1stage _cons_1stage r2_1stage
	qui capture drop p90hat
	qui predict p90hat
	// reduced form
	reg dx`i' ivrule2_area2 x88 x88r2 x88r3 ncas88 ncas88r2 x88ncas88, vce(robust)
	mat b_redfrm_c3 = e(b), e(r2)
	mat se_redfrm_c3 = (_se[ivrule2_area2],_se[x88],_se[x88r2],_se[x88r3],_se[ncas88],_se[ncas88r2],_se[x88ncas88],_se[_cons])
	local k = colsof(se_redfrm_c3)
	matrix p_redfrm_c3 = J(1,`k',0)
	forvalues j=1/`k' {
		mat p_redfrm_c3[1,`j']=tprob(e(N)-`k', b_redfrm_c3[1,`j']/se_redfrm_c3[1,`j'])
	}
	mat list p_redfrm_c3
	mat colnames b_redfrm_c3 = ivrule2_area2_redfrm x88_redfrm x88r2_redfrm x88r3_redfrm ncas88_redfrm ncas88r2_redfrm x88ncas88_redfrm _cons_redfrm r2_redfrm

	// second stage
	eststo reg_iv`i'_c3: reg dx`i' p90hat x88 x88r2 x88r3 ncas88 ncas88r2 x88ncas88, vce(robust)
	mat b_2stage_c3 = e(b), e(r2)
	mat colnames b_2stage_c3  = p90hat x88 x88r2 x88r3 ncas88 ncas88r2 x88ncas88 _cons r2
	mat se_2stage_c3 = (_se[p90hat],_se[x88],_se[x88r2],_se[x88r3],_se[ncas88],_se[ncas88r2],_se[x88ncas88],_se[_cons])
	mat C3b =  b_1stage_c3, b_redfrm_c3, b_2stage_c3
	estadd mat b_stack = C3b
	mat C3se =  se_1stage_c3, se_redfrm_c3, se_2stage_c3
	mat colnames C3se = ivrule2_area2_1stage x88_1stage x88r2_1stage x88r3_1stage ncas88_1stage ncas88r2_1stage x88ncas88_1stage _cons_1stage ///
		ivrule2_area2_redfrm x88_redfrm x88r2_redfrm x88r3_redfrm ncas88_redfrm ncas88r2_redfrm x88ncas88_redfrm _cons_redfrm ///
		p90hat x88 x88r2 x88r3 ncas88 ncas88r2 x88ncas88 _cons
	estadd mat se_stack = C3se
	local k = colsof(se_2stage_c3)
	matrix p_2stage_c3 = J(1,`k',0)
	forvalues j=1/`k' {
		mat p_2stage_c3[1,`j']=tprob(e(N)-`k', b_2stage_c3[1,`j']/se_2stage_c3[1,`j'])
	}
	mat list p_2stage_c3
	mat C3p = p_1stage_c3, p_redfrm_c3, p_2stage_c3
	mat colnames C3p = ivrule2_area2_1stage x88_1stage x88r2_1stage x88r3_1stage ncas88_1stage ncas88r2_1stage x88ncas88_1stage _cons_1stage ///
		ivrule2_area2_redfrm x88_redfrm x88r2_redfrm x88r3_redfrm ncas88_redfrm ncas88r2_redfrm x88ncas88_redfrm _cons_redfrm ///
		p90hat x88 x88r2 x88r3 ncas88 ncas88r2 x88ncas88 _cons
	estadd mat p_stack = C3p
	
		
	mat list C1b
	mat list C1se
	mat list C1p

	mat list C2b
	mat list C2se
	mat list C2p

	mat list C3b
	mat list C3se
	mat list C3p
}

/// Part (h)
lowess ses90 rule2_area2 if ivrule2_area2, mean noweight bw(0.3) gen(ses90se) nograph
lowess ses90 rule2_area2 if ~ivrule2_area2, mean noweight bw(0.1) gen(ses90si) nograph

local fcount = `fcount'+1
twoway line ses90se rule2_area2 if dx92~=., lwidth(thick) || ///
	line ses90si rule2_area2 if dx92~=., lwidth(thick) ||, ///
		title("") ///
		ytitle("Smoothed 1990 SES") ///
		legend(on ///
			label(1 "Eligible") ///
				label(2 "Ineligible")) ///
		name(fig`fcount', replace) saving(Figures\fig`fcount'.gph, replace)
graph export Figures\fig`fcount'.eps, replace

lowess dses rule2_area2 if ivrule2_area2, mean noweight bw(0.3) gen(dsesse) nograph
lowess dses rule2_area2 if ~ivrule2_area2, mean noweight bw(0.1) gen(dsessi) nograph

local fcount = `fcount'+1
twoway line dsesse rule2_area2 if dx92~=., lwidth(thick) || ///
	line dsessi rule2_area2 if dx92~=., lwidth(thick) ||, ///
		title("") ///
		ytitle("Smoothed 1990-1992 SES gain") ///
		legend(on ///
			label(1 "Eligible") ///
				label(2 "Ineligible")) ///
		name(fig`fcount', replace) saving(Figures\fig`fcount'.gph, replace)
graph export Figures\fig`fcount'.eps, replace

foreach within in within7 within3 {
	lowess ses90 rule2_area2 if ivrule2_area2, mean noweight bw(0.3) gen(ses90se_`within') nograph
	lowess ses90 rule2_area2 if ~ivrule2_area2, mean noweight bw(0.1) gen(ses90si_`within') nograph

	local fcount = `fcount'+1
	twoway line ses90se rule2_area2 if dx92~=. & `within', lwidth(thick) || ///
		line ses90si rule2_area2 if dx92~=. & `within', lwidth(thick) ||, ///
			title("") ///
			ylabel(#4) yscale(range(50 65)) ///
			ytitle("Smoothed 1990 SES") ///
			legend(on ///
				label(1 "Eligible") ///
					label(2 "Ineligible")) ///
			name(fig`fcount', replace) saving(Figures\fig`fcount'.gph, replace)
	graph export Figures\fig`fcount'.eps, replace

	lowess dses rule2_area2 if ivrule2_area2, mean noweight bw(0.3) gen(dsesse_`within') nograph
	lowess dses rule2_area2 if ~ivrule2_area2, mean noweight bw(0.1) gen(dsessi_`within') nograph

	local fcount = `fcount'+1
	twoway line dsesse rule2_area2 if dx92~=. & `within', lwidth(thick) || ///
		line dsessi rule2_area2 if dx92~=. & `within', lwidth(thick) ||, ///
			title("") ///
			ylabel(#4) yscale(range(-25 -10)) ///
			ytitle("Smoothed 1990-1992 SES gain") ///
			legend(on ///
				label(1 "Eligible") ///
					label(2 "Ineligible")) ///
			name(fig`fcount', replace) saving(Figures\fig`fcount'.gph, replace)
	graph export Figures\fig`fcount'.eps, replace

	// first stage
	eststo reg_`within'_1stage: reg p90 ivrule2_area2 x88 x88r2 x88r3 ncas88 ncas88r2 x88ncas88 ///
		if `within', vce(robust)
	qui capture drop p90hat
	qui predict p90hat
	// reduced form
	eststo reg_`within'_redfrm: reg dx92 ivrule2_area2 x88 x88r2 x88r3 ncas88 ncas88r2 x88ncas88 ///
		if `within', vce(robust)
	// second stage
	eststo reg_`within'_2stage: reg dx92 p90hat x88 x88r2 x88r3 ncas88 ncas88r2 x88ncas88 ///
		if `within', vce(robust)
}

 
/// Output regression tables
local rcount = `rcount'+1
esttab reg_dx90p90 reg_dx92p90 using TEXDocs/reg`rcount'.tex, replace ///
	drop(_cons*) ///
	order(p90) ///
	varlabels(p90 "P-900") ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	mtitles("1988--1990" "1988--1992") ///
	label ///
	collabels(none) ///
	legend ///
	note("Huber-White standard errors in parentheses.") ///
	title("OLS Results for 1988{--}1990 and 1988{--}1992 Average Gain Scores\label{reg:OLS}") ///
	substitute(none) ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

local rcount = `rcount'+1
esttab reg_iv90_c1 reg_iv90_c2 reg_iv90_c3 ///
	reg_iv92_c1 reg_iv92_c2 reg_iv92_c3 using TEXDocs/reg`rcount'.tex, replace ///
	drop(_cons*) ///
	order(ivrule2_area2_1stage x88_1stage x88r2_1stage x88r3_1stage ncas88_1stage ncas88r2_1stage x88ncas88_1stage r2_1stage ivrule2_area2_redfrm x88_redfrm x88r2_redfrm x88r3_redfrm ncas88_redfrm ncas88r2_redfrm x88ncas88_redfrm r2_redfrm p90hat x88 x88r2 x88r3 ncas88 ncas88r2 x88ncas88 r2) ///
	refcat(ivrule2_area2_1stage "Panel A: First-stage estimates (P-900)" ///
		ivrule2_area2_redfrm "Panel B: Reduced-form estimates (Average gain scores)" ///
		p90hat "Panel C: IV estimates (Average gain scores)", nolabel) ///
	varlabels(ivrule2_area2_1stage "Eligible" ///
		x88_1stage "\ensuremath{\overline{y_{j}^{88}}}" ///
		x88r2_1stage "\ensuremath{\left(\overline{y_{j}^{88}}\right)^{2}}" ///
		x88r3_1stage "\ensuremath{\left(\overline{y_{j}^{88}}\right)^{3}}" ///
		ncas88_1stage "\ensuremath{N_{j}^{88}}" ///
		ncas88r2_1stage "\ensuremath{\left(N_{j}^{88}\right)^{2}}" ///
		x88ncas88_1stage "\ensuremath{\overline{y_{j}^{88}}\cdot N_{j}^{88}}" ///
		r2_1stage "\ensuremath{R^{2}}" ///
		ivrule2_area2_redfrm "Eligible" ///
		x88_redfrm "\ensuremath{\overline{y_{j}^{88}}}" ///
		x88r2_redfrm "\ensuremath{\left(\overline{y_{j}^{88}}\right)^{2}}" ///
		x88r3_redfrm "\ensuremath{\left(\overline{y_{j}^{88}}\right)^{3}}" ///
		ncas88_redfrm "\ensuremath{N_{j}^{88}}" ///
		ncas88r2_redfrm "\ensuremath{\left(N_{j}^{88}\right)^{2}}" ///
		x88ncas88_redfrm "\ensuremath{\overline{y_{j}^{88}}\cdot N_{j}^{88}}" ///
		r2_redfrm "\ensuremath{R^{2}}" ///
		p90hat "P-900" ///
		x88 "\ensuremath{\overline{y_{j}^{88}}}" ///
		x88r2 "\ensuremath{\left(\overline{y_{j}^{88}}\right)^{2}}" ///
		x88r3 "\ensuremath{\left(\overline{y_{j}^{88}}\right)^{3}}" ///
		ncas88 "\ensuremath{N_{j}^{88}}" ///
		ncas88r2 "\ensuremath{\left(N_{j}^{88}\right)^{2}}" ///
		x88ncas88 "\ensuremath{\overline{y_{j}^{88}}\cdot N_{j}^{88}}" ///
		r2 "\ensuremath{R^{2}}") ///
	cells(b_stack(star fmt(%9.3f) pvalue(p_stack)) se_stack(par fmt(%9.3f))) ///
	starkeep(*) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(N, fmt(%9.0g) ///
		layout("\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{N}")) ///
	mgroups("1988--1990" "1988--1992", pattern(1 0 0 1 0 0) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) ///
		span erepeat(\cmidrule(lr){@span})) ///
	nomtitles ///
	label ///
	collabels(none) ///
	legend ///
	note("Huber-White standard errors in parentheses.") ///
	title("2SLS Results for Average Gain Scores, Using Eligibility for P-900 as an Instrument\label{reg:2SLS}") ///
	substitute("footnotesize" "tiny" ///
	"                    &                     &                     &                     &                     &                     &                     \\" ///
	"\addlinespace" ///
	"Panel A: First-stage estimates (P-900)&                     &                     &                     &                     &                     &                     \\" ///
	"\multicolumn{7}{l}{\em Panel A: First-stage estimates (P-900)} \\" ///
	"Panel B: Reduced-form estimates (Average gain scores)&                     &                     &                     &                     &                     &                     \\" ///
	"\multicolumn{7}{l}{\em Panel B: Reduced-form estimates (Average gain scores)} \\" ///
	"Panel C: IV estimates (Average gain scores)&                     &                     &                     &                     &                     &                     \\" ///
	"\multicolumn{7}{l}{\em Panel C: IV estimates (Average gain scores)} \\" ///
	"\begin{table}[htbp]\centering" "\begin{table}[htbp]\centering \footnotesize") ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

local rcount = `rcount'+1
esttab reg_iv92_c3 reg_within7_2stage reg_within3_2stage using TEXDocs/reg`rcount'.tex, replace ///
	drop(_cons*) ///
	order(p90hat x88 x88r2 x88r3 ncas88 ncas88r2 x88ncas88) ///
	varlabels(p90hat "P-900" ///
		x88 "\ensuremath{\overline{y_{j}^{88}}}" ///
		x88r2 "\ensuremath{\left(\overline{y_{j}^{88}}\right)^{2}}" ///
		x88r3 "\ensuremath{\left(\overline{y_{j}^{88}}\right)^{3}}" ///
		ncas88 "\ensuremath{N_{j}^{88}}" ///
		ncas88r2 "\ensuremath{\left(N_{j}^{88}\right)^{2}}" ///
		x88ncas88 "\ensuremath{\overline{y_{j}^{88}}\cdot N_{j}^{88}}") ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	mtitles("Full Sample" "\ensuremath{\pm7} Points" "\ensuremath{\pm3} Points") ///
	label ///
	collabels(none) ///
	legend ///
	note("Huber-White standard errors in parentheses.") ///
	title("2SLS Results for 1988{--}1992 Average Gain Scores, within Narrow Bands of the Selection Threshold\label{reg:2SLSbw}") ///
	substitute(none) ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

capture log close PS3
