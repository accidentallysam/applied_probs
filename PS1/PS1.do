cd "E:\2011 Fall\Labor and Population Economics\Problem Sets\PS1"

set more off

set matsize 10000

cap log close PS1
log using PS1, replace text name(PS1)

cap which parmest
if _rc ssc install parmest
cap which eclplot
if _rc ssc install eclplot
cap which estout
if _rc ssc install estout

set scheme s1mono
graph set eps logo off
graph set eps preview off
graph set eps orientation portrait
graph set eps mag 100

use pubtwins.dta, replace
outsheet using pubtwins.txt, nolabel replace

gen age3 = age2*age
gen age4 = age3*age

gen educ2 = educ^2
gen educ_age=educ*age
gen female_age=female*age
gen female_educ=female*educ
gen white_age=white*age
gen white_educ=white*educ

recode first (.=0)

gen educd=round(educ)
tab educd

la var educ "Education"
la var educ2 "\ensuremath{\text{Educ}^2}"
la var age "Age"
la var age2 "\ensuremath{\text{Age}^{2}}"
la var age3 "\ensuremath{\text{Age}^{3}}"
la var age4 "\ensuremath{\text{Age}^{4}}"
la var female "Female"
la var white "White"
la var educ_age "\ensuremath{\text{Educ}\cdot\text{Age}}"
la var female_age "\ensuremath{\text{Female}\cdot\text{Age}}"
la var female_educ "\ensuremath{\text{Female}\cdot\text{Educ}}"
la var white_age "\ensuremath{\text{White}\cdot\text{Age}}"
la var white_educ "\ensuremath{\text{White}\cdot\text{Educ}}"
la var hrwage "Hourly Wage"
la var selfemp "Self-Employed"
la var uncov "Union Member"
la var married "Ever Married"
la var daded "Father's Educ."
la var momed "Mother's Educ."

eststo r_lw_e: reg lwage educ
predict lwage_hat
eststo r_lw_e_r: reg lwage educ, vce(robust)
mat def se_r=(_se[educ],_se[_cons])
matrix colnames se_r = educ _cons
estadd mat se_r : r_lw_e // add the robust standard errors to the OLS regression

graph tw con lwage_hat educ || scatter lwage educ, ///
	title(Log Wage vs. Education) xtitle("Education (yrs)") ytitle("Log Wages (ln $/hr)") ///
	legend(label(1 "Fitted Values") label(2 "Observed values")) ///
	name(fig1, replace) saving(Figures\fig1.gph, replace)
graph export Figures\fig1.eps, replace

eststo r_lw_eaa2fw: reg lwage educ age age2 female white
predict res_lw_eaa2fw, resid
gen res2_lw_eaa2fw=res_lw_eaa2fw^2
eststo r_lw_eaa2fw_r: reg lwage educ age age2 female white, vce(robust)
mat def se_r=(_se[educ],_se[age],_se[age2],_se[female],_se[white],_se[_cons])
matrix colnames se_r = educ age age2 female white _cons
estadd mat se_r : r_lw_eaa2fw // add the robust standard errors to the OLS regression

eststo r_lw_eaa2a3a4fw: reg lwage educ age age2 age3 age4 female white
eststo r_lw_eaa2a3a4fw_r: reg lwage educ age age2 age3 age4 female white, vce(robust)
mat def se_r=(_se[educ],_se[age],_se[age2],_se[age3],_se[age4],_se[female],_se[white],_se[_cons])
matrix colnames se_r = educ age age2 age3 age4 female white _cons
estadd mat se_r : r_lw_eaa2a3a4fw // add the robust standard errors to the OLS regression

preserve

keep if educ==12 | educ==16

label define hscomp 12 "High School" 16 "College"
label values educ hscomp

eststo mt_hrw_afw_12: estpost summarize hrwage age female white selfemp uncov married daded momed if educ==12
eststo mt_hrw_afw_16: estpost summarize hrwage age female white selfemp uncov married daded momed if educ==16
eststo mt_hrw_afw_tot: estpost summarize hrwage age female white selfemp uncov married daded momed

esttab mt_hrw_afw_12 mt_hrw_afw_16 mt_hrw_afw_tot using TEXDocs/tab1.tex, replace ///
	main("mean") aux("sd") ///
	stats(N, fmt(%9.0g) layout("\multicolumn{1}{c}{@}") labels("\ensuremath{N}")) ///
	nodepvar ///
	nonumber ///
	mtitles("High School" "College" "Total") ///
	label ///
	note("Mean values; standard deviations in parentheses.") ///
	title("Descriptive Statistics\label{tab:groupm}") ///
	substitute(none) ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

eststo tt_hrw_afw: estpost ttest hrwage age female white selfemp uncov married daded momed, by(educ)

esttab tt_hrw_afw using TEXDocs/tab2.tex, replace ///
	wide ///
	stats(N, fmt(%9.0g) layout("\multicolumn{1}{c}{@}") labels("\ensuremath{N}")) ///
	nonumber ///
	mtitle("Difference") ///
	label ///
	legend ///
	nonote ///
	addnotes("High School minus College mean values;" "\ensuremath{t}-statistics in parentheses.") ///
	title("Mean Comparison Tests\label{tab:meant}") ///
	substitute("l*{1}" "l*{3}") ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

restore

eststo r_hrw_d: xi, noomit: regress hrwage i.educd, nocons
parmest, saving(parmydh, replace)

eststo r_lw_d: xi, noomit: regress lwage i.educd, nocons
parmest, saving(parmydl, replace)

eststo r_lw_daa2fw: xi, noomit: regress lwage i.educd age age2 female white, nocons
eststo r_lw_daa2fw_r: xi, noomit: regress lwage i.educd age age2 female white, nocons vce(robust)
mat def se_r=(_se[age],_se[age2],_se[female],_se[white])
matrix colnames se_r = age age2 female white
estadd mat se_r : r_lw_daa2fw // add the robust standard errors to the OLS regression

eststo r_hrw_eaa2fw: reg hrwage educ age age2 female white
predict res_hrw_eaa2fw, resid
gen res2_hrw_eaa2fw=res_hrw_eaa2fw^2
eststo r_hrw_eaa2fw_r: reg hrwage educ age age2 female white, vce(robust)
mat def se_r=(_se[educ],_se[age],_se[age2],_se[female],_se[white],_se[_cons])
matrix colnames se_r = educ age age2 female white _cons
estadd mat se_r : r_hrw_eaa2fw // add the robust standard errors to the OLS regression

eststo r_hrw_daa2fw: xi, noomit: regress hrwage i.educd age age2 female white, nocons
eststo r_hrw_daa2fw_r: xi, noomit: regress hrwage i.educd age age2 female white, nocons vce(robust)
mat def se_r=(_se[age],_se[age2],_se[female],_se[white])
matrix colnames se_r = age age2 female white
estadd mat se_r : r_hrw_daa2fw // add the robust standard errors to the OLS regression

// Regression Table for:
//   lwage  on educ
//   lwage  on educ age age2 female white
//   lwage  on educ age age2 age3 age4 female white
//   lwage  on i.educd age age2 female white
//   hrwage on educ age age2 female white
//   hrwage on i.educd age age2 female white
xi, noomit: esttab r_lw_e r_lw_eaa2fw r_lw_eaa2a3a4fw r_lw_daa2fw r_hrw_eaa2fw r_hrw_daa2fw using TEXDocs/reg1.tex, replace ///
	order(_cons educ i.educd age age2 age3 age4 female white) ///
	varlabels(_cons "Constant") ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) se_r(par([ ]) fmt(%9.3f))) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	mgroups("Log Wages" "Level Wages", pattern(1 0 0 0 1 0) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) ///
		span erepeat(\cmidrule(lr){@span})) ///
	indicate(Educ. Dummies = _Ieducd*, labels("\multicolumn{1}{c}{\ensuremath{\text{Yes}}}" ///
		"\multicolumn{1}{c}{\ensuremath{\text{No}}}")) ///
	nomtitles ///
	label ///
	collabels(none) ///
	legend ///
	note("Standard errors in parentheses; robust standard errors in brackets.") ///
	title("OLS Estimates of Wage Equations\label{reg:ols}") ///
	substitute({table} {sidewaystable}) ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

preserve
use parmydh, clear
gen numparm = real(word(subinstr(parm,"_"," ",.), -1))
label variable numparm "Education (yrs)"
label variable estimate "Return to Hourly Wages ($/hr)"
eclplot estimate min95 max95 numparm, ///
	eplottype(scatter) rplottype(rcap) ///
	estopts(sort) ciopts() ///
	title(Return to Wage vs. Education) ///
	/*plot(lfit estimate numparm, lp(dash) || qfit estimate numparm)*/ ///
	legend(on label(1 "95% Confidence Intervals") label(2 "Coefficients")) ///
	name(fig2, replace) saving(Figures\fig2.gph, replace)
graph export Figures\fig2.eps, replace
restore

preserve
use parmydl, clear
gen numparm = real(word(subinstr(parm,"_"," ",.), -1))
label variable numparm "Education (yrs)"
label variable estimate "Return to Log Wages (ln $/hr)"
eclplot estimate min95 max95 numparm, ///
	eplottype(scatter) rplottype(rcap) ///
	estopts(sort) ciopts() ///
	title(Return to Log Wage vs. Education) ///
	/*plot(lfit estimate numparm, lp(dash) || qfit estimate numparm)*/ ///
	plot(line estimate numparm if numparm<12, lp(dash) || ///
		line estimate numparm if numparm>=12 & numparm<16, lp(dash) || ///
		line estimate numparm if numparm>=16, lp(dash)) ///
	legend(on ///
		order(1 2 - "Piecewise Line Plots:" - " " 3 4 5) ///
		label(1 "95% Confidence Intervals") ///
			label(2 "Coefficients") ///
			label(3 "Less Than High School") ///
			label(4 "Between H.S. and College") ///
			label(5 "Greater Than College")) ///
	name(fig3, replace) saving(Figures\fig3.gph, replace)
graph export Figures\fig3.eps, replace
restore

scatter hrwage educ, ///
	title(Hourly Wage vs. Education) xtitle("Education (yrs)") ytitle("Hourly Wages ($/hr)") ///
	name(fig4, replace) saving(Figures\fig4.gph, replace)
graph export Figures\fig4.eps, replace


eststo r_res2_lw_eaa2fw: reg res2_lw_eaa2fw educ age age2 female white
eststo r_res2_hrw_eaa2fw: reg res2_hrw_eaa2fw educ age age2 female white
eststo r_res2_lw_eaa2fwi: reg res2_lw_eaa2fw educ educ2 age age2 female white educ_age female_age female_educ white_age white_educ
eststo r_res2_hrw_eaa2fwi: reg res2_hrw_eaa2fw educ educ2 age age2 female white educ_age female_age female_educ white_age white_educ

// Regression Table for:
//   res2_lw_eaa2fw  on educ age age2 female white
//   res2_lw_eaa2fw  on educ educ2 age age2 female white educ_age female_age female_educ white_age white_educ
//   res2_hrw_eaa2fw on educ age age2 female white
//   res2_hrw_eaa2fw on educ educ2 age age2 female white educ_age female_age female_educ white_age white_educ
esttab r_res2_lw_eaa2fw r_res2_lw_eaa2fwi r_res2_hrw_eaa2fw r_res2_hrw_eaa2fwi using TEXDocs/reg2.tex, replace ///
	order(_cons educ educ2 age age2 female white educ_age female_age female_educ white_age white_educ) ///
	varlabels(_cons "Constant") ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	mgroups("Log Wages" "Level Wages", pattern(1 0 1 0) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) ///
		span erepeat(\cmidrule(lr){@span})) ///
	nomtitles ///
	label ///
	collabels(none) ///
	legend ///
	note("Standard errors in parentheses.") ///
	title("OLS Estimates of Residual Wage Equations\label{reg:ols_res}") ///
	substitute(none) ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)


// generate id's for each twin pair
gen id=_n
replace id=id/2
replace id=round(id,1)

eststo r_lw_e_c: reg lwage educ, vce(cluster id)
mat def se_c=(_se[educ],_se[_cons])
matrix colnames se_c = educ _cons
estadd mat se_c : r_lw_e // add the cluster standard errors to the OLS regression

eststo r_lw_eaa2fw_c: reg lwage educ age age2 female white, vce(cluster id)
mat def se_c=(_se[educ],_se[age],_se[age2],_se[female],_se[white],_se[_cons])
matrix colnames se_c = educ age age2 female white _cons
estadd mat se_c : r_lw_eaa2fw // add the cluster standard errors to the OLS regression

// generate mean values for each twin pair
egen lwage_m=mean(lwage), by(id)
egen educ_m=mean(educ), by(id)

eststo r_lw_e_m: reg lwage_m educ_m if first==1
eststo r_lw_e_m_r: reg lwage_m educ_m if first==1, vce(robust)
mat def se_r=(_se[educ_m],_se[_cons])
matrix colnames se_r = educ_m _cons
estadd mat se_r : r_lw_e_m // add the robust standard errors to the OLS regression

eststo r_lw_eaa2fw_m: reg lwage_m educ_m age age2 female white if first==1
eststo r_lw_eaa2fw_m_r: reg lwage_m educ_m age age2 female white if first==1, vce(robust)
mat def se_r=(_se[educ_m],_se[age],_se[age2],_se[female],_se[white],_se[_cons])
matrix colnames se_r = educ_m age age2 female white _cons 
estadd mat se_r : r_lw_eaa2fw_m // add the robust standard errors to the OLS regression

// Regression Table for:
//   lwage_m on educ_m
//   lwage   on educ age age2 female white
//   lwage_m on educ_m
//   lwage_m on educ_m age age2 female white
esttab r_lw_e r_lw_eaa2fw r_lw_e_m r_lw_eaa2fw_m using TEXDocs/reg3.tex, replace ///
	order(_cons educ age age2 female white) ///
	rename(educ_m educ) ///
	varlabels(_cons "Constant") ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) se_r(par([ ]) fmt(%9.3f)) se_c(par(\{ \}) fmt(%9.3f))) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	mgroups("Log Wages" "\ensuremath{\overline{\text{Log Wages}}}", pattern(1 0 1 0) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) ///
		span erepeat(\cmidrule(lr){@span})) ///
	nomtitles ///
	label ///
	collabels(none) ///
	legend ///
	addnotes("Standard errors in parentheses; robust standard errors in brackets;" "clustered standard errors in braces.") ///
	title("Robust OLS Estimates of Wage Equations\label{reg:ols_robust}") ///
	substitute(none) ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

eststo r_lw_eaa2fw_iv: ivreg lwage age age2 female white (educ = educt_t)
eststo r_lw_eaa2fw_iv_r: ivreg lwage age age2 female white (educ = educt_t), robust
mat def se_r=(_se[educ],_se[age],_se[age2],_se[female],_se[white],_se[_cons])
matrix colnames se_r = educ age age2 female white _cons
estadd mat se_r : r_lw_eaa2fw_iv // add the robust standard errors to the 2SLS regression
eststo r_lw_eaa2fw_iv_c: ivreg lwage age age2 female white (educ = educt_t), cluster(id)
mat def se_c=(_se[educ],_se[age],_se[age2],_se[female],_se[white],_se[_cons])
matrix colnames se_c = educ age age2 female white _cons
estadd mat se_c : r_lw_eaa2fw_iv // add the cluster standard errors to the 2SLS regression

eststo r_dlw_de: reg dlwage deduc if first==1, noconstant
eststo r_dlw_de_r: reg dlwage deduc if first==1, noconstant vce(robust)
mat def se_r=(_se[deduc])
matrix colnames se_r = deduc
estadd mat se_r : r_dlw_de // add the robust standard errors to the OLS regression

eststo r_dlw_de_iv: ivreg dlwage (deduc = deduct) if first==1, noconstant
eststo r_dlw_de_iv_r: ivreg dlwage (deduc = deduct) if first==1, noconstant robust
mat def se_r=(_se[deduc])
matrix colnames se_r = deduc
estadd mat se_r : r_dlw_de_iv // add the robust standard errors to the 2SLS regression

// Regression Table for:
//   lwage  on educ age age2 female white
//   lwage  on educ age age2 female white, 2SLS
//   dlwage on deduc
//   dlwage on deduc, 2SLS
esttab r_lw_eaa2fw r_lw_eaa2fw_iv r_dlw_de r_dlw_de_iv using TEXDocs/reg4.tex, replace ///
	order(_cons educ age age2 female white) ///
	rename(deduc educ) ///
	varlabels(_cons "Constant" educ "Education" age "Age" age2 "\ensuremath{\text{Age}^{2}}" female "Female" white "White") ///
	mgroups(none) ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) se_r(par([ ]) fmt(%9.3f)) se_c(par(\{ \}) fmt(%9.3f))) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	mtitles("OLS" "2SLS" "\ensuremath{1^{\textrm{st}}}-Diff." "\ensuremath{1^{\textrm{st}}}-Diff. 2SLS") ///
	collabels(none) ///
	legend ///
	addnotes("Own-reports of education instrumented for with twin-reports." "Standard errors in parentheses; robust standard errors in brackets;" "clustered standard errors in braces.") ///
	title("OLS, 2SLS, and \ensuremath{1^{\textrm{st}}}-Diff. Estimates of Log Wage Equations\label{reg:ols_2SLS_Diff}") ///
	substitute(none) ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

scatter dlwage deduc if first==1, ///
	title(Difference in Log Wage vs. Difference in Education) xtitle("Difference in Education (yrs)") ytitle("Difference in Log Hourly Wages (ln $/hr)") ///
	name(fig5, replace) saving(Figures\fig5.gph, replace)
graph export Figures\fig5.eps, replace

eststo clear


use restricted92.dta, replace
outsheet using restricted92.txt, nolabel replace

eststo r_lw_eee2fmc: reg lnw ed exp exp2 female mar computer
eststo r_lw_eee2fmc_r: reg lnw ed exp exp2 female mar computer, vce(robust)
mat def se_r=(_se[ed],_se[exp],_se[exp2],_se[female],_se[mar],_se[computer],_se[_cons])
matrix colnames se_r = ed exp exp2 female mar computer _cons
estadd mat se_r : r_lw_eee2fmc // add the robust standard errors to the OLS regression

eststo r_lw_eee2fmcptch: reg lnw ed exp exp2 female mar computer pencil telefon calc hammer
eststo r_lw_eee2fmcptch_r: reg lnw ed exp exp2 female mar computer pencil telefon calc hammer, vce(robust)
mat def se_r=(_se[ed],_se[exp],_se[exp2],_se[female],_se[mar],_se[computer],_se[pencil],_se[telefon],_se[calc],_se[hammer],_se[_cons])
matrix colnames se_r = ed exp exp2 female mar computer pencil telefon calc hammer _cons
estadd mat se_r : r_lw_eee2fmcptch // add the robust standard errors to the OLS regression

eststo r_lw_eee2fmcptchd: xi, noomit: reg lnw ed exp exp2 female mar computer pencil telefon calc hammer i.occ, nocons
eststo r_lw_eee2fmcptchd_r: xi, noomit: reg lnw ed exp exp2 female mar computer pencil telefon calc hammer i.occ, nocons vce(robust)
mat def se_r=(_se[ed],_se[exp],_se[exp2],_se[female],_se[mar],_se[computer],_se[pencil],_se[telefon],_se[calc],_se[hammer])
matrix colnames se_r = ed exp exp2 female mar computer pencil telefon calc hammer
estadd mat se_r : r_lw_eee2fmcptchd // add the robust standard errors to the OLS regression

la var ed "Education"
la var exp "Experience"
la var exp2 "\ensuremath{\text{Experience}^{2}}"
la var female "Female"
la var mar "Married"
la var computer "Computer"
la var pencil "Pencil"
la var telefon "Telephone"
la var calc "Calculator"
la var hammer "Hammer"

// Regression Table for:
//   lnw on ed exp exp2 female mar computer
//   lnw on ed exp exp2 female mar computer pencil telefon calc hammer
//   lnw on ed exp exp2 female mar computer pencil telefon calc hammer i.occ
xi, noomit: esttab r_lw_eee2fmc r_lw_eee2fmcptch r_lw_eee2fmcptchd using TEXDocs/reg5.tex, replace ///
	order(_cons ed exp exp2 female mar computer pencil telefon calc hammer i.occ) ///
	varlabels(_cons "Constant") ///
	cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) se_r(par([ ]) fmt(%9.3f))) ///
	stats(r2 F N, fmt(%9.3f %9.3f %9.0g) ///
		layout("@" "@" "\multicolumn{1}{c}{@}") ///
		labels("\ensuremath{R^2}" "\ensuremath{F}" "\ensuremath{N}")) ///
	indicate(Occ. Dummies = _Iocc*, labels("\multicolumn{1}{c}{\ensuremath{\text{Yes}}}" ///
		"\multicolumn{1}{c}{\ensuremath{\text{No}}}")) ///
	nomtitles ///
	label ///
	collabels(none) ///
	legend ///
	note("Standard errors in parentheses; robust standard errors in brackets.") ///
	title("OLS Estimates of Wage Equations\label{reg:ols_comp}") ///
	substitute(none) ///
	alignment(D{.}{.}{-1}) ///
	booktabs ///
	style(tex)

log close PS1
