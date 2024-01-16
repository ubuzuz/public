

/*********************************************
****************Dataset links****************
Does not need to download to run this code.
Most of webpages do not provide English version.

The Labor Force Survey at Establishments (LFSE; 사업체노동력조사)
http://laborstat.moel.go.kr (opened to public)

Employment Permit System (EPS; 고용허가제고용동향)
https://www.open.go.kr/ (opened to Korean citizen)

Monthly Survey of Mining And Manufacturing (MSMM; 광업제조업 동향조사)
https://kosis.kr/ (opened to public)

Economically Active Population Survey (EAPS; 경제활동인구조사)
https://mdis.kostat.go.kr/ (opened to Korean citizen)

Employment Information System (EIS; 고용보험통계)
https://eis.work.go.kr/ (opened to public)

Korean Labor and Income Panel Study (KLIPS; 한국노동패널)
https://www.kli.re.kr/klips/ (opened to public; required to sign up)

Worknet Job Search Trend (워크넷 구인구직)
https://eis.work.go.kr/ (opened to public)

Minimum Wage Trend (최저임금위원회 최저임금제도)
https://www.minimumwage.go.kr/minWage/policy/decisionMain.do (opened to public)

Korea Immigration Service Monthly Statistics (출입국외국인정책 통계월보)
https://www.immigration.go.kr/immigration/1569/subview.do (opened to public)

Survey on Immigrant's Living Conditions and Labour Force (이민자체류실태및고용조사)
https://mdis.kostat.go.kr/ (opened to Korean citizen)

Currency Exchange Rate (원달러환율)
https://www.index.go.kr/potal/main/EachDtlPageDetail.do?idx_cd=1068 (opened to public)

*********************************************/

** LScode ver11.0.do
cls
clear all
set scheme s1color, perm 

/*********************************************
*********************************************/
* NEED TO SET YOUR PREFERRED PATH
global path="E:\Dropbox\Study\UC Davis\Writings\Labor Shortage\210718\Github move\Latex\Dissertation Draft ver11.0"   
/*********************************************
*********************************************/
cd "${path}"

/*********************************************
Required programs
*********************************************/
net install Jay_ado.pkg, from(https://raw.githubusercontent.com/jayjeo/public/master/adofiles)
copy "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/X12A.EXE" "${path}/X12A.exe"
net install st0255, from(http://www.stata-journal.com/software/sj12-2)
adopath + "${path}"
cap ado uninstall ftools
net install ftools, from("https://raw.githubusercontent.com/sergiocorreia/ftools/master/src/")
cap ado uninstall reghdfe
net install reghdfe, from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/")
cap ado uninstall ivreg2
ssc install ivreg2, replace
cap ado uninstall ivreghdfe
net install ivreghdfe, from("https://raw.githubusercontent.com/sergiocorreia/ivreghdfe/master/src/")
ssc install ranktest, replace
*To completely uninstall the files
*ado uninstall filename



/*********************************************
Graphs generation
*********************************************/
*!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/minwage.csv", clear 
tsset ym
format ym %tm
gen minwagereal=minwage*100/cpi
save minwage, replace

cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/worknet_parttime.csv", clear 
tsset ym
format ym %tm
gen partpercent=worknet_parttime/worknet_total*100
gen partpercent_high=worknet_parttime_high/worknet_total_high*100  // below tertiary
gen partpercent_occ8=worknet_parttime_occ8/worknet_total_occ8*100  // among occ=8 (manufacture occupation)
replace partpercent=partpercent*0.883765357084955 if ym>=734
replace partpercent_high=partpercent_high*0.883765357084955 if ym>=734
replace partpercent_occ8=partpercent_occ8*0.883765357084955 if ym>=734

merge 1:1 ym using minwage, nogenerate

twoway (tsline minwagereal, lwidth(thick) lcolor(gs0) yaxis(1)) /// 
        (tsline partpercent, lcolor(gs0) yaxis(2)) ///
        (tsline partpercent_high, lcolor(gs0) clpattern(longdash) yaxis(2)) ///
        (tsline partpercent_occ8, lcolor(gs0) clpattern(shortdash) yaxis(2)) ///
    , xtitle("") ytitle("") xline(720) ysize(1) xsize(3) xlabel(624(12)744) ylabel(4(4)14, axis(2)) ///
    caption("Source: Worknet Job Search Trend (Korea Employment Information Service)" "              Minimum Wage Trend (Minimum Wage Commission)") ///
    legend(label(1 "Minimun Wage") label(2 "Total Seekers") label(3 "Below Tertiary") label(4 "Occupation=8") ) 
graph export partpercent.eps, replace


*********************
*!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/monthlye9.csv", clear 
gen date=ym(year,month)
tsset date
format date %tm

tsfilter hp e9inflow_hp = e9inflow, trend(smooth_e9inflow) smooth(6)
keep if date>=648
replace e9stock=e9stock/1000
replace smooth_e9inflow=smooth_e9inflow/1000

twoway (tsline smooth_e9inflow, lcolor(gs0))(tsline e9stock, lwidth(thick) lcolor(gs0) yaxis(2)) ///
, xlabel(648(6)744) xlabel(, grid angle(270)) ylabel(0(50)200, axis(2)) xline(720) ytitle("a thousand person", axis(1)) ytitle("a thousand person", axis(2)) scheme(s1mono) ///
ysize(3.5) xsize(8) ///
legend(label(1 "E9 inflow (left)") label(2 "E9 stock (right)")) ///
caption("Source: Employment Permit System (EPS)")
graph export monthlye9.eps, replace


*********************
*!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/uib.csv", varnames(1) clear 
gen ym=t+592
format ym %tm
tsset ym
gen uib_adj=uib
*replace uib_adj=uib*0.7 if ym>=720  

gen recession1=0
replace recession1=1 if 717<=ym   //2019m10
gen recession2=0
replace recession2=1 if 719<=ym
gen recession3=0
replace recession3=1 if 721<=ym
gen recession4=0
replace recession4=1 if 724<=ym

reg ut uib recession1-recession4
predict uib_p
replace uib_p=uib if ym<717
twoway (tsline ut, lcolor(gs0))(tsline uib, lcolor(red))(tsline uib_p, lcolor(blue) clpattern(longdash)) ///
    , xtitle("") ytitle("%") xline(720) /// 
    ysize(3.5) xsize(8) ///
    legend(label(1 "Unemployment rate") label(2 "Unemployment Insurance Benefit") label(3 "Unemployment Insurance Benefit (adjusted)") order(1 2 3))
graph export uib.eps, replace


*********************
*!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/SVARdata.csv", clear 
gen month=_n+623
tsset month 
format month %tm

gen forpercent=fw/(fw+dw)*100
keep if month >= 648 

twoway (tsline forpercent, lcolor(gs0)) ///
, xlabel(648(6)743) ylabel(0(2)11) xlabel(, grid angle(270)) xline(720) xtitle("") ytitle("%") scheme(s1mono) ///
ysize(3) xsize(8) legend(off) ///
caption("Source: Korea Immigration Service Monthly Statistics & Survey on Immigrant's Living Conditions and Labour Force")
graph export forpercent.eps, replace


*********************
*!start
***** Need to manually download dataset from https://www.dropbox.com/s/gpy3ekn6w3ve8np/cps.dta
cd "${path}"
use cps, clear
gen date= ym(real(substr(string(infile),1,4)), real(substr(string(infile),5,2)))
save cpsdate, replace 

use cpsdate, clear
keep if 50<=age&age<80
keep if retired==1
collapse (sum) num [pweight=wgt], by(date)
tsset date
lowess num date if date<=720, gen(lowessnum2)
lowess num date, gen(lowessnum3)
ipolate lowessnum2 date, gen(lowessnum) epolate
gen er=lowessnum3/lowessnum
replace er=1 if date<720
*twoway (tsline num)(tsline lowessnum)(tsline lowessnum3 if date>720)
gen state=0
save er0, replace 

use er0, clear
format date %tm
replace num=num/1000/1000  // 1 thousand persons
replace lowessnum=lowessnum/1000/1000  // 1 thousand persons
replace lowessnum3=lowessnum3/1000/1000  // 1 thousand persons
twoway (tsline num, lcolor(gs0))(tsline lowessnum, lcolor(gs0) lwidth(thick))(tsline lowessnum3 if date>720, lwidth(thick) lcolor(gs0)) ///
    , xtitle("") xline(720) ysize(1) xsize(1.5) xlabel(600(24)744) ///
    ytitle("Million persons") ///
    legend(label(1 "Number of retired people") label(2 "Trend") order(1 2))
graph export Excessretire.eps, replace

***** Another more accurate method. 
cd "${path}"
use cpsdate, clear
keep if 20<=age&age<80
*tab date 
gen agecat=0
replace agecat=1 if 20<=age&age<25
replace agecat=2 if 25<=age&age<30
replace agecat=3 if 30<=age&age<35
replace agecat=4 if 35<=age&age<40
replace agecat=5 if 40<=age&age<45
replace agecat=6 if 45<=age&age<50
replace agecat=7 if 50<=age&age<55
replace agecat=8 if 55<=age&age<60
replace agecat=9 if 60<=age&age<65
replace agecat=10 if 65<=age&age<70
replace agecat=11 if 70<=age&age<75
replace agecat=12 if 75<=age&age<80
save cpsagecat, replace 

use cpsagecat, clear 
keep if 708<=date&date<=719 // 2019m1~2019m12
collapse (sum) num [pweight=wgt], by(agecat retired)
reshape wide num, i(agecat) j(retired) 
keep if 7<=agecat
gen prob=num1/(num0+num1)
keep agecat prob 
save prob, replace 

cd "${path}"
use cpsagecat, clear
keep if 7<=agecat
keep if 719<date
collapse (sum) num [pweight=wgt], by(agecat date)
reshape wide num, i(agecat) j(date) 
merge 1:1 agecat using prob, nogenerate
forvalues i=720(1)743 {
    gen retiredestimate`i'=num`i'*prob
}
keep agecat retiredestimate*
reshape long retiredestimate, i(agecat) j(date) 
collapse (sum) retiredestimate, by(date)
save retiredestimate, replace 

use cpsagecat, clear
keep if 7<=agecat
keep if retired==1
collapse (sum) num [pweight=wgt], by(date)
merge 1:1 date using retiredestimate, nogenerate
tsset date 
format date %tm
replace num=num/1000/1000  // 1 thousand persons
replace retiredestimate=retiredestimate/1000/1000  // 1 thousand persons
lowess num date, gen(lowessnum)
twoway (tsline num, lcolor(gs0))(tsline lowessnum, lcolor(gs0) lwidth(thick))(tsline retiredestimate, lcolor(gs0) lwidth(thick)) ///
    , xtitle("") xline(720) ysize(1) xsize(1.5) xlabel(600(24)744) ///
    ytitle("Million persons") ///
    legend(label(1 "Number of retired people") label(2 "Estimated retired people") order(1 2))
graph export Excessretire_est.eps, replace


*********************
*!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/unauthorizedshare.csv", varnames(1) clear 
twoway (connected share time, lcolor(gs0) lwidth(med) mcolor(gs0)) ///
, xlabel(1"y99" 2"y00" 3"y01" 4"y02" 5"y03" 6"y04" 7"y05" 8"y06" 9"y07" 10"y08" 11"y09" 12"y10" 13"y11" 14"y12" 15"y13" 16"y14" 17"y15" 18"y16" 19"y17" 20"y18" 21"y19" 22"y20" 23"y21" 24"y22m3") ///
xtitle("") ytitle("%") ysize(1) xsize(3) ymtick(#20, grid tstyle(none))
graph export unauthorizedshare.eps, replace



/*********************************************
Data Merge
*********************************************/
//!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/orig.csv", varnames(1) clear 
keep if ym==648
rename numd numd648
keep indmc ym numd648
save numd648, replace 

//!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/u.csv", varnames(1) clear 
        // E:\Dropbox\Study\UC Davis\Writings\Labor Shortage\210718\경제활동인구조사\rawdata\infile3 (2015~2017추가).do   =>  nonuC
rename nonuc ut
replace ut=ut
rename uc uC
gen indmc=0
save ut, replace 

use ut, clear
//!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/e9inflow.csv", varnames(1) clear 
destring *, replace dpcomma
reshape long ym, i(indmc) j(j)
rename ym e9inflow
rename j ym
save e9inflow, replace 

//!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/cpi.csv", varnames(1) clear 
save cpi, replace

//!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/exchangerate.csv", varnames(1) clear 
save exchangerate, replace

//!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/totalforeignproportion.csv", varnames(1) clear 
save forper, replace 

//!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/orig.csv", varnames(1) clear 
merge 1:1 ym indmc using ut, nogenerate
merge m:1 ym using cpi, nogenerate
merge m:1 ym using exchangerate, nogenerate
merge 1:1 ym indmc using e9inflow, nogenerate
merge m:1 indmc using forper, nogenerate
merge m:1 indmc using numd648, nogenerate
merge m:1 indmc using forper_jikjong, nogenerate
merge m:1 indmc using forper_TFWshare, nogenerate
merge m:1 indmc using forper_TFWchg, nogenerate

xtset indmc ym   // indmc = sub-sector of manufacturing industry. ; ym = monthly time.
format ym %tm
gen ymraw=ym

rename (nume numd exit numefull numdfull numepart numdpart) (numE numD EXIT numEfull numDfull numEpart numDpart)  
// numE = number of vacant spots ; numD = number of workers ; EXIT = number of separated workers
gen v=numE/numD*100   // v = vacancy rate
gen vfull=numEfull/numDfull*100   // v = vacancy rate (full-time workers)
gen vpart=numEpart/numDpart*100   // v = vacancy rate (part-time workers)

gen uibCC=uib/numD*100
gen uibOriginal=uib/numD*100*0.896503381
*gen uibCC=uib/numD*100*0.896503381 if ym<720
*replace uibCC=uib/numD*100*0.63 if ym>=720

gen wage=wage_tot*100/cpi/hour/exchangerate  // cpi adjusted hourly wage (unit=USD)
gen wagefull=wage_totfull*100/cpi/hourfull/exchangerate  // cpi adjusted hourly wage (unit=USD)
gen wagepart=wage_totpart*100/cpi/hourpart/exchangerate  // cpi adjusted hourly wage (unit=USD)

gen uibmoney2=uibmoney/numd648/cpi/exchangerate
drop uibmoney
rename uibmoney2 uibmoney

drop if inlist(indmc,12)  // tobacco industry. Extremely few workers, and production data is not available.
sort indmc ym
keep if 648<=ym&ym<=747   // largest available data span.

gen Break1=0
replace Break1=1 if ym>=717
gen Break2=0
replace Break2=1 if ym>=718
gen Break3=0
replace Break3=1 if ym>=719
gen Break4=0
replace Break4=1 if ym>=720
gen Break5=0
replace Break5=1 if ym>=721
gen recession1=0
replace recession1=1 if 668<=ym
gen recession2=0
replace recession2=1 if ym<=672
gen recession3=0
replace recession3=1 if ym<=677
gen recession4=0
replace recession4=1 if 699<=ym
gen recession5=0
replace recession5=1 if ym<=710

gen months=month(dofm(ym))
tabulate months, generate(tau)
gen quarters=quarter(dofm(ym))
tabulate quarters, generate(rho)
save panelm_uib, replace 

use panelm_uib, clear
xtset ym indmc
reg ut uibCC recession1-recession5 rho1-rho4 tau2-tau12 if indmc==0
predict uibC
keep indmc ym uibC
save uibC_master, replace 

use panelm_uib, clear
merge 1:1 indmc ym using uibC_master, nogenerate
save panelm, replace 

/*
use panelm, clear 
keep if indmc==0
twoway (tsline uibC)(tsline ut, lwidth(thick))

tsset ym 
gen uibCCC=uib/numD*100
sax12 ut, satype(single) inpref(ut.spc) outpref(ut) transfunc(log) regpre( const ) ammodel((0,1,1)(0,1,1)) ammaxlead(0) x11mode(mult) x11seas(S3x9)
sax12im "ut.out", ext(d11)
twoway (tsline uibCCC uibCC)(tsline uibC, lwidth(thick))(tsline ut_d11, lcolor(red) lwidth(thick)), xline(720)

use panelm, clear 
twoway (tsline uibC if indmc==0)(tsline uibC if indmc==10)(tsline uibC if indmc==11)(tsline uibC if indmc==13)(tsline uibC if indmc==14)(tsline uibC if indmc==15)(tsline uibC if indmc==16)(tsline uibC if indmc==17)(tsline uibC if indmc==18)(tsline uibC if indmc==19)(tsline uibC if indmc==20)(tsline uibC if indmc==30)
*/


*!start
cd "${path}"
use panelm, clear
keep ym indmc numD e9 hourfull numE
reshape wide numD e9 hourfull numE, i(indmc) j(ym)

** 719=2019m12; 722=2020m3; 724=2020m5; 739=2021m8

gen vchg=(numE744-numE715)/numD715*100
gen e9chg=(e9744-e9715)/numD715*100
gen e9share=e9715/numD715*100
gen e9share684=e9684/numD684*100
gen e9share678=e9678/numD678*100
gen e9share660=e9660/numD660*100
gen e9share740=e9740/numD740*100

keep indmc vchg e9chg e9share e9share684 e9share678 e9share660 e9share740 hourfull716
save chg, replace 

use panelm, clear
merge m:1 indmc using chg, nogenerate
sort indmc ym 
gen numDpartproportion=numDpart/numDfull*100
gen l=numD/(1-uibC/100)
label var v "Vacancy" 
label var vfull "Vacancy(Full)" 
label var vpart "Vacancy(Part)"
label var hour "Work Hours" 
label var wage "Wage" 
label var wagefull "Wage(Full)" 
label var wagepart "Wage(Part)" 
label var numDpartproportion "Part/Full" 
label var uibC "Non-emloyment rate" 
label var prod "Production"
label var proddome "ProdDomestic"
label var prodabroad "ProdAbroad"
label var prodoper "ProdOperation" 
save panelf2_temp2, replace 

use panelf2_temp2, clear
keep ym indmc numD
keep if 713<=ym&ym<=719
collapse (mean) numD, by(indmc)
rename numD numDbefore
save numDbefore, replace 

use panelf2_temp2, clear
keep ym indmc numDfull
keep if 713<=ym&ym<=719
collapse (mean) numDfull, by(indmc)
rename numDfull numDfullbefore
save numDfullbefore, replace 

use panelf2_temp2, clear
merge m:1 indmc using numDbefore, nogenerate
merge m:1 indmc using numDfullbefore, nogenerate
gen v_alter=numE/numDbefore*100
replace v_alter=v if ym<720
gen vfull_alter=numEfull/numDfullbefore*100
replace vfull_alter=vfull if ym<720
label var v_alter "Vacancy(alter)"  
label var vfull_alter "Vacancy(Full,alter)"  
gen theta=v/uibC
gen theta_alter=v_alter/uibC
label var theta "Tightness" 
label var theta_alter "Tightness(alter)" 
save panelf3_temp4, replace 


/*********************************************
Deseasonalize by using seasonal dummy 
*********************************************/
use panelf3_temp4, clear
foreach i of numlist 0 10 11 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 {
    preserve
        keep if indmc==`i'
        tsset ym
        reg v tau2-tau12 rho1-rho4
        predict v_p, residuals
        replace v_p=v_p+_b[_cons]
        reg vpart tau2-tau12 rho1-rho4
        predict vpart_p, residuals
        replace vpart_p=v_p+_b[_cons]
        reg vfull tau2-tau12 rho1-rho4
        predict vfull_p, residuals
        replace vfull_p=v_p+_b[_cons]
        reg wage tau2-tau12
        predict wage_p, residuals
        replace wage_p=wage_p+_b[_cons]
        reg wagefull tau2-tau12
        predict wagefull_p, residuals
        replace wagefull_p=wagefull_p+_b[_cons]
        reg wagepart tau2-tau12
        predict wagepart_p, residuals
        replace wagepart_p=wagepart_p+_b[_cons]
        reg hour tau2-tau12
        predict hour_p, residuals
        replace hour_p=hour_p+_b[_cons]
        reg hourfull tau2-tau12
        predict hourfull_p, residuals
        replace hourfull_p=hourfull_p+_b[_cons]
        reg hourpart tau2-tau12
        predict hourpart_p, residuals
        replace hourpart_p=hourpart_p+_b[_cons]
        drop v vpart vfull wage wagefull wagepart hour hourfull hourpart
        rename (v_p vpart_p vfull_p wage_p wagefull_p wagepart_p hour_p hourfull_p hourpart_p)(v vpart vfull wage wagefull wagepart hour hourfull hourpart)
        save panelf3_temp5_seasonal`i', replace 
    restore
}
use panelf3_temp5_seasonal0, clear
foreach i of numlist 10 11 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 {
    append using panelf3_temp5_seasonal`i'
}
save panelf3_temp5_seasonal, replace 

use panelf3_temp5, clear  
drop lambda wage wagefull wagepart hour hourfull hourpart
//drop v vfull vpart v_alter theta theta_alter wage wagefull wagepart hour hourfull hourpart
merge 1:1 indmc ym using panelf3_temp5_seasonal, nogenerate
save panelf3, replace 


/*********************************************
DID Regressions
*********************************************/
*!start
cd "${path}"
use panelf3, clear
xtset indmc ym
gen Luibmoney=L.uibmoney
drop if indmc==0    // information for total manufacturing sectors. 
//drop if indmc==32|indmc==16  // too much fluctuations
drop if indmc==19  // too few observations
gen d=0 if  708<=ym&ym<=719  // 684<=ym&ym<=719 // inlist(ym,712,713,714,715,716,717,718,719) 
replace d=1 if 725<=ym&ym<=745 // inlist(ym,738,739,740,741,742,743,744)
drop if d==.

gen forperd=forper*d
gen e9shared=e9share*d
drop e9share684
rename forper114 e9share684
gen e9share684d=e9share684*d
gen e9chgd=e9chg*d
label var d "T" 
label var e9shared "E9SHARE $\times$ D" 
label var e9share684d "E9SHARE $\times$ D" 
label var e9chgd "E9CHG $\times$ D" 
label var Luibmoney "UIB" 
label var wagefull "Wage(Full)" 
label var hourfull "Hour(Full)" 

******* Reduced form
eststo clear 
eststo: xtreg theta e9share684d Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)
eststo: xtreg v e9share684d Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)
eststo: xtreg vfull e9share684d Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)
eststo: xtreg vpart e9share684d Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)
eststo: xtreg numDpartproportion e9share684d Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)
eststo: xtreg wagefull e9share684d Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)
eststo: xtreg hourfull e9share684d Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)

esttab * using "tableapril1.tex", ///
    title(\label{tableapril1}) ///
    b(%9.3f) se(%9.3f) ///
    lab se r2 pr2 noconstant replace ///
    addnotes("$\text{S}_i$ and $\text{T}_t$ included but not reported.")	

******* IV
eststo clear 
eststo: xtivreg theta (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)
eststo: xtivreg v (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)
eststo: xtivreg vfull (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)
eststo: xtivreg vpart (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)
eststo: xtivreg numDpartproportion (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)
eststo: xtivreg wagefull (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)
eststo: xtivreg hourfull (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)

esttab * using "tableapril2.tex", ///
    title(\label{tableapril2}) ///
    b(%9.3f) se(%9.3f) ///
    lab se r2 pr2 noconstant replace ///
    addnotes("$\text{S}_i$ and $\text{T}_t$ included but not reported.")	


// Find First-stage F statistics. Does not work below Stata version 17
ivreghdfe theta (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, absorb(indmc) cluster(indmc) first  
ivreghdfe v (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, absorb(indmc) cluster(indmc) first  
ivreghdfe vfull (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, absorb(indmc) cluster(indmc) first  
ivreghdfe vpart (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, absorb(indmc) cluster(indmc) first  
ivreghdfe numDpartproportion (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, absorb(indmc) cluster(indmc) first  
ivreghdfe wagefull (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, absorb(indmc) cluster(indmc) first  
ivreghdfe hourfull (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, absorb(indmc) cluster(indmc) first


******* Graphs
twoway (scatter e9share TFWshare)(lfit e9share TFWshare) ///
        , xtitle("TFW Share (%)") ytitle("E9 Share (%)") legend(off)
graph export TFWe9share.eps, replace

twoway (scatter forper hourfull716)(lfit forper hourfull716), ///
        xtitle("Fulltime Workers' Monthly Work Hours") ytitle("TFW Share (%)") legend(off) ///
        title("Panel (F): Corr between Work hours and TFW share") xline(174)
graph export TFWsharehourfull716.eps, replace


******* IV (Robustness Check)
eststo clear 
eststo: xtivreg theta_alter (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)
eststo: xtivreg v_alter (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, fe vce(cluster indmc)
esttab * using "tableapril4.tex", ///
    title(\label{tableapril4}) ///
    b(%9.3f) se(%9.3f) ///
    lab se r2 pr2 noconstant replace ///
    addnotes("$\text{S}_i$ and $\text{T}_t$ included but not reported.")	

// Find First-stage F statistics. Does not work below Stata version 16
ivreghdfe theta_alter (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, absorb(indmc) cluster(indmc) first  
ivreghdfe v_alter (e9chgd=e9share684d) Luibmoney proddome prodabroad prodoper i.ym, absorb(indmc) cluster(indmc) first  



/*********************************************
Continuous DID Regressions (monthly) 2014~
*********************************************/
*!start
cd "${path}"
use panelf3, clear
drop if indmc==0    // information for total manufacturing sectors. 
//drop if indmc==32|indmc==16  // too much fluctuations
drop if indmc==19  // too few observations

keep if 648<=ym&ym<=745
tab ym, gen(dum)

label var theta "Tightness" 

foreach i of numlist 1/98 {
    gen e9share684dum`i'=e9share684*dum`i'
}
* dum61 = 2020m1

order *, sequential
capture program drop contdidreg
program contdidreg 
args i j
    preserve
            xtreg `i' e9share684dum1-e9share684dum71 e9share684dum73-e9share684dum98 i.ym proddome prodabroad prodoper uibmoney, fe vce(cluster indmc) 
            mat b2=e(b)'
            mat b=b2[1..71,1]\0\b2[72..97,1]   
            mat v2=vecdiag(e(V))'
            mat v=v2[1..71,1]\0\v2[72..97,1]
            scalar invttail=invttail(e(df_r),0.025)
            matain b
            matain v
            mata se=sqrt(v)
            clear
            getmata b  
            getmata se
            gen lb=b-invttail*se
            gen ub=b+invttail*se
            gen t=_n
            replace t=t+647
            tsset t, monthly
            format t %tm
    
            gen theta=.
            gen theta_alter=.
            gen v=.
            gen vfull=.
            gen vpart=.
            gen v_alter=.
            gen vfull_alter=.
            gen numDpartproportion=.
            gen hourfull=.
            gen wagefull=.
            gen dw_approx=.
            gen a_unbiased=.
            gen lambda=.

            label var theta "Tightness"
            label var theta_alter "Tightness(alter)" 
            label var v "Vacancy" 
            label var vfull "Vacancy(Full)" 
            label var vpart "Vacancy(Part)" 
            label var v_alter "Vacancy(alter)" 
            label var vfull_alter "Vacancy(Full,alter)" 
            label var numDpartproportion "Part/Full" 
            label var hourfull "Work Hours(Full)" 
            label var wagefull "Wage(Full)" 
            label var dw_approx "Domestic Workers" 
            label var a_unbiased "Match Efficiency" 
            label var lambda "Termination" 

            twoway (rspike ub lb t, lcolor(gs0))(rcap ub lb t, msize(medsmall) lcolor(gs0))(scatter b t), xline(719) yline(0) xtitle("") ytitle("") /// 
            legend(off) xlabel(648(12)745) ///
            title(Panel(`j'): `: variable label `i'')
            graph export contdid`i'`j'.eps, replace
    restore
end

contdidreg theta A
contdidreg v B
contdidreg vfull C
contdidreg vpart D
contdidreg numDpartproportion E
contdidreg wagefull G
contdidreg hourfull H
contdidreg theta_alter A
contdidreg v_alter B
contdidreg vfull_alter C



/*********************************************
Continuous DID Regressions (Robustness Check)
*********************************************/
//!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/SVARdata_tempextended.csv", clear 
gen ym=_n+623
keep ym fw dw
save SVARdata_DID, replace 
//!start
cd "${path}"
use panelf3, clear
drop if indmc==0
collapse (sum) e9, by(ym)
rename e9 e9tot
replace e9tot=129808 if e9tot==0 //temporary
save e9tot, replace 
//!start
cd "${path}"
use panelf3, clear
merge m:1 ym using SVARdata_DID
drop if _merge==2
drop _merge
merge m:1 ym using e9tot
keep if _merge==3
keep if 648<=ym
preserve  // check if numD==dw+fw (yes)
    keep if indmc==0
    keep ym numD dw fw
    sort ym
    gen numD_compare=dw+fw
    gen check=numD-numD_compare
    keep if check!=0
    di _N
restore 
drop if indmc==0
sort indmc ym 
replace e9=e9[_n-1] if ym==745 // temporary
gen e9weight=e9/e9tot
gen fw_approx=fw*e9weight
gen dw_approx=numD-fw_approx
gen share_approx=fw_approx/(fw_approx+dw_approx)*100
sort indmc ym 
xtset indmc ym 
xtline share_approx, overlay
save numDrobust, replace 

//!start
use numDrobust, clear 
drop if indmc==0
drop if indmc==19  // too few observations
xtset indmc ym 

keep if 648<=ym&ym<=745
tab ym, gen(dum)

foreach i of numlist 1/98 {
    gen e9share684dum`i'=e9share684*dum`i'
}

order *, sequential
capture program drop contdidreg2
program contdidreg2
args i j
    preserve
            xtreg `i' e9share684dum1-e9share684dum71 e9share684dum73-e9share684dum98 i.ym proddome prodabroad prodoper uibmoney, fe vce(cluster indmc)
            mat b2=e(b)'
            mat b=b2[1..71,1]\0\b2[72..97,1]   
            mat v2=vecdiag(e(V))'
            mat v=v2[1..71,1]\0\v2[72..97,1]
            scalar invttail=invttail(e(df_r),0.025)
            matain b
            matain v
            mata se=sqrt(v)
            clear
            getmata b  
            getmata se
            gen lb=b-invttail*se
            gen ub=b+invttail*se
            gen t=_n
            replace t=t+647
            tsset t, monthly
            format t %tm
    
            gen theta=.
            gen v=.
            gen vfull=.
            gen vpart=.
            gen v_alter=.
            gen numDpartproportion=.
            gen hourfull=.
            gen wagefull=.
            gen dw_approx=.

            label var dw_approx "Domestic Workers" 

            twoway (rspike ub lb t, lcolor(gs0))(rcap ub lb t, msize(medsmall) lcolor(gs0))(scatter b t), xline(719) yline(0) xtitle("") ytitle("") /// 
            legend(off) xlabel(648(12)745) ///
            title(Panel(`j'): `: variable label `i'')
            graph export contdid`i'`j'.eps, replace
    restore
end

contdidreg2 dw_approx D


/*********************************************
Local Projection method (DD)
*********************************************/
*!start
cd "${path}"

capture program drop LPDID
program LPDID 
    args j depvar
    use panelf3, clear
    xtset indmc ym

    drop if indmc==0    // information for total manufacturing sectors. 
    //drop if indmc==32|indmc==16  // too much fluctuations
    drop if indmc==19  // too few observations
    keep if 708<=ym

    label var theta "Tightness" 
    label var uibmoney "UIB" 
    label var theta "Tightness" 
    label var v "Vacancy" 
    label var vfull "Vacancy(Full)" 
    label var vpart "Vacancy(Part)" 
    label var hourfull "Work Hours(Full)" 
    label var wagefull "Wage(Full)" 
    label var v_alter "Vacancy(Alternative)" 

    gen e9numD=e9/numD*100
    gen LP=.
    gen ub=.
    gen lb=.

    forvalues h=0(1)18 {
        preserve
            gen Fv=F`h'.`depvar'
            gen d=0 if  710<=ym&ym<=719  
            replace d=1 if 720<=ym&ym<=729
            drop if d==.
            gen e9share684d=e9share684*d
            xtreg Fv e9share684d proddome prodabroad prodoper uibmoney, fe vce(cluster indmc)
        restore
        replace LP = _b[e9share684d] if _n==`h'+1
        replace ub = _b[e9share684d] + 1.645* _se[e9share684d] if _n==`h'+1
        replace lb = _b[e9share684d] - 1.645* _se[e9share684d] if _n==`h'+1
    }

    replace ym=ym+19
    keep if _n<=19
    gen Zero=0
    twoway ///
    (rarea ub lb  ym,  ///
    fcolor(gs13) lcolor(gs13) lw(none) lpattern(solid)) ///
    (line LP ym, lcolor(blue) ///
    lpattern(solid) lwidth(thick)) ///
    (line Zero ym, lcolor(black)), legend(off) ///
    ytitle("", size(medsmall)) xtitle("", size(medsmall)) ///
    graphregion(color(white)) plotregion(color(white)) xlabel(729(4)747) ///
    title(Panel(`j'): `: variable label `depvar'') ///
    ysize(1) xsize(1.6)
    graph export LP`depvar'.eps, replace
end

LPDID A theta
LPDID B v_alter
LPDID C v
LPDID D vfull
LPDID E vpart
LPDID F hourfull
LPDID G wagefull


/*********************************************
Generate figures
*********************************************/
//!start
cd "${path}"
use panelf3, clear
drop if indmc==0
drop if indmc==32|indmc==16  // too much fluctuations
drop if indmc==19  // too few observations
twoway (scatter e9share vchg) (lfit e9share vchg) ///
, xtitle("Change of unfilled vacancies (%p)") ytitle("Share of E9 (%)") legend(off)
graph export vchge9.eps, replace


*!start
cd "${path}"
use panelf3, clear

keep if ym>=696
gen e9shareconcur=e9/numD*100
local var="e9shareconcur"
twoway ///
(tsline `var' if indmc==10, lcolor(gs0)) ///
(tsline `var' if indmc==11, lcolor(gs0)) ///
(tsline `var' if indmc==13, lcolor(gs0)) ///
(tsline `var' if indmc==14, lcolor(gs0)) ///
(tsline `var' if indmc==15, lcolor(gs0)) ///
(tsline `var' if indmc==17, lcolor(gs0)) ///
(tsline `var' if indmc==18, lcolor(gs0)) ///
(tsline `var' if indmc==20, lcolor(gs0)) ///
(tsline `var' if indmc==21, lcolor(gs0)) ///
(tsline `var' if indmc==22, lcolor(gs0)) ///
(tsline `var' if indmc==23, lcolor(gs0)) ///
(tsline `var' if indmc==24, lcolor(gs0)) ///
(tsline `var' if indmc==25, lcolor(gs0)) ///
(tsline `var' if indmc==26, lcolor(gs0)) ///
(tsline `var' if indmc==27, lcolor(gs0)) ///
(tsline `var' if indmc==28, lcolor(gs0)) ///
(tsline `var' if indmc==29, lcolor(gs0)) ///
(tsline `var' if indmc==30, lcolor(gs0)) ///
(tsline `var' if indmc==31, lcolor(gs0)) ///
(tsline `var' if indmc==33, lcolor(gs0)) ///
, xline(720) ylabel(0(4)12) ytitle("E9 Share (%)") xtitle("") legend(off)
graph export e9shareconcur2.eps, replace

//!start
cd "${path}"
use panelf3, clear
drop if indmc==0
drop if indmc==32|indmc==16  // too much fluctuations
drop if indmc==19  // too few observations
keep if ym>=719
local var="v"
tsfilter hp v_hp = v, trend(smooth_v) smooth(12)
drop v
rename smooth_v v
twoway ///
(tsline `var' if indmc==10, lcolor(gs0)) ///
(tsline `var' if indmc==11, lcolor(gs0)) ///
(tsline `var' if indmc==13, lcolor(gs0)) ///
(tsline `var' if indmc==14, lcolor(gs0)) ///
(tsline `var' if indmc==15, lcolor(gs0)) ///
(tsline `var' if indmc==17, lcolor(gs0)) ///
(tsline `var' if indmc==18, lcolor(gs0)) ///
(tsline `var' if indmc==20, lcolor(gs0)) ///
(tsline `var' if indmc==21, lcolor(gs0)) ///
(tsline `var' if indmc==22, lcolor(gs0)) ///
(tsline `var' if indmc==23, lcolor(gs0)) ///
(tsline `var' if indmc==24, lcolor(gs0)) ///
(tsline `var' if indmc==25, lcolor(gs0)) ///
(tsline `var' if indmc==26, lcolor(gs0)) ///
(tsline `var' if indmc==27, lcolor(gs0)) ///
(tsline `var' if indmc==28, lcolor(gs0)) ///
(tsline `var' if indmc==29, lcolor(gs0)) ///
(tsline `var' if indmc==30, lcolor(gs0)) ///
(tsline `var' if indmc==31, lcolor(gs0)) ///
(tsline `var' if indmc==33, lcolor(gs0)) ///
, xline(720) xlabel(719(6)750)  ytitle("Unfilled Vacancies (%)") xtitle("") legend(off)
graph export vconcur2.eps, replace

keep if ym==746
keep indmc v e9share
gsort -v

//!start
cd "${path}"
use panelf3, clear
keep if indmc==0
merge 1:1 ym using ut
keep ym v prod uC
drop if uC==.
tsset ym, monthly
tsfilter hp v_hp = v, trend(smooth_v) smooth(1)
drop v
rename smooth_v v
tsfilter hp uC_hp = uC, trend(smooth_uC) smooth(1)
drop uC
rename smooth_uC uC
tsfilter hp prod_hp = prod, trend(smooth_prod) smooth(1)
drop prod
rename smooth_prod prod
label var uC "Unemployment rate"
label var v "Vacancy rate"
label var prod "Production"
twoway (tsline v, lwidth(thick) lcolor(gs0) yaxis(1)) /// 
    (tsline uC, lcolor(gs0) yaxis(2)) ///
    (tsline prod, lcolor(gs0) clpattern(longdash) yaxis(3)) ///
    , xtitle("") ytitle("") xline(720) xline(724) ysize(1) xsize(3) xlabel(660(12)744)
graph export vup.eps, replace



//!start
cd "${path}"
use panelf3, clear
drop if indmc==0
//drop if indmc==32|indmc==16  // too much fluctuations
drop if indmc==19  // too few observations
keep if ym>=684
local var="v"
tsfilter hp v_hp = v, trend(smooth_v) smooth(12)
drop v
rename smooth_v v
twoway ///
(tsline `var' if indmc==10, lcolor(gs0)) ///
(tsline `var' if indmc==11, lcolor(gs0)) ///
(tsline `var' if indmc==13, lcolor(gs0)) ///
(tsline `var' if indmc==14, lcolor(gs0)) ///
(tsline `var' if indmc==15, lcolor(gs0)) ///
(tsline `var' if indmc==17, lcolor(gs0)) ///
(tsline `var' if indmc==18, lcolor(gs0)) ///
(tsline `var' if indmc==20, lcolor(gs0)) ///
(tsline `var' if indmc==21, lcolor(gs0)) ///
(tsline `var' if indmc==22, lcolor(gs0)) ///
(tsline `var' if indmc==23, lcolor(gs0)) ///
(tsline `var' if indmc==24, lcolor(gs0)) ///
(tsline `var' if indmc==25, lcolor(gs0)) ///
(tsline `var' if indmc==26, lcolor(gs0)) ///
(tsline `var' if indmc==27, lcolor(gs0)) ///
(tsline `var' if indmc==28, lcolor(gs0)) ///
(tsline `var' if indmc==29, lcolor(gs0)) ///
(tsline `var' if indmc==30, lcolor(gs0)) ///
(tsline `var' if indmc==31, lcolor(gs0)) ///
(tsline `var' if indmc==33, lcolor(gs0)) ///
(tsline `var' if indmc==32, lcolor(red)) ///
(tsline `var' if indmc==16, lcolor(red)) ///
, xline(720) xlabel(719(6)750)  ytitle("Unfilled Vacancies (%)") xtitle("") legend(off)  xline(708) xline(696)
graph export v3216.eps, replace


/*********************************************
VAR with sign restrictions
*********************************************/
//!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/orig_extended.csv", varnames(1) clear 
keep if indmc==0
keep if 624<=ym&ym<=748 // 2012m1~2022m5
gen v=nume/numd*100
gen u=uib/(uib+numd)*100
keep ym u v
save SVARuv, replace 

//!start
cd "${path}"
import delimited "https://raw.githubusercontent.com/jayjeo/public/master/LaborShortage/SVARdataset.csv", varnames(1) clear 
save SVARdataset, replace 

use SVARuv, clear
merge 1:1 ym using SVARdataset, nogenerate
tsset ym, monthly 
format ym %tmdd/NN/CCYY

foreach var of varlist fw dw u v {
    gen ln`var'=ln(`var')
    drop `var'
    rename ln`var' `var'
}

preserve 
keep dw fw u v
order dw fw u v
export delimited using "${path}\SVARdata_seasondummyadj.csv", replace
*manually saved it to ".../Rubio_Ramirez_Replication/data/SVARdata_seasondummyadj.csv"
restore 

preserve 
keep ym 
order ym
export delimited using "${path}\SVARdata_seasondummyadj_dates.csv", replace
*manually saved it to ".../Rubio_Ramirez_Replication/data/SVARdata_seasondummyadj_dates.csv"
restore 

/*************** Executable using Matlab code by Antolín-Díaz and Rubio-Ramírez 2018
1) Download Replication data: Narrative Sign Restrictions for SVARs from https://www.openicpsr.org/openicpsr/project/113168/version/V1/view
2) Download entire files from https://github.com/jayjeo/public/tree/main/LaborShortage/Rubio_Ramirez_Replication, and merge it to the previous one.
3) Run Application_3_LS.m
********************/

