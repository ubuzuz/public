
*-----------------------------------------------------------------------------
* This coding uses some ado files below:
net install Jay_ado.pkg, from(https://raw.githubusercontent.com/jayjeo/public/master/adofiles)

/*
To see contents of the Jay_ado.pkg:
https://github.com/jayjeo/stata

To completely uninstall Jay_ado.pkg:
ado uninstall Jay_ado
-----------------------------------------------------------------------------*/

/******** useful materials
TRI = Colin Cameron, Pravin K. Trivedi, Microeconometrics: Methods and Applications. 2005
http://blog.stata.com/2016/01/05/programming-an-estimation-command-in-stata-computing-ols-objects-in-mata/  

*/

*!start
clear all
use "https://raw.githubusercontent.com/jayjeo/public/master/GH210712_REGexercise/krueger_example.dta", clear
set matsize 11000, perm
set matastrict on
version 14
save original, replace


******** Take a look at missing values
//ssc install mdesc
keep schgroup pscore wa free sex totexpk cs sm classid
mdesc
drop if totexpk==.

sort schgroup
	scalar last = schgroup[_N]+1
	di last
	replace schgroup=last if missing(schgroup)
qui list schgroup
//ssc install moremata


******** generate basic variables
tabulate schgroup, generate(d)
putmata *, replace

mata: D=d2
forvalues i=3(1)79 {
	mata: D=D,d`i'
	}  //d1 dropped.

foreach var in pscore wa free sex totexpk cs sm  {
	egen `var'm = mean(`var'), by(schgroup)
	gen `var'd=`var'-`var'm

	putmata `var'm
	mata `var'd=`var'-`var'm
}


******** OLS
mata
	Y=pscore
	n=rows(Y)

	*X=wa, free, sex, totexpk, J(n,1,1), cs  // This order also works for even with IV
	*Z=wa, free, sex, totexpk, J(n,1,1), sm  // This order also works for even with IV
	X=cs, wa, free, sex, totexpk, J(n,1,1)   
	Z=sm, wa, free, sex, totexpk, J(n,1,1)

	XX=quadcross(X,X)
	ZZ=quadcross(Z,Z)
	XZ=quadcross(X,Z)
	ZX=quadcross(Z,X)
	XY=quadcross(X,Y)
	ZY=quadcross(Z,Y)

	k=cols(X)
		
	Yd=pscored
	Xd=csd, wad, freed, sexd, totexpkd

	b_ols=invsym(XX)*XY
	e_ols=Y-X*b_ols
	e_ols_2=e_ols:*e_ols
	sigma2_ols=mean(e_ols_2)
	v_ols=n*invsym(n-k)*sigma2_ols*invsym(XX)
end

mata b_ols
mata sqrt(diagonal(v_ols))

reg pscore cs wa free sex totexpk

preserve  // To see what if missing rows had not been dropped.
	use original, replace
	reg pscore cs wa free sex totexpk 
restore 

save default, replace
mata: mata matsave default *,replace
// use these files as default. From below, overwrite data if necessary


******** Weighting
*!start
clear all
use default
mata: mata matuse default
set seed 10101

gen wgt=abs(rnormal(1,1))
putmata wgt
mata 
	W=diag(wgt)
	w=diag(sqrt(wgt))  // not used 
	IW=luinv(W)  // not used 
end 

reg pscore cs wa free sex totexpk [aweight=wgt]

mata	
	b_ols=invsym(quadcross(X,wgt,X))*quadcross(X,wgt,Y)
	b_ols 
	b_ols=invsym(X'*W*X)*(X'*W*Y)
	b_ols 
end

mata
	e_ols=Y-X*b_ols
	e_ols_2=e_ols:*e_ols
	D0=diag(e_ols_2)
	v_ols_ro=n*invsym(n-k)*luinv(X'*W*X)*X'*W*D0*W*X*luinv(X'*W*X)  
end

mata sqrt(diagonal(v_ols_ro))
reg pscore cs wa free sex totexpk [aweight=wgt], robust 


******** Robust variance
*!start
clear all
use default
mata: mata matuse default

mata
	//beta same as b_ols
	D0=diag(e_ols_2)
	XD0X=quadcross(X,D0)*X
	v_ols_ro=n*invsym(n-k)*invsym(XX)*quadcross(X,e_ols_2,X)*invsym(XX) // This also works. 
	v_ols_ro=n*invsym(n-k)*invsym(XX)*quadcross(X,D0)*X*invsym(XX)  
end

mata XD0X
mata sqrt(diagonal(v_ols_ro))

reg pscore cs wa free sex totexpk, ro

preserve  // To see what if missing rows had not been dropped.
	use original, replace
	reg pscore cs wa free sex totexpk, ro
restore 


******** Cluster variance 
* assuming small clusters, C goes to infinity. (TRI 834 (24.34))
*!start
clear all
use default
mata: mata matuse default

mata
	//beta same as b_ols
	info=panelsetup(schgroup,1)
	nc=rows(info)
	
	SXX=J(k,k,0)
	SXuuX=J(k,k,0)
		for(i=1; i<=nc; i++){
			xi=panelsubmatrix(X,i,info)
			ui=panelsubmatrix(e_ols,i,info)
			SXX=SXX+xi'*xi
			SXuuX=SXuuX+xi'*(ui*ui')*xi
		}

	dfc=(n-1)/(n-k)*nc/(nc-1)
	v_ols_cl=dfc*invsym(SXX)*SXuuX*invsym(SXX)
end

mata sqrt(diagonal(v_ols_cl))
reg pscore cs wa free sex totexpk, vce(cluster schgroup)  


******** TSLS 
* error independent and identical
*!start
clear all
use default
mata: mata matuse default

mata 
	fsb=invsym(ZZ)*(ZX)
	Xh=Z*fsb
	b_tsls=invsym(quadcross(Xh,Xh))*quadcross(Xh,Y)
	// v_tsls the same as v_ils
end

mata b_tsls

ivregress 2sls pscore (cs=sm) wa free sex totexpk 


******** ILS
* error independent and identical
*!start
clear all
use default
mata: mata matuse default

mata 
	b_ils=luinv(ZX)*ZY   // invsym does not work. Should use luinv instead.
	e_ils=Y-X*b_ils
	e_ils_2=e_ils:*e_ils
	sigma2_ils=mean(e_ils_2)
	v_ils=invsym(XZ*invsym(ZZ)*ZX)*sigma2_ils   // do not multiply n*invsym(n-k)
end

*mata invsym(ZX)
*mata luinv(ZX)

mata b_ils
mata sqrt(diagonal(v_ils))

ivregress 2sls pscore (cs=sm) wa free sex totexpk 


******** ILS
* error independent but not identical (robust)

mata 
	//beta same as b_ils.
	D0=diag(e_ils_2)
	ZD0Z=quadcross(Z,D0)*Z
	v_ils_ro=luinv(XZ*luinv(ZZ)*ZX)*(XZ*luinv(ZZ)*ZD0Z*luinv(ZZ)*ZX)*luinv(XZ*luinv(ZZ)*ZX)
      // do not multiply n*luinv(n-k)
end

mata sqrt(diagonal(v_ils_ro))

ivregress 2sls pscore (cs=sm) wa free sex totexpk, robust


******** ILS
* error not independent and not identical (dependent by school group)

mata
	//beta same as b_ils.
	info=panelsetup(schgroup,1)
	nc=rows(info)

	ZuuZ=J(k,k,0)
		for(i=1; i<=nc; i++){
			zi=panelsubmatrix(Z,i,info)
			ui=panelsubmatrix(e_ils,i,info)
			ZuuZ=ZuuZ+zi'*(ui*ui')*zi
		}
	v_ils_ro_cl=luinv(X'*Z*luinv(Z'*Z)*Z'*X)*(X'*Z*luinv(Z'*Z)*ZuuZ*luinv(Z'*Z)*Z'*X)*luinv(X'*Z*luinv(Z'*Z)*Z'*X) 
end

mata sqrt(diagonal(v_ils_ro_cl))

ivregress 2sls pscore (cs=sm) wa free sex totexpk, cluster(schgroup)


******** FE
*!start
clear all
use default
mata: mata matuse default

mata
	k=k-1   // Constant dropped from default.
	info=panelsetup(schgroup,1)
	nc=rows(info)

	XdXd=J(k,k,0)
	XdYd=J(k,1,0)
		for(i=1; i<=nc; i++){
			xdi=panelsubmatrix(Xd,i,info)
			ydi=panelsubmatrix(Yd,i,info)

			XdXd=XdXd+xdi'*xdi
			XdYd=XdYd+xdi'*ydi
		}

	XdXd2=Xd'*Xd   // XdXd2 is the same as XdXd, but these two are asymptotically different. 
	XdYd2=Xd'*Yd   // XdYd2 is the same as XdYd, but these two are asymptotically different.

	b_fe=luinv(XdXd)*XdYd

	e_fe=Yd-Xd*b_fe

	XdD0Xd=J(k,k,0)		    
	eded=0
		for(i=1; i<=nc; i++){
			xdi=panelsubmatrix(Xd,i,info)
			ydi=panelsubmatrix(Yd,i,info)
			edi=ydi-xdi*b_fe
			ededi=edi:*edi
			D0i=diag(ededi)
			XdD0Xd=XdD0Xd+xdi'*D0i*xdi
			eded=eded+edi'*edi
		}	
	
	SXX_fe=J(k,k,0)
	SXuuX_fe=J(k,k,0)
	for(i=1; i<=nc; i++){
		xdi=panelsubmatrix(Xd,i,info)
		udi=panelsubmatrix(e_fe,i,info)

		SXX_fe=SXX_fe+xdi'*xdi
		SXuuX_fe=SXuuX_fe+xdi'*(udi*udi')*xdi
	}

	dfc=1/(n-nc-k)
	dfcw=(n)/(n-nc-k)
	dfcw2=(n)/(n-k)
	dfc1=(n-1)/(n-nc-k)*nc/(nc-1)       
	dfc2=(n-1)/(n-1-k)*nc/(nc-1)
	dfc3=(n-1)/(n-k)*nc/(nc-1)

	v_fe=dfc*eded*luinv(XdXd)						     // homoskedasity estimator (BRUn_616 (17.36))
	v_fe_white=dfcw*luinv(XdXd)*XdD0Xd*luinv(XdXd)        // White type robust estimator (BRU21_642 (17.56.b))
	v_fe_white2=dfcw2*luinv(XdXd)*XdD0Xd*luinv(XdXd)      // Without considering nc. This is not the correct dfc. 
	v_fe_cl1=dfc1*luinv(SXX_fe)*SXuuX_fe*luinv(SXX_fe)   // FE cluster type robust estimator (BRUn_617 (17.40))
	v_fe_cl2=dfc2*luinv(SXX_fe)*SXuuX_fe*luinv(SXX_fe)   // xtreg fe robust result (Don't know what this is)
	v_fe_cl3=dfc3*luinv(SXX_fe)*SXuuX_fe*luinv(SXX_fe)   // Cluster robust estimator (non-FE version)
end

mata b_fe
mata sqrt(diagonal(v_fe))

xtset schgroup
xtreg pscore wa free sex totexpk cs, fe

mata sqrt(diagonal(v_fe_white))
reg pscored csd wad freed sexd totexpkd, ro nocons

mata sqrt(diagonal(v_fe_cl2))
xtreg pscore wa free sex totexpk cs, fe robust

areg pscore wa free sex totexpk cs, absorb(schgroup) vce(robust)



******** FE (Numerical optimization using nl)
*!start
clear all
use default

foreach var in pscore cs wa free sex totexpk {
	egen m`var'=mean(`var'), by(schgroup)
}
gen dpscore=pscore-mpscore

nl (dpscore={b1}*(cs-mcs)+{b2}*(wa-mwa)+{b3}*(free-mfree)+{b4}*(sex-msex)+{b5}*(totexpk-mtotexpk)), vce(robust)

xtset schgroup
xtreg pscore wa free sex totexpk cs, fe robust



******** FE TSLS
* error independent but not identical (robust)
* Fixed effect IV model. BRUn633 (17.69.b)
*!start
clear all
use default
mata: mata matuse default

mata
	I=I(n)
	M=I-D*luinv(D'*D)*D'
	b_iv_fe_ro=luinv(X'*M*Z*luinv(Z'M*Z)*Z'*M*X)*(X'*M*Z*luinv(Z'*M*Z)*Z'M*Y)

	MY=M*Y
	MX=M*X
	MZ=M*Z

	e_iv_fe_ro=MY-MX*b_iv_fe_ro

	info=panelsetup(schgroup,1)
	nc=rows(info)

	MZeeMZ=J(k,k,0)
		for(i=1; i<=nc; i++){
			mzi=panelsubmatrix(MZ,i,info)
			edi=panelsubmatrix(e_iv_fe_ro,i,info)

			MZeeMZ=MZeeMZ+mzi'*(edi*edi')*mzi
		}
	v_iv_fe_ro=luinv(MX'*MZ*luinv(MZ'*MZ)*MZ'*MX)*MX'*MZ*luinv(MZ'*MZ)*MZeeMZ*luinv(MZ'*MZ)*MZ'*MX*luinv(MX'*MZ*luinv(MZ'*MZ)*MZ'*MX)
									//  Cluster robust estimator (BRUn635)
	dfc3=(n-1)/(n-k)*nc/(nc-1)	
	v_iv_fe_ro3=dfc3*v_iv_fe_ro     //  xtivreg fe robust result
end

mata b_iv_fe_ro
mata sqrt(diagonal(v_iv_fe_ro3))

xtset schgroup
xtivreg pscore wa free sex totexpk (cs=sm), fe vce(robust)



******** FE (If data was a balanced panel)
// Heteroskedasticity-Robust Estimation for Balanced Case (BRU21_643 (17.58))
// Make arbitrary data set. Generate individual(ind) variable. 
*!start
clear all
use default

sort schgroup
gen ind=1
replace ind=ind[_n-1]+1 if schgroup==schgroup[_n-1]

putmata schgroup
mata 
	info=panelsetup(schgroup,1)
	infon=info[.,2]-info[.,1]+J(rows(info),1,1)
	T=min(infon)
	st_numscalar("T", T)
end
di T

drop if ind>T
//tsset schgroup ind

putmata pscore wa free sex totexpk cs sm schgroup, replace

foreach var in pscore wa free sex totexpk cs sm  {
	egen `var'mm = mean(`var'), by(schgroup)
	replace `var'd=`var'-`var'mm

	putmata `var'mm
	mata `var'd=`var'-`var'mm
}

mata
	Y=pscore
	n=rows(Y)
		
	Yd=pscored
	Xd=csd, wad, freed, sexd, totexpkd
	k=cols(Xd)
end


mata
	info=panelsetup(schgroup,1)
	nc=rows(info)

	XdXd=J(k,k,0)
	XdYd=J(k,1,0)
		for(i=1; i<=nc; i++){
			xdi=panelsubmatrix(Xd,i,info)
			ydi=panelsubmatrix(Yd,i,info)

			XdXd=XdXd+xdi'*xdi
			XdYd=XdYd+xdi'*ydi
		}

	b_fe=luinv(XdXd)*XdYd

	e_fe=Yd-Xd*b_fe

	XdD0Xd=J(k,k,0)	
	XdS2Xd=J(k,k,0)		    
		for(i=1; i<=nc; i++){
			xdi=panelsubmatrix(Xd,i,info)
			ydi=panelsubmatrix(Yd,i,info)
			edi=ydi-xdi*b_fe
			ededi=edi:*edi
			D0i=diag(ededi)
			XdD0Xd=XdD0Xd+xdi'*D0i*xdi

			sigma2i=1*invsym(rows(ededi)-1)*colsum(ededi)  // BRU21_643 (17.59)
			XdS2Xd=XdS2Xd+xdi'*xdi*sigma2i

			eded`i'_check=ededi
		}		

	dfcw=n/(n-nc-k)
	dfcw2=n/(n-k)
	dfcw3=n/(n-nc)
	v_fe_white=dfcw*luinv(XdXd)*XdD0Xd*luinv(XdXd)        // White type robust estimator (BRU21_642 (17.56.b))
	v_fe_white2=dfcw2*luinv(XdXd)*XdD0Xd*luinv(XdXd)       // Without considering nc. This is not the correct dfc. 
	v_fe_white3=luinv(XdXd)*XdD0Xd*luinv(XdXd)       
	B_fe=luinv(XdXd)*XdS2Xd*luinv(XdXd)
	v_fe_stock=(T-1)/(T-2)*v_fe_white-1/(T-1)*B_fe         // Stock and Watson estimator (BRU21_643 (17.58))
	v_fe_stock3=(T-1)/(T-2)*v_fe_white3-1/(T-1)*B_fe
end

mata sqrt(diagonal(v_fe_stock))
mata sqrt(diagonal(v_fe_white))
mata sqrt(diagonal(v_fe_white2))
reg pscored csd wad freed sexd totexpkd, ro nocons

mata sqrt(diagonal(v_fe_stock3))



******** FE (If data was an unbalanced panel)
// Heteroskedasticity-Robust Estimation for Unbalanced Case (BRU21_643 (17.60))
// Make arbitrary data set. Generate individual(ind) variable. 
*!start
clear all
use default

sort schgroup
gen ind=1
replace ind=ind[_n-1]+1 if schgroup==schgroup[_n-1]
gen n=_n

putmata schgroup ind n, replace 
mata 
	info=panelsetup(schgroup,1)
	Ti=info[.,2]-info[.,1]+J(rows(info),1,1)
	ijn=schgroup,ind,n
end

putmata pscore wa free sex totexpk cs sm schgroup, replace

foreach var in pscore wa free sex totexpk cs sm  {
	egen `var'mm = mean(`var'), by(schgroup)
	replace `var'd=`var'-`var'mm

	putmata `var'mm
	mata `var'd=`var'-`var'mm
}

mata
	Y=pscore
	n=rows(Y)
		
	Yd=pscored
	Xd=csd, wad, freed, sexd, totexpkd
	k=cols(Xd)
end

mata
	info=panelsetup(schgroup,1)
	nc=rows(info)

	XdXd=J(k,k,0)
	XdYd=J(k,1,0)
		for(i=1; i<=nc; i++){
			xdi=panelsubmatrix(Xd,i,info)
			ydi=panelsubmatrix(Yd,i,info)

			XdXd=XdXd+xdi'*xdi
			XdYd=XdYd+xdi'*ydi
		}

	b_fe=luinv(XdXd)*XdYd

	e_fe=Yd-Xd*b_fe
	e2=e_fe:*e_fe

	omega=J(k,k,0)
		for(i=1; i<=nc; i++){
			edi=panelsubmatrix(e_fe,i,info)
			ededi=edi:*edi
			sigma2i=1*invsym(rows(ededi)-1)*colsum(ededi)

			xdi=panelsubmatrix(Xd,i,info)

			for(j=1; j<=Ti[i]; j++){
				sel=select(ijn, ijn[.,1]:==i)
				sel=select(sel, sel[.,2]:==j) 
				ij=sel[.,3]
				Xdij=Xd[ij,.]
				e2ij=e2[ij,.]

				omega=omega+Xdij'*Xdij*((Ti[i]*e2ij-sigma2i)*invsym(Ti[i]-2))
			}
		}	
	v_fe_unbal=luinv(Xd'*Xd)*omega*luinv(Xd'*Xd)
end

mata v_fe_unbal
mata sqrt(diagonal(v_fe_unbal))
// In balanced panels, this estimator simplifies to the Stock-Watson estimator with the number of control variables going to zero. 



******** M-estimation (Numerical optimization using nl)
// This is mathematically identical to OLS. 
*!start
clear all
use default

gen cons=1
nl (pscore={xb:cs wa free sex totexpk cons})

reg pscore cs wa free sex totexpk



******** MoM
*for ols and just-identified IV, MoM results are identical to conventional ols and iv. 
*for ols there is no need to use GMM. 
*for over-identified IV,GMM is necessary. 



******** GMM (Numerical maximization using mata)
// https://blog.stata.com/2016/01/28/programming-an-estimation-command-in-stata-using-optimize-to-estimate-poisson-parameters/
*!start
clear all
use default
mata: mata matuse default

mata
	void plleval(real scalar todo, real vector b, ///
				real vector Y, real matrix X, ///
				val, grad, hess)
		{
			val=-(Y:-X*b'):*(Y:-X*b')
		}

	Y=pscore
	X=cs, wa, free, sex, totexpk, J(rows(Y),1,1)  
	
	S=optimize_init()
	optimize_init_argument(S,1,Y)
	optimize_init_argument(S,2,X)
	optimize_init_evaluator(S, &plleval())
	optimize_init_evaluatortype(S, "gf0")
	optimize_init_params(S,J(1,6,0.01))
	bh=optimize(S)

	bh
	sqrt(diagonal(optimize_result_V_robust(S)))'    // Not identical to canned result. Don't know why. 
end


gmm (pscore - {xb:cs wa free sex totexpk _cons}), instruments(cs wa free sex totexpk)
ivregress gmm pscore cs wa free sex totexpk (=cs wa free sex totexpk), robust  



******** GMM
* Over-identified case. 
* The result varies by selection on weighting matirx, W. See TRI186 Table 6.2
* Here, focus on OGMM (TRI187)

*corr(cs wa free sex totexpk classid)
*bidensity cs wa, levels(10)
// make wa the second instrument, which should be not good IV. 

*!start
clear all
use default
mata: mata matuse default
	
mata	
	Y=pscore
	n=rows(Y)

	X=cs,     free, sex, totexpk, J(n,1,1)   
	Z=sm, wa, free, sex, totexpk, J(n,1,1)	  // over-identified case. 

	XX=quadcross(X,X)
	ZZ=quadcross(Z,Z)
	XZ=quadcross(X,Z)
	ZX=quadcross(Z,X)
	XY=quadcross(X,Y)
	ZY=quadcross(Z,Y)

	k=cols(X)
	l=cols(Z)

	b_2sls=luinv(X'*Z*luinv(Z'*Z)*Z'*X)*X'*Z*luinv(Z'*Z)*Z'*Y
	
	u_2sls=Y-X*b_2sls
	u_2sls2=u_2sls:*u_2sls
	D0=diag(u_2sls2)
	S=luinv(n)*Z'*D0*Z    // TRI185 (6.40)

	b_ogmm=luinv((X'*Z)*luinv(S)*(Z'*X))*((X'*Z)*luinv(S)*(Z'*Y))
			// TRI187 (6.43)

	u_ogmm=Y-X*b_ogmm
	u_ogmm2=u_ogmm:*u_ogmm
	D0_ogmm=diag(u_ogmm2)
	S_ogmm=luinv(n)*Z'*D0_ogmm*Z    // TRI185 (6.40)

	v_ogmm1=n*luinv(X'*Z*luinv(S)*Z'*X)
	v_ogmm2=n*luinv(X'*Z*luinv(S_ogmm)*Z'*X)     // ivregress gmm robust result. (TRI 187)
end

mata b_ogmm
mata sqrt(diagonal(v_ogmm1))
mata sqrt(diagonal(v_ogmm2))

ivregress gmm pscore free sex totexpk (cs=sm wa), robust first  




******** MLE (logit), Numerical maximization using ml
// use sm as dependent variable for practice purpose. 
*!start
clear all
use default
mata: mata matuse default

capture program drop mylogit
prog mylogit
	version 14
	args lnf theta1
	tempvar Logit
	local y "$ML_y1"
	gen double `Logit'=exp(`theta1')/(1+exp(`theta1'))
	qui replace `lnf' =`y'*ln(`Logit')+(1-`y')*ln(1-`Logit')
end

ml model lf mylogit (sm = wa free sex totexpk)
ml maximize


logit sm wa free sex totexpk



******** MLE (logit), Numerical maximization using mata
*!start
clear all
use default
mata: mata matuse default

mata
	void plleval(real scalar todo, real vector b, ///
				real vector Y, real matrix X, ///
				val, grad, hess)
		{
			val=sum(Y:*ln(exp(X*b'):/(1:+exp(X*b'))):+(1:-Y):*ln(1:-exp(X*b'):/(1:+exp(X*b'))))
		}

	Y=sm
	X=wa, free, sex, totexpk, J(rows(Y),1,1)  
	
	S=optimize_init()
	optimize_init_argument(S,1,Y)
	optimize_init_argument(S,2,X)
	optimize_init_evaluator(S, &plleval())
	optimize_init_evaluatortype(S, "d0")
	optimize_init_params(S,J(1,5,0.01))
	bh=optimize(S)

	bh
	sqrt(diagonal(optimize_result_V_oim(S)))'
end


logit sm wa free sex totexpk



******** M-estimation (NLS) (logit), Numerical optimization using nl
// This is mathematically different from MLE
*!start
clear all
use default
mata: mata matuse default

gen cons=1
nl (sm=exp({xb:wa free sex totexpk cons})/(1+exp({xb:wa free sex totexpk cons})))

logit sm wa free sex totexpk   // Compare this.



******** inverse ols method. (logit) 
* This method does not work. Need great number of obs to work. (POR454)
*!start
clear all
use default
mata: mata matuse default

//npregress kernel sm i.wa i.free i.sex totexpk, kernel(gaussian) // only works above Stata14
//predict p
//save npreg, replace 
use "https://raw.githubusercontent.com/jayjeo/public/master/GH210712_REGexercise/npreg.dta", clear  // If your Stata is below version 15. 

histogram p

preserve
	drop if p<=0 | p>=1
	gen L=ln(p/(1-p))
	reg L wa free sex totexpk
	logit sm wa free sex totexpk
restore

logit sm wa free sex totexpk



******** Kernel regression
*!start
clear all
use default
*drop if _n > 1000  // Spends too long if used all of 5722 observations. 

putmata pscore cs, replace 
mata 
	Y=pscore
	X=cs
	n=rows(X)
	k=cols(X)
end

mata
	real matrix bLL(real matrix Y, real matrix X, real scalar x, real scalar h)
	{
		real matrix bLL
		real matrix K
		real matrix Z
		real matrix kk
		real matrix zz
		real scalar i

		
		kk=normalden((X[1]-x)/h)
		for(i=2; i<=rows(X); i++){ 
			kk=kk\normalden((X[i]-x)/h)
		}
		K=diag(kk)

		zz=(1,(X[1]-x))
		for(i=2; i<=rows(X); i++){ 
			zz=zz\(1,(X[i]-x))
		}
		Z=zz

		bLL=luinv(quadcross(Z,kk,Z))*quadcross(Z,kk,Y)
		//bLL=luinv(Z'*K*Z)*Z'*K*Y   // same as above. 
		return(bLL)
	}

end

mata h=0.3

mata 
	bLL=bLL(Y,X,X[1],h)'
	for(i=2; i<=rows(X); i++){ 
		bLL=bLL\bLL(Y,X,X[i],h)'
	}
end

mata bLL
getmata (mean cs_d)=bLL

egen cs_m=mean(mean), by(cs)
sort cs
twoway (dot pscore cs) (line cs_m cs)

 // only works above Stata14
/*
npregress kernel pscore cs, kernel(gaussian) predict(mean cs_d) bwidth(0.3 0.3, copy)
save npreg2, replace 

npgraph
*/
use "https://raw.githubusercontent.com/jayjeo/public/master/GH210712_REGexercise/npreg2.dta", clear  // If your Stata is below version 15. 





******** Kernel regression (multiple regressors)
*!start
clear all
use default
drop if _n > 1000  // Spends too long if used all of 5722 observations. 

putmata pscore cs wa sex, replace 
mata 
	Y=pscore
	X=cs,wa,sex
	n=rows(X)
	k=cols(X)
end

//define Li-Racine kernel function (EV210717)
mata
	function liracine(real scalar xij, real scalar xj, real scalar h)
	{
		real scalar K
		if (xij==xj) K=1
		else K=h

		return(K)
	}
end

mata
	real matrix bLL(real matrix Y, real matrix X, real matrix x, real scalar h)
	{
		real matrix bLL
		real matrix K
		real matrix Z
		real matrix kk
		real matrix zz
		real scalar i

		
		kk=normalden((X[1,1]-x[1])/h)*liracine(X[1,2],x[2],h)*liracine(X[1,3],x[3],h)
		for(i=2; i<=rows(X); i++){ 
			kk=kk\normalden((X[i,1]-x[1])/h)*liracine(X[i,2],x[2],h)*liracine(X[i,3],x[3],h)
		}
		K=diag(kk)

		zz=1,(X[1,1]-x[1]),(X[1,2]-x[2]),(X[1,3]-x[3])
		for(i=2; i<=rows(X); i++){ 
			zz=zz\(1,(X[i,1]-x[1]),(X[i,2]-x[2]),(X[i,3]-x[3]))
		}
		Z=zz

		bLL=luinv(Z'*K*Z)*Z'*K*Y
		return(bLL)
	}
end

mata h=0.3

mata 
	bLL=bLL(Y,X,X[1,.],h)'
	for(i=2; i<=rows(X); i++){ 
		bLL=bLL\bLL(Y,X,X[i,.],h)'
	}
end

mata bLL

getmata (mean cs_d wa_d sex_d)=bLL
egen cs_m=mean(mean), by(cs)
sort cs
twoway (dot pscore cs) (line cs_m cs)

// only works above Stata14, Discrete kernel: liracine 
/*
npregress kernel pscore cs i.wa i.sex, kernel(gaussian) dkernel(liracine) ///
			bwidth(Mean:cs=0.3 Effect:cs=0.3 Mean:wa=0.3 Effect:wa=0.3 Mean:sex=0.3 Effect:sex=0.3) ///
			predict(mean cs_d wa_d sex_d) 
save npreg3, replace 
*/
use "https://raw.githubusercontent.com/jayjeo/public/master/GH210712_REGexercise/npreg3.dta", clear  // If your Stata is below version 15. 

