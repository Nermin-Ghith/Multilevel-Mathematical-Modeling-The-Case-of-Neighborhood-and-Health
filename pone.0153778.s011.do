********************************************************************************
* MERLO et al 2015
********************************************************************************

********************************************************************************
* TABLE 1
********************************************************************************

* Load the data
use "data_43291_Stata.dta", clear



* Number of individuals and number of neighborhoods
codebook neigh, compact



* Number of poor neighborhoods and number of individuals in those neighborhoods
codebook neigh if poorarea==1, compact



* Number of rich neighborhoods and number of individuals in those neighborhoods
codebook neigh if richarea==1, compact



* Descriptive statistics by neighborhood type
tabulate agegroup, missing generate(agegr)
by richarea, sort: tabstat psycmed private poor male agegr?, columns(statistics) format(%9.2f)



********************************************************************************
* TABLE 2 - PSYCHOTROPIC DRUG USE
********************************************************************************

* Load the data
use "data_43291_Stata.dta", clear



*==============================================================================*
* MODEL 1 - Simple logistic regression analysis
*==============================================================================*

* Fit model
logit psycmed male i.agegroup poor



* Store model results
estimates store r1m1



*------------------------------------------------------------------------------*
* Specific individual average effects
*------------------------------------------------------------------------------*

* Odds-ratios
logit, or cformat(%9.2f)



* Area under receiver operating characteristic (AU-ROC)
predict r1m1p, p
roctab psycmed r1m1p, graph summary
scalar r1m1auroc = r(area)



*==============================================================================*
* MODEL 2 - Multilevel logistic regression
*==============================================================================*

* Fit model
melogit psycmed male i.agegroup poor || neigh:



* Store model results
estimates store r1m2



*------------------------------------------------------------------------------*
* Specific individual average effects
*------------------------------------------------------------------------------*

* Odds-ratios
melogit, or cformat(%9.2f)



*------------------------------------------------------------------------------*
* General contextual effects
*------------------------------------------------------------------------------*

* Neighborhood variance
melogit, coeflegend
nlcom _b[var(_cons[neigh]):_cons], cformat(%9.3f)
scalar r1m2sigma2u =  _b[var(_cons[neigh]):_cons]



* Intraclass correlation (ICC)
estat icc



* Median odds-ratio (MOR)
nlcom exp(sqrt(2*_b[var(_cons[neigh]):_cons])*invnormal(0.75)), cformat(%9.2f)



* Area under receiver operating characteristic (AU-ROC) curve
predict r1m2p, mu
roctab psycmed r1m2p, graph summary
scalar r1m2auroc = r(area)



* AU-ROC change
display %9.3f r1m2auroc - r1m1auroc



*==============================================================================*
* MODEL 3 - Multilevel logistic regression with neighborhood income
*==============================================================================*

* Fit model
melogit psycmed male i.agegroup poor poorarea || neigh:



* Store model results
estimates store r1m3



*------------------------------------------------------------------------------*
* Specific individual average effects
*------------------------------------------------------------------------------*

* Odds-ratios
melogit, or cformat(%9.2f)



*------------------------------------------------------------------------------*
* Specific contextual average effects
*------------------------------------------------------------------------------*

* Low vs. high neighborhood income
nlcom exp(_b[poorarea]), cformat(%9.3f)



* 80% interval odds-ratio (IOR)
display %9.2f exp(_b[poorarea] - sqrt(2*_b[var(_cons[neigh]):_cons])*invnormal(0.9))
display %9.2f exp(_b[poorarea] + sqrt(2*_b[var(_cons[neigh]):_cons])*invnormal(0.9))



* Proportion of opposed odds-ratios (POOR)
display %9.2f normal(-_b[poorarea]/sqrt(2*_b[var(_cons[neigh]):_cons]))





*------------------------------------------------------------------------------*
* General contextual effects
*------------------------------------------------------------------------------*

* Neighborhood variance
melogit, coeflegend
nlcom _b[var(_cons[neigh]):_cons], cformat(%9.3f)
scalar r1m3sigma2u =  _b[var(_cons[neigh]):_cons]



* Proportion change in variance (PCV)
display %9.2f (r1m2sigma2u - r1m3sigma2u)/r1m2sigma2u



* Intraclass correlation (ICC)
estat icc



* Median odds-ratio (MOR)
nlcom exp(sqrt(2*_b[var(_cons[neigh]):_cons])*invnormal(0.75)), cformat(%9.2f)



* Compute area under receiver operating characteristic (AU-ROC) curve and graph 
* the curve
predict r1m3p, mu
roctab psycmed r1m3p, graph summary
scalar r1m3auroc = r(area)



* AU-ROC change
display %9.3f r1m3auroc - r1m2auroc



********************************************************************************
* FIGURE 1 - AU-ROC
********************************************************************************

roccomp psycmed r1m1p r1m2p, graph



********************************************************************************
* FIGURE 2 - PSYCHOTROPIC DRUG USE NEIGHBORHOOD RANKINGS
********************************************************************************

*==============================================================================*
* MODEL 2 - Multilevel logistic regression
*==============================================================================*

estimates restore r1m2

predict r1m2u, reffects ebmeans reses(r1m2use)

egen pickone = tag(neigh)

egen r1m2urank = rank(r1m2u) if pickone==1

serrbar r1m2u r1m2use r1m2urank if pickone==1, scale(1.96)



*==============================================================================*
* MODEL 3 - Multilevel logistic regression with neighborhood income
*==============================================================================*

estimates restore r1m3

predict r1m3u, reffects ebmeans reses(r1m3use)

egen r1m3urank = rank(r1m3u) if pickone==1

serrbar r1m3u r1m3use r1m3urank if pickone==1, scale(1.96)



********************************************************************************
* TABLE 3 - PRIVATE GP USE
********************************************************************************

* Load the data
use "data_43291_Stata.dta", clear


*==============================================================================*
* MODEL 1 - Simple logistic regression analysis
*==============================================================================*

* Fit model
logit private male i.agegroup rich



* Store model results
estimates store r2m1



*------------------------------------------------------------------------------*
* Specific individual average effects
*------------------------------------------------------------------------------*

* Odds-ratios
logit, or cformat(%9.2f)



* Area under receiver operating characteristic (AU-ROC)
predict r2m1p, p
roctab private r2m1p, graph summary
scalar r2m1auroc = r(area)



*==============================================================================*
* MODEL 2 - Multilevel logistic regression
*==============================================================================*

* Fit model
melogit private male i.agegroup rich || neigh:



* Store model results
estimates store r2m2



*------------------------------------------------------------------------------*
* Specific individual average effects
*------------------------------------------------------------------------------*

* Odds-ratios
melogit, or cformat(%9.2f)



*------------------------------------------------------------------------------*
* General contextual effects
*------------------------------------------------------------------------------*

* Neighborhood variance
melogit, coeflegend
nlcom _b[var(_cons[neigh]):_cons], cformat(%9.3f)
scalar r2m2sigma2u =  _b[var(_cons[neigh]):_cons]



* Intraclass correlation (ICC)
estat icc



* Median odds-ratio (MOR)
nlcom exp(sqrt(2*_b[var(_cons[neigh]):_cons])*invnormal(0.75)), cformat(%9.2f)



* Area under receiver operating characteristic (AU-ROC)
predict r2m2p, mu
roctab private r2m2p, graph summary
scalar r2m2auroc = r(area)



*==============================================================================*
* MODEL 3 - Multilevel logistic regression with neighborhood income
*==============================================================================*

* Fit model
melogit private male i.agegroup rich richarea || neigh:


* Store model results
estimates store r2m3



*------------------------------------------------------------------------------*
* Specific individual average effects
*------------------------------------------------------------------------------*

* Odds-ratios
melogit, or cformat(%9.2f)



*------------------------------------------------------------------------------*
* Specific contextual average effects
*------------------------------------------------------------------------------*

* High vs. low neighbourhood income
nlcom exp(_b[richarea]), cformat(%9.3f)



* 80% interval odds-ratio (IOR)
display %9.2f exp(_b[richarea] - sqrt(2*_b[var(_cons[neigh]):_cons])*invnormal(0.9))
display %9.2f exp(_b[richarea] + sqrt(2*_b[var(_cons[neigh]):_cons])*invnormal(0.9))



* Proportion of opposed odds-ratios (POOR)
display %9.2f normal(-_b[richarea]/sqrt(2*_b[var(_cons[neigh]):_cons]))



*------------------------------------------------------------------------------*
* General contextual effects
*------------------------------------------------------------------------------*

* Neighborhood variance
melogit, coeflegend
nlcom _b[var(_cons[neigh]):_cons], cformat(%9.3f)
scalar r2m3sigma2u =  _b[var(_cons[neigh]):_cons]



* Proportion change in variance (PCV)
display %9.2f (r2m2sigma2u - r2m3sigma2u)/r2m2sigma2u



* Intraclass correlation (ICC)
estat icc



* Median odds-ratio (MOR)
nlcom exp(sqrt(2*_b[var(_cons[neigh]):_cons])*invnormal(0.75)), cformat(%9.2f)



* Area under receiver operating characteristic (AU-ROC)
predict r2m3p, mu
roctab private r2m3p, graph summary
scalar r2m3auroc = r(area)



* AU-ROC change
display %9.3f r2m3auroc - r2m2auroc



********************************************************************************
* FIGURE 3 - AU-ROC
********************************************************************************
roccomp private r2m1p r2m2p, graph



********************************************************************************
* FIGURE 4 - PRIVATE GP USE NEIGHBORHOOD RANKINGS
********************************************************************************

*==============================================================================*
* MODEL 2 - Multilevel logistic regression
*==============================================================================*

estimates restore r2m2

predict r2m2u, reffects ebmeans reses(r2m2use)

egen pickone = tag(neigh)

egen r2m2urank = rank(r2m2u) if pickone==1

serrbar r2m2u r2m2use r2m2urank if pickone==1, scale(1.96)



*==============================================================================*
* MODEL 3 - Multilevel logistic regression with neighborhood income
*==============================================================================*

estimates restore r2m3

predict r2m3u, reffects ebmeans reses(r2m3use)

egen r2m3urank = rank(r2m3u) if pickone==1

serrbar r2m3u r2m3use r2m3urank if pickone==1, scale(1.96)



********************************************************************************
