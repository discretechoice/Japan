/*

STATA 13.0 code for

The effect of the Free High School Tuition 
law on upper-secondary school choice in Japan

YEAR 2012

Petr Mariel 
Department of Quantitative Methods
University of the Basque Country (UPV/EHU)
Avda. Lehendakari Aguirre, 83
E48015 Bilbao, Spain
E-mail: petr.mariel@ehu.es
Tel.: +34.94.601.3848

Nobuhiro Sanko
Graduate School of Business Administration
Kobe University
2-1 Rokkodai-cho, Nada-ku, Kobe-shi 657-8501 Japan

Ainhoa Vega-Bayo
Departament of Economic Analysis
University of the Basque Country (UPV/EHU) 
Avda. Lehendakari Aguirre, 83
E48015 Bilbao, Spain, 
*/

drop _all
set more off
set scheme s1mono

/* cd "/Users/.." */
use "pisa2012japan.dta"

/* OUTCOME VARIABLE */

gen     SCTYPEREC = .
replace SCTYPEREC = 1 if STRATUM=="JPN0101" /* Public and Academic*/
replace SCTYPEREC = 2 if STRATUM=="JPN0202" /* Public and Practical*/
replace SCTYPEREC = 3 if STRATUM=="JPN0203" /* Private and Academic*/
replace SCTYPEREC = 4 if STRATUM=="JPN0204" /* Private and Practical*/


label define SCTYPEREC 1 "PubAc", modify
label define SCTYPEREC 2 "PubPr", modify
label define SCTYPEREC 3 "PriAc", modify
label define SCTYPEREC 4 "PriPr", modify

tab SCTYPEREC

/* EXPLANATORY VARIABLE  */

/*
FEMALE          float   %9.0g      Gender           
NOIMMIG         float   %9.0g      No immigration            
TWOPARENTS      float   %9.0g      Two parent family           
FNWORKING       float   %9.0g      Father not working             
MNWORKING       float   %9.0g      Mother not working             
FUNIV           float   %9.0g      Father university education           
MUNIV           float   %9.0g      Mother university education           
UNIQUESC        long    %8.0g     
ESTRATO         long    %8.0g      ESTRATO    7-Digit Stratum ID
PRIVATE         float   %9.0g                 
TRUANCYSTD      float   %9.0g      Standardized values of (TRUANCY)
CONTROLSTD      float   %9.0g 
AGE             float   %9.0g      Age of student
ANXMAT          float   %9.0g      Mathematics Anxiety
COGACT          float   %9.0g      Cognitive Activation in Mathematics Lessons
CULTPOS         float   %9.0g      Cultural Possessions
ESCS            float   %9.0g      Index of economic, social and cultural status
FAILMAT         float   %9.0g      Attributions to Failure in Mathematics
FISCED          float   %17.0g     label48    Educational level of father (ISCED)
HEDRES          float   %9.0g      Home educational resources
HISEI           float   %9.0g      Highest parental occupational status
*/

/* CLEANING DATA */
	  
drop if missing(FNWORKING)		  
drop if missing(MNWORKING)
drop if missing(TWOPARENTS)
drop if missing(SIBLINGS)
drop if missing(GRANDPARENTS)
drop if missing(CULTPOS)
drop if missing(MISCED)
drop if missing(FISCED)
drop if missing(WEALTH)
drop if missing(HEDRES)
drop if missing(HISEI)
   
/* Descriptive statistics by URBAN-RURAL */

/* CITY AND LARGECITY */
summarize FNWORKING MNWORKING TWOPARENTS SIBLINGS GRANDPARENTS /// 
           MISCED FISCED CULTPOS WEALTH HEDRES HISEI 	 if CITYANDLARGECITY == 1
tab MISCED if CITYANDLARGECITY == 1
tab FISCED if CITYANDLARGECITY == 1
		   
/* TOWN AND SMALL TOWN */
summarize FNWORKING MNWORKING TWOPARENTS SIBLINGS GRANDPARENTS /// 
           MISCED FISCED CULTPOS WEALTH HEDRES HISEI 	 if CITYANDLARGECITY == 0

tab MISCED if CITYANDLARGECITY == 0
tab FISCED if CITYANDLARGECITY == 0  


/*------------------------------------*/
/*  ANALYSIS FOR CITY AND LARGECITY   */
/* ---------------------------------- */

tab SCTYPEREC if CITYANDLARGECITY == 1

tab SCTYPEREC FNWORKING if CITYANDLARGECITY == 1
tab SCTYPEREC MNWORKING if CITYANDLARGECITY == 1
tab SCTYPEREC TWOPARENTS if CITYANDLARGECITY == 1
tab SCTYPEREC SIBLINGS if CITYANDLARGECITY == 1
tab SCTYPEREC GRANDPARENTS if CITYANDLARGECITY == 1

mlogit SCTYPEREC ///    
               /* dummy */ ///
               FNWORKING MNWORKING TWOPARENTS SIBLINGS GRANDPARENTS  ///
               /* continous */ ///
               CULTPOS  MISCED  /// 
               FISCED WEALTH  /// 
               HEDRES HISEI if CITYANDLARGECITY == 1, base(4) 

/*************** TESTS ****************/
mlogtest, lr               
mlogtest, combine

/*

 Ho: All coefficients except intercepts associated with a given pair
     of alternatives are 0 (i.e., alternatives can be combined).

Alternatives tested|     chi2   df   P>chi2
-------------------+------------------------
       1-       2  |  181.114   11    0.000
       1-       3  |   86.371   11    0.000
       1-       4  |   41.904   11    0.000
       2-       3  |  338.077   11    0.000
       2-       4  |   17.453   11    0.095
       3-       4  |   96.305   11    0.000
--------------------------------------------

*/

/*************** RESULTS, GRAPHS ****************/

prchange, x(FNWORKING=0 MNWORKING=1 TWOPARENTS=1 SIBLINGS=1 GRANDPARENTS=0  ///
               MISCED=5    FISCED=5  /// 
               WEALTH=mean CULTPOS=mean /// 
               HEDRES=mean HISEI=mean)
               
mlogplot   FNWORKING MNWORKING TWOPARENTS SIBLINGS GRANDPARENTS  ///
               MISCED FISCED /// 
               WEALTH CULTPOS ///
			   HEDRES HISEI, dc std(00000uussss) labels packed min(-.11)  max(.11)

			   
/**********************************/
/*** Effect of Father education ***/
/**********************************/
    	   
prgen FISCED, x(FNWORKING=0 MNWORKING=1 TWOPARENTS=1 GRANDPARENTS=0 ///
               CULTPOS=mean MISCED=5  ///
               WEALTH=mean  ///
               HEDRES=mean HISEI=mean)  n(6) ///
               generate(SFISC) from(1) to(6)			   

label var SFISCp1 "Pub Academic"
label var SFISCp2 "Pub Practical"
label var SFISCp3 "Pri Academic"
label var SFISCp4 "Pri Practical"

twoway connected SFISCp4 SFISCp3 SFISCp2 SFISCp1 SFISCx , ///
        l1("Probability") msymbol(o d t sh x oh) ///
        legend(order(4 3 2 1)) ///
        lcolor(gs0 gs0 gs0 gs0) ///
        lpattern(dot solid dash dot ) ///
        mfcolor(gs0 gs0 gs0 gs0 ) ///
        mlcolor(gs0 gs0 gs0 gs0 ) ///
        msize(vlarge vlarge vlarge vlarge)
		
/************************/
/*** Effect of Wealth ***/
/************************/
    
prgen WEALTH, x(FNWORKING=0 MNWORKING=1 TWOPARENTS=1 GRANDPARENTS=0 ///
               CULTPOS=mean MISCED=5  ///
               FISCED=mean  ///
               HEDRES=mean HISEI=mean)  n(6) ///
               generate(SWFC) from(-2.5) to(2.5)			   

label var SWFCp1 "Pub Academic"
label var SWFCp2 "Pub Practical"
label var SWFCp3 "Pri Academic"
label var SWFCp4 "Pri Practical"

twoway connected SWFCp4 SWFCp3 SWFCp2 SWFCp1 SWFCx , ///
        l1("Probability") msymbol(o d t sh x oh) ///
        legend(order(4 3 2 1)) ///
        lcolor(gs0 gs0 gs0 gs0) ///
        lpattern(dot solid dash dot ) ///
        mfcolor(gs0 gs0 gs0 gs0 ) ///
        mlcolor(gs0 gs0 gs0 gs0 ) ///
        msize(vlarge vlarge vlarge vlarge)

/*------------------------------------*/
/*  ANALYSIS FOR TOWN AND SMALL TOWN  */
/* ---------------------------------- */

tab SCTYPEREC if CITYANDLARGECITY == 0

tab SCTYPEREC FNWORKING if CITYANDLARGECITY == 0
tab SCTYPEREC MNWORKING if CITYANDLARGECITY == 0
tab SCTYPEREC TWOPARENTS if CITYANDLARGECITY == 0
tab SCTYPEREC SIBLINGS if CITYANDLARGECITY == 0
tab SCTYPEREC GRANDPARENTS if CITYANDLARGECITY == 0

mlogit SCTYPEREC ///    
               /* dummy */ ///
               FNWORKING MNWORKING TWOPARENTS SIBLINGS GRANDPARENTS  ///
               /* continous */ ///
               CULTPOS  MISCED  /// 
               FISCED WEALTH  /// 
               HEDRES HISEI if CITYANDLARGECITY == 0, base(4) 
			   
/*************** TESTS ****************/
mlogtest, lr               
mlogtest, combine

/*

 Ho: All coefficients except intercepts associated with a given pair
     of alternatives are 0 (i.e., alternatives can be combined).

Alternatives tested|     chi2   df   P>chi2
-------------------+------------------------
       1-       2  |   81.211   11    0.000
       1-       3  |   27.882   11    0.003
       1-       4  |   19.889   11    0.047
       2-       3  |   73.924   11    0.000
       2-       4  |   21.337   11    0.030
       3-       4  |   17.825   11    0.086
--------------------------------------------


*/

/*************** RESULTS, GRAPHS ****************/

prchange, x(FNWORKING=0 MNWORKING=1 TWOPARENTS=1 SIBLINGS=1 GRANDPARENTS=0  ///
               MISCED=5    FISCED=5  /// 
               WEALTH=mean CULTPOS=mean /// 
               HEDRES=mean HISEI=mean)
               
mlogplot   FNWORKING MNWORKING TWOPARENTS SIBLINGS GRANDPARENTS  ///
               MISCED FISCED /// 
               WEALTH CULTPOS ///
			   HEDRES HISEI, dc std(00000uussss) labels packed min(-.11)  max(.11)
		   
/**********************************/
/*** Effect of Father education ***/
/**********************************/
			   
prgen FISCED, x(FNWORKING=0 MNWORKING=1 TWOPARENTS=1 GRANDPARENTS=0 ///
               CULTPOS=mean MISCED=5  ///
               WEALTH=mean  ///
               HEDRES=mean HISEI=mean)  n(6) ///
               generate(RSFISC) from(1) to(6)			   

label var RSFISCp1 "Pub Academic"
label var RSFISCp2 "Pub Practical"
label var RSFISCp3 "Pri Academic"
label var RSFISCp4 "Pri Practical"

twoway connected RSFISCp4 RSFISCp3 RSFISCp2 RSFISCp1 RSFISCx , ///
        l1("Probability") msymbol(o d t sh x oh) ///
        legend(order(4 3 2 1)) ///
        lcolor(gs0 gs0 gs0 gs0) ///
        lpattern(dot solid dash dot ) ///
        mfcolor(gs0 gs0 gs0 gs0 ) ///
        mlcolor(gs0 gs0 gs0 gs0 ) ///
        msize(vlarge vlarge vlarge vlarge)
		
/************************/
/*** Effect of Wealth ***/
/************************/
    
prgen WEALTH, x(FNWORKING=0 MNWORKING=1 TWOPARENTS=1 GRANDPARENTS=0 ///
               CULTPOS=mean MISCED=5  ///
               FISCED=mean  ///
               HEDRES=mean HISEI=mean)  n(6) ///
               generate(RSWFC) from(-2.5) to(2.5)			   

label var RSWFCp1 "Pub Academic"
label var RSWFCp2 "Pub Practical"
label var RSWFCp3 "Pri Academic"
label var RSWFCp4 "Pri Practical"			   
			   
twoway connected RSWFCp4 RSWFCp3 RSWFCp2 RSWFCp1 RSWFCx , ///
        l1("Probability") msymbol(o d t sh x oh) ///
        legend(order(4 3 2 1)) ///
        lcolor(gs0 gs0 gs0 gs0) ///
        lpattern(dot solid dash dot ) ///
        mfcolor(gs0 gs0 gs0 gs0 ) ///
        mlcolor(gs0 gs0 gs0 gs0 ) ///
        msize(vlarge vlarge vlarge vlarge)
		
/*    END OF FILE   */        
			   
