/*

STATA 13.0 code for

The effect of the Free High School Tuition 
law on upper-secondary school choice in Japan

YEAR 2009

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

use "pisa2009japan.dta"

/* CLEAN AND RENAME DATA */

/* FEMALE */
gen FEMALE = .
replace FEMALE = 1 if st04q01 == 1
replace FEMALE = 0 if st04q01 == 2

/* Family immig status, =1 if family is immigrant */
gen NOIMMIG = .
replace NOIMMIG = 1 if immig == 2 | immig == 2
replace NOIMMIG = 0 if immig == 1

/* Family structure, two parents, =1 if two parents */
gen TWOPARENTS = .
replace TWOPARENTS = 1 if famstruc == 2
replace TWOPARENTS = 0 if famstruc !=2 & famstruc !=.

/* SIBLINGS yes = 1 */
gen SIBLINGS = .
replace SIBLINGS=1 if (st08q03==1 | st08q04==1)
replace SIBLINGS=0 if (st08q03!=1 & st08q04!=1 & st08q03!=. & st08q04!=.)

/* FATHER / MOTHER NOT WORKING FT = 1 */
gen FNWORKING = .
replace FNWORKING = 1 if st16q01 != 1
replace FNWORKING = 0 if st16q01 == 1
gen MNWORKING = .
replace MNWORKING = 1 if st12q01 != 1
replace MNWORKING = 0 if st12q01 == 1

/* PARENTS' EDUC */
gen FUNIV = .
replace FUNIV = 1 if fisced>=5
replace FUNIV = 0 if fisced<=4

gen MUNIV = .
replace MUNIV = 1 if misced>=5
replace MUNIV = 0 if misced<=4

/* Unique SCHOOL ID */
tostring schoolid, gen(SCHOOL)
gen SCHOOLUNICO = cnt + SCHOOL
encode SCHOOLUNICO, gen(UNIQUESC)

/* GRANDPARENTS */
gen GRANDPARENTS = 0
replace GRANDPARENTS = 1 if st08q05 == 1 
tab GRANDPARENTS

gen COMPETHIGH  = 0
replace COMPETHIGH = 1 if sc13q07 == 1

/* OUTCOME VARIABLE */

gen     SCTYPEREC = .
replace SCTYPEREC = 1 if stratum==39201 /* Public and Academic*/
replace SCTYPEREC = 2 if stratum==39202 /* Public and Practical*/
replace SCTYPEREC = 3 if stratum==39203 /* Private and Academic*/
replace SCTYPEREC = 4 if stratum==39204 /* Private and Practical*/


label define SCTYPEREC 1 "PubAc", modify
label define SCTYPEREC 2 "PubPr", modify
label define SCTYPEREC 3 "PriAc", modify
label define SCTYPEREC 4 "PriPr", modify

tab SCTYPEREC

/* RENAME */

rename cultposs CULTPOS
rename misced MISCED
rename fisced FISCED
rename wealth WEALTH
rename hedres HEDRES
rename hisei HISEI
rename age AGE
rename escs ESCS

/* Clean the data */

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


/* 
 SC04Q01 School Community 25 F1 61 - 61 
 1 Village
 2 Small Town
 3 Town
 4 City
 5 Large City
 
 A <village, <village, <village, hamlet or rural area> (fewer than 3 000 people) people) people) 1
 A <small town> (3 000 to about 15 000 people) 2
 A <town> (15 000 to about 100 000 people) people) people) 3
 A <city> (100 000 to about 1 000 000 people) 4
 A large <city> with over 1 000 000 people
 
 */
 
tab sc04q01
gen LOCAT = sc04q01

gen LOCAT1 = 0
replace LOCAT1 = 1 if LOCAT == 1 

gen LOCAT12 = 0
replace LOCAT12 = 1 if LOCAT == 1  | LOCAT == 2

gen LOCAT123 = 0
replace LOCAT123 = 1 if LOCAT == 1  | LOCAT == 2 | LOCAT == 3

gen LOCAT1234 = 0
replace LOCAT1234 = 1 if LOCAT == 1  | LOCAT == 2 | LOCAT == 3 | LOCAT == 4

gen LOCAT12345 = 0
replace LOCAT12345 = 1 if LOCAT == 1  | LOCAT == 2 | LOCAT == 3 | LOCAT == 4 | LOCAT == 5

gen CITYANDLARGECITY  = 1 - LOCAT123 
gen LARGECITY         = 1 - LOCAT1234
 
tab CITYANDLARGECITY
tab LARGECITY

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
Alternatives tested|     chi2   df   P>chi2
-------------------+------------------------
       1-       2  |  158.613   11    0.000
       1-       3  |  161.483   11    0.000
       1-       4  |   60.687   11    0.000
       2-       3  |  361.547   11    0.000
       2-       4  |   12.158   11    0.352
       3-       4  |  166.418   11    0.000
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
        msize(vlarge vlarge vlarge vlarge) ///
		yscale(range(0 0.8))
		
/*******************/
/*** Effect of Wealth ***/
/********************/
    

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

		
/*--------------- --------------------*/
/*  ANALYSIS FOR TOWN AND SMALL TOWN  */
/* ---------------------------------- */

tab SCTYPEREC if CITYANDLARGECITY == 0
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
       1-       2  |  106.764   11    0.000
       1-       3  |   24.838   11    0.010
       1-       4  |   17.021   11    0.107
       2-       3  |   48.851   11    0.000
       2-       4  |   31.626   11    0.001
       3-       4  |   10.496   11    0.486
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

