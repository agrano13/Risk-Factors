/*This program imports the MEPS H192 full population characteritics data file
into SAS and conducts data analysis on several variables including: income,
access to healthcare, overall health, mental health, and diabetes complications
The beginning of the program is mostly from the MEPS website*/


/*This is the beginning of the program downloaded from the MEPS website*/
/****************************************************************\

PROGRAM:       C:\MEPS\PROG\EXAMPLE_EM1.SAS

DESCRIPTION:  	THIS EXAMPLE SHOWS HOW TO BUILD AN ANALYTIC FILE
               AND CREATE NEW VARIABLES TO EXAMINE THE
               RELATIONSHIP BETWEEN PERCEIVED HEALTH STATUS
               AND A PERSON'S WEEKLY EARNINGS, WHERE WEEKLY 
               EARNINGS ARE THOSE OF THE CURRENT MAIN JOB. 
               
               PERSON-LEVEL RECORDS ARE DIVIDED INTO QUARTILES
               BASED ON WEEKLY EARNINGS AND PERSON-LEVEL WEIGHTS.
               THE RESULT IS 4 EQUALLY WEIGHTED QUARTILES FOR
               WEEKLY EARNINGS. 

INPUT FILE:  	C:\MEPS\DATA\H62.SAS7BDAT (2002 FULL-YEAR POPULATION)
                     -- PERSON-LEVEL FILE

\****************************************************************/
/*Libname tells SAS the location to save the dataset*/
LIBNAME CMEPS V8 '/folders/myfolders' ;

/*Define the values to be used for the tables and tabulations*/
PROC FORMAT;
   VALUE GTZF
   -10 = '-10'
   0<-HIGH = '>0';
   /*This is the Value used with perceived health statuses*/
   VALUE HLTHF
   1 = '1 EXCELLENT'
   2 = '2 VERY GOOD'
   3 = '3 GOOD'
   4 = '4 FAIR'
   5 = '5 POOR';
   /*This is the value for weekly income quartiles*/
   VALUE QUARTF
   1 = '   1 LOWEST '
   2 = '   2        '
   3 = '   3        '
   4 = '   4 HIGHEST';
   /*Value used with weekly income frequencies*/
   VALUE WKLYF
   0.13-<49.99 = '    0.13 -  49.99'
   50-499.99 =   '   50.00 - 499.99'
   500-999.99 =  '  500.00 - 999.99'
   1000-HIGH =   '1,000.00+        ';
   /*Value used for "Access to Healthcare" varialbe*/
   VALUE ACCESS
   1 = 'UNABLE TO GET ACCESS'
   2 = 'ABLE TO GET ACCESS';
   /*Value used for diabetes complication variable*/
   VALUE DCOMP
   1 = 'DIABETES HAS CAUSED EYE PROBLEMS'
   2 = 'DIABETES HAS NOT CAUSED EYE PROBLEMS';
   /*Value used for poverty categories*/
   VALUE POVERTY
   1 = 'POOR (<= 100%)'
   2 = 'NEAR POOR (100% - 125%)'
   3 = 'LOW INCOME (125% - 200%)'
   4 = 'MIDDLE INCOME (200% - 400%)'
   5 = 'HIGH INCOME (>= 400%)';
   /*Value used for high cholesterol variable*/
   VALUE CHOL
   1 = 'HIGH CHOLESTEROL'
   0 = 'NEVER DIAGNOSED WITH HIGH CHOLESTEROL';
   /*Value used for smoking status*/
   VALUE SMOKE
   1 = 'CURRENTLY SMOKES'
   0 = 'NONSMOKER'
   -9 = 'NOT APPLICABLE';
   /*Value used for diabetes history*/
   VALUE DIAB
   1 = 'DIABETES'
   0 = 'NEVER DIAGNOSED WITH DIABETES';
   /*Value used for high blood pressure diagnosis*/
   VALUE BP
   1 = 'HIGH BLOOD PRESSURE'
   0 = 'NEVER DIAGNOSED WITH HIGH BLOOD PRESSURE';
RUN;

/***** THIS DATA STEP READS IN THE REQUIRED VARIABLES FROM THE *****/
/***** FULL-YEAR POPULATION FILE (HC-062).                     *****/

/***** THE 'WHERE' STATEMENT SUBSETS TO THE DESIRED SUBSET OF  *****/
/***** PERSONS: THOSE WITH A CURRENT MAIN JOB (CMJ) IN ROUNDS  *****/
/***** 4/2 THAT REPORTED EARNINGS, WEEKLY HOURS AND PERCEIVED  *****/
/***** HEALTH STATUS.                                          *****/

/***** A VALUE OF "-2" FOR THE '42' VARIABLES INDICATES THAT   *****/
/***** THE ACTUAL VALUE CAN BE OBTAINED FROM THE PRIOR ROUND,  *****/
/***** THE '31' VARIABLES.                                     *****/

/***** A WEEKLY EARNINGS VARIABLE "WKLYEARN' IS CREATED AS     *****/
/***** THE PRODUCE OF HOURLY WAGE ("HRLYWAGE") TIMES NUMBER    *****/
/***** OF WEEKLY WORK HOURS ("HOURS").                         *****/





/********						CHOLESTEROL						********/
DATA CHOLESTEROL;
   SET CMEPS.H192 (KEEP=  PERWT16F POVCAT16 CHOLDX FAMWT16F POVLEV16 ADSMOK42
				  DIABDX HIBPDX);
   WHERE    
            (CHOLDX > -1) AND (POVLEV16 > 0);
/* RECODE HIGH CHOLESTEROL VARIABLE TO BE 1 OR 0*/
   IF CHOLDX = 2
   		THEN CHOLDX = 0;
/* CLEAN UP INVALID VALUES FOR POVLEV16*/
   IF POVLEV16 = 0
   		THEN POVLEV16 = .1;
RUN;
/*Create the title to be on the Results*/
TITLE1 'SOCIOECONOMIC STATUS AND CHOLESTEROL - 2016 DATA';

/*Tabulate table to show poverty category vs. cholesterol diagnosis*/
PROC TABULATE DATA= CHOLESTEROL;
   CLASS POVCAT16 CHOLDX;
   TABLE POVCAT16,CHOLDX*(PCTN<CHOLDX>);
   FORMAT POVCAT16 POVERTY. CHOLDX CHOL. ;
   FREQ PERWT16F;
   WEIGHT PERWT16F;
RUN;


/*Run a test for normality for continuous poverty variable*/
TITLE1 'SOCIOECONOMIC STATUS AND CHOLESTEROL - 2016 DATA';
TITLE2 'TEST FOR NORMALITY';
ods graphics on;
proc univariate data= CHOLESTEROL;
	/*Use 'Cholesterol' variable to distinguish between groups being compared*/
	/*class CHOLDX;
	/*Define 'poverty' variable to be tested for normality*/
	var POVLEV16;
	/*Create a histogram with normal curve*/
	histogram/normal;
run;
ods graphics off;
/*Use Box-Cox transformation to make weekly income normal*/
TITLE1 'SOCIOECONOMIC STATUS AND CHOLESTEROL - 2016 DATA';
TITLE2 'BOX-COX TRANSFORMATION';
proc transreg data= CHOLESTEROL test;
model BOXCOX(POVLEV16)=identity(CHOLDX);
/*Output of the transformation will be in a new SAS table called "Transformation"*/
output out=TRANSFORMATION;
run;

/*Run a test for normality for the transformed poverty variable*/
TITLE1 'SOCIOECONOMIC STATUS AND CHOLESTEROL - 2016 DATA';
TITLE2 'TEST FOR NORMALITY (TRANSFORMED VALUES)';
proc univariate data= TRANSFORMATION;
	/*Use 'Cholesterol' variable to distinguish between groups being compared*/
	class TCHOLDX;
	/*Define 'poverty' variable to be tested*/
	var TPOVLEV16;
	/*Create a histogram with normal curve*/
	histogram/normal;
run;

/*Create a new SAS table called "TTEST"*/
data TTEST (KEEP = TPOVLEV16 AND TCHOLDX); 
SET TRANSFORMATION;
RUN;

/*Use two sample t test to test hypothesis that poverty level does not affect cholesterol*/
TITLE1 'SOCIOECONOMIC STATUS AND CHOLESTEROL - 2016 DATA';
TITLE2 'HYPOTHESIS TEST';
ods graphics on;
proc ttest data= TTEST cochran CI=equal umpu;
	/*Use the transformed cholesterol variable to distinguish between groups being compared*/
	class TCHOLDX;
	/*Define the transformed poverty variable to be tested for normality*/
	var TPOVLEV16;
run;

/* Run a nonparametric  significance test*/
proc NPAR1WAY DATA= CHOLESTEROL WILCOXON;
	VAR POVLEV16;
	CLASS CHOLDX;
RUN;
ods graphics off;





/********						SMOKING						********/
DATA SMOKING;
   SET CMEPS.H192 (KEEP=  PERWT16F POVCAT16 CHOLDX FAMWT16F POVLEV16 ADSMOK42
				  DIABDX HIBPDX);
   WHERE    
            (ADSMOK42 > -1) AND (POVLEV16 > 0);
/* RECODE SMOKING VARIABLE TO BE 1 OR 0*/
   IF ADSMOK42 = 2
   		THEN ADSMOK42 = 0;
/* CLEAN UP INVALID VALUES FOR POVLEV16*/
   IF POVLEV16 = 0
   		THEN POVLEV16 = .1;
RUN;

/*Tabulate table to show poverty category vs. smoking status*/
TITLE1 'SOCIOECONOMIC STATUS AND SMOKING - 2016 DATA';

PROC TABULATE DATA= SMOKING;
   CLASS POVCAT16 ADSMOK42;
   TABLE POVCAT16,ADSMOK42*(PCTN<ADSMOK42>);
   FORMAT POVCAT16 POVERTY. ADSMOK42 SMOKE. ;
   FREQ PERWT16F;
   WEIGHT PERWT16F;
RUN;

/*Run a test for normality for continuous poverty variable*/
TITLE1 'SOCIOECONOMIC STATUS AND SMOKING - 2016 DATA';
TITLE2 'TEST FOR NORMALITY';
ods graphics on;
proc univariate data= SMOKING;
	/*Use 'SMOKING' variable to distinguish between groups being compared*/
	/*class CHOLDX;
	/*Define 'poverty' variable to be tested for normality*/
	var POVLEV16;
	/*Create a histogram with normal curve*/
	histogram/normal;
run;
ods graphics off;
/*Use Box-Cox transformation to make weekly income normal*/
TITLE1 'SOCIOECONOMIC STATUS AND SMOKING - 2016 DATA';
TITLE2 'BOX-COX TRANSFORMATION';
proc transreg data= SMOKING test;
model BOXCOX(POVLEV16)=identity(ADSMOK42);
/*Output of the transformation will be in a new SAS table called "Transformation"*/
output out=STRANSFORMATION;
run;

/*Run a test for normality for the transformed poverty variable*/
TITLE1 'SOCIOECONOMIC STATUS AND SMOKING - 2016 DATA';
TITLE2 'TEST FOR NORMALITY (TRANSFORMED VALUES)';
proc univariate data= STRANSFORMATION;
	/*Use 'Cholesterol' variable to distinguish between groups being compared*/
	class TADSMOK42;
	/*Define 'poverty' variable to be tested*/
	var TPOVLEV16;
	/*Create a histogram with normal curve*/
	histogram/normal;
run;

/*Create a new SAS table called "TTEST"*/
data TTEST (KEEP = TPOVLEV16 AND TADSMOK42); 
SET STRANSFORMATION;
RUN;

/*Use two sample t test to test hypothesis that poverty level does not affect SMOKING*/
TITLE1 'SOCIOECONOMIC STATUS AND SMOKING - 2016 DATA';
TITLE2 'HYPOTHESIS TEST';
ods graphics on;
proc ttest data= TTEST cochran CI=equal umpu;
	/*Use the transformed SMOKING variable to distinguish between groups being compared*/
	class TADSMOK42;
	/*Define the transformed poverty variable to be tested for normality*/
	var TPOVLEV16;
run;

/* Run a nonparametric  significance test*/
proc NPAR1WAY DATA= SMOKING WILCOXON;
	VAR POVLEV16;
	CLASS ADSMOK42;
RUN;
ods graphics off;



/********						DIABETES					********/
DATA DIABETES;
   SET CMEPS.H192 (KEEP=  PERWT16F POVCAT16 CHOLDX FAMWT16F POVLEV16 ADSMOK42
				  DIABDX HIBPDX);
   WHERE    
            (DIABDX > -1) AND (POVLEV16 > 0);
/* RECODE DIABETES VARIABLE TO BE 1 OR 0*/
   IF DIABDX = 2 
   		THEN DIABDX = 0;
/* CLEAN UP INVALID VALUES FOR POVLEV16*/
   IF POVLEV16 = 0
   		THEN POVLEV16 = .1;
RUN;

/*Tabulate table to show poverty category vs. diabetes diagnosis*/
TITLE1 'SOCIOECONOMIC STATUS AND DIABETES - 2016 DATA';

PROC TABULATE DATA= DIABETES;
   CLASS POVCAT16 DIABDX;
   TABLE POVCAT16,DIABDX*(PCTN<DIABDX>);
   FORMAT POVCAT16 POVERTY. DIABDX DIAB. ;
   FREQ PERWT16F;
   WEIGHT PERWT16F;
RUN;

/*Run a test for normality for continuous poverty variable*/
TITLE1 'SOCIOECONOMIC STATUS AND DIABETES - 2016 DATA';
TITLE2 'TEST FOR NORMALITY';
ods graphics on;
proc univariate data= DIABETES;
	/*Use 'DIABETES' variable to distinguish between groups being compared*/
	/*class CHOLDX;
	/*Define 'poverty' variable to be tested for normality*/
	var POVLEV16;
	/*Create a histogram with normal curve*/
	histogram/normal;
run;
ods graphics off;
/*Use Box-Cox transformation to make weekly income normal*/
TITLE1 'SOCIOECONOMIC STATUS AND DIABETES - 2016 DATA';
TITLE2 'BOX-COX TRANSFORMATION';
proc transreg data= DIABETES test;
model BOXCOX(POVLEV16)=identity(DIABDX);
/*Output of the transformation will be in a new SAS table called "DTransformation"*/
output out=DTRANSFORMATION;
run;

/*Run a test for normality for the transformed poverty variable*/
TITLE1 'SOCIOECONOMIC STATUS AND DIABETES - 2016 DATA';
TITLE2 'TEST FOR NORMALITY (TRANSFORMED VALUES)';
proc univariate data= DTRANSFORMATION;
	/*Use 'DIABETES' variable to distinguish between groups being compared*/
	class TDIABDX;
	/*Define 'poverty' variable to be tested*/
	var TPOVLEV16;
	/*Create a histogram with normal curve*/
	histogram/normal;
run;

/*Create a new SAS table called "TTEST"*/
data TTEST (KEEP = TPOVLEV16 AND TDIABDX); 
SET DTRANSFORMATION;
RUN;

/*Use two sample t test to test hypothesis that poverty level does not affect DIABETES*/
TITLE1 'SOCIOECONOMIC STATUS AND DIABETES - 2016 DATA';
TITLE2 'HYPOTHESIS TEST';
ods graphics on;
proc ttest data= TTEST cochran CI=equal umpu;
	/*Use the transformed cholesterol variable to distinguish between groups being compared*/
	class TDIABDX;
	/*Define the transformed poverty variable to be tested for normality*/
	var TPOVLEV16;
run;

/* Run a nonparametric  significance test*/
proc NPAR1WAY DATA= DIABETES WILCOXON;
	VAR POVLEV16;
	CLASS DIABDX;
RUN;
ods graphics off;



/********						BLOOD PRESSURE				********/
DATA BPRESSURE;
   SET CMEPS.H192 (KEEP=  PERWT16F POVCAT16 CHOLDX FAMWT16F POVLEV16 ADSMOK42
				  DIABDX HIBPDX);
   WHERE    
            (HIBPDX > -1) AND (POVLEV16 > 0);
/*RECODE BLOODPRESSURE VARIABLE TO BE 1 OR 0*/
   IF HIBPDX = 2
   		THEN HIBPDX = 0;
/* CLEAN UP INVALID VALUES FOR POVLEV16*/
   IF POVLEV16 = 0
   		THEN POVLEV16 = .1;
RUN;

/*Tabulate table to show poverty category vs. high blood pressure diagnosis*/
TITLE1 'SOCIOECONOMIC STATUS AND HIGH BLOOD PRESSURE - 2016 DATA';

PROC TABULATE DATA= BPRESSURE;
   CLASS POVCAT16 HIBPDX;
   TABLE POVCAT16,HIBPDX*(PCTN<HIBPDX>);
   FORMAT POVCAT16 POVERTY. HIBPDX BP. ;
   FREQ PERWT16F;
   WEIGHT PERWT16F;
RUN;

/*Run a test for normality for continuous poverty variable*/
TITLE1 'SOCIOECONOMIC STATUS AND BLOOD PRESSURE - 2016 DATA';
TITLE2 'TEST FOR NORMALITY';
ods graphics on;
proc univariate data= BPRESSURE;
	/*Use 'BLOOD PRESSURE' variable to distinguish between groups being compared*/
	/*class CHOLDX;
	/*Define 'poverty' variable to be tested for normality*/
	var POVLEV16;
	/*Create a histogram with normal curve*/
	histogram/normal;
run;
ods graphics off;
/*Use Box-Cox transformation to make weekly income normal*/
TITLE1 'SOCIOECONOMIC STATUS AND BLOOD PRESSURE - 2016 DATA';
TITLE2 'BOX-COX TRANSFORMATION';
proc transreg data= BPRESSURE test;
model BOXCOX(POVLEV16)=identity(HIBPDX);
/*Output of the transformation will be in a new SAS table called "BPTransformation"*/
output out=BPTRANSFORMATION;
run;

/*Run a test for normality for the transformed poverty variable*/
TITLE1 'SOCIOECONOMIC STATUS AND BLOOD PRESSURE - 2016 DATA';
TITLE2 'TEST FOR NORMALITY (TRANSFORMED VALUES)';
proc univariate data= BPTRANSFORMATION;
	/*Use 'BLOOD PRESSURE' variable to distinguish between groups being compared*/
	class HIBPDX;
	/*Define 'poverty' variable to be tested*/
	var TPOVLEV16;
	/*Create a histogram with normal curve*/
	histogram/normal;
run;

/*Create a new SAS table called "TTEST"*/
data TTEST (KEEP = TPOVLEV16 AND THIBPDX); 
SET BPTRANSFORMATION;
RUN;

/*Use two sample t test to test hypothesis that poverty level does not affect BLOOD PRESSURE*/
TITLE1 'SOCIOECONOMIC STATUS AND BLOOD PRESSURE - 2016 DATA';
TITLE2 'HYPOTHESIS TEST';
ods graphics on;
proc ttest data= TTEST cochran CI=equal umpu;
	/*Use the transformed cholesterol variable to distinguish between groups being compared*/
	class THIBPDX;
	/*Define the transformed poverty variable to be tested for normality*/
	var TPOVLEV16;
run;

/* Run a nonparametric  significance test*/
proc NPAR1WAY DATA= BPRESSURE WILCOXON;
	VAR POVLEV16;
	CLASS HIBPDX;
RUN;
ods graphics off;
