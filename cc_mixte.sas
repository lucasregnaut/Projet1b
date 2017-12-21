/*----------------------------------------------------------------------------------------------------------*/
/* 	   					                   PROGRAM : cc_mixte.sas		      	    		  	 			*/
/*----------------------------------------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------------*/
/*                                DATA MANAGEMENT                      	 			*/
/*----------------------------------------------------------------------------------*/

libname bibsas "F:\M2_biostat\STA302_mixte\Projet";
libname bibsas "E:\M2_biostat\STA302_mixte\Projet";
libname bibsas "D:\M2_biostat\STA302_mixte\Projet";

PROC CONTENTS data=bibsas.projet1b;
RUN;
/* notre jeu de donn�es contient 677 observations et 11 variables */

*d�finition des labels;
data projet1b;
	set bibsas.projet1b;
	label SEXE='Sexe du patient';
	label AGEDIAG='Age au diagnostic';
	label TYPE_AMS="Type d'atrophie multisyst�matis�e (AMS)";
	label CERTITUDE='Certitude du diagnostic';
	label DYSAUTO='Pr�sence et type de premiers sympt�mes';
	label DCD='Indicateur de d�c�s';
	label AGE_EVNT="Age � l'�v�nement";
	label DELAI_VIS="D�lai depuis le diagnostic (en ann�es)";
	label DELAI_SYMPT="D�lai entre les premiers sympt�mes et la maladie (en ann�es)";
	label UMSARS_1_2="Somme des UMSARS 1 et 2";
run;

*pour avoir une observation par individu;
data first;
set projet1b;
	by id;
	if first.id=1 then output;
run;

*d�finition du temps r�trospectif;
data projet1b;
set projet1b;
	temps_retro=AGEDIAG-AGE_EVNT+DELAI_VIS;
	label temps_retro="Temps r�trospectif avant le d�c�s (en ann�es)";
run;

*on ne prend que les d�c�d�s;
data projet1b_deces;
set projet1b;
	if DCD="Oui" then output;
run;

data first_deces;
set projet1b_deces;
	by id;
	if first.id=1 then output;
run;

*FAUX;
data first_deces2;
set first;
	if DCD="Oui" then output;
run;

*exporter pour travailler sur R : To export to comma-delimited use the following syntax;
PROC EXPORT DATA=projet1b 
OUTFILE='D:\M2_biostat\STA302_mixte\Projet\projet1b.csv' 
DBMS=CSV REPLACE;
DELIMITER=";"; 
RUN;
PROC EXPORT DATA=first 
OUTFILE='D:\M2_biostat\STA302_mixte\Projet\first.csv' 
DBMS=CSV REPLACE;
DELIMITER=";"; 
RUN;


/* 
normalit� : pour intercept et pente al�atoire
*/



/*----------------------------------------------------------------------------------*/
/*                             ANALYSES STATISTIQUES                  	 			*/
/*----------------------------------------------------------------------------------*/

/*************************/
/*      Descriptif       */
/*************************/

*v�rifier la normalit� (� baseline);
proc univariate data=first_deces;
var UMSARS_1_2 ;
histogram UMSARS_1_2 /normal;
run;

*descriptif variables quali (au total, et selon l'indicateur de d�c�s);
proc tabulate data=first_deces missing noseps formchar(1,3,4,5,6,7,8,9,10,11)=" ���������";
	class	SEXE TYPE_AMS certitude dysauto;
	table	(SEXE TYPE_AMS certitude dysauto all="Total")
			,
			(all="TOTAL")*(n="N" colpctn="%")*f=5.1
		/misstext=" " rtspace=50;
run;
*verif ok;
proc freq data=first_deces;
tables sexe type_ams certitude dysauto sexe*dcd type_ams*dcd certitude*dcd dysauto*dcd;
run;

*descriptif variables quanti (table first);
proc tabulate data=first_deces missing noseps formchar (1,3,4,5,6,7,8,9,10,11)=" ���������" vardef=df;
	var		agediag delai_sympt age_evnt;
	table	(agediag delai_sympt age_evnt)
			,
			(all="TOTAL")*(n nmiss mean std min q1 median q3 max)*f=15.1
		/misstext=" " rtspace=35;
	keylabel n="N" nmiss="Donn�es manquantes" mean="Moyenne" std="Ecart-type" stderr="SE" 
			min="Minimum" q1="1er quartile" median="M�diane" q3="3e quartile" max="Maximum";
run;
*verif ok;
proc means data=first_deces n nmiss mean std min q1 median q3 max maxdec=2;
var agediag delai_sympt age_evnt;
class dcd;
run;

*descriptif variables quanti (table projet1b);
proc tabulate data=projet1b_deces missing noseps formchar (1,3,4,5,6,7,8,9,10,11)=" ���������" vardef=df;
	var		delai_vis UMSARS_1_2 temps_retro;
	table	(delai_vis UMSARS_1_2 temps_retro)
			,
			(all="TOTAL")*(n nmiss mean std min q1 median q3 max)*f=15.1
		/misstext=" " rtspace=35;
	keylabel n="N" nmiss="Donn�es manquantes" mean="Moyenne" std="Ecart-type" stderr="SE" 
			min="Minimum" q1="1er quartile" median="M�diane" q3="3e quartile" max="Maximum";
run;
*verif ok;
proc means data=projet1b n nmiss mean std min q1 median q3 max maxdec=2;
var delai_vis UMSARS_1_2 temps_retro;
class dcd;
run;
proc means data=projet1b_deces n nmiss mean std min q1 median q3 max maxdec=2;
var delai_vis UMSARS_1_2 temps_retro;
run;




/*************************/
/* Analyses univariables */
/*************************/

/* SEXE */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id SEXE;
model UMSARS_1_2= SEXE temps_retro temps_retro*temps_retro SEXE*temps_retro SEXE*temps_retro*temps_retro/s;
random intercept temps_retro /sub=id type=UN G GCORR;
run;

/* TYPE_AMS */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id TYPE_AMS;
model UMSARS_1_2= TYPE_AMS temps_retro temps_retro*temps_retro TYPE_AMS*temps_retro TYPE_AMS*temps_retro*temps_retro/s;
random intercept temps_retro /sub=id type=UN G GCORR;
run;

/* CERTITUDE */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id CERTITUDE;
model UMSARS_1_2= CERTITUDE temps_retro temps_retro*temps_retro CERTITUDE*temps_retro CERTITUDE*temps_retro*temps_retro/s;
random intercept temps_retro /sub=id type=UN G GCORR;
run;

/* DYSAUTO */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id dysauto;
model UMSARS_1_2= DYSAUTO temps_retro temps_retro*temps_retro DYSAUTO*temps_retro DYSAUTO*temps_retro*temps_retro/s;
random intercept temps_retro /sub=id type=UN G GCORR;
run;

/* DELAI_SYMPT */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id;
model UMSARS_1_2=temps_retro temps_retro*temps_retro DELAI_SYMPT DELAI_SYMPT*temps_retro DELAI_SYMPT*temps_retro*temps_retro/s ;
random intercept temps_retro /sub=id type=UN G GCORR;
run; *significatif;



/*************************/
/* Analyse multivariable */
/*************************/
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id type_ams SEXE CERTITUDE DYSAUTO;
model UMSARS_1_2=temps_retro temps_retro*temps_retro DELAI_SYMPT DELAI_SYMPT*temps_retro SEXE CERTITUDE DYSAUTO TYPE_AMS/s residual vciry outp=cond outpm=marg;
random intercept temps_retro /sub=id type=UN G GCORR;
run; 

*sans type_ams;
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id SEXE CERTITUDE DYSAUTO;
model UMSARS_1_2=temps_retro temps_retro*temps_retro DELAI_SYMPT DELAI_SYMPT*temps_retro SEXE CERTITUDE DYSAUTO/s residual vciry outp=cond outpm=marg;
random intercept temps_retro /sub=id type=UN G GCORR;
run;

*sans dysauto/type_ams;
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id SEXE CERTITUDE;
model UMSARS_1_2=temps_retro temps_retro*temps_retro DELAI_SYMPT DELAI_SYMPT*temps_retro SEXE CERTITUDE/s residual vciry outp=cond outpm=marg;
random intercept temps_retro temps_retro*temps_retro/sub=id type=UN G GCORR;
run;

*sans certitude/dysauto/type_ams ;
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id sexe;
model UMSARS_1_2=temps_retro temps_retro*temps_retro SEXE DELAI_SYMPT DELAI_SYMPT*temps_retro /s residual vciry outp=cond outpm=marg;
random intercept temps_retro /sub=id type=UN G GCORR;
estimate "Niveau moyen chez les femmes, au d�lai symptomatique=0, � T=0" int 1 SEXE 1 0 DELAI_SYMPT 0/ cl;
estimate "Niveau moyen chez les hommes, au d�lai symptomatique=0, � T=0" int 1 SEXE 0 1 DELAI_SYMPT 0/ cl;
estimate "Niveau moyen chez les femmes, au d�lai symptomatique=2, � T=-2" int 1 SEXE 1 0 temps_retro -2 temps_retro*temps_retro -2 DELAI_SYMPT*temps_retro -2/ cl;
estimate "Niveau moyen chez les hommes, au d�lai symptomatique=0, � T=-2" int 1 SEXE 0 1 temps_retro -2 temps_retro*temps_retro -2 DELAI_SYMPT*temps_retro 0/ cl;
run; *mod�le final;


/*
Selection pas � pas descendante (mod�le � pente et intercept al�atoire) :
- enlever la p valeur la plus �lev�e
- regarder AIC
- regarder si les coefficients ne changent pas trop
- si on enl�ve une interaction : test du rapport de vraisemblance
*/


/* Cr�ation des quantiles pour la variable temps r�tro 
proc univariate data=projet1b_deces;
var temps_retro;
output out=quantile pctlpre=P_ pctlpts=0 to 100 by 20;
run;
data marg;
  set marg;
  if temps_retro >= -8.19 and temps_retro < -3.78 then quant=0;
  if temps_retro >= -3.78 and temps_retro < -2.38 then quant=1;
  if temps_retro >= -2.38 and temps_retro < -1.59 then quant=2;
  if temps_retro >= -1.59 and temps_retro < -0.86 then quant=3;
  if temps_retro >= -0.86 and temps_retro <= -0.04 then quant=4;
run;
*/




/*************************/
/* Ad�quation du mod�le  */
/*************************/

/* Cr�ation des d�ciles pour la variable temps r�tro */
proc univariate data=projet1b_deces;
var temps_retro;
output out=quantile pctlpre=P_ pctlpts=0 to 100 by 10;
run;
data marg;
  set marg;
  if temps_retro >= -8.19 and temps_retro < -4.81 then quant=0;
  if temps_retro >= -4.81 and temps_retro < -3.78 then quant=1;
  if temps_retro >= -3.78 and temps_retro < -2.94 then quant=2;
  if temps_retro >= -2.94 and temps_retro < -2.38 then quant=3;
  if temps_retro >= -2.38 and temps_retro < -2.04 then quant=4;
  if temps_retro >= -2.04 and temps_retro < -1.59 then quant=5;
  if temps_retro >= -1.59 and temps_retro < -1.18 then quant=6;
  if temps_retro >= -1.18 and temps_retro < -0.86 then quant=7;
  if temps_retro >= -0.86 and temps_retro < -0.51 then quant=8;
  if temps_retro >= -0.51 and temps_retro <= -0.04 then quant=9;
run;
/* R�partition de UMSARS selon les quantiles */
proc sgpanel data=marg;
  panelby quant/novarname; 
  histogram UMSARS_1_2;
run;


/* Residus de cholesky */
proc sgplot data=marg;
histogram scaledresid;
run;
/* Heteroscedasticite */
proc sgplot data=marg;
scatter y=resid x=pred;
run;







/* valeurs pr�dites */
ods output 'Summary statistics'=stats; 
proc sort data=marg out=temp_; 
by quant;
proc means data=temp_ mean std stderr clm;
var pred;
by quant;
run;
ods output close;

data plotds_p (keep=quant y); 
set stats; 
y=pred_mean; output; 
y=Pred_LCLM; output;
y=Pred_UCLM; output; 
proc sort data=plotds_p; 
by quant; 
run;
data plotds_p;
set plotds_p;
groupe=1;
run;

/*
symbol interpol=HILOTJ; 
proc gplot data=plotds_p; 
plot y*quant;
run;
*/

/* valeurs observ�es */
ods output 'Summary statistics'=stats2; 
proc sort data=marg out=temp_; 
by quant;
proc means data=temp_ mean std stderr clm;
var UMSARS_1_2;
by quant;
run;
ods output close;

data plotds_o (keep=quant y); 
set stats2; 
y=UMSARS_1_2_mean; output; 
y=UMSARS_1_2_LCLM; output;
y=UMSARS_1_2_UCLM; output; 
proc sort data=plotds_o;
by quant;
run;
data plotds_o;
set plotds_o;
groupe=0;
run;
/*
symbol interpol=HILOTJ; 
proc gplot data=plotds_o; 
plot y*quant;
run;
*/

*les deux;
data plotds;
set plotds_o plotds_p;
run;
proc sort data=plotds;
by groupe quant;
run;
/*
data plotds;
set plotds;
if quant=0 then centre=-5.985;
if quant=1 then centre=-3.08;
if quant=2 then centre=-1.985;
if quant=3 then centre=-1.225;
if quant=4 then centre=-0.45;
run;
*/

data plotds;
set plotds;
if quant=0 then centre=-6.5;
if quant=1 then centre=-4.295;
if quant=2 then centre=-3.36;
if quant=3 then centre=-2.66;
if quant=4 then centre=-2.21;
if quant=5 then centre=-1.815;
if quant=6 then centre=-1.385;
if quant=7 then centre=-1.02;
if quant=8 then centre=-0.685;
if quant=9 then centre=-0.275;
run;

symbol1 color=blue interpol=HILOTJ width=1 mode=include; 
symbol2 color=red interpol=HILOTJ width=1 mode=include; 
proc gplot data=plotds;
	axis1   color=black value=(font=arial height=1.5)
			label=(font=arial height=1.5 angle=90 "Somme des UMSARS 1 et 2")
			width=1 minor=none major=(width=1) order=(20 to 80 by 5);
	axis2   color=black value=(font=arial height=1.5)
			label=(font=arial height=1.5 "Temps r�trospectif avant le d�c�s (en ann�es)")
			width=1 minor=none major=(width=1) order=(-7 to 0 by 0.5);
	legend1	mode=share position=(top center inside) frame down=2 label=none
			value=(font=arial height=1.5 justify=left "Valeurs observ�es" "Valeurs estim�es");
plot y*centre=groupe /vaxis=axis1 haxis=axis2 legend=legend1 noframe;
run;

*Spaghetti plot;
proc sgplot data=projet1b_deces;
series y=UMSARS_1_2 x=temps_retro/group=ID;
run;
