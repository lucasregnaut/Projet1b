/*----------------------------------------------------------------------------------------------------------*/
/* 	   					                   PROGRAM : cc_mixte.sas		      	    		  	 			*/
/*----------------------------------------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------------*/
/*                                DATA MANAGEMENT                      	 			*/
/*----------------------------------------------------------------------------------*/

*Google doc : https://docs.google.com/document/d/11PwSYzxIvCW-VYp9960BlWDgeKJQpqmahfjYhh8H7LU/edit ;

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
/* quand c'est la premi�re visite, DELAI_VIS=0. On a donc 237 sujets diff�rents */

*d�finition du temps r�trospectif;
data projet1b;
set projet1b;
	*retro1=AGE_EVNT-AGEDIAG;
	*retro2=AGE_EVNT-AGEDIAG-DELAI_VIS;
	temps_retro=AGEDIAG-AGE_EVNT+DELAI_VIS;
	label temps_retro="Temps r�trospectif avant le d�c�s (en ann�es)";
run;

*on ne prend que les d�c�d�s;
data projet1b_deces;
set projet1b;
	if DCD="Oui" then output;
run;



/* Variable � expliquer : UMSARS
�a suit � peu pr�s une loi normale : mod�le lin�aire mixte (v�rifier l'h�t�rosc�dasticit� des r�sidus a posteriori)

"temps r�trospectif avant le d�c�s" : attention on ne s'int�resse qu'aux d�c�d�s !

normalit� : par temps de visite ?

Discuter de mani�re pouss�e des limites, proposer une analyse satisfaisante pour d�crire la progression jusqu'au d�c�s
mod�le conjoint ? on a un indicateur du d�c�s, un age d'�v�nement, 2 d�lais (pas de troncature � gauche ?)

forme quadratique ! (pas lin�aire sur la fin du spaghetti plot + d�viations dans les r�sidus) 
http://www.math.ttu.edu/~atrindad/software/MixedModels-RandSAScomparison.pdf
https://www.theanalysisfactor.com/regression-modelshow-do-you-know-you-need-a-polynomial/

ANOVA, tests de corr�lations ?
*/



/*----------------------------------------------------------------------------------*/
/*                             ANALYSES STATISTIQUES                  	 			*/
/*----------------------------------------------------------------------------------*/

/*************************/
/*      Descriptif       */
/*************************/

*v�rifier la normalit� (� baseline);
proc univariate data=first;
var UMSARS_1_2 ;
histogram UMSARS_1_2 /normal;
run;

*descriptif variables quali (au total, et selon l'indicateur de d�c�s);
proc tabulate data=first missing noseps formchar(1,3,4,5,6,7,8,9,10,11)=" ���������";
	class	SEXE TYPE_AMS certitude dysauto dcd;
	table	(SEXE TYPE_AMS certitude dysauto all="Total")
			,
			(DCD all="TOTAL")*(n="N" colpctn="%")*f=5.2
		/misstext=" " rtspace=50;
run;
*verif ok;
proc freq data=first;
tables sexe type_ams certitude dysauto sexe*dcd type_ams*dcd certitude*dcd dysauto*dcd;
run;

*descriptif variables quanti (table first);
proc tabulate data=first missing noseps formchar (1,3,4,5,6,7,8,9,10,11)=" ���������" vardef=df;
	class 	dcd;
	var		agediag delai_sympt age_evnt;
	table	(agediag delai_sympt age_evnt)
			,
			(DCD all="TOTAL")*(n nmiss mean std min q1 median q3 max)*f=15.2
		/misstext=" " rtspace=35;
	keylabel n="N" nmiss="Donn�es manquantes" mean="Moyenne" std="Ecart-type" stderr="SE" 
			min="Minimum" q1="1er quartile" median="M�diane" q3="3e quartile" max="Maximum";
run;
*verif ok;
proc means data=first n nmiss mean std min q1 median q3 max maxdec=2;
var agediag delai_sympt age_evnt;
class dcd;
run;

*descriptif variables quanti (table projet1b);
proc tabulate data=projet1b missing noseps formchar (1,3,4,5,6,7,8,9,10,11)=" ���������" vardef=df;
	class 	dcd;
	var		delai_vis UMSARS_1_2 temps_retro;
	table	(delai_vis UMSARS_1_2 temps_retro)
			,
			(DCD all="TOTAL")*(n nmiss mean std min q1 median q3 max)*f=15.2
		/misstext=" " rtspace=35;
	keylabel n="N" nmiss="Donn�es manquantes" mean="Moyenne" std="Ecart-type" stderr="SE" 
			min="Minimum" q1="1er quartile" median="M�diane" q3="3e quartile" max="Maximum";
run;
*verif ok;
proc means data=projet1b n nmiss mean std min q1 median q3 max maxdec=2;
var delai_vis UMSARS_1_2 temps_retro;
class dcd;
run;

/* attention : on n'�tudie que les d�c�d�s (DCD=Oui) */



/*************************/
/* Analyses univariables */
/*************************/
*au seuil 5% pour les interactions;

/* SEXE */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id SEXE;
model UMSARS_1_2= SEXE temps_retro temps_retro*temps_retro SEXE*temps_retro SEXE*temps_retro*temps_retro/s;
random intercept temps_retro/sub=id type=UN G GCORR;
run;

/* TYPE_AMS */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id TYPE_AMS;
model UMSARS_1_2= TYPE_AMS temps_retro temps_retro*temps_retro TYPE_AMS*temps_retro TYPE_AMS*temps_retro*temps_retro/s;
random intercept temps_retro/sub=id type=UN G GCORR;
run;

/* CERTITUDE */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id CERTITUDE;
model UMSARS_1_2= CERTITUDE temps_retro temps_retro*temps_retro CERTITUDE*temps_retro CERTITUDE*temps_retro*temps_retro/s;
random intercept temps_retro/sub=id type=UN G GCORR;
run;

/* DYSAUTO */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id dysauto;
model UMSARS_1_2= DYSAUTO temps_retro temps_retro*temps_retro DYSAUTO*temps_retro DYSAUTO*temps_retro*temps_retro/s;
random intercept temps_retro/sub=id type=UN G GCORR;
run;

/* DELAI_SYMPT */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id;
model UMSARS_1_2=temps_retro temps_retro*temps_retro DELAI_SYMPT DELAI_SYMPT*temps_retro DELAI_SYMPT*temps_retro*temps_retro/s 
	residual vciry outp=cond outpm=marg;
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
class id type_ams SEXE CERTITUDE DYSAUTO;
model UMSARS_1_2=temps_retro temps_retro*temps_retro DELAI_SYMPT DELAI_SYMPT*temps_retro SEXE CERTITUDE DYSAUTO/s residual vciry outp=cond outpm=marg;
random intercept temps_retro /sub=id type=UN G GCORR;
run;

*sans dysauto/type_ams;
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id SEXE CERTITUDE;
model UMSARS_1_2=temps_retro temps_retro*temps_retro DELAI_SYMPT DELAI_SYMPT*temps_retro SEXE CERTITUDE/s residual vciry outp=cond outpm=marg;
random intercept temps_retro /sub=id type=UN G GCORR;
run;

*sans certitude/dysauto/type_ams;
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id SEXE;
model UMSARS_1_2=temps_retro temps_retro*temps_retro SEXE DELAI_SYMPT DELAI_SYMPT*temps_retro /s residual vciry outp=cond outpm=marg;
random intercept temps_retro /sub=id type=UN G GCORR;
run; *mod�le final;

/*
Selection pas � pas descendante (mod�le � pente et intercept al�atoire) :
- enlever la p valeur la plus �lev�e
- regarder AIC
- regarder si les coefficients ne changent pas trop
- si on enl�ve une interaction : test du rapport de vraisemblance

repeated /type=sp(pow)(temps_retro) sub=id R RCORR LOCAL;
estimate
*/



/*************************/
/* Ad�quation du mod�le  */
/*************************/

/* Cr�ation des quantiles pour la variable temps r�tro */
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
  if temps_retro >= -0.86 and temps_retro < -0.04 then quant=4;
  if temps_retro >= -0.04  then quant=5; *inutile !!! ;
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

*�tude du fit chez des individus;
symbol1 i = join value=circle; 
proc gplot data=marg;
by id ;
plot1 umsars_1_2*temps_retro/legend overlay;
plot2 pred*temps_retro/ legend overlay;
where id in (5,11,15,73,69,106,111);
run ;

*graphique C�cile;
* http://support.sas.com/documentation/cdl/en/grstatproc/62603/HTML/default/viewer.htm#a003155517.htm ;
* https://stackoverflow.com/questions/14069629/plotting-confidence-intervals ;
/*
proc sgplot data=marg;
	title "Repr�sentation des moyennes pr�dites en fonction du temps";
  reg x=temps_retro y=Pred / CLM;
run;
*/


* https://communities.sas.com/t5/SAS-GRAPH-and-ODS-Graphics/How-to-plot-two-lines-with-confidence-bands/td-p/323349 ;
/*
proc sgplot data=marg;
*band x=temps_retro lower=lower upper=upper / group=id transparency=0.5;
*scatter x=temps_retro y=UMSARS_1_2 / group=type_ams;
series x=temps_retro y=Pred / group=id;
YAXIS GRID;
run;
*/

/*
*trouver donn�e aberrante;
data donnees_aberrantes;
set marg;
if Resid>40 then output;
run;
*le supprimer;
data projet1b_deces;
set projet1b_deces;
if ID=113 then delete;
run;
*/


/*
* Valeur predite ;
proc means data=marg mean std clm;
class quant;
var pred;
output out=mp mean=mean LCLM=lclm Uclm=uclm;
run;
data mp2;
  set mp;
  if _TYPE_ eq 0 then delete;
run;
proc sgplot data=mp;
series x=quant y=mean;
where _TYPE_ EQ 1;
run;

* Valeur observee ;
proc means data=marg mean std clm;
class quant;
var UMSARS_1_2;
output out=m mean=mean LCLM=lclm Uclm=uclm;
run;
data m2;
  set m;
  if _TYPE_ eq 0 then delete;
run;
proc sgplot data=m2;
series x=quant y=mean;
run;

* Graphe avec les y_pred et y_obs ;
proc sort data=m2;
  by quant;
run;
proc sort data=mp2;
  by quant;
run;
data tab;
  merge m2 (rename=(mean=y_obs))
        mp2 (rename=(mean=y_pred));
by quant;
run;
proc sgplot data=tab;
series x=quant y=y_obs;
series x=quant y=y_pred ;
run;
*/

/* 
http://www.lexjansen.com/pharmasug/2009/cc/cc03.pdf (error)
http://www.lexjansen.com/wuss/2009/anl/ANL-Rutledge.pdf
*/


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
data plotds;
set plotds;
if quant=0 then centre=-5.985;
if quant=1 then centre=-3.08;
if quant=2 then centre=-1.985;
if quant=3 then centre=-1.225;
if quant=4 then centre=-0.45;
run;

symbol1 color=blue interpol=HILOTJ width=1; 
symbol2 color=red interpol=HILOTJ width=1; 
proc gplot data=plotds;
	title "Repr�sentation des moyennes et de leurs intervalles de confiance en fonction du temps";
	axis1   color=black value=(font=arial height=1.5)
			label=(font=arial height=1.5 angle=90 "Somme des UMSARS 1 et 2")
			width=1 minor=none major=(width=1) order=(20 to 80 by 5);
	axis2   color=black value=(font=arial height=1.5)
			label=(font=arial height=1.5 "Temps r�trospectif avant le d�c�s (en ann�es)")
			width=1 minor=none major=(width=1) order=(-6 to 0 by 1);
	legend1	mode=share position=(top center inside) frame down=2 label=none
			value=(font=arial height=1.5 justify=left "Valeurs observ�es" "Valeurs pr�dites");
plot y*centre=groupe /vaxis=axis1 haxis=axis2 legend=legend1 noframe;
run;

*Spaghetti plot;
proc sgplot data=projet1b_deces;
title "Spaghetti plot";
series y=UMSARS_1_2 x=temps_retro/group=ID;
run;

/* Discussion : le mod�le semble mal estimer les effets plus anciens. 
Un effet al�atoire sur le terme quadratique aurait pu corrig� cela, mais son apport est limit� */
