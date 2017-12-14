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
/* notre jeu de données contient 677 observations et 11 variables */

*définition des labels;
data projet1b;
	set bibsas.projet1b;
	label SEXE='Sexe du patient';
	label AGEDIAG='Age au diagnostic';
	label TYPE_AMS="Type d'atrophie multisystématisée (AMS)";
	label CERTITUDE='Certitude du diagnostic';
	label DYSAUTO='Présence et type de premiers symptômes';
	label DCD='Indicateur de décès';
	label AGE_EVNT="Age à l'évènement";
	label DELAI_VIS="Délai depuis le diagnostic (en années)";
	label DELAI_SYMPT="Délai entre les premiers symptômes et la maladie (en années)";
	label UMSARS_1_2="Somme des UMSARS 1 et 2";
run;

*pour avoir une observation par individu;
data first;
set projet1b;
	by id;
	if first.id=1 then output;
run;
/* quand c'est la première visite, DELAI_VIS=0. On a donc 237 sujets différents */

*définition du temps rétrospectif;
data projet1b;
set projet1b;
	*retro1=AGE_EVNT-AGEDIAG;
	*retro2=AGE_EVNT-AGEDIAG-DELAI_VIS;
	temps_retro=AGEDIAG-AGE_EVNT+DELAI_VIS;
	label temps_retro="Temps rétrospectif avant le décès (en années)";
run;

*on ne prend que les décédés;
data projet1b_deces;
set projet1b;
	if DCD="Oui" then output;
run;



/* Variable à expliquer : UMSARS
ça suit à peu près une loi normale : modèle linéaire mixte (vérifier l'hétéroscédasticité des résidus a posteriori)

"temps rétrospectif avant le décès" : attention on ne s'intéresse qu'aux décédés !

normalité : par temps de visite ?

Discuter de manière poussée des limites, proposer une analyse satisfaisante pour décrire la progression jusqu'au décès
modèle conjoint ? on a un indicateur du décès, un age d'évènement, 2 délais (pas de troncature à gauche ?)

forme quadratique ! (pas linéaire sur la fin du spaghetti plot + déviations dans les résidus) 
http://www.math.ttu.edu/~atrindad/software/MixedModels-RandSAScomparison.pdf
https://www.theanalysisfactor.com/regression-modelshow-do-you-know-you-need-a-polynomial/
*/



/*----------------------------------------------------------------------------------*/
/*                             ANALYSES STATISTIQUES                  	 			*/
/*----------------------------------------------------------------------------------*/

/*************************/
/*      Descriptif       */
/*************************/

*vérifier la normalité;
proc univariate data=projet1b_deces;
var UMSARS_1_2 ;
histogram UMSARS_1_2 /normal;
run;
proc univariate data=first;
var UMSARS_1_2 ;
histogram UMSARS_1_2 /normal;
run;

*descriptif variables quali (au total, et selon l'indicateur de décès);
proc tabulate data=first missing noseps formchar(1,3,4,5,6,7,8,9,10,11)=" ƒƒƒƒƒƒƒƒƒ";
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
proc tabulate data=first missing noseps formchar (1,3,4,5,6,7,8,9,10,11)=" ƒƒƒƒƒƒƒƒƒ" vardef=df;
	class 	dcd;
	var		agediag delai_sympt age_evnt;
	table	(agediag delai_sympt age_evnt)
			,
			(DCD all="TOTAL")*(n nmiss mean std min q1 median q3 max)*f=15.2
		/misstext=" " rtspace=35;
	keylabel n="N" nmiss="Données manquantes" mean="Moyenne" std="Ecart-type" stderr="SE" 
			min="Minimum" q1="1er quartile" median="Médiane" q3="3e quartile" max="Maximum";
run;
*verif ok;
proc means data=first n nmiss mean std min q1 median q3 max maxdec=2;
var agediag delai_sympt age_evnt;
class dcd;
run;

*descriptif variables quanti (table projet1b);
proc tabulate data=projet1b missing noseps formchar (1,3,4,5,6,7,8,9,10,11)=" ƒƒƒƒƒƒƒƒƒ" vardef=df;
	class 	dcd;
	var		delai_vis UMSARS_1_2 temps_retro;
	table	(delai_vis UMSARS_1_2 temps_retro)
			,
			(DCD all="TOTAL")*(n nmiss mean std min q1 median q3 max)*f=15.2
		/misstext=" " rtspace=35;
	keylabel n="N" nmiss="Données manquantes" mean="Moyenne" std="Ecart-type" stderr="SE" 
			min="Minimum" q1="1er quartile" median="Médiane" q3="3e quartile" max="Maximum";
run;
*verif ok;
proc means data=projet1b n nmiss mean std min q1 median q3 max maxdec=2;
var delai_vis UMSARS_1_2 temps_retro;
class dcd;
run;

/* attention : on n'étudie que les décédés (DCD=Oui) */


/*************************/
/* Analyses univariables */
/*************************/
*au seuil 20%;

/* SEXE */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id SEXE;
model UMSARS_1_2= SEXE temps_retro/s;
random intercept temps_retro/sub=id type=UN G GCORR;
run; *S p-value=0.1636 (n=677) / S p-value=0.0960 (n=274);

/* TYPE_AMS */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id TYPE_AMS;
model UMSARS_1_2= TYPE_AMS temps_retro/s;
random intercept temps_retro/sub=id type=UN G GCORR;
run; *NS p-value=0.2193 (n=677) / NS p-value=0.4675 (n=274);

/* CERTITUDE */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id CERTITUDE;
model UMSARS_1_2= CERTITUDE temps_retro/s;
random intercept temps_retro/sub=id type=UN G GCORR;
run; *NS p-value=0.3854 (n=677) / S p-value=0.1128 (n=274);

/* DYSAUTO */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id DYSAUTO;
model UMSARS_1_2= DYSAUTO temps_retro/s;
random intercept temps_retro/sub=id type=UN G GCORR;
run; *S p-value=0.0128 (n=677) / S p-value=0.0994 (n=274);

/* DELAI_SYMPT */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id ;
model UMSARS_1_2= DELAI_SYMPT temps_retro/s;
random intercept temps_retro/sub=id type=UN G GCORR;
run; *S p-value=<.0001 (n=677) / S p-value=0.0452 (n=274);

/* ou on met quand même toutes les variables dans l'analyse multivariable ? */


/********************************************/
/* Tests des interactions (avec le temps) ? */
/********************************************/


/**********************************/
/* ANOVA, tests de corrélations ? */
/**********************************/


/*************************/
/* Analyse multivariable */
/*************************/

/* Selection de variable pour le modele avec intercept et pente aleatoire */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id type_ams SEXE CERTITUDE DYSAUTO DELAI_SYMPT;
model UMSARS_1_2=temps_retro type_ams SEXE CERTITUDE DYSAUTO DELAI_SYMPT 
	temps_retro*SEXE temps_retro*type_ams temps_retro*CERTITUDE temps_retro*DYSAUTO temps_retro*DELAI_SYMPT/s residual;
random intercept temps_retro/sub=id type=UN G GCORR;
run;
/*
Selection pas à pas descendante :
- enlever la p valeur la plus élevée
- regarder AIC
- regarder si les coefficients ne changent pas trop
- si on enlève une interaction : test du rapport de vraisemblance

repeated /type=sp(pow)(temps_retro) sub=id R RCORR LOCAL;
estimate
*/



/*************************/
/* Adéquation du modèle  */
/*************************/

/* Etude de la distribution de UMSARS */
proc sgplot data=projet1b_deces;
histogram UMSARS_1_2;
run;
*Spaghetti plot;
proc sgplot data=projet1b_deces;
series y=UMSARS_1_2 x=temps_retro/group=ID;
run;

/* Création des quantiles pour la variable temps rétro */
proc univariate data=projet1b_deces;
var temps_retro;
output out=quantile pctlpre=P_ pctlpts=0 to 100 by 20;
run;
data quantile_temps;
  set projet1b;
  if temps_retro >= -8.19 and temps_retro < -3.78 then quant=0;
  if temps_retro >= -3.78 and temps_retro < -2.38 then quant=1;
  if temps_retro >= -2.38 and temps_retro < -1.59 then quant=2;
  if temps_retro >= -1.59 and temps_retro < -0.86 then quant=3;
  if temps_retro >= -0.86 and temps_retro < -0.04 then quant=4;
  if temps_retro >= -0.04  then quant=5;
run;
/* Répartition de UMSARS selon les quantiles */
proc sgpanel data=quantile_temps;
  panelby quant/novarname; 
  histogram UMSARS_1_2;
run;


/* Exemple (avec forme quadratique)
retirés du modèle : temps_retro*type_ams  type_ams temps_retro*DYSAUTO temps_retro*SEXE SEXE DYSAUTO */
proc mixed data=projet1b_deces method=ml noclprint covtest;
class id type_ams SEXE CERTITUDE DYSAUTO DELAI_SYMPT;
model UMSARS_1_2=temps_retro temps_retro*temps_retro CERTITUDE DELAI_SYMPT 
	  temps_retro*CERTITUDE temps_retro*DELAI_SYMPT
		temps_retro*temps_retro*DELAI_SYMPT /s residual vciry outp=cond outpm=marg;
random intercept temps_retro /sub=id type=UN G GCORR;
run; *AIC=2081; 

/* Residus de cholesky */
proc sgplot data=marg;
histogram scaledresid;
run;
/* Heteroscedasticite */
proc sgplot data=marg;
scatter y=resid x=pred;
run;

/*
*trouver donnée aberrante;
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
