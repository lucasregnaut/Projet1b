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
/* quand c'est la première visite, DELAI_VIS=0 */

*définition du temps rétrospectif;
data projet1b;
set projet1b;
*retro1=AGE_EVNT-AGEDIAG;
	temps_retro=AGE_EVNT-AGEDIAG-DELAI_VIS;
	label temps_retro="Temps rétrospectif avant le décès (en années)";
run;



/* Variable à expliquer : UMSARS
ça suit à peu près une loi normale : modèle linéaire mixte
Temps rétrospectif avant le décès ???

normalité : par temps de visite ?

biblio : "retrospective mixed model"

Discuter de manière poussée des limites, proposer une analyse satisfaisante pour décrire la progression jusqu'au décès
modèle conjoint ? on a un indicateur du décès, un age d'évènement, 2 délais (pas de troncature à gauche ?)
*/



/*----------------------------------------------------------------------------------*/
/*                             ANALYSES STATISTIQUES                  	 			*/
/*----------------------------------------------------------------------------------*/

/*************************/
/*      Descriptif       */
/*************************/

*vérifier la normalité (?);
proc univariate data=projet1b;
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

*descriptif variables quanti (first);
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

*descriptif variables quanti (projet1b);
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

/* avec en plus une analyse descriptive séparée pour les décédés (DCD=oui) et les censurés (DCD=non) */


/*************************/
/* Analyses univariables */
/*************************/

/* SEXE */
proc mixed data=projet1b method=ml noclprint covtest;
class id SEXE;
model UMSARS_1_2= SEXE /s;
random intercept temps_retro/sub=id type=UN G GCORR;
run; *NS p-value=0.1619;

/* TYPE_AMS */
proc mixed data=projet1b method=ml noclprint covtest;
class id TYPE_AMS;
model UMSARS_1_2= TYPE_AMS /s;
random intercept temps_retro/sub=id type=UN G GCORR;
run; *NS p-value=0.4390;

/* CERTITUDE */
proc mixed data=projet1b method=ml noclprint covtest;
class id CERTITUDE;
model UMSARS_1_2= CERTITUDE /s;
random intercept temps_retro/sub=id type=UN G GCORR;
run; *NS p-value=0.3382;

/* DYSAUTO */
proc mixed data=projet1b method=ml noclprint covtest;
class id DYSAUTO;
model UMSARS_1_2= DYSAUTO /s;
random intercept temps_retro/sub=id type=UN G GCORR;
run; *Signif p-value=0.0131;

/* AGEDIAG */
proc mixed data=projet1b method=ml noclprint covtest;
class id ;
model UMSARS_1_2= AGEDIAG /s;
random intercept temps_retro/sub=id type=UN G GCORR;
run; *NS p-value=0.5831;

/* DELAI_SYMPT */
proc mixed data=projet1b method=ml noclprint covtest;
class id ;
model UMSARS_1_2= DELAI_SYMPT /s;
random intercept temps_retro/sub=id type=UN G GCORR;
run; *Signif p-value=<.0001;

/* AGE_EVNT */
proc mixed data=projet1b method=ml noclprint covtest;
class id ;
model UMSARS_1_2= AGE_EVNT /s;
random intercept temps_retro/sub=id type=UN G GCORR;
run; *Signif p-value=0.0220;

/* ou on met quand même toutes les variables dans l'analyse multivariable ? */


/******************************************/
/* Tests des interactions (avec le temps) */
/******************************************/


/********************************/
/* ANOVA, tests de corrélations */
/********************************/


/*************************/
/* Analyse multivariable */
/*************************/

/* Selection de variable pour le modele avec intercept et pente aleatoire */
/* Selection pas à pas descendant */
proc mixed data=projet1b method=ml noclprint covtest;
class id type_ams SEXE CERTITUDE DYSAUTO;
model UMSARS_1_2=temps_retro type_ams SEXE CERTITUDE DYSAUTO temps_retro*type_ams/s; * /s : estimation des Béta (effets fixes) en plus;
random intercept temps_retro/sub=id type=UN G GCORR;
run;
/*
repeated /type=sp(pow)(temps_retro) sub=id R RCORR LOCAL;
estimate

à retirer du modèle (non significatif) :
temps_retro*type_ams
*/



/*************************/
/* Adéquation du modèle  */
/*************************/

/* Etude de la distribution de UMSARS */
proc sgplot data=projet1b;
histogram UMSARS_1_2;
run;
*Spaghetti plot;
proc sgplot data=projet1b;
series y=UMSARS_1_2 x=temps_retro/group=ID;
run;

/* Création des quantiles pour la variable temps rétro */
proc univariate data=projet1b;
var temps_retro;
output out=quantile pctlpre=P_ pctlpts=0 to 100 by 20;
run;
data quantile_temps;
  set projet1b;
  if temps_retro >= 0.02 and temps_retro < 0.83 then quant=0;
  if temps_retro >= 0.83 and temps_retro < 1.72 then quant=1;
  if temps_retro >= 1.72 and temps_retro < 2.76 then quant=2;
  if temps_retro >= 2.76 and temps_retro < 4.89 then quant=3;
  if temps_retro >= 4.89 and temps_retro < 9.36 then quant=4;
  if temps_retro >= 9.36  then quant=5;
run;
/* Répartition de UMSARS selon les quantiles */
proc sgpanel data=quantile_temps;
  panelby quant/novarname; 
  histogram UMSARS_1_2;
run;

/* Exemple */
proc mixed data=projet1b method=ml noclprint covtest;
class id type_ams SEXE CERTITUDE DYSAUTO;
model UMSARS_1_2=temps_retro type_ams SEXE CERTITUDE DYSAUTO temps_retro*type_ams/s residual vciry outp=cond outpm=marg;
random intercept temps_retro/sub=id type=UN G GCORR;
run;
/* Residus de cholesky */
proc sgplot data= marg;
histogram scaledresid;
run;
/* Heteroscedasticite */
proc sgplot data=marg;
scatter y=resid x=pred;
run;
