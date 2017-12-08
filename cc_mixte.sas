/*** CC mixte ***/
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
	label DELAI_VIS="D�lai depuis le diagnostic";
	label DELAI_SYMPT="D�lai entre les premiers sympt�mes et la maladie";
	label UMSARS_1_2="Somme des UMSARS 1 et 2";
run;

*pour avoir une observation par individu;
data first;
set projet1b;
by id;
if first.id=1 then output;
run;
/* quand c'est la premi�re visite, DELAI_VIS=0 */

/* DESCRIPTIF */
proc univariate data=projet1b;
var UMSARS_1_2 ;
histogram UMSARS_1_2 /normal;
run;

*descriptif variables quali;
proc tabulate data=first missing noseps formchar(1,3,4,5,6,7,8,9,10,11)=" ���������";
	class	SEXE TYPE_AMS certitude dysauto dcd;
	table	(SEXE TYPE_AMS certitude dysauto dcd all="Total")
			,
			(all="PATIENTS")*(n="N" colpctn="%")*f=5.2
		/misstext=" " rtspace=50;
run;
*verif ok;
proc freq data=first;
tables sexe type_ams certitude dysauto dcd;
run;

*descriptif variables quanti (first);
proc tabulate data=first missing noseps formchar (1,3,4,5,6,7,8,9,10,11)=" ���������" vardef=df;
	var		agediag delai_vis delai_sympt age_evnt UMSARS_1_2;
	table	(agediag delai_vis delai_sympt age_evnt UMSARS_1_2)
			,
			(n nmiss mean std min q1 median q3 max)*f=15.2
		/misstext=" " rtspace=35;
	keylabel n="N" nmiss="Donn�es manquantes" mean="Moyenne" std="Ecart-type" stderr="SE" 
			min="Minimum" q1="1er quartile" median="M�diane" q3="3e quartile" max="Maximum";
run;
*verif ok;
proc means data=first n nmiss mean std min q1 median q3 max maxdec=2;
var agediag delai_vis delai_sympt age_evnt UMSARS_1_2 retro2;
run;

*descriptif variables quanti (first);
proc tabulate data=first missing noseps formchar (1,3,4,5,6,7,8,9,10,11)=" ���������" vardef=df;
	var		agediag delai_vis delai_sympt age_evnt UMSARS_1_2;
	table	(agediag delai_vis delai_sympt age_evnt UMSARS_1_2)
			,
			(n nmiss mean std min q1 median q3 max)*f=15.2
		/misstext=" " rtspace=35;
	keylabel n="N" nmiss="Donn�es manquantes" mean="Moyenne" std="Ecart-type" stderr="SE" 
			min="Minimum" q1="1er quartile" median="M�diane" q3="3e quartile" max="Maximum";
run;
*verif ok;
proc means data=first n nmiss mean std min q1 median q3 max maxdec=2;
var agediag delai_vis delai_sympt age_evnt UMSARS_1_2 retro2;
run;


/* Variable � expliquer : UMSARS
�a suit � peu pr�s une loi normale : mod�le lin�aire mixte
Temps r�trospectif avant le d�c�s ???

normalit� : par temps de visite ?

retrospective linear mixed

Discuter de mani�re pouss�e des limites, proposer une analyse satisfaisante pour d�crire la progression jusqu'au d�c�s
mod�le conjoint ? on a un indicateur du d�c�s, un age d'�v�nement, 2 d�lais (pas de troncature � gauche ?)
*/


data projet1b;
set projet1b;
*retro1=AGE_EVNT-AGEDIAG;
	retro2=AGE_EVNT-AGEDIAG-DELAI_VIS;
	label retro2="Temps r�trospectif avant le d�c�s";
run;
proc means data=projet1b n nmiss mean std min q1 median q3 max maxdec=2;
var retro1 retro2;
run;


*** Mod�les univariables;
/* SEXE */
proc mixed data=projet1b method=ml noclprint covtest;
class id SEXE;
model UMSARS_1_2= SEXE /s;
random intercept retro2/sub=id type=UN G GCORR;
run; *NS p-value=0.1619;

/* TYPE_AMS */
proc mixed data=projet1b method=ml noclprint covtest;
class id TYPE_AMS;
model UMSARS_1_2= TYPE_AMS /s;
random intercept retro2/sub=id type=UN G GCORR;
run; *NS p-value=0.4390;

/* CERTITUDE */
proc mixed data=projet1b method=ml noclprint covtest;
class id CERTITUDE;
model UMSARS_1_2= CERTITUDE /s;
random intercept retro2/sub=id type=UN G GCORR;
run; *NS p-value=0.3382;

/* DYSAUTO */
proc mixed data=projet1b method=ml noclprint covtest;
class id DYSAUTO;
model UMSARS_1_2= DYSAUTO /s;
random intercept retro2/sub=id type=UN G GCORR;
run; *Signif p-value=0.0131;

/* AGEDIAG */
proc mixed data=projet1b method=ml noclprint covtest;
class id ;
model UMSARS_1_2= AGEDIAG /s;
random intercept retro2/sub=id type=UN G GCORR;
run; *NS p-value=0.5831;

/* DELAI_SYMPT */
proc mixed data=projet1b method=ml noclprint covtest;
class id ;
model UMSARS_1_2= DELAI_SYMPT /s;
random intercept retro2/sub=id type=UN G GCORR;
run; *Signif p-value=<.0001;

/* AGE_EVNT */
proc mixed data=projet1b method=ml noclprint covtest;
class id ;
model UMSARS_1_2= AGE_EVNT /s;
random intercept retro2/sub=id type=UN G GCORR;
run; *Signif p-value=0.0220;



*tests des inteactions;

*ANOVA, test des corr�lations;

*Mod�le multivariable (on n'a pas fait la strat�gie de mod�lisation !);
proc mixed data=projet1b method=ml noclprint covtest;
class id type_ams SEXE CERTITUDE DYSAUTO;
model UMSARS_1_2=retro2 type_ams SEXE CERTITUDE DYSAUTO retro2*type_ams/s; * /s : estimation des B�ta (effets fixes) en plus;
random intercept retro2/sub=id type=UN G GCORR;
run;
/*
repeated /type=sp(pow)(retro2) sub=id R RCORR LOCAL;
estimate

� retirer du mod�le (non significatif) :
retro2*type_ams
*/
