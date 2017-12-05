/*** CC mixte ***/
libname bibsas "F:\M2_biostat\STA302_mixte\Projet";
libname bibsas "E:\M2_biostat\STA302_mixte\Projet";
libname bibsas "D:\M2_biostat\STA302_mixte\Projet";

PROC CONTENTS data=bibsas.projet2b;
RUN;
/* notre jeu de données contient 677 observations et 14 variables */

*pour avoir une observation par individu;
data first;
set bibsas.projet2b;
by id;
if first.id=1 then output;
run;
/* quand c'est la première visite, DELAI_VIS=0 */

/* DESCRIPTIF */
proc means data=bibsas.projet2b n nmiss mean std min q1 median q3 max maxdec=2;
var delai_vis PAS_COU PAD_COU PAS_DEB PAD_DEB;
run; *il n'y a que la description de PAS et PAD qui est intéréssante;
proc univariate data=bibsas.projet2b;
var PAS_COU PAD_COU PAS_DEB PAD_DEB;
histogram PAS_COU PAD_COU PAS_DEB PAD_DEB/normal;
run;
proc freq data=first;
tables sexe type_ams certitude dysauto dcd;
run;
proc means data=first n nmiss mean std min q1 median q3 max maxdec=2;
var agediag delai_sympt age_evnt;
run;


/* Variable à expliquer : pression artérielle
poisson ou linéaire ?
ça suit à peu près une loi normale
voir dans la littérature : blood pressure linear mixed model
Temps rétrospectif avant le décès ???

Discuter de manière poussée des limites, proposer une analyse satisfaisante pour décrire la progression jusqu'au décès
modèle conjoint ? on a un indicateur du décès, un age d'évènement, 2 délais (pas de troncature à gauche ?)
*/


data projet2b;
set bibsas.projet2b;
retro1=AGE_EVNT-AGEDIAG;
retro2=AGE_EVNT-AGEDIAG-DELAI_VIS; *bon;
PAS_diff=PAS_DEB - PAS_COU;
PAD_diff=PAD_DEB - PAD_COU;
PAM_DEB=(PAS_DEB + 2*PAD_DEB)/3;
PAM_COU=(PAS_COU + 2*PAD_COU)/3;
PAM_diff=PAM_DEB - PAM_COU;
PAM_diff2=(PAS_diff + 2*PAD_diff)/3; *pareil;
run;
proc means data=projet2b n nmiss mean std min q1 median q3 max maxdec=2;
var retro1 retro2;
run;
proc univariate data=projet2b;
var PAM_diff2 PAM_diff;
histogram PAM_diff2 PAM_diff/normal;
run;
proc univariate data=projet2b;
var PAS_diff PAD_diff;
histogram PAS_diff PAD_diff/normal;
run;


*Modèle multivariable (on n'a pas fait la stratégie de modélisation !);
proc mixed data=projet2b method=ml noclprint covtest;
class id type_ams SEXE CERTITUDE DYSAUTO;
model PAM_diff=retro2 type_ams SEXE CERTITUDE DYSAUTO retro2*type_ams/s; * /s : estimation des Béta (effets fixes) en plus;
random intercept retro2/sub=id type=UN G GCORR;
run;
/*
repeated /type=sp(pow)(retro2) sub=id R RCORR LOCAL;
estimate

à retirer du modèle (non significatif) :
retro2*type_ams
*/
