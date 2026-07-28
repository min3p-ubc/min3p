/*******************************************************************************/
/*Copyright 2014, Loic Pages, INRA Institut National de la Recherche Agronomique
/*ArchiSimple is registered at the french Agence de Protection des Programmes (APP)
/* 
/*Licensed under the Apache License, Version 2.0 (the "License");
/*you may not use this file except in compliance with the License.
/*You may obtain a copy of the License at
/*
/*    http://www.apache.org/licenses/LICENSE-2.0
/*
/*Unless required by applicable law or agreed to in writing, software
/*distributed under the License is distributed on an "AS IS" BASIS,
/*WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
/*See the License for the specific language governing permissions and
/*limitations under the License.
/*Please cite Pages et al. (2014) Calibration and evaluation of ArchiSimple, a simple
/*model of root system architecture. Ecol. Model. 290:76-84
/******************************************************************************/

//#ifdef ARCHISIMPLE

extern "C"
{
//---------------ARCHISIMPLE-----------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h> // CBF 
//#include <unistd.h>
#include <time.h>
#include <cstdio> // CBF for use of snprint in c++11 standard used for CGAL in MIN3P

#ifdef WINDOWS
#include <windows.h>   // pour la version windows
#endif

#ifdef LINUX
#include <sys/time.h>  // pour la version linux
#endif

#define NBPASMAX 201 /* Nombre maximal de pas de temps */
#define NBHORMAX 60 /* Nombre maximal d'horizons de sol */
#define MAXLINE 150  /* Longueur maxi de la ligne dans fichiers texte */
#define NBCASEZMAX 100000  /* Nombre maximal de cases en Z (profondeur) !FG set to larger value in waiting for dynamic allocation*/
#define NBCASEDMAX 100  /* Nombre maximal de cases en D (distance ? l'axe) */


float deltaT=1;          /* Pas de temps, en jours */ //CB PRENDRE PAS DE TEMPS MIN3P A LA PLACE// avant  : const int
// CB POUR L'INSTANT CA VA CAR ILS SONT LES MEMES

// FG May 2021 - upscale root segments
// DSU, Jan 2022 - make root uptake scaling factor as an input parameter
double upscfact_x;
double upscfact_y;
double upscfact_z;

const double epsilon=1.0e-8; /* Petite valeur, proche de 0 */
const double pi=3.141592653589793238462;  /* Valeur approch?e de la constante Pi */
const double epaissHor=50.0;  /* Epaisseur horizons de sol (en mm) */
const float longSegNorm=5.0;  /* Longueur habituelle des segments form?s (mm) */
const float longSegMin=0.3;  /* Longueur minimale des segments form?s, quand faible croissance (mm) */
const float dureeSansCreation=1.9; /* Dur?e maximale sans cr?ation de noeud, quand faible croissance (jour) */
const double mailleMin=6.0;  /* Valeur de la maille minimale de sol (mm) */
const double d1=3.0;   /* Premi?re valeur de distance (mm) */
const double d2=30.0;  /* Deuxi?me valeur de distance (plus grande) (mm) */



typedef float r2[2];  /* Tableau 2D */
typedef float r3[3];  /* Tableau 3D */

typedef struct SysRac *pTSysRac; /* Pour le syst?me racinaire entier */
typedef struct Axe *pTAxe;   /* Pour chacun des axes */
typedef struct Pointe *pTPointe; /* Pour les pointes, parties apicales des racines */
typedef struct Seg *pTSeg; /* Pour les segments, qui sont des portions d'axes */


struct SysRac /* Ensemble d'axes */
  {
  long int nbAxeForm;  /* Nombre d'axes form?s */
  long int nbAxeSup;   /* Nombre d'axes supprim?s */
  long int nbSegForm;    /* Nombre de segments form?s */
  long int nbSeg; /* Nombre de segments tels qu'ils sont compt?s aux 3 dates */
  int nbPrim;  /* Nombre de primaires ?mises */
  int nbAdv;  /* Nombre de racines adventives ?mises */
  float angDep;        /* Orientation */
  r3 origine;           /* Position de l'origine */
  pTAxe premAxe;       /* Premier axe du syst?me (acc?s ? la liste) */
  pTAxe dernAxe;       /* Dernier axe produit */
  float volMax[NBPASMAX];  /* Volume racinaire maximal pendant chaque pas de temps */
  float volDem[NBPASMAX];  /* Volume racinaire demand? pendant chaque pas de temps */
  float tSatis[NBPASMAX];  /* Taux de satisfaction de la demande ? chaque pas de temps */
  float longueur; /* Longueur totale de racines */
  float profMax, profMoy; /* Profondeurs maximale et moyenne */
  float distMax, distMoy; /* Distances maximale et moyenne ? l'axe du syst?me */
  float diamMax; /* Diam?tre maximal, du plus gros segment */
  float xbinf,ybinf,zbinf,xbsup,ybsup,zbsup; /* Bornes en x, y et z */
  float volProd,volPrim,volTot; /* Volumes racinaires : produit, primaire et total */
  float secPointe; /* Section totale des pointes matures et non s?niles */
  float tSatisMoy; /* Taux de satisfaction moyen */
  float volSolD1, volSolD2; /* Volumes de sol ? distance d1 et d2 */
  } ;

struct Pointe /* M?rist?me apical, ou pointe de chaque racine */
  {
  float distPrimInit;  /* Distance de l'apex au dernier primordium initi? */
  float longueur;  /* Longueur non encore exprim?e en allongement de l'axe */
  int dateDerniereCreation; /* Date ? laquelle il y a eu cr?ation d'un noeud */
  r3 coord;           /* Coordonn?es de la pointe */
  r3 dirCroiss;       /* Direction de croissance */
  r3 dirInit;         /* Direction initiale */
  float age;          /* Age du m?rist?me */
  float diametre;     /* Diam?tre de la pointe */
  unsigned char stop;           /* Stopp?e ?, ou encore en croissance ... */
  unsigned char senile;         /* S?nile ?, ou encore actif ... */
  unsigned char mature;         /* Mature ?, ou encore au stade primordium ... */
  } ;

struct Axe /* Ensemble constitu? d'un m?rist?me et liste de noeuds */
  {
  long int num;      /* Num?ro de l'axe */
  int nbSeg;       /* Nombre de noeuds */
  pTPointe pointe; /* M?rist?me apical */
  pTAxe suivant;     /* Suivant de la liste */
  pTAxe precedent;   /* Pr?c?dent de la liste */
  pTAxe pere;        /* Axe p?re, sur lequel celui-ci est branch? */
  pTSeg premSeg; /* Premier segment de l'axe, sa base */
  pTSeg dernSeg; /* Dernier segment de l'axe, apical */
  } ;

struct Seg
  {
  long int num;      /* Num?ro d'ordre de cr?ation */
  int jourForm;      /* Date de formation (en jours) */
  unsigned char complet; /* Complet (1), c'est-?-dire avec ses deux points, ou non (0) */
  float diametre;    /* Diametre */
  r3 posO;            /* Position dans l'espace de son origine */
  r3 posE;            /* Position dans l'espace de son extr?mit? */
  pTSeg suiv;      /* Suivant dans le prolongement (NULL sinon) */
  pTSeg prec;     /* Pr?c?dent, sur le m?me axe quand non base, sur axe p?re sinon */
  pTAxe axe;        /* Axe auquel appartient le segment */
  unsigned char necrose;       /* Necrose ? 0 : non; 1 : oui */
  } Seg ;

struct Horizon  /* Horizon de sol */
  {
  float croiss;  /* Coefficient de croissance, compris entre 0 et 1 */
  float ramif;   /* Coefficient multiplicateur de distance inter-ramif  */
  float iCMeca;  /* Intensit? de la contrainte m?canique */
  int oCMeca;    /* Orientation de la contrainte m?canique (O iso, ou 1 vert) */
  } ;

typedef Horizon TSol[NBHORMAX];  /* Sol pour la croissance, tableau d'horizons */

/* Fichiers */

//CBF removed for coupling to MIN3P :
//FILE *FSeg;      /* Fichier contenant la structure sous forme de segments */
FILE *FPar;      /* Param?tres */
FILE *FSol;      /* Informations sur le sol, par horizons */
FILE *FVol;      /* Informations sur le volume racinaire possible, ? chaque pas de temps */
// FILE *FAudit;  /* Audit sur le d?roulement de la simulation */
// FILE *FSynth;  /* Fichier contenant des variables de synth?se */
//FILE *FCol;       /* Informations sur la colonisation du sol */



/* Param?tres, lus dans fichier param?tres */
int P_duree=50; /* Dur?e de la simulation, en jours */ // CBF non utilise car MIN3P ma�tre  

float x_MIN3P, y_MIN3P, z_MIN3P; //, RSD; // CBF ajout

// Caract?risation de l'?mission des racines primaires
float P_vitEmissionPrim=0.5; /* Vitesse d'?mission des primaires (en jour-1) */
int P_nbMaxPrim=1; /* Nombre maximal de racines primaires */

float P_angInitMoyVertPrim=0.7854; /* Angle d'insertion moyen par rapport ? la verticale pour les primaires */
float P_angInitETVertPrim=0.35;  /* ?cart-type de l'angle d'insertion des primaires */

// Caract?risation de l'?mission des adventives
float P_ageEmissionAdv=12.0; /* ?ge de commencement de l'?mission des racines adventives */
float P_vitEmissionAdv=2.0; /* Vitesse d'?mission des adventives (en jour-1) */
float P_dBaseMaxAdv=30.0; /* Distance ? la base maximale pour les adventives (mm) */
float P_propDiamAdv=1.0; /* Proportion du diam?tre des adventives (par rapport aux diam?tre max) */
int P_nbMaxAdv=40; /* Nombre maximal de racines adventives */

float P_angInitMoyVertAdv=1.4; /* Angle d'insertion moyen par rapport ? la verticale pour les adventives */
float P_angInitETVertAdv=0.7;  /* ?cart-type de l'angle d'insertion des adventives */

// Croissance radiale
float P_coeffCroissRad=0.6; // coefficient de croissance radiale

// Allongement (croissance axiale)
float P_diamMin=0.10;  /* Diam?tre minimal en de?a duquel il n'y a pas de croissance (mm) */
float P_diamMax=1.1;   /* Diam?tre maximal donn? aux racines primaires (mm) */
float P_penteVitDiam=12.0; /* pente de la relation entre vitesse de croissance et diam?tre (mm.mm.jour-1) */
int P_tendanceDirTropisme=2;  /* Type de tropisme (0: plagio; -1: geo-; +1: geo+; 2: exo */
float P_intensiteTropisme=0.2; /* Coefficient multipli? par le diam?tre pour chaque racine */
float P_penteDureeCroissDiam2=3000.0; /* pente de la relation dur?e de croissance versus diam?tre^2 */

// Ramification
float P_ageMaturitePointe=4.5;  /* ?ge de maturit? des m?rist?mes (jours) */
float P_distRamif=4.0; /* distance inter-ramification (mm) */
float P_propDiamRamif=0.2; /* proportion de diam?tre des filles par rapport ? leur m?re */
float P_coeffVarDiamRamif=0.30; /* coefficient de variation du diam?tre des ramifs */
float P_angLat=1.3; /* angle d'insertion des racines lat?rales */

// Mortalit?
float P_TMD=0.2; /* Tissue mass density, ou masse volumique */
float P_penteDureeVieDiamTMD=2000.0; /* pente de la relation dur?e de vie versus diam?tre et TMD */

/* Variables globales diverses */
int temps=0;  /* Le temps, en jours */
r3 orig;  /* Position d'origine du syst?me racinaire */


float maille=mailleMin; /* Valeur initialis?e de la maille de sol */
double volElemSol;  /* Volume ?l?mentaire de sol associ? ? la maille (mm3) */

pTSysRac sR;  /* Le syst?me racinaire */
TSol sol;     /* Le sol */

/****************************************************************************/
double dRandUnif(void)
/* Cette fonction tire un al?atoire uniforme r?el entre 0 et 1 */
{
double tirage;
  //srand( (int) 123);// CBF for DSU benchmark tests, constant random, affect ArchiSimple capability
  tirage=(double) rand()/(double) RAND_MAX;
  //DSU this will cause difference in every running. Is it possible to add a seed parameter to random function
  //DSU so that the results are identical
  if (tirage<epsilon) { tirage=epsilon; }
  return(tirage);
}
/****************************************************************************/
/****************************************************************************/
void norme(r3 u, r3 un)
/* Cette fonction norme le vecteur u de l'espace de dimension 3.
  Le vecteur norme de retour est appele un. */
{
double norU;
  norU=sqrt((u[0]*u[0])+(u[1]*u[1])+(u[2]*u[2]));
  if (norU<epsilon)
  {
  printf("ATTENTION, vecteur nul ! Sa norme vaut : %f \n",norU);
  exit(1);
  }
  else
  {
   un[0]=u[0]/norU;
   un[1]=u[1]/norU;
   un[2]=u[2]/norU;
  }
}  /* Fonction Norme */
/****************************************************************************/
/****************************************************************************/
double prodScal(r3 u,r3 v)
/* Cette fonction retourne le produit scalaire de 2 vecteurs u et v de
  l'espace a 3 dimensions. */
{
double prodScal;
  prodScal=(u[0]*v[0])+(u[1]*v[1])+(u[2]*v[2]);
  return(prodScal);
}  /* Fonction prodScal */
/****************************************************************************/
/****************************************************************************/
void prodVect(r3 u, r3 v, r3 u_vect_v)
/* Cette fonction calcule le produit vectoriel de deux vecteurs u et v
  de l'espace de dimension 3. Le vecteur resultant est u_vect_v. */
{
  u_vect_v[0]=(u[1]*v[2])-(v[1]*u[2]);
  u_vect_v[1]=(u[2]*v[0])-(v[2]*u[0]);
  u_vect_v[2]=(u[0]*v[1])-(v[0]*u[1]);
}   /* Fonction prodVect */
/****************************************************************************/
/****************************************************************************/
void rotVect(double omega, r3 u, r3 x, r3 rot_x)

/* Cette fonction calcule le vecteur rot_x dans l'espace de dimension 3,
  issu de la rotation du vecteur x autour d'un axe dont u est un vecteur
  unitaire. La rotation se fait d'un angle omega radians. Elle appelle
  PRODSCAL, PRODVECT. */
{
double uscalx;   /* produit scalaire u.x  */
r3    uvectx;   /* produit vectoriel u^x */

  uscalx=prodScal(u,x);
  prodVect(u,x,uvectx);

  rot_x[0]=((1-cos(omega))*uscalx*u[0])
      +(cos(omega)*x[0])+(sin(omega)*uvectx[0]);
  rot_x[1]=((1-cos(omega))*uscalx*u[1])
      +(cos(omega)*x[1])+(sin(omega)*uvectx[1]);
  rot_x[2]=((1-cos(omega))*uscalx*u[2])
      +(cos(omega)*x[2])+(sin(omega)*uvectx[2]);

}  /* Fonction rotVect */
/****************************************************************************/
/****************************************************************************/
void rotZ(r3 u, r3 v, double teta)
/* Cette fonction fait tourner "u" d'un angle "teta" autour de l'axe (Oz);
  le vecteur calcule est "v" */
{
  v[0]=(u[0]*cos(teta))-(u[1]*sin(teta));
  v[1]=(u[0]*sin(teta))+(u[1]*cos(teta));
  v[2]=u[2];
}
/****************************************************************************/
/****************************************************************************/
int iRandUnif(int imax)

/* Cette fonction tire un al?atoire uniforme entier entre 0 et imax */
{
  int tirage;

  tirage=imax+1;
  //srand( (int) 123);// CBF for DSU benchmark tests, constant random, affect ArchiSimple capability
  while (tirage>imax) tirage=rand();
  return tirage;
}
/****************************************************************************/
/****************************************************************************/
void ouvreFichiers(char *directory, int pos)
/* Cette fonction ouvre les fichiers, en lecture et ?criture */
{



// CBF : input and output directory is obtained in 'directory' by coupling to MIN3P
// CBF : 'pos' is the length of the string 'directory'


char *rep = (char*)malloc((pos+1)*sizeof(char)); // CBF exact string of directory
#ifdef WINDOWS
_snprintf(rep,pos+1,"%s",directory);
rep[pos] = '\0';  //DSU, important note on Windows using Visual Studio, '\0' required to avoid messy code in rep as
                  //the null-terminator is not appended in _snprintf when length of directory >= length of rep.
#else
snprintf(rep,pos+1,"%s",directory);
#endif

char *paramdir = (char*)malloc((pos+strlen("paramAS.txt")+1)*sizeof(char)); // CBF c++11 standard used in CGAL
#ifdef WINDOWS
_snprintf(paramdir,(pos+strlen("paramAS.txt")+1),"%s%s", rep,"paramAS.txt");
//DSU, Here paramdir[pos+strlen...] = '\0' is not required as length of rep is less than paramdir and
//the null-terminator is automatically appended.
#else
snprintf(paramdir,(pos+strlen("paramAS.txt")+1),"%s%s", rep,"paramAS.txt");
#endif
//printf("\n paramdir = %s ", paramdir);
FPar = fopen(paramdir, "rt"); // fichier des parametres du modele


char *soldir = (char*)malloc((pos+(strlen("sol.txt"))+1)*sizeof(char)); // CBF : for c++11 standard used in CGAL
#ifdef WINDOWS
_snprintf(soldir,pos+(strlen("sol.txt"))+1,"%s%s", rep,"sol.txt");
//DSU, Here soldir[pos+strlen...] = '\0' is not required as length of rep is less than paramdir and
//the null-terminator is automatically appended.
#else
snprintf(soldir,pos+(strlen("sol.txt"))+1,"%s%s", rep,"sol.txt");
#endif
//printf("\n soldir = %s ", soldir);
FSol = fopen(soldir,"rt");      // fichier des caracteristiques du sol


char *volracdir = (char*)malloc((pos+(strlen("volrac.txt"))+1)*sizeof(char)); // CBF : for c++11 standard used in CGAL
#ifdef WINDOWS
_snprintf(volracdir,pos+(strlen("volrac.txt"))+1,"%s%s", rep,"volrac.txt");
//DSU, Here volracdir[pos+strlen...] = '\0' is not required as length of rep is less than paramdir and
//the null-terminator is automatically appended.
#else
snprintf(volracdir,pos+(strlen("volrac.txt"))+1,"%s%s", rep,"volrac.txt");
#endif
//printf("\n volracdir = %s ", volracdir);
FVol = fopen(volracdir,"rt");   // fichier des caracteristiques du volume racinaire maximal



} /* Fonction ouvreFichiers */
/****************************************************************************/
/****************************************************************************/
void litSol(void)
/* Fonction de lecture des caract?ristiques du sol, une ligne par horizon */
{
int hor;              /* Compteur des horizons */
char bid[MAXLINE];    /* Cha?ne qui accueille les caract?res suppl?mentaires */

fgets(bid,MAXLINE-1,FSol);          /* Ligne ent?te */
for (hor=0; hor<NBHORMAX; hor++)
{
  fscanf(FSol,"%f %f %f %i",&sol[hor].croiss,&sol[hor].ramif,&sol[hor].iCMeca,&sol[hor].oCMeca);
//  fscanf(FSol,"%f",&sol[hor].croiss); // Favorable ? la croissance
//  fscanf(FSol,"%f",&sol[hor].ramif);  // Favorable ? la ramification
//  fscanf(FSol,"%f",&sol[hor].iCMeca); // Intensit? de la contrainte
//  fscanf(FSol,"%d",&sol[hor].oCMeca); // Orientation 0: iso, 1: verticale


  fgets(bid,MAXLINE-1,FSol);
}

} /* Fonction litSol */
/****************************************************************************/
/****************************************************************************/
void litVolumeMaxSR(pTSysRac sR)
/* Fonction de lecture des volumes maximaux ? chaque pas de temps */
{
int pas;              /* Compteur des pas de temps */
char bid[MAXLINE];    /* Cha?ne qui accueille les caract?res suppl?mentaires */

fgets(bid,MAXLINE-1,FVol);          /* Ligne ent?te */
for (pas=0; pas<NBPASMAX; pas++)
{
  fscanf(FVol,"%f",&(sR->volMax[pas]));

  //printf("\n sR->volMax[pas] = %f ",(sR->volMax[pas]));

  fgets(bid,MAXLINE-1,FVol);
}

} /* Fonction litVolume */
/****************************************************************************/
/****************************************************************************/
double croissSol(TSol sol, double profondeur)
/* Renvoie le coefficient de croissance du sol ? la Profondeur donn?e */
{
int hor;

  hor=(int) floor(profondeur/epaissHor);
  if (hor>=NBHORMAX) hor=NBHORMAX-1;
  if (hor<0) hor=0;

  return(sol[hor].croiss);
} /* Fonction croissSol */
/****************************************************************************/
/****************************************************************************/
double ramifSol(TSol sol, double profondeur)
/* Renvoie le coefficient de ramification du sol ? la profondeur donn?e */
{
int hor;

  hor=(int) floor(profondeur/epaissHor);
  if (hor>=NBHORMAX) hor=NBHORMAX-1;
  if (hor<0) hor=0;

  return(sol[hor].ramif);
} /* Fonction ramifSol */
/****************************************************************************/
/****************************************************************************/
double iCMecaSol(TSol sol, double profondeur)
/* Renvoie l'intensit? de la contraine m?ca du sol ? la Profondeur donn?e */
{
int hor;

  hor=(int) floor(profondeur/epaissHor);
  if (hor>=NBHORMAX) hor=NBHORMAX-1;
  if (hor<0) hor=0;

  return(sol[hor].iCMeca);
} /* Fonction iCMecaSol */
/****************************************************************************/
/****************************************************************************/
int oCMecaSol(TSol sol, double profondeur)
/* Renvoie l'indice de la direction de contrainte : 0 pour iso, 1 pour verti */
{
int hor;

  hor=(int) floor(profondeur/epaissHor);
  if (hor>=NBHORMAX) hor=NBHORMAX-1;
  if (hor<0) hor=0;

  return(sol[hor].oCMeca);
} /* Fonction oCMecaSol */
/****************************************************************************/
/****************************************************************************/
double tireGaussien(float moy, float et)
{  /* R?alise un tirage gaussien dans une distribution de moyenne moy et ?cart-type et */
  double tireGaussien,tire1,tire2;

  tire1=dRandUnif();
  tire2=dRandUnif();
  tireGaussien=moy+(et*sqrt(-log(tire1))*cos(pi*tire2)*1.414);
  return(tireGaussien);
} /* Fonction tireGaussien */
/****************************************************************************/
/****************************************************************************/
double tireAngRad(void)
{   /* Tire l'angle radial dans l'intervalle 0 - 2*Pi */

return (2.0*pi*dRandUnif());
} /* Fonction TireAngRad */
/****************************************************************************/
/****************************************************************************/
void increNbSegSR(pTSysRac sR)
/* Incr?mente le nombre de noeuds qui a ?t? form? dans ce syst?me sR */
{
  sR->nbSegForm++;
} /* Fonction increNbSegSR */
/****************************************************************************/
/****************************************************************************/
pTSeg creeSeg(void)
/* Cette fonction retourne une nouvelle variable de type pTSeg,
  c'est-?-dire un pointeur sur le type Seg */
{
pTSeg seg;
  seg=(pTSeg) malloc(sizeof(Seg));
  if (seg==NULL)
  { printf("Probl?me m?moire allocation dans creeSeg \n"); exit(1); }

return seg;
} /* Fonction creeSeg */
/****************************************************************************/
/****************************************************************************/
pTSeg initialiseSeg(long int num, r3 posOrig, r3 posExtrem, double diam, pTAxe axeSeg, unsigned char comp, pTSeg precedent)
/* Cette fonction retourne une nouvelle variable de type pTSeg,)
  dont une partie des valeurs est initialis?e */
{
pTSeg seg;


  seg=creeSeg();

  seg->num=num;
  seg->jourForm=temps;
  seg->necrose=0;
  seg->complet=comp;

  seg->diametre=diam;
  seg->axe=axeSeg;
  seg->posO[0]=posOrig[0];
  seg->posO[1]=posOrig[1];
  seg->posO[2]=posOrig[2];


  seg->posE[0]=posExtrem[0];
  seg->posE[1]=posExtrem[1];
  seg->posE[2]=posExtrem[2];


  //printf("\n temps=%d - seg->posO[0]=%lf\n", temps, seg->posO[0]);
 // printf("\n temps=%d - seg->posO[1]=%lf\n", temps, seg->posO[1]);
  //printf("\n temps=%d - seg->posO[2]=%lf\n", temps, seg->posO[2]);

  //printf("\n temps=%d - seg->posE[0]=%lf\n", temps, seg->posE[0]);
  //printf("\n temps=%d - seg->posE[1]=%lf\n", temps, seg->posE[1]);
  //printf("\n temps=%d - seg->posE[2]=%lf\n", temps, seg->posE[2]);

  

  seg->suiv=NULL;  // pour l'instant
  seg->prec=precedent;

return seg;
} /* Fonction initialiseSeg */
/****************************************************************************/
/****************************************************************************/
void detruitSeg(pTSeg segADetruire)
/* Supprime un noeud en m?moire */
{
  free(segADetruire);
} /* Fonction detruitSeg */
/****************************************************************************/
/****************************************************************************/
pTPointe creePointe(void)
/* Cette fonction retourne une nouvelle variable de type pTPointe,
  c'est-?-dire un pointeur sur le type TPointe */
{
pTPointe pointe;
  pointe=(pTPointe) malloc(sizeof(Pointe));
  if (pointe==NULL)
  { printf("Probl?me m?moire allocation dans creePointe \n"); exit(1); }

return pointe;
} /* Fonction creePointe */
/****************************************************************************/
/****************************************************************************/
pTPointe initialisePointe(float diam, r3 position, r3 direction)
/* Cette fonction retourne une nouvelle variable de type pTPointe,
  dont les valeurs sont en partie initialis?es */
{
pTPointe pointe;

  pointe=creePointe();

  pointe->distPrimInit=0.0;
  pointe->longueur=0.0;
  pointe->age=0.0;
  pointe->diametre=diam;
  pointe->stop=0;
  pointe->senile=0;
  pointe->mature=0;

  pointe->coord[0]=position[0];
  pointe->coord[1]=position[1];
  pointe->coord[2]=position[2];

  pointe->dirCroiss[0]=direction[0];
  pointe->dirCroiss[1]=direction[1];
  pointe->dirCroiss[2]=direction[2];

  pointe->dirInit[0]=direction[0];
  pointe->dirInit[1]=direction[1];
  pointe->dirInit[2]=direction[2];


return pointe;
} /* Fonction initialisePointe */
/****************************************************************************/
/****************************************************************************/
void deflecMecaPointe(pTPointe pointe, r3 dirApresMeca, double elong)
{
const double teta=15.0; /* Angle autour de G, en degres */

r3 vTire,vTireN,dirInt;
double profondeur, cont;

  profondeur=pointe->coord[2];
  cont=iCMecaSol(sol,profondeur);
  if (oCMecaSol(sol,profondeur)==1)  /* Contrainte anisotrope verticale */
  {
  /* Tirage vecteur dans l'angle Teta autour de G */
  do
  {
  vTire[0]=(2.0*dRandUnif()-1.0)*sin(pi*teta/180.0);
  vTire[1]=(2.0*dRandUnif()-1.0)*sin(pi*teta/180.0);
  do { vTire[2]=dRandUnif(); } while (vTire[2]>cos(pi*teta/180.0));
  norme(vTire,vTireN);
  }  while (vTireN[2]>cos(pi*teta/180.0));
  dirInt[0]=pointe->dirCroiss[0]+(elong*vTireN[0]*cont);
  dirInt[1]=pointe->dirCroiss[1]+(elong*vTireN[1]*cont);
  dirInt[2]=pointe->dirCroiss[2]+(elong*vTireN[2]*cont);
  }
  else    /* Contrainte isotrope [oCMecaSol(Profondeur)==0] */
    {
    vTire[0]=2.0*dRandUnif()-1.0;
    vTire[1]=2.0*dRandUnif()-1.0;
    vTire[2]=2.0*dRandUnif()-1.0;
    norme(vTire,vTireN);
  if (prodScal(vTireN,pointe->dirCroiss)<0.0)
    {
    vTireN[0]=-vTireN[0];
    vTireN[1]=-vTireN[1];
    vTireN[2]=-vTireN[2];
  }
  dirInt[0]=pointe->dirCroiss[0]+(elong*vTireN[0]*cont);
  dirInt[1]=pointe->dirCroiss[1]+(elong*vTireN[1]*cont);
  dirInt[2]=pointe->dirCroiss[2]+(elong*vTireN[2]*cont);
  }
  norme(dirInt,dirApresMeca);

} /* Fonction deflecMecaPointe */
/****************************************************************************/
/****************************************************************************/
void deflecGeoPointe(pTPointe pointe, r3 dirApresMeca, r3 dirApresGeo, double elong)
/* Version avec plagiotropisme */
{
r3 dirInt,vGeoInt,vGeo;

  switch (P_tendanceDirTropisme) {
    case -1 : vGeo[0]=0.0;                  /* Gravitropisme n?gatif */
              vGeo[1]=0.0;
              vGeo[2]=-1.0;
              break;
    case 0 : vGeoInt[0]=pointe->dirInit[0]; /* Plagiotropisme */
             vGeoInt[1]=pointe->dirInit[1];
             vGeoInt[2]=0.0;
             norme(vGeoInt,vGeo);
             break;
    case 1 : vGeo[0]=0.0;                  /* Gravitropisme positif */
             vGeo[1]=0.0;
             vGeo[2]=1.0;
              break;
    case 2 : vGeoInt[0]=pointe->dirInit[0]; /* Exotropisme */
             vGeoInt[1]=pointe->dirInit[1];
             vGeoInt[2]=pointe->dirInit[2];
             norme(vGeoInt,vGeo);
             break;
    default : vGeo[0]=0.0;                 /* Gravitropisme positif */
              vGeo[1]=0.0;
              vGeo[2]=1.0;
              break;
  }

  dirInt[0]=dirApresMeca[0]+(vGeo[0]*P_intensiteTropisme*elong*pointe->diametre);
  dirInt[1]=dirApresMeca[1]+(vGeo[1]*P_intensiteTropisme*elong*pointe->diametre);
  dirInt[2]=dirApresMeca[2]+(vGeo[2]*P_intensiteTropisme*elong*pointe->diametre);

  norme(dirInt,dirApresGeo);
} /* Fonction deflecGeoPointe */
/****************************************************************************/
/****************************************************************************/
void deflecSurfPointe(pTPointe Pointe, r3 dirApresGeo, r3 dirApresSurf)
{
const double profLim=50.0*dRandUnif();
r3 dirInt;
  dirInt[0]=dirApresGeo[0];
  dirInt[1]=dirApresGeo[1];
  dirInt[2]=dirApresGeo[2];

  if ((dirInt[2]<0.0) && ((Pointe->coord[2])<profLim)) dirInt[2]=dirInt[2]/10.0;
  norme(dirInt,dirApresSurf);
} /* Fonction deflecSurfPointe */
/****************************************************************************/
/****************************************************************************/
void reorientePointe(pTPointe pointe, double elong)
{
r3 dirInt1, dirInt2, nouvDir;

  deflecMecaPointe(pointe,dirInt1,elong);
  deflecGeoPointe(pointe,dirInt1,dirInt2,elong);
  deflecSurfPointe(pointe,dirInt2,nouvDir);

  pointe->dirCroiss[0]=nouvDir[0];
  pointe->dirCroiss[1]=nouvDir[1];
  pointe->dirCroiss[2]=nouvDir[2];


} /* Fonction reorientePointe */
/****************************************************************************/
/****************************************************************************/
double calcElongationPointe(pTPointe pointe, TSol sol)
/* Calcul de l'?longation potentielle affect?e par le sol */
{
  if ((pointe->mature) && (!pointe->stop) && (!pointe->senile) && (pointe->diametre>P_diamMin))
    return pointe->diametre*deltaT*P_penteVitDiam*croissSol(sol,pointe->coord[2]);
    
  else return 0.0;

} /* Fonction calcElongationPointe */
/****************************************************************************/
/****************************************************************************/
void developpePointe(pTPointe pointe)
{ /* Assure l'?volution de la pointe, en la faisant vieillir et en changeant ses variables d'?tat au cours de sa vie */
 

     // CB TEST printf("\n DEVELOPPE POINTE \n");

  pointe->age=pointe->age+deltaT; /* Incr?mente l'?ge du m?rist?me selon le pas de temps */ 
  
  if ((!pointe->mature)&&(pointe->age>P_ageMaturitePointe))
  {
    pointe->mature=1;  /* Le primordium devient m?rist?me vrai */
    pointe->age=0.0;   /* Son ?ge est r?initialis? ? 0 en tant que pointe mature */
  }
  
  if ((pointe->mature)&&(!pointe->stop)&&(pointe->age>(P_penteDureeCroissDiam2*pointe->diametre*pointe->diametre)))
  {
    pointe->stop=1;  /* La pointe stoppe sa croissance */
  }
  
  if ((pointe->mature)&&(pointe->stop)&&(!pointe->senile)&&(pointe->age>(P_penteDureeVieDiamTMD*pointe->diametre*P_TMD)))
  {
    pointe->senile=1;  /* La pointe devient s?nile */
  }
  
} /* Fonction developpePointe */
/****************************************************************************/
/****************************************************************************/
void deplacePointe(pTPointe pointe, double elong)
{ /* Assure le d?placement du m?rist?me suite ? croissance axiale */

  /* Sa position est modifi?e */
  pointe->coord[0]=pointe->coord[0]+(elong*pointe->dirCroiss[0]);
  pointe->coord[1]=pointe->coord[1]+(elong*pointe->dirCroiss[1]);
  pointe->coord[2]=pointe->coord[2]+(elong*pointe->dirCroiss[2]);

  /* Son attribut distPrimInit est modifi? */
  pointe->distPrimInit+=elong;

} /* Fonction deplacePointe */
/****************************************************************************/
/****************************************************************************/
double distInterRamifPointe(pTPointe pointe, TSol sol)
{ /* Renvoie la valeur locale de la distance inter-ramification de la pointe */

  return (P_distRamif*ramifSol(sol,pointe->coord[2]));

} /* Fonction distInterRamifPointe */
/****************************************************************************/
/****************************************************************************/
void detruitPointe(pTPointe pointeADetruire)
/* Supprime une pointe */
{
  free(pointeADetruire);
} /* Fonction detruitPointe */
/****************************************************************************/
/****************************************************************************/
pTAxe creeAxe(void)
/* Cette fonction retourne une nouvelle variable de type pTAxe,
  c'est-?-dire un pointeur sur le type Axe */
{
pTAxe axe;
  axe=(pTAxe) malloc(sizeof(Axe));
  if (axe==NULL)
  { printf("Probl?me m?moire allocation dans creeAxe \n"); exit(1); }

return axe;
} /* Fonction creeAxe */
/****************************************************************************/
/****************************************************************************/

/****************************************************************************/
pTAxe initialiseAxe(long int numAxe, float diamPointe, r3 origine, r3 dirInit, pTAxe axePere, pTSeg segPorteur)
/* Cette fonction retourne une nouvelle variable de type pTAxe,
  c'est-?-dire un pointeur sur le type Axe */
{
  pTAxe nouvAxe;
  pTSeg premierSeg;

  nouvAxe=creeAxe();
  premierSeg=initialiseSeg(sR->nbSegForm+1,origine,origine,diamPointe,nouvAxe,0,segPorteur);
  nouvAxe->pointe=initialisePointe(diamPointe,origine,dirInit);
  nouvAxe->premSeg=premierSeg;
  nouvAxe->dernSeg=premierSeg;
  nouvAxe->nbSeg=1;
  nouvAxe->num=numAxe;
  nouvAxe->pere=axePere;

  nouvAxe->suivant=NULL;
  nouvAxe->precedent=NULL;

  return nouvAxe;
} /* Fonction initialiseAxe */
/****************************************************************************/
void ajouteSegProlongeAxe(pTAxe axe, pTSeg segAAjouter)
/* Cette fonction ajoute un segment de prolongement en position apicale
? l'axe concern?, et incr?mente son compteur de segments */
{
pTSeg ancienSegTerm;

  ancienSegTerm=axe->dernSeg;

  // Si ce dernier segment est complet, il faut prolonger la liste
  if (ancienSegTerm->complet) {
    ancienSegTerm->suiv=segAAjouter;
    segAAjouter->prec=ancienSegTerm;
    axe->dernSeg=segAAjouter;
    axe->nbSeg++;
  }
  // Sinon, il faut juste compl?ter le dernier segment
  else {
    // rien ? faire
    // les mises ? jour sont faites dans developpeAxeSR
    // on ne doit pas passer ici
  }

} /* Fonction ajouteSegProlongeAxe */
/****************************************************************************/
/****************************************************************************/
void ajouteAxeSR(pTSysRac sR, pTAxe axeAAjouter)
/* Cette fonction ins?re un axe dans la cha?ne des axes du syst?me racinaire,
elle incr?mente en m?me temps le compteur d'axes et de segments */
{

  if (sR->premAxe==NULL)  /* Le syst?me racinaire est vide */
  {
    axeAAjouter->suivant=NULL;
    axeAAjouter->precedent=NULL;
    sR->premAxe=axeAAjouter;
    sR->dernAxe=axeAAjouter;
  }
  else /* Le syst?me contient d?j? des axes, assure le cha?nage double des axes */
  {
    axeAAjouter->suivant=NULL;
    axeAAjouter->precedent=sR->dernAxe;
    sR->dernAxe->suivant=axeAAjouter;
    sR->dernAxe=axeAAjouter;
  }
  sR->nbAxeForm++;
  sR->nbSegForm++;   // ? chaque axe, un segment

} /* Fonction ajouteAxeSR */
/****************************************************************************/
/****************************************************************************/
int axeRamifiable(pTAxe axe)
{   /* Renvoie 1 ou 0 suivant que l'axe est ramifiable ou non */

  return((axe->pointe->diametre > 1.6*P_diamMin) && (axe->pointe->distPrimInit > P_distRamif));

} /* Fonction axeRamifiable */
/****************************************************************************/
/****************************************************************************/
float tireDiamPointeFille(pTAxe axePere)
{   /* Tire le diam?tre d'un m?rist?me de ramification suivant celui du p?re
       pour la ramification s?quentielle */

    float moy=(axePere->pointe->diametre*P_propDiamRamif) + (P_diamMin*(1.0-P_propDiamRamif));
    float et=moy*P_coeffVarDiamRamif;
    float diamPFille=100.0;  // initialisation ? une forte valeur pour boucle de tirage
    while (diamPFille>(1.05*axePere->pointe->diametre)) diamPFille=tireGaussien(moy,et);

    return diamPFille;

} /* Fonction tireDiamPointeFille */
/****************************************************************************/
/****************************************************************************/
void origineAdv(pTSeg segPere, r3 origineFils)
{   /* Calcule la position du point d'origine d'une tardive sur le seg p?re */
    
    
  double rel=dRandUnif();  /* definira la position relative sur le segment */
  
  origineFils[0]=(rel*segPere->posO[0]) + ((1.0-rel)*segPere->posE[0]);
  origineFils[1]=(rel*segPere->posO[1]) + ((1.0-rel)*segPere->posE[1]);
  origineFils[2]=(rel*segPere->posO[2]) + ((1.0-rel)*segPere->posE[2]);


} /* Fonction origineTard */
/****************************************************************************/
/****************************************************************************/
void origineRamif(pTAxe axePere, r3 origineFils)
{   /* Calcule la position du point d'origine d'une ramification */
origineFils[0]=axePere->pointe->coord[0]-
                  (axePere->pointe->distPrimInit*axePere->pointe->dirCroiss[0]);
origineFils[1]=axePere->pointe->coord[1]-
                  (axePere->pointe->distPrimInit*axePere->pointe->dirCroiss[1]);
origineFils[2]=axePere->pointe->coord[2]-
                  (axePere->pointe->distPrimInit*axePere->pointe->dirCroiss[2]);
} /* Fonction origineRamif */
/****************************************************************************/
/****************************************************************************/
void orienteRamif(pTAxe axePere, r3 dirFils)
{   /* Calcule la direction d'un axe fils issu de ramification */
r3 vAxeRot,rotDirCroiss;
double norVProjHor,angRot;

/* Calcul de la norme de la projection direction sur plan horizontal */
norVProjHor=sqrt((axePere->pointe->dirCroiss[0]*axePere->pointe->dirCroiss[0])+
                 (axePere->pointe->dirCroiss[1]*axePere->pointe->dirCroiss[1]));
if (norVProjHor<epsilon)
{
  vAxeRot[0]=1.0; /* Vecteur initial vertical */
  vAxeRot[1]=0.0;
  vAxeRot[2]=0.0; /* Vecteur (1,0,0) choisi pour axe de rotation */
}
else
{
  vAxeRot[0]=axePere->pointe->dirCroiss[1]/norVProjHor;
  vAxeRot[1]=-axePere->pointe->dirCroiss[0]/norVProjHor;
  vAxeRot[2]=0.0;
}
/* On fait tourner dirCroiss autour de vAxeRot d'un angle d'insertion */
angRot=P_angLat;
rotVect(angRot,vAxeRot,axePere->pointe->dirCroiss,rotDirCroiss);

/* On fait tourner rotDirCroiss autour de dirCroiss d'un angle radial */
angRot=tireAngRad();
rotVect(angRot,axePere->pointe->dirCroiss,rotDirCroiss,dirFils);
} /* Fonction orienteRamif */
/****************************************************************************/
/****************************************************************************/
void ramifieAxe(pTAxe axePere)
{
pTAxe nouvAxe;
float diamRamif;
r3 origRamif, dirRamif;

  /* D?cr?mente la distance au dernier primordium initi? */
  axePere->pointe->distPrimInit-=distInterRamifPointe(axePere->pointe,sol);

  /* Calcul des attributs d'une ramification */
  diamRamif=tireDiamPointeFille(axePere);    /* Tire le diam?tre de sa pointe */

  if (diamRamif > P_diamMin)
  {
    origineRamif(axePere,origRamif);         /* Calcule sa position */
    orienteRamif(axePere,dirRamif);          /* Calcule sa direction */

    nouvAxe=initialiseAxe(sR->nbAxeForm+1,diamRamif,origRamif,dirRamif,axePere,axePere->dernSeg);

    ajouteAxeSR(sR,nouvAxe);

  }

} /* Fonction ramifieAxe */
/****************************************************************************/
/****************************************************************************/
void developpeAxe(pTAxe axe,float taux)
/* Assure le d?veloppement de l'axe, avec diff?rentes composantes */
{
double elongation;
pTSeg nouvSeg;

  elongation=taux*calcElongationPointe(axe->pointe,sol);

  axe->pointe->longueur+=elongation;

  while (axe->pointe->longueur > longSegNorm) { // on fait un segment "normal"

    axe->pointe->dateDerniereCreation=temps;

    axe->pointe->longueur-=longSegNorm;

    /* Calcule et affecte la nouvelle direction de croissance du m?rist?me */
    reorientePointe(axe->pointe,longSegNorm);

    /* Le m?rist?me se d?place */
    deplacePointe(axe->pointe,longSegNorm);

    if (axe->dernSeg->complet) {
      /* Il g?n?re un nouveau segment sur cet axe ? sa nouvelle position */
      increNbSegSR(sR);
      nouvSeg=initialiseSeg(sR->nbSegForm,axe->dernSeg->posE,axe->pointe->coord,axe->pointe->diametre,axe,1,axe->dernSeg);
      ajouteSegProlongeAxe(axe,nouvSeg);
    }
    else { // le premier segment est incomplet, on le modifie
      axe->dernSeg->complet=1;
      axe->dernSeg->posE[0]=axe->pointe->coord[0];
      axe->dernSeg->posE[1]=axe->pointe->coord[1];
      axe->dernSeg->posE[2]=axe->pointe->coord[2];
      axe->dernSeg->jourForm=temps;
    }


    while (axeRamifiable(axe)) ramifieAxe(axe); // on ramifie ?ventuellement

  } // fin du while  (axe->pointe->longueur > longSegNorm)

  if (((temps - axe->pointe->dateDerniereCreation) > dureeSansCreation)&&(axe->pointe->longueur > longSegMin)) { /* production segment court  */

    axe->pointe->dateDerniereCreation=temps;


    /* Calcule et affecte la nouvelle direction de croissance de la pointe */
    reorientePointe(axe->pointe,axe->pointe->longueur);

    /* La pointe se d?place */
    deplacePointe(axe->pointe,axe->pointe->longueur);

    /* Elle g?n?re un nouveau segment sur cet axe ? sa nouvelle position */
    if (axe->dernSeg->complet) {
      /* Il g?n?re un nouveau segment sur cet axe ? sa nouvelle position */
      increNbSegSR(sR);
      nouvSeg=initialiseSeg(sR->nbSegForm,axe->dernSeg->posE,axe->pointe->coord,axe->pointe->diametre,axe,1,axe->dernSeg);
      ajouteSegProlongeAxe(axe,nouvSeg);
    }
    else { // le premier segment est incomplet, on le modifie
      axe->dernSeg->complet=1;
      axe->dernSeg->posE[0]=axe->pointe->coord[0];
      axe->dernSeg->posE[1]=axe->pointe->coord[1];
      axe->dernSeg->posE[2]=axe->pointe->coord[2];
      axe->dernSeg->jourForm=temps;
    }

    axe->pointe->longueur=0.0; // remet la longueur en attente du m?rist?me ? 0

    while (axeRamifiable(axe)) ramifieAxe(axe); // on ramifie ?ventuellement

  } // fin du if (production d'un segment court)


} /* Fonction developpeAxe */
/****************************************************************************/
/****************************************************************************/
void calcTSatisMoySR(pTSysRac sR)
{
/* Calcul du taux de satisfaction moyen sur la p?riode ?coul?e (de 1 ? temps) */

  double tSatisCum=0.0;

  for (int date=1; date<=temps; date++) /* Boucle sur la p?riode ?coul?e */
  {
    tSatisCum+=sR->tSatis[date];
  }
  sR->tSatisMoy=tSatisCum/temps;

}  /* Fonction calcTSatisMoySR */
/****************************************************************************/
/****************************************************************************/
float calcTauxSatis(float volDemande, float volDisponible)
{
float taux;

  calcTSatisMoySR(sR);

  if (sR->tSatisMoy<=0.7) return 0.0;  // sert ? r?duire quand demande trop forte

  else {
    if (volDemande==0.0) { taux=1.0; }
    else {
      taux=volDisponible/volDemande;
      if (taux>1.0) taux=1.0;
    }
    return taux;
  }

} /* Fonction calcTauxSatis */
/****************************************************************************/
/****************************************************************************/
double calcDemandeVolume(pTAxe axe)
{
/* Calcule la demande en volume correspondant ? la croissance en longueur
   pour un axe donn? */

  return pi*(axe->pointe->diametre)*(axe->pointe->diametre)*calcElongationPointe(axe->pointe,sol)/4.0;

} /* Fonction calcDemandeVolume */
/****************************************************************************/
/****************************************************************************/
void detruitAxe(pTAxe axeADetruire)
/* Supprime un axe en supprimant ses segments, puis l'axe lui-m?me */
{
pTSeg segCour, segAEnlever;

  /* Lib?rer tous les segments de cet axe */
  segCour=axeADetruire->premSeg;
  while (segCour->suiv!=NULL)
  {
    segAEnlever=segCour;
    segCour=segCour->suiv;
//    if (ndCour->suivSPere!=NULL) { printf("Probl?me : Axe ramifi? ? enlever\n"); exit(1); }
    detruitSeg(segAEnlever);
  }
  detruitSeg(segCour); /* Enl?ve le segment apical */

  detruitPointe(axeADetruire->pointe);

  /* Enlever l'axe en m?moire */
  free(axeADetruire);

} /* Fonction detruitAxe */
/****************************************************************************/
/****************************************************************************/
int axeToutNecrose(pTAxe axe)
/* Cette fonction retourne la valeur 1 si l'axe a tous ses segments n?cros?s et 0 sinon */
{
pTSeg segCour;
int resu=1; // on initialise la valeur r?sultat ? vrai (1)

  if (!axe->pointe->senile) resu=0; // non tout n?cros? si pointe non s?nile
  segCour=axe->premSeg;
  while (segCour!=NULL) {
    if (!segCour->necrose) resu=0;  // non tout n?cros? si un segment non n?cros?
    segCour=segCour->suiv;
  }
  return resu;

} /* Fonction axeToutNecrose */
/****************************************************************************/
/****************************************************************************/
void affecValNecroseAxe(pTAxe axe, int valNecrose)
/* Cette fonction affecte ? chacun des segments de l'axe
   la valeur de necrose (0 ou 1) */
{
pTSeg segCour;

  segCour=axe->premSeg;
  while (segCour!=NULL)
  {
    segCour->necrose=valNecrose;
    segCour=segCour->suiv;
  }  // fin du while

} /* Fonction affecValNecroseAxe */
/****************************************************************************/
/****************************************************************************/
void affecValNecroseAmont(pTAxe axe, int valNecrose)
/* Cette fonction affecte a chacun des segments en amont de l'axe
la valeur de necrose (0 ou 1) */
{
pTSeg segCour;

  segCour=axe->dernSeg;
  while (segCour!=NULL)
  {
    segCour->necrose=valNecrose;
    segCour=segCour->prec;
  }

} /* Fonction affecValNecroseAmont */
/****************************************************************************/
/****************************************************************************/
void affecValDiamAxe(pTAxe axe, float diam)
/* Cette fonction affecte ? chacun des segments de l'axe
   la valeur de diam?tre diam */
{
pTSeg segCour;

  segCour=axe->premSeg;
  while (segCour!=NULL)
  {
    segCour->diametre=diam;
    segCour=segCour->suiv;
  }  // fin du while

} /* Fonction affecValDiamAxe */
/****************************************************************************/
/****************************************************************************/
void increValDiamAmont(pTAxe axe, double diam, double coeff)
/* Cette fonction incremente le diametre de chacun des noeuds en amont de l'axe */
{
pTSeg segCour;
double section,diamInit;

  segCour=axe->premSeg->prec; // segment duquel l'axe est segment lat?ral
  while (segCour!=NULL)
  {
    diamInit=segCour->diametre;
    section=(pi*diamInit*diamInit/4.0)+(pi*coeff*diam*diam/4.0);
    segCour->diametre=sqrt(4.0*section/pi);
    segCour=segCour->prec;
  } // fin du while

} /* Fonction increValDiamAmont */
/****************************************************************************/
/****************************************************************************/
pTSysRac creeSR(void)
/* Cette fonction retourne une nouvelle variable de type pTSysRac,
  c'est-?-dire un pointeur sur le type SysRac */
{
pTSysRac sR;
  sR=(pTSysRac) malloc(sizeof(SysRac));
  if (sR==NULL)
  { printf("Probl?me m?moire allocation dans CreeSR \n"); exit(1); }

return sR;
} /* Fonction creeSR */
/****************************************************************************/
/****************************************************************************/
void enleveAxeSR(pTSysRac sR, pTAxe axeAEnlever)
/* Cette fonction enl?ve un axe dans la cha?ne des axes du syst?me racinaire */
{
unsigned char axeDestructible=0;

  if (sR->premAxe==NULL)  /* Le syst?me racinaire est vide */
  {
    printf("ATTENTION, probleme dans enleveAxeSR, sR vide \n");
    exit(1);
  }
  else
  {
    if ((axeAEnlever->precedent!=NULL)&&(axeAEnlever->suivant!=NULL)) {
      // On pourra le supprimer, on refait le cha?nage
      axeAEnlever->precedent->suivant=axeAEnlever->suivant;
      axeAEnlever->suivant->precedent=axeAEnlever->precedent;
      axeDestructible=1;
    } // fin du if !=NULL && !=NULL

    if ((axeAEnlever->precedent==NULL)&&(axeAEnlever->suivant!=NULL)) {
      // On pourra le supprimer, on refait le cha?nage
      axeAEnlever->suivant->precedent=NULL;
      sR->premAxe=axeAEnlever->suivant;
      axeDestructible=1;
    } // fin du if ==NULL && !=NULL

    if ((axeAEnlever->precedent!=NULL)&&(axeAEnlever->suivant==NULL)) {
      // On pourra le supprimer, on refait le cha?nage
      axeAEnlever->precedent->suivant=NULL;
      sR->dernAxe=axeAEnlever->precedent;
      axeDestructible=1;
    } // fin du if !=NULL && ==NULL

    if ((axeAEnlever->precedent==NULL)&&(axeAEnlever->suivant==NULL)) {
      // On ne pourra pas le supprimer, car il est seul
      axeDestructible=0;
    } // fin du if ==NULL && ==NULL

    if (axeDestructible) {
      sR->nbAxeSup++;
      detruitAxe(axeAEnlever); // D?truit ses segments, sa pointe, et lui-m?me
    }
  }
} /* Fonction enleveAxeSR */
/****************************************************************************/
/****************************************************************************/
pTSysRac initialiseSR(r3 origine)
{
/* Initialisation du syst?me racinaire */

pTSysRac sR;

  sR=creeSR();  /* Cr?ation d'un syst?me racinaire */



  sR->nbAxeForm=0;  /* Initialisation des variables */
  sR->nbAxeSup=0;
  sR->nbSegForm=0;
  sR->nbSeg=0;
  sR->nbPrim=0;
  sR->nbAdv=0;//---Lo?c Pages 19 June,2014
  sR->premAxe=NULL;
  sR->dernAxe=NULL;
  sR->tSatisMoy=1;

  sR->origine[0]=origine[0];  /* Origine du syst?me racinaire */
  sR->origine[1]=origine[1];
  sR->origine[2]=origine[2];

  sR->angDep=2.0*pi*dRandUnif();  /* Orientation */

  for (int i=0; i<NBPASMAX; i++) sR->tSatis[i]=1.0;

  return(sR);
}  /* Fonction initialiseSR */
/****************************************************************************/
/****************************************************************************/
float longSeg(pTSeg seg)
/* Calcule la longueur d'un segment */
{
  return sqrt(((seg->posE[0]-seg->posO[0])*(seg->posE[0]-seg->posO[0]))+
              ((seg->posE[1]-seg->posO[1])*(seg->posE[1]-seg->posO[1]))+
              ((seg->posE[2]-seg->posO[2])*(seg->posE[2]-seg->posO[2])));

}  /* Fonction longSeg */
/****************************************************************************/
/****************************************************************************/
int calcNouvNbPrim(void)
{
/* Calcul du nouveau nombre de primaires */

  int nouvNbPrim;

  // CB TEST printf("\n CALC NOUV NB PRIM - TEMPS = %d\n", temps);

  nouvNbPrim=int (P_vitEmissionPrim*temps);

  if (nouvNbPrim>=P_nbMaxPrim) nouvNbPrim=P_nbMaxPrim;

  return nouvNbPrim;

}  /* Fonction calcNouvNBPrim */
/****************************************************************************/
/****************************************************************************/
int calcNouvNbAdv(void)
{
/* Calcul du nouveau nombre de racines adventives */

  int nouvNbAdv;

  // CB TEST printf("\n CALC NOUV NB ADV - TEMPS = %d\n", temps);


  nouvNbAdv=int (P_vitEmissionAdv*(temps-P_ageEmissionAdv));


  if (nouvNbAdv>P_nbMaxAdv) nouvNbAdv=P_nbMaxAdv;

//  printf("nouvNbAdv ***** %d  \n",nouvNbAdv);
//  printf("P_vitEmissionAdv ***** %f  \n",P_vitEmissionAdv);
//  printf("P_ageEmissionAdv ***** %f  \n",P_ageEmissionAdv);

  return nouvNbAdv;


}  /* Fonction calcNouvNbAdv */
/****************************************************************************/
/****************************************************************************/
void emissionPrimSR(pTSysRac sR)
{
/* Emission de nouveaux axes primaires sur le syst?me racinaire */

  pTAxe nouvAxe;
  int numPrim, nbPrimAEmettre;
  r3 vInit, dirInit;
  double angRot,angI;


  nbPrimAEmettre=calcNouvNbPrim() - sR->nbPrim; /* Nombre de primaires ? ?mettre */
  //printf("nombre Adv ? emettre  %d \n",nbAdvAEmettre);//---- TO DO SUPPRIME LLab
  for ((numPrim=1); (numPrim<=nbPrimAEmettre); (numPrim++)) /* Pour les nouvelles primaires ? ?mettre */
  {
//    printf("Je suis dans emissionPrimSR %3i \n",sR->nbPrim);
    /* Calcul de la direction initiale de l'axe */
    if (sR->nbPrim==0) angI=tireGaussien(0.0,0.1); // ?mission de la radicule qui a un gravitropisme initial fort
      else angI=tireGaussien(P_angInitMoyVertPrim,P_angInitETVertPrim); // angle par rapport ? la verticale
    vInit[0]=sin(angI);
    vInit[1]=0.0;
    vInit[2]=cos(angI);
    angRot=sR->angDep+tireAngRad();
    rotZ(vInit,dirInit,angRot);

    /* G?n?ration de l'axe et int?gration dans le syst?me racinaire */

    nouvAxe=initialiseAxe(sR->nbAxeForm+1,P_diamMax,sR->origine,dirInit,NULL,NULL);
    ajouteAxeSR(sR,nouvAxe);
    sR->nbPrim++;
  }



  }  /* Fonction emissionPrimSR */
/****************************************************************************/
/****************************************************************************/
void emissionAdvSR(pTSysRac sR) 
{
/* Emission de nouveaux axes adventifs sur le syst?me racinaire */

  pTAxe nouvAxe;
  pTSeg segPere;  /* Segment sur lequel la racine adventive sera ?mise */
  int numAdv, nbAdvAEmettre;
  r3 vInit, dirInit, posInit;
  double angRot,angI,dBaseAdv,dBaseCour;


  nbAdvAEmettre=calcNouvNbAdv() - sR->nbAdv; /* Nombre de racines adventives ? ?mettre */

  
  for ((numAdv=1); (numAdv<=nbAdvAEmettre); (numAdv++)) /* Pour les nouvelles adventives ? ?mettre */
  {
//    printf("Je suis dans emissionAdvSR %3i \n",sR->nbAdv);
    /* Calcul de la position initiale de l'axe */
      /* Tirage de la distance ? la base de cette adventive */
      dBaseAdv=dRandUnif()*P_dBaseMaxAdv;

      /* D?termination du segment p?re, sur le premier axe */
      segPere=sR->premAxe->premSeg;
      dBaseCour=longSeg(segPere);
      while ((dBaseCour < dBaseAdv) && (segPere->suiv!=NULL)) {
        segPere=segPere->suiv;
        dBaseCour+=longSeg(segPere);
      }

      /* Position sur ce segment */
     
      origineAdv(segPere,posInit);
    /* Calcul de la direction initiale de l'axe */
    angI=tireGaussien(P_angInitMoyVertAdv,P_angInitETVertAdv); // angle par rapport ? la verticale
    vInit[0]=sin(angI);
    vInit[1]=0.0;
    vInit[2]=cos(angI);
    angRot=tireAngRad();
    rotZ(vInit,dirInit,angRot);

    /* G?n?ration de l'axe et int?gration dans le syst?me racinaire */
    
    nouvAxe=initialiseAxe(sR->nbAxeForm+1,P_propDiamAdv*P_diamMax,posInit,dirInit,sR->premAxe,segPere);
    ajouteAxeSR(sR,nouvAxe);
    sR->nbAdv++;// Lo?c PAGES
  }

  }  /* Fonction emissionAdvSR */
/****************************************************************************/
/****************************************************************************/


void calcVolProdSR(pTSysRac sR)
{
/* Calcul du volume racinaire produit sur la p?riode ?coul?e */
  int date;

  sR->volProd=0.0;
  for ((date=1); (date<=temps); (date++)) /* Boucle sur la p?riode ?coul?e */
  {
    sR->volProd+=sR->volDem[date]*sR->tSatis[date];
  }

}  /* Fonction calcVolProdSR */
/****************************************************************************/
/****************************************************************************/
void litParam(double xmax, double ymax,double zmax)

/* Fonction de lecture des parametres de la simulation */
{
 
   char bid[MAXLINE];
  
  // Seed Position X
  fscanf(FPar,"%f",&orig[0]);

    
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne 
  // Translate MIN3P frame for seed position in ArchiSimple frame :
  orig[0]=1000*(orig[0]-(xmax/2)); // *1000 to swich from meters (MIN3P) to millimeters (ArchiSimple)
   // printf("\n orig[0]= %f ", orig[0]);
  
  // Seed position Y
  fscanf(FPar,"%f",&orig[1]);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
     

  // Translate MIN3P frame for seed position in ArchiSimple frame :
  //printf("\n orig[1] : %f ",orig[1]);
  orig[1]=1000*(-orig[1]/2);// *1000 to swich from meters (MIN3P) to millimeters (ArchiSimple)
   
   //printf("\n orig[1] : %f ",orig[1]);
    
  // Seed position Z
  fscanf(FPar,"%f",&orig[2]);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  
  //printf("\n &orig[2]= %f ", orig[2]);

  // Translate MIN3P frame for seed position in ArchiSimple frame :
  //printf("\n orig[2] : %f ",orig[2]);
  orig[2]=1000*(zmax-orig[2]);// *1000 to swich from meters (MIN3P) to millimeters (ArchiSimple)
   
   //printf("\n orig[2] : %f \n",orig[2]);

  // Dur?e de simulation
  // CBF removed because simulation time is obtained from MIN3P
  //fscanf(FPar,"%i",&P_duree);
  //fgets(bid,MAXLINE-1,FPar); // reste de la ligne

  fscanf(FPar,"%f",&P_vitEmissionPrim);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_vitEmissionPrim= %f ", P_vitEmissionPrim);

  fscanf(FPar,"%d",&P_nbMaxPrim);// CBF change %i to %d for c++11
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_nbMaxPrim= %d ", P_nbMaxPrim);

  fscanf(FPar,"%f",&P_ageEmissionAdv);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_ageEmissionAdv= %f ", P_ageEmissionAdv);

  fscanf(FPar,"%f",&P_dBaseMaxAdv);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_dBaseMaxAdv= %f ", P_dBaseMaxAdv);

  fscanf(FPar,"%f",&P_vitEmissionAdv);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_vitEmissionAdv= %f ", P_vitEmissionAdv);

  fscanf(FPar,"%f",&P_propDiamAdv);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_propDiamAdv= %f ", P_propDiamAdv);

  fscanf(FPar,"%d",&P_nbMaxAdv);// CBF change %i to %d for c++11
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_nbMaxAdv= %d ", P_nbMaxAdv);

  fscanf(FPar,"%f",&P_diamMin);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_diamMin= %f ", P_diamMin);

  fscanf(FPar,"%f",&P_diamMax);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_diamMax= %f ", P_diamMax);

  fscanf(FPar,"%f",&P_penteVitDiam);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_penteVitDiam= %f ", P_penteVitDiam);

  fscanf(FPar,"%d",&P_tendanceDirTropisme);// CBF change %i to %d for c++11
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_tendanceDirTropisme= %d ", P_tendanceDirTropisme);

  fscanf(FPar,"%f",&P_intensiteTropisme);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_intensiteTropisme= %f ", P_intensiteTropisme);

  fscanf(FPar,"%f",&P_ageMaturitePointe);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_ageMaturitePointe= %f ", P_ageMaturitePointe);

  fscanf(FPar,"%f",&P_distRamif);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_distRamif= %f ", P_distRamif);

  fscanf(FPar,"%f",&P_propDiamRamif);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_propDiamRamif= %f ", P_propDiamRamif);

  fscanf(FPar,"%f",&P_coeffVarDiamRamif);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_coeffVarDiamRamif= %f ", P_coeffVarDiamRamif);

  fscanf(FPar,"%f",&P_TMD);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_TMP= %f ", P_TMD);

  fscanf(FPar,"%f",&P_penteDureeCroissDiam2);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_penteDureeCroissDiam2= %f ", P_penteDureeCroissDiam2);

  fscanf(FPar,"%f",&P_penteDureeVieDiamTMD);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_penteDureeVieDiamTMD= %f ", P_penteDureeVieDiamTMD);


  fscanf(FPar,"%f",&P_coeffCroissRad);
  fgets(bid,MAXLINE-1,FPar); // reste de la ligne
  //printf("\n &P_coeffCroissRad= %f ", P_coeffCroissRad);

  if (P_nbMaxPrim==1) {
    P_angInitMoyVertPrim=0.0; /* la racine ?mise est proche de la verticale */
    P_angInitETVertPrim=0.05;  /* ?cart-type de l'angle d'insertion  */
  }


} /* Fonction litParam */
/****************************************************************************/
/****************************************************************************/
void origineEmission(pTAxe nouvAxe)
{
nouvAxe->pointe->coord[0]=sR->origine[0];
nouvAxe->pointe->coord[1]=sR->origine[1];
nouvAxe->pointe->coord[2]=sR->origine[2];
} /* Fonction origineEmission */
/****************************************************************************/
/****************************************************************************/
void orienteEmission(pTAxe nouvAxe, int num)
{
double angRot,angI;
r3 vInit;

angI=tireGaussien(P_angInitMoyVertPrim,P_angInitETVertPrim);
vInit[0]=sin(angI);
vInit[1]=0.0;
vInit[2]=cos(angI);

angRot=sR->angDep+(2*pi*num/P_nbMaxPrim);
rotZ(vInit,nouvAxe->pointe->dirCroiss,angRot);
} /* Fonction orienteEmission */
/****************************************************************************/
/****************************************************************************/
float volPrimSeg(pTSeg seg)
/* Calcule le volume primaire du segment */
{
  return 0.25*pi*seg->axe->pointe->diametre*seg->axe->pointe->diametre*
  sqrt(((seg->posE[0]-seg->posO[0])*(seg->posE[0]-seg->posO[0]))+
       ((seg->posE[1]-seg->posO[1])*(seg->posE[1]-seg->posO[1]))+
       ((seg->posE[2]-seg->posO[2])*(seg->posE[2]-seg->posO[2])));

}  /* Fonction volSeg */
/****************************************************************************/
/****************************************************************************/
float volTotalSeg(pTSeg seg)
/* Calcule le volume total du segment */
{
  return 0.25*pi*seg->diametre*seg->diametre*sqrt(((seg->posE[0]-seg->posO[0])*(seg->posE[0]-seg->posO[0]))+
                                                  ((seg->posE[1]-seg->posO[1])*(seg->posE[1]-seg->posO[1]))+
                                                  ((seg->posE[2]-seg->posO[2])*(seg->posE[2]-seg->posO[2])));

}  /* Fonction volSeg */
/****************************************************************************/
/****************************************************************************/
float distHorSeg(pTSeg seg)
/* Calcule la distance horizontale d'un segment */
{
  return sqrt(((seg->posE[0]+seg->posO[0])*(seg->posE[0]+seg->posO[0])/4)+
              ((seg->posE[1]+seg->posO[1])*(seg->posE[1]+seg->posO[1])/4));

}  /* Fonction distHorSeg */
/****************************************************************************/
/****************************************************************************/
void calcLimitesSR(pTSysRac sR)
{
/* Calcul des limites du syst?me racinaire et de quelques autres variables */
  pTAxe axeCour;
  pTSeg segCour;
  float distHor,distHorLong,profLong,longS,profS,amplMax;

  // Initialisation des variables
  sR->volPrim=0.0;  // volume des structures primaires
  sR->secPointe=0.0;  // section totale des pointes actives
  sR->volTot=0.0;   // volume total
  sR->longueur=0.0;  // longueur
  sR->diamMax=-1.0e10; // diam?tre maximal, du plus gros segment
  sR->distMax=-1.0e10;  // extension maximale
  sR->profMax=-1.0e10;  // profondeur maximale

  sR->xbinf=+1.0e10; sR->ybinf=+1.0e10; sR->zbinf=+1.0e10; // initialisation des valeurs
  sR->xbsup=-1.0e10; sR->ybsup=-1.0e10; sR->zbsup=-1.0e10;

  distHorLong=0.0;
  profLong=0.0;

  axeCour=sR->premAxe;
  while (axeCour!=NULL)  // Calcul du volume "demand?"
  {
    sR->volTot+=pi*axeCour->pointe->diametre*axeCour->pointe->diametre*axeCour->pointe->longueur/4;
    sR->volPrim+=pi*axeCour->pointe->diametre*axeCour->pointe->diametre*axeCour->pointe->longueur/4;
    if ((axeCour->pointe->mature)&&(!axeCour->pointe->senile))
      sR->secPointe+=pi*axeCour->pointe->diametre*axeCour->pointe->diametre/4;
    segCour=axeCour->premSeg;
    while (segCour!=NULL) { // Tant que ce segment existe
      // Calculs sur le segment courant segCour
      if (segCour->posO[0] < sR->xbinf) { sR->xbinf=segCour->posO[0]; }
      if (segCour->posE[0] < sR->xbinf) { sR->xbinf=segCour->posE[0]; }
      if (segCour->posO[0] > sR->xbsup) { sR->xbsup=segCour->posO[0]; }
      if (segCour->posE[0] > sR->xbsup) { sR->xbsup=segCour->posE[0]; }

      if (segCour->posO[1] < sR->ybinf) { sR->ybinf=segCour->posO[1]; }
      if (segCour->posE[1] < sR->ybinf) { sR->ybinf=segCour->posE[1]; }
      if (segCour->posO[1] > sR->ybsup) { sR->ybsup=segCour->posO[1]; }
      if (segCour->posE[1] > sR->ybsup) { sR->ybsup=segCour->posE[1]; }

      if (segCour->posO[2] < sR->zbinf) { sR->zbinf=segCour->posO[2]; }
      if (segCour->posE[2] < sR->zbinf) { sR->zbinf=segCour->posE[2]; }
      if (segCour->posO[2] > sR->zbsup) { sR->zbsup=segCour->posO[2]; }
      if (segCour->posE[2] > sR->zbsup) { sR->zbsup=segCour->posE[2]; }

      if (segCour->diametre > sR->diamMax) { sR->diamMax=segCour->diametre; }

      distHor=distHorSeg(segCour);
      if (distHor > sR->distMax) { sR->distMax=distHor; }

      if (segCour->posO[2]>segCour->posE[2]) profS=segCour->posO[2]; else profS=segCour->posE[2];
      if (profS > sR->profMax) { sR->profMax=profS; }

      sR->volTot+=volTotalSeg(segCour);
      sR->volPrim+=volPrimSeg(segCour);
      longS=longSeg(segCour);
      sR->longueur+=longS;
      distHorLong+=distHor*longS;
      profLong+=profS*longS;

      segCour=segCour->suiv;
    }  // fin du while segCour
    axeCour=axeCour->suivant;
  }  // fin du while axeCour

  sR->xbinf=sR->xbinf-d2; sR->xbsup=sR->xbsup+d2;
  sR->ybinf=sR->ybinf-d2; sR->ybsup=sR->ybsup+d2;
  sR->zbinf=sR->zbinf-d2; sR->zbsup=sR->zbsup+d2;

  // Calcul de la maille de sol, en fonction de l'amplitude ? balayer

  amplMax=0.0;
  if ((sR->xbsup-sR->xbinf) > amplMax) amplMax=sR->xbsup-sR->xbinf;
  if ((sR->ybsup-sR->ybinf) > amplMax) amplMax=sR->ybsup-sR->ybinf;
  if ((sR->zbsup-sR->zbinf) > amplMax) amplMax=sR->zbsup-sR->zbinf;

// maille=amplMax/(NBCASEMAX-1);
  if (maille<mailleMin) maille=mailleMin;
  volElemSol=maille*maille*maille;

//  maille=5.0; volElemSol=maille*maille*maille;

  sR->distMoy=distHorLong/sR->longueur;
  sR->profMoy=profLong/sR->longueur;

//  printf(" xbinf :%7.2f",sR->xbinf); printf(" xbsup :%7.2f\n",sR->xbsup);
//  printf(" ybinf :%7.2f",sR->ybinf); printf(" ybsup :%7.2f\n",sR->ybsup);
//  printf(" zbinf :%7.2f",sR->zbinf); printf(" zbsup :%7.2f\n",sR->zbsup);
//  printf(" amplMax :%7.2f",amplMax); printf(" maille :%7.2f\n",maille);


}  /* Fonction calcLimitesSR */
/****************************************************************************/
/*************************************************************************/
void translateSR(pTSysRac sR)
/* Translate le syst?me racinaire de fa?on ? ce que tout se passe en territoire
 positif et d?marre de 0*/

{
pTAxe axeCour;
pTSeg segCour;

  axeCour=sR->premAxe;
  while (axeCour!=NULL)  // Calcul du volume "demand?"
  {
    // Translation de la pointe de l'axe
    axeCour->pointe->coord[0] -= sR->xbinf;
    axeCour->pointe->coord[1] -= sR->ybinf;
    axeCour->pointe->coord[2] -= sR->zbinf;

    segCour=axeCour->premSeg;
    while (segCour!=NULL) { // Tant qu'il y a des segments sur l'axe
      // Translation du segment segCour
      segCour->posO[0] -= sR->xbinf;
      segCour->posO[1] -= sR->ybinf;
      segCour->posO[2] -= sR->zbinf;

      segCour->posE[0] -= sR->xbinf;
      segCour->posE[1] -= sR->ybinf;
      segCour->posE[2] -= sR->zbinf;

      segCour=segCour->suiv;
    }
    axeCour=axeCour->suivant;
  }

  sR->xbsup-=sR->xbinf; sR->ybsup-=sR->ybinf; sR->zbsup-=sR->zbinf;
  sR->xbinf=0.0; sR->ybinf=0.0; sR->zbinf=0.0;

} /* Fonction translateSR */
/*************************************************************************/
/*************************************************************************/
float coordPointCase(int rangCase)
{ // renvoie les coordonn?es d'un point dans la case

  return (((rangCase+0.5)*maille)+(0.5*maille*dRandUnif()));

} /* Fonction coordPointCase */
/*************************************************************************/
/*************************************************************************/
float coordCentreCase(int rangCase)
{ // renvoie les coordonn?es du centre de la case

  return ((rangCase+0.5)*maille);

} /* Fonction coordCentreCase */

/****************************************************************************/
/****************************************************************************/
//---------------------------------------------------------------------------------------------------
void SetRootUpscaleFactor(double upscfact_x_min3p, double upscfact_y_min3p, double upscfact_z_min3p)
{
  upscfact_x = upscfact_x_min3p;
  upscfact_y = upscfact_y_min3p;
  upscfact_z = upscfact_z_min3p;
}

/*************************************************************************/
/*************************************************************************/
void calcColSR2D(pTSysRac sR, int rangDate, int nvz_gbl, int nvx_gbl, double zmax, int nvz, double xmax, int nvx, float *x, float *y, float *z,double *RSD, int tmax)

/* Calcule le tableau de colonisation du systeme racinaire */
// CB : Ici, en 2D
{
   pTAxe axeCour;
   pTSeg segCour;

   float xMil,yMil,zMil,distMil,xp1,yp1,zp1, xp2,yp2,zp2; // CB : coordon?es du milieu des segments
 // float x_MIN3P, y_MIN3P, z_MIN3P, r1, r2, RSD; // CB ajout
  //int hauteur_MIN3P=1; // CB : a entrer automatiquement ds archisimple



//#ifdef WINDOWS // CBF Windows (On VS2010 C99 standard is not considered ...)
// float* col2=new float[(nvz*nvx)-1];
// int* incre=new int[(nvz*nvx)-1];
//#endif

//#ifdef LINUX // CBF Linux
// float col2[(nvz*nvx)-1];
// int incre[(nvz*nvx)-1];
//#endif
int *incre = (int*)malloc((nvx*nvz-1)*sizeof(int)); // CBF : for c++11 standard used in CGAL
float *col2 = (float*)malloc(((nvz*nvx)-1)*sizeof(float)); // CBF : for c++11 standard used in CGAL



 //*  int caseD,caseZ;
 int incre_B4;
  int ivol,n,i,flag,inc,f;

//-----------------------------
  float volume_carre=((zmax*1000)/nvz_gbl)*((xmax*1000)/nvx_gbl);
  float coltot,variable; 
  
   n=0;
   incre_B4=0;
   coltot=0;
   variable=0;
   flag=0;
  

  
 //  printf("\n CALCOLSR2D : zmax = %d - nvz = %d - xmax= %d - nvx = %d \n", zmax, nvz, xmax, nvx);

   for(ivol=0;ivol<=((nvx*nvz)-1);ivol++)
     {
         RSD[ivol]=0;
         col2[ivol]=0.0f;
         incre[ivol]=0;
     }

 axeCour=sR->premAxe;

  while (axeCour!=NULL) // Tant qu'il y a des axes dans le syst?me racinaire
  {
    segCour=axeCour->premSeg;


    if (segCour->complet) 
    {
        while (segCour!=NULL)  // Tant qu'il y a des segments sur l'axe
        {
            xp1=segCour->posO[0]; yp1=segCour->posO[1]; zp1=segCour->posO[2]; // CB : coord origine segment
            xp2=segCour->posE[0]; yp2=segCour->posE[1]; zp2=segCour->posE[2]; // CB : coord extremite segment

//!FG May 2021 fast coding to upscale root system (upscale factor)
//!DSU, Jan 2022, enable different scaling factors for x, y, z directions
            if (upscfact_x > 0.0) {
              xp1=upscfact_x*xp1;
              xp2=upscfact_x*xp2;
            }

            if (upscfact_y > 0.0) {
              yp1=upscfact_y*yp1;    //FG too enable in order to consider ymax (compilation pb with ymax)
              yp2=upscfact_y*yp2;  
            }

            if (upscfact_z > 0.0) {
              zp1=upscfact_z*zp1;
              zp2=upscfact_z*zp2;
				    }

            // Calcul des coordonn?es du milieu du segment
            // CBF xMil=0.5+(0.001*((xp1+xp2)/2.0f));// CB changement de repere, 0.5 : centrage racine
            xMil=(xmax/2)+(0.001*((xp1+xp2)/2.0f));
            yMil=0.001*((yp1+yp2)/2.0f);// CB changement de repere (passage en metres)
            zMil=((-(zp1+zp2)/2.0f)*0.001)+zmax;// CB changement de repere

            distMil=sqrt((xMil*xMil)+(yMil*yMil));
        
            //*coltot+=pi*segCour->diametre*longSeg(segCour); // COLTOT total pour un pas de temps

            f=0;

            for(ivol=0;ivol<=(nvx*nvz-1);ivol++)
            {
                if(x[ivol]<=xMil) 
                    if(xMil<x[ivol+1])
                        if(z[ivol]<=zMil)
                             if(zMil<z[ivol+1+nvx])
                                if(yMil>=-0.3)
                                    if(yMil<0.3)
                                    {
                                        col2[ivol]+=pi*segCour->diametre*longSeg(segCour);
                                        RSD[ivol]=RSD[ivol]+((pi*segCour->diametre*longSeg(segCour))/volume_carre);
                                        //printf("\n CALCOLSR2D : RSD[%d]=%lf\n", ivol, RSD[ivol]);
                                        //printf("\n CALCOLSR2D : RSD[%d]=%lf", ivol, RSD[ivol]);
                                        if(ivol!=incre_B4)
                                        {
                                                for(i=1;i<=n;i++)
                                                {
                                                    if(ivol==incre[i]) 
                                                    {
                                    
                                                        f=1;
                                                    }
                                                }
                                        
                                                
                                                if(f==1) break;

                                                n=n+1;
                                                incre_B4=ivol;
                                                incre[n]=ivol;
                                        }
                                    }            
            }
            
            flag=1;
            segCour=segCour->suiv;

        }  // fin du while (segCour!=NULL)
    } // fin du if (segCour->complet)
    axeCour=axeCour->suivant;
  } // fin du while (axeCour!=NULL)

    if(flag==1)
    {
        for(i=1;i<=n;i++)
        {
            inc=incre[i];
            coltot=coltot+col2[inc];
            //printf("\n COLTOT_CONTROL_VOLUME[%d]=%lf \n", inc, col3[inc]);    
        }

    }



    //  for(ivol=0;ivol<=(nvx*nvz-1);ivol++) 
      //{
    
        //printf("\n COL(%d) = %f ", ivol, col2[ivol]);

      //}



    //printf("\n COLTOT=%lf\n ", coltot);
  //*    fprintf(Ftest, "%d %lf\n", rangDate, coltot);
    

}  /* Fonction calcColSR  */
/****************************************************************************/
/****************************************************************************/
void calcColSR1D(pTSysRac sR, int rangDate,int nvz_gbl, double zmax, int nvz,int tmax,double xmax, double *RSD) // CB tableau 1D : plus besoin de d et distmil
/* Calcule le tableau de colonisation du systeme racinaire */
// CB : Ici en 1D, 
{
  pTAxe axeCour;
  pTSeg segCour;

  float xMil,yMil,zMil,xp1,yp1,zp1,xp2,yp2,zp2; // CB : coordon?es du milieu des segments
  int compteur;
  
//  #ifdef WINDOWS // CBF Windows (On VS2010 C99 standard is not considered ...)
//  float* col1=new float[nvz+1];
//  #endif
//  #ifdef LINUX
//  float col1[nvz+1];
//  #endif
  float *col1 = (float*)malloc((nvz+1)*sizeof(float)); // CBF : for c++11 standard used in CGAL

  int caseZ;

  float hauteur_camembert=1000*zmax/nvz_gbl; // mm : largeur case en millimetres
  float rayon=1000*(xmax/2); // DSU: why use xmax as a variable here for 1D (e.g., Z direction) problem.
  float pi=3.151593;
  float volume_camembert=(pi*(hauteur_camembert)*rayon*rayon); // mm3
  volume_camembert=0.000000001*volume_camembert; //m3


  // Initialisation des valeurs dans la couche du tableau rangDate
  for (int z=0; z<nvz; z++) { // nvz : nb d'elements en z ds MIN3P. Remplace NBCASEZMAX.
     col1[z]=0.0f; //
  }  // fin du for sur z

  // Calcul des longueurs dans chaque case
  axeCour=sR->premAxe;
  while (axeCour!=NULL) // Tant qu'il y a des axes dans le syst?me racinaire
  {
    segCour=axeCour->premSeg;
    if (segCour->complet) {
      while (segCour!=NULL)  // Tant qu'il y a des segments sur l'axe
      {
        xp1=segCour->posO[0]; yp1=segCour->posO[1]; zp1=segCour->posO[2]; // CB : coord origine segment
        xp2=segCour->posE[0]; yp2=segCour->posE[1]; zp2=segCour->posE[2]; // CB : coord extremite segment

        // Calcul des coordonn?es du milieu du segment
        xMil=(xp1+xp2)/2.0f;
        yMil=(yp1+yp2)/2.0f;
        zMil=(zp1+zp2)/2.0f;
       
    //printf("\n xMil=%f - yMil=%f - zMil=%f ", xMil, yMil, zMil);
    

        //caseZ=int (zMil/20.0f); // CB prendre en charge maillage de min3p -> N? de la case ? partir de la graine, je pense
        caseZ=int (zMil/(1000*zmax/nvz_gbl)); /// CB modif taille cases : zmax/nvz = largeur 1 case en metre , *1000 pour passage en millimetres - caseZ en mm
                                              // on garde largeur case en millimetres car les positions des segments (zMil) sont en millimetres
      //  if (caseZ<0) caseZ=0; else if (caseZ>NBCASEZMAX) caseZ=NBCASEZMAX;// CB controle pour voir si la case est dans le domaine du tableau col 
        if (caseZ<0) caseZ=0; else if (caseZ>nvz) caseZ=nvz;
        
        ///col1[caseZ][rangDate]+=pi*segCour->diametre*longSeg(segCour); // CB incrementation de la case en question avec la surface du segment (diam*longueur*pi)
        col1[caseZ]+=pi*segCour->diametre*longSeg(segCour);
                                                            //CB ds une case donnee, on a valeur de la surface 
                                                                    //CB a diviser par le volume de la case


                                                                    // CB col : nb de surface de segment en mm2 dans un volume cylindrique en mm3 
        // CB : en 1D, on a enlev? toute la dimension horizontale sp?cifi?e par le parametre d.
        // Incr?ment de longueur dans la bonne case du tableau
        segCour=segCour->suiv;
      }  // fin du while (segCour!=NULL)
    } // fin du if (segCour->complet)
    axeCour=axeCour->suivant;
  } // fin du while (axeCour!=NULL)


    compteur=0;

    for (int z=0; z<nvz; z++) 
    { // CB nvz : nb d'elements en z dans MIN3P
                col1[z]=0.000001*col1[z]; // CBF en mm2 -> passage en m2
                RSD[compteur]=col1[z]/volume_camembert; // CBF m2/m3
                compteur=compteur+1;
    } 

                //int ivol;
                //for(ivol=0;ivol<nvz;ivol++)
                //{
                //    printf("\n ArchiSimple - CalcolSR1D : RSD[%d]=%lf", ivol, RSD[ivol]);
                //}


}  /* Fonction calcCol1SR  */
//*****************************************************************************************************
//*****************************************************************************************************
void developpeSR(pTSysRac sR)
{
/* D?veloppement : croissance et ramification de chaque axe du syst?me */
pTAxe axeCour;
double volumeDem=0.0;

 

  axeCour=sR->premAxe;
  while (axeCour!=NULL)  // Calcul du volume "demand?"
  {
    volumeDem+=calcDemandeVolume(axeCour);
    axeCour=axeCour->suivant;
  }

  sR->volDem[temps]=volumeDem;
  sR->tSatis[temps]=calcTauxSatis(volumeDem,sR->volMax[temps]);

  axeCour=sR->premAxe;
  while (axeCour!=NULL)  // D?veloppement
  {
    developpeAxe(axeCour,sR->tSatis[temps]);
    developpePointe(axeCour->pointe);    // modifie les attributs de la pointe
    axeCour=axeCour->suivant;
  }
  // printf(" Volume demand? : %16.5f \n",volumeDem);
  // printf(" NbRac : %6i \n",sR->nbAxeForm);

}  /* Fonction developpeSR */
/****************************************************************************/
/****************************************************************************/
void mortaliteSR(pTSysRac sR)
{
pTAxe axeCour, axeAEnlever;
 

  // Premier passage : calcul de la s?nilit? et affectation n?crose sur l'ensemble des axes */
  axeCour=sR->premAxe; // Dans le sens de premiers vers les derniers
  while (axeCour!=NULL)
  {
    if (axeCour->pointe->senile)
    { /* L'axe est n?cros? */
      affecValNecroseAxe(axeCour, 1);
    }
    else
    {  /* L'axe n'est pas n?cros? */
      /* Tous les noeuds en amont de la pointe ne sont pas necros?s non plus */
      affecValNecroseAmont(axeCour, 0);
    }

    axeCour=axeCour->suivant;
  }

  // Calcul de l'?lagage, enl?vement des axes tout n?cros?s
  axeCour=sR->dernAxe; // Dans le sens de derniers vers les premiers
  while (axeCour!=NULL)
  {
    if (axeToutNecrose(axeCour))
    {
      axeAEnlever=axeCour;
      axeCour=axeCour->precedent;
      if (axeAEnlever->pere!=NULL) enleveAxeSR(sR,axeAEnlever);
    }
    else axeCour=axeCour->precedent;
  }
 

}  /* Fonction mortaliteSR */
/****************************************************************************/
/****************************************************************************/
void croissanceRadialeSR(pTSysRac sR, float coeffCroiss)
{
pTAxe axeCour;
float diam;

 // CB TEST printf("\n CROISSANCE RADIALE SR \n");

  /* Premier passage, initialisation aux diametres primaires */
  axeCour=sR->dernAxe;
  while (axeCour!=NULL)
  {
    diam=axeCour->pointe->diametre;
    affecValDiamAxe(axeCour, diam);
    axeCour=axeCour->precedent;
  }

  /* Deuxi?me passage, avec incr?ment des diametres */
  axeCour=sR->dernAxe;
  while (axeCour!=NULL)
  {
    /* les noeuds en amont sont incr?ment?s si axe en croissance (pointe mature et non senile) */
    if ((axeCour->pointe->mature)&&(!axeCour->pointe->senile))
    {
      diam=axeCour->pointe->diametre;
      increValDiamAmont(axeCour, diam, coeffCroiss);
    }
    axeCour=axeCour->precedent;
  }



}  /* Fonction croissanceRadialeSR */
/****************************************************************************/
/****************************************************************************/
void imprimeSeg(pTSeg seg)
/* Imprime un segment sur le fichier des segments */
{
/*
  long int suivant,precedent;

  if (seg->prec==NULL) precedent=0;
  else precedent=seg->prec->num;

  if (seg->suiv==NULL) suivant=0;
  else suivant=seg->suiv->num;

  fprintf(FSeg,"%5li %5i %5li %5li %5li %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f\n",
    seg->num,seg->jourForm,seg->axe->num,suivant,precedent,seg->diametre,
    seg->posO[0],seg->posO[1],seg->posO[2],seg->posE[0],seg->posE[1],seg->posE[2]);
*/
//CBF removed for coupling to MIN3P :
/*
  printf("%5li %5i %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f\n",
    seg->axe->num,seg->jourForm,seg->diametre,
    seg->posO[0],seg->posO[1],seg->posO[2],seg->posE[0],seg->posE[1],seg->posE[2]);
*/

  //printf("%7.2f %7.2f %7.2f %7.2f %7.2f %7.2f\n",
  //  seg->posO[0],seg->posO[1],seg->posO[2],seg->posE[0],seg->posE[1],seg->posE[2]);

}  /* Fonction imprimeSeg */
/****************************************************************************/
/****************************************************************************/
void imprimeSRGlobal(pTSysRac sR)
{

//  fprintf(FSynth,"        longueur           volTot           volPrim          volProd          secPointe          diamMax        tSatisMoy          profMax          profMoy          distMax          distMoy         volSolD1         volSolD2\n");
/*
    fprintf(FSynth,"%16.2f %16.2f %16.2f %16.2f %16.2f %16.2f %16.2f %16.2f %16.2f %16.2f %16.2f\n",
          sR->longueur,sR->volTot,sR->volPrim,sR->volProd,sR->tSatisMoy,sR->profMax,
          sR->profMoy,sR->distMax,sR->distMoy,sR->volSolD1/1000.0,sR->volSolD2/1000.0);
*/
}  /* Fonction imprimeSRGlobal */
/****************************************************************************/
/****************************************************************************/
void imprimeAudit(void)
{/*                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       

  fprintf(FAudit,"voldispo                voldem             tsatis\n");
  for (int pas=1; pas<NBPASMAX; pas++)
  {
    fprintf(FAudit,"%16.2f %16.2f %8.5f\n",sR->volMax[pas],sR->volDem[pas],sR->tSatis[pas]);
  }
*/
}  /* Fonction imprimeAudit */
/****************************************************************************/
/****************************************************************************/
//void imprimeSRSegmentsEntete(void)
//{   /* Imprime l'ent?te du fichier contenant les noeuds du syst?me racinaire */
// fprintf(FSeg,"NumSeg Jour NumAxe Suiv Prec Diam     X1       Y1       Z1      X2       Y2       Z2\n");
//}  /* Fonction imprimeSRSegmentsEntete */
/****************************************************************************/
/****************************************************************************/
void imprimeAxeSegments(pTAxe axe)
{   /* Imprime les segments de l'axe */

pTSeg segCour;

  segCour=axe->premSeg;
  while (segCour!=NULL) {
    if (segCour->complet) imprimeSeg(segCour);
    segCour=segCour->suiv;
  }

}  /* Fonction imprimeAxeSegments */
/****************************************************************************/
/****************************************************************************/
//CBF removed for coupling to MIN3P :
//void imprimeSRSegments(pTSysRac sR)
//{  /* Imprime l'ensemble des segments du syst?me racinaire */

//pTAxe axeCour;

//  imprimeSRSegmentsEntete();

//  axeCour=sR->premAxe;

//  while (axeCour!=NULL)
//  {
//    imprimeAxeSegments(axeCour);
//    axeCour=axeCour->suivant;
//  }
 
//}  /* Fonction imprimeSRSegments */
/****************************************************************************/
/****************************************************************************/
void calcResumeSR(pTSysRac sR)
{  /* Calcule les diff?rentes variables r?sum?es et ?criture sur fichier */
/*
  calcTSatisMoySR(sR);
  calcLimitesSR(sR);
  if ((sR->tSatisMoy>0.6)&&(sR->longueur>150)) { // Si le syst?me est assez grand et le taux de satisfaction moyen est suffisant
    calcVolProdSR(sR);
    translateSR(sR);
    initialiseTabSol();
    calcDistancesSR(sR);
    imprimeSRGlobal(sR);
  }
  else fprintf(FSynth,"NA NA NA NA NA NA NA NA NA NA NA\n"); // donn?es manquantes
*/
}  /* Fonction calcResumeSR */
/****************************************************************************/
/****************************************************************************/
void fermeFichiers(void)
{
//CBF removed for coupling to MIN3P :
 // fclose(FSeg);
  fclose(FPar);
  fclose(FSol);
  fclose(FVol);

}  /* Fonction fermeFichiers */


void INIT_ARCHI(char *directory, int seed, int pos, double xmax, double ymax, double zmax)
{

		// DSU Add flag to control rand generator, by default, seed is negative and time(NULL) is used
		if (seed >= 0) {
			srand( (unsigned) seed); // CBF uncomment for debug
		}
		else {
			srand( (unsigned) time(NULL) ); /* Initialisation du g?n?rateur al?atoire, version windows */
		}

        ouvreFichiers(directory,pos); 
        

        litParam(xmax,ymax,zmax);
    


        litSol();
            
        sR=initialiseSR(orig);
            
        litVolumeMaxSR(sR);
            
}

//---------------------------------------------------------------------------------------------------
    void COMP_AS(int flag, double time_MIN3P, float delt_MIN3P, int nvx_gbl, int nvy_gbl, int nvz_gbl, \
                 int nvx, int nvy, int nvz, float *x_g, float *y_g, float *z_g, double *RSD, \
                 double xmax, double ymax, double zmax, int tmax)
    {

//  DSU : xmax and ymax can be zero for 1 D simulation, use 1.0 instead for 1D simulation.
            double xmax_loc, ymax_loc, zmax_loc;

            xmax_loc = (nvx_gbl > 1) ? xmax : 1.0;
            ymax_loc = (nvy_gbl > 1) ? ymax : 1.0;
            zmax_loc = (nvz_gbl > 1) ? zmax : 1.0;

// CBF : uncomment for debug            
            //printf("\n TIME COMPUTARCHI = %d - DELTA T COMPUTARCHI= %f \n",temps, deltaT);
            //printf("\n \n");
            //printf("\n flag =%d - time_MIN3P = %lf - delt_MIN3P = %f - zmax = %lf - nvz = %d - xmax= %lf - nvx = %d -tmax = %lf \n", flag, time_MIN3P, delt_MIN3P, zmax, nvz, xmax, nvx, tmax);

            temps=time_MIN3P; // CB ATTENTION il faut que time_MIN3P ait des valeurs entieres car ds archisimple temps est un entier
            deltaT=delt_MIN3P;

            /* Emission des racines primaires */
            emissionPrimSR(sR);

            /* Emission des racines adentives */
            emissionAdvSR(sR);
 
            /* D?veloppement du syst?me racinaire */
            developpeSR(sR);

            /* Croissance radiale du syst?me racinaire */
            croissanceRadialeSR(sR, P_coeffCroissRad);

            /* Mortalit? du syst?me racinaire */
            mortaliteSR(sR);
                
            //    if(int(time_MIN3P)%1==0)    // CBF  Calul RSD tout les 10 pas de temps pour le test


            if(flag==1)    // 1D
            {
                calcColSR1D(sR,temps,nvz_gbl,zmax_loc,nvz,tmax,xmax_loc,RSD); 
            }
            else        // 2D
            { 
                calcColSR2D(sR,temps,nvz_gbl,nvx_gbl,zmax_loc,nvz,xmax_loc,nvx,x_g,y_g,z_g,RSD,tmax);
                //printf("\n CALCOLSR2D : RSD[%d]=%lf\n", ivol, RSD[ivol]);
            }
                
                //int ivol;

                //for(ivol=0;ivol<(nvx*nvz-1);ivol++)
                //{
                //    printf("\n ArchiSimple : RSD[%d]=%lf", ivol, RSD[ivol]);
                //}
                
                    
                //imprimeSRSegments(sR); // CBF pour verif des segments ? chaque timestep
            
                
    }

    void END_ARCHI()
    {

    
        ////CBF removed for coupling to MIN3P :
        //imprimeSRSegments(sR);
        fermeFichiers();
        
    }

            
}

//#endif

        




    



