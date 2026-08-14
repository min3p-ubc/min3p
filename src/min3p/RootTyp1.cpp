/*******************************************************************************/
/*Copyright 2004, Loic Pages, INRA Institut National de la Recherche Agronomique
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
/*Please cite Pages et al. (2004) Root Typ: a generic model to depict and analyse
/* the root system architecture. Plant and Soil. 258:103-119
/******************************************************************************/





extern "C" //CBF
{ // CBF
// CBF : to include ROOT TYP and ARCHISIMPLE in the same MIN3P version, the following variables names
// have been modified in Root Typ (else it lead to multiple definitions problems at compiling) :
// FPar -> FFPar
// FSol -> FFSol
// dRandUnif -> ddRandUnif

/*****************************************************************************/
/*     ROOT TYP   r?vis? le 11/03/2016 (g?n?rateur al?atoire)                 
/*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h> // CBF
#include <cstdio>
#define NBTYPMAX 8  /* Nbre maximal de types */
#define NBHORMAX 200 /* Nombre d'horizons de sol */
#define NBVAGMAX 12  /* Nombre maximal de vagues de r?it?ration */
#define MAXLINE 120  /* Longueur maxi de la ligne dans fichiers texte */

//const int DeltaT=2;         /* Pas de temps, en jours */ // CBF REMOVED

const double Epsilon=1.0e-6; /* Petite valeur, proche de 0 */
const double Pi=3.14159265;  /* Valeur approch?e de la constante Pi */
const double EpaissHor=5.0;  /* Epaisseur horizons de sol */

typedef double r2[2];  /* Tableau 2D */
typedef double r3[3];  /* Tableau 3D */

typedef struct sysrac *PTSysRac;
typedef struct axe *PTAxe;
typedef struct meristeme *PTMeristeme;
typedef struct noeud *PTNoeud;

typedef struct sysrac /* Ensemble d'axes */
  {
  long int NbAxeForm;  /* Nombre d'axes form?s */
  long int NbAxeSup;   /* Nombre d'axes supprim?s */
  long int NbNoeudForm;    /* Nombre de noeuds form?s */
  int NumVReitCourante; /* Num?ro de vague de r?it?ration courante */
  int ReitPossible;     /* R?it?ration possible : 1 ou 0 */
  double AngDep;          /* Orientation */
  r3 Origine;           /* Position de l'origine */
  PTAxe PremAxe;       /* Premier axe du syst?me (acc?s ? la liste) */
  PTAxe DernAxe;       /* Dernier axe produit */
  } TSysRac ;

typedef struct meristeme /* M?rist?me apical, ou pointe de chaque racine */
  {
  double DistPrimInit; /* Distance de l'apex au dernier primordium initi? */
  r3 Coord;           /* Coordonn?es de l'apex */
  r3 DirCroiss;       /* Direction de croissance */
  r3 DirInit;         /* Direction initiale */
  double Age;          /* Age du m?rist?me */
  double Diametre;     /* Diam?tre au niveau de la pointe */
  r2 PCroiss;         /* Caract?ristiques de croissance potentielle du m?rist?me */
  double PRamif;       /* Distance inter-ramif potentielle */
  int Type;           /* Type (?tat morphog?n?tique) de m?rist?me : de 0 a 7 */
  int Senile;         /* S?nile ?, ou actif ... */
  int Mature;         /* Mature ?, ou primordium ... */
  } TMeristeme ;

typedef struct axe /* Ensemble constitu? d'un m?rist?me et liste de noeuds */
  {
  long int Num;      /* Num?ro de l'axe */
  int NbNoeud;       /* Nombre de noeuds */
  PTMeristeme Meris; /* M?rist?me apical */
  PTAxe Suivant;     /* Suivant de la liste */
  PTAxe Precedent;   /* Precedent de la liste */
  PTAxe Pere;        /* Axe p?re, sur lequel celui-ci est branch? */
  PTNoeud PremNoeud; /* Premier noeud de l'axe, sa base */
  PTNoeud DernNoeud; /* Dernier noeud de l'axe, apical */
  } TAxe ;

typedef struct noeud
  {
  long int Num;      /* Num?ro d'ordre de cr?ation */
  int JourForm;      /* Date de formation (en jours) */
  double Diametre;    /* Diametre au niveau du noeud */
  r3 Pos;            /* Position dans l'espace */
  PTNoeud SuivSPere; /* Suivant sur le p?re (quand branchement) */
  PTNoeud SuivSFils; /* Suivant sur le fils, axe d'appartenance */
  PTNoeud Prec;      /* Precedent */
  PTAxe Pere;        /* Axe p?re, pointe sur NULL quand non branchement */
  PTAxe Fils;        /* Axe fils, d'appartenance du noeud */
  int Necrose;       /* Necrose ? 0 : non; 1 : oui */
  } TNoeud ;

typedef struct horizon
  {
  float Croiss;  /* Coefficient de croissance, compris entre 0 et 1 */
  float Ramif;   /* Coefficient multiplicateur de distance inter-ramif  >1 */
  float ICMeca;  /* Intensit? de la contrainte m?canique */
  int OCMeca;    /* Orientation de la contrainte m?canique (O iso, ou 1 vert) */
  } THorizon ;

typedef THorizon TSol[NBHORMAX];  /* Sol, tableau d'horizons */

/* Fichiers */
// CBF removed for coupling to MIN3P :
//FILE *FMeris;  /* Contient les informations sur les m?rist?mes */
//FILE *FNoeud;  /* Contient la structure, sous forme de noeuds */
FILE *FFPar;    /* Param?tres */
FILE *FFSol;    /* Informations sur le sol, par horizons */
//FILE *Frsd;   /* CBF added: RSD en fct du temps


/* Param?tres, lus dans fichier param?tres */
int P_Duree; /* Dur?e de la simulation */
int P_NbVag; /* Nombre de vagues de r?it?ration */
int P_NbAxeOG; /* Nombre d'axes au d?part sur l'organe g?n?rateur */
int P_TpsReitVag[NBVAGMAX+1]; /* Vagues de r?it?ration */
float P_CoeffCroissRad;
float P_AngIMoy[NBTYPMAX]; /* Angle d'insertion ramification moyenne */
float P_AngIEt[NBTYPMAX];  /* Angle d'insertion ramification ?cart-type */
float P_AngIReitMoy[NBTYPMAX]; /* Angle d'insertion r?it?ration moyenne */
float P_AngIReitEt[NBTYPMAX];  /* Angle d'insertion r?it?ration ?cart-type */
float P_DurDevPrim[NBTYPMAX];  /* Dur?e de d?veloppement du primordium */
float P_CroissMoy[2][NBTYPMAX]; /* Param?tres de la courbe de croissance (moy) */
float P_CroissEt[2][NBTYPMAX]; /* Param?tres de la courbe de croissance (e-t) */
float P_RamifMoy[NBTYPMAX]; /* Distance inter-ramification (moyenne) */
float P_RamifEt[NBTYPMAX]; /* Distance inter-ramification (?cart-type)*/
int P_TTrop[NBTYPMAX]; /* Type de tropisme (0: plagio; -1: geo-; +1: geo+; 2: exo */
float P_ITrop[NBTYPMAX]; /* Intensit? du tropisme */
float P_SCMeca[NBTYPMAX]; /* Sensibilit? contrainte m?canique */
float P_PropTypRamif[NBTYPMAX][NBTYPMAX]; /* Proportion des types de ramif */
float P_DureeNecrose[NBTYPMAX]; /* Dur?e entre arr?t de croissance et n?crose */
float P_DiamPrim[NBTYPMAX]; /* Diam?tre primaire (de la pointe) */
float P_ProbReit[NBTYPMAX]; /* Probabilit? de r?it?ration durant le pas (quand vague) */
int P_NbReitMin[NBTYPMAX]; /* Nombre minimal de r?it?rations, lorsque r?it?ration */
int P_NbReitMax[NBTYPMAX]; /* Nombre maximal de r?it?rations, lorsque r?it?ration */
float P_AgeTransf[NBTYPMAX]; /* Age de d?clenchement possibilit? de transformation */
float P_ProbTransf[NBTYPMAX]; /* Probabilit? de transformation durant une journ?e */
int P_SensTransf[NBTYPMAX]; /* Sens de la transformation : -1 ou +1 */

/* Variables globales diverses */
float DeltaT;  //CBF ADD (REMOVED FROM LINES BEFORE)
int Temps=0;  /* Le temps, en jours */
double aire=0;//CBF pour initialisation aire RSD
r3 Orig; /* Position d'origine de l'organe g?n?rateur */

PTSysRac SR;  /* Le syst?me racinaire */
TSol Sol;     /* Le sol */

/****************************************************************************/
double ddRandUnif(void)
/* Cette fonction tire un al?atoire uniforme r?el entre 0 et 1 */
{
  //srand( (int) 123);// CBF for DSU benchmark tests, constant random, affect ArchiSimple capability
  double tirage=((double) rand() + 1)/((double) RAND_MAX + 2);
  //DSU this will cause difference in every running. Is it possible to add a seed parameter to random function
  //DSU so that the results are identical
  return(tirage);
}
/****************************************************************************/
/****************************************************************************/

void Norme(r3 u, r3 un)
/* Cette fonction norme le vecteur u de l'espace de dimension 3.
  Le vecteur norme de retour est appele un. */
{
double NorU;
  NorU=sqrt((u[0]*u[0])+(u[1]*u[1])+(u[2]*u[2]));
  if (NorU<Epsilon)
  {
  printf("ATTENTION, vecteur nul ! Sa norme vaut : %f \n",NorU);
  exit(1);
  }
  else
  {
   un[0]=u[0]/NorU;
   un[1]=u[1]/NorU;
   un[2]=u[2]/NorU;
  }
}  /* Fonction Norme */
/****************************************************************************/
/****************************************************************************/
double ProdScal(r3 u,r3 v)
/* Cette fonction retourne le produit scalaire de 2 vecteurs u et v de
  l'espace a 3 dimensions. */
{
double ProdScal;
  ProdScal=(u[0]*v[0])+(u[1]*v[1])+(u[2]*v[2]);
  return(ProdScal);
}  /* Fonction ProdScal */
/****************************************************************************/
/****************************************************************************/
void ProdVect(r3 u, r3 v, r3 u_vect_v)
/* Cette fonction calcule le produit vectoriel de deux vecteurs u et v
  de l'espace de dimension 3. Le vecteur resultant est u_vect_v. */
{
  u_vect_v[0]=(u[1]*v[2])-(v[1]*u[2]);
  u_vect_v[1]=(u[2]*v[0])-(v[2]*u[0]);
  u_vect_v[2]=(u[0]*v[1])-(v[0]*u[1]);
}   /* Fonction ProdVect */
/****************************************************************************/
/****************************************************************************/
void RotVect(double omega, r3 u, r3 x, r3 rot_x)

/* Cette fonction calcule le vecteur rot_x dans l'espace de dimension 3,
  issu de la rotation du vecteur x autour d'un axe dont u est un vecteur
  unitaire. La rotation se fait d'un angle omega radians. Elle appelle
  PRODSCAL, PRODVECT. */
{
double uscalx;   /* produit scalaire u.x  */
r3    uvectx;   /* produit vectoriel u^x */

  uscalx=ProdScal(u,x);
  ProdVect(u,x,uvectx);

  rot_x[0]=((1-cos(omega))*uscalx*u[0])
      +(cos(omega)*x[0])+(sin(omega)*uvectx[0]);
  rot_x[1]=((1-cos(omega))*uscalx*u[1])
      +(cos(omega)*x[1])+(sin(omega)*uvectx[1]);
  rot_x[2]=((1-cos(omega))*uscalx*u[2])
      +(cos(omega)*x[2])+(sin(omega)*uvectx[2]);

}  /* Fonction RotVect */
/****************************************************************************/
/****************************************************************************/
void RotZ(r3 u, r3 v, double teta)
/* Cette fonction fait tourner "u" d'un angle "teta" autour de l'axe (Oz);
  le vecteur calcule est "v" */
{
  v[0]=(u[0]*cos(teta))-(u[1]*sin(teta));
  v[1]=(u[0]*sin(teta))+(u[1]*cos(teta));
  v[2]=u[2];
}
/****************************************************************************/
/****************************************************************************/
int IRandUnif(int imax)

/* Cette fonction tire un al?atoire uniforme entier entre 0 et imax */
{
  int tirage;
  //srand( (int) 123);// CBF for DSU benchmark tests, constant random, affect ArchiSimple capability
  tirage=imax+1;
  while (tirage>imax) tirage=rand();
  return tirage;
}
/****************************************************************************/
/****************************************************************************/
void OuvreFichiers(char *directory, int pos)
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

char *paramdir = (char*)malloc((pos+strlen("paramRT.txt")+1)*sizeof(char)); // CBF c++11 standard used in CGAL

#ifdef WINDOWS
_snprintf(paramdir,(pos+strlen("paramRT.txt")+1),"%s%s", rep,"paramRT.txt");
//DSU, Here paramdir[pos+strlen...] = '\0' is not required as length of rep is less than paramdir and 
//the null-terminator is automatically appended.
#else
snprintf(paramdir,(pos+strlen("paramRT.txt")+1),"%s%s", rep,"paramRT.txt");
#endif
//printf("\n paramdir = %s ", paramdir);
FFPar = fopen(paramdir, "rt");    // fichier des parametres du modele

char *soldir = (char*)malloc((pos+(strlen("sol.txt"))+1)*sizeof(char)); // CBF : for c++11 standard used in CGAL
#ifdef WINDOWS
_snprintf(soldir,pos+(strlen("sol.txt"))+1,"%s%s", rep,"sol.txt");
//DSU, Here soldir[pos+strlen...] = '\0' is not required as length of rep is less than paramdir and 
//the null-terminator is automatically appended.
#else
snprintf(soldir,pos+(strlen("sol.txt"))+1,"%s%s", rep,"sol.txt");
#endif
//printf("\n soldir = %s ", soldir);
FFSol = fopen(soldir,"rt");      // fichier des caracteristiques du sol

//CBF removed for coupling to MIN3P :
// FSeg = fopen(("/home/celine/Documents/MIN3P_UBC/trunk_qroot_coupling/LOCAL_TEST/TRANSP2D/seg.txt"), "rt"); 

} /* Fonction ouvreFichiers */
/****************************************************************************/
/****************************************************************************/
void LitSol(void)
/* Fonction de lecture des caract?ristiques du sol, une ligne par horizon */
{
int hor;              /* Compteur des horizons */
char bid[MAXLINE];    /* Cha?ne qui accueille les caract?res suppl?mentaires */

fgets(bid,MAXLINE-1,FFSol);          /* Ligne ent?te */
for (hor=0; hor<NBHORMAX; hor++)
{
fscanf(FFSol,"%f %f %f %d",&Sol[hor].Croiss,&Sol[hor].Ramif,&Sol[hor].ICMeca,&Sol[hor].OCMeca);
//printf("\n FFSol :  %f %f %f %d ",Sol[hor].Croiss,Sol[hor].Ramif,Sol[hor].ICMeca,Sol[hor].OCMeca);
/*  fscanf(FFSol,"%f",&Sol[hor].Croiss); /* Favorable ? la croissance */
/*  fscanf(FFSol,"%f",&Sol[hor].Ramif);  /* Favorable ? la ramification */
/*  fscanf(FFSol,"%f",&Sol[hor].ICMeca); /* Intensit? de la contrainte */
/*  fscanf(FFSol,"%d",&Sol[hor].OCMeca); /* Orientation 0: iso, 1: verticale */
  fgets(bid,MAXLINE-1,FFSol);
}

} /* Fonction LitSol */
/****************************************************************************/
/****************************************************************************/
double CroissSol(TSol Sol, double Profondeur)
/* Renvoie le coefficient de croissance du sol ? la Profondeur donn?e */
{
int Hor;

  Hor=(int) floor(Profondeur/EpaissHor);
  if (Hor>=NBHORMAX) Hor=NBHORMAX-1;
  if (Hor<0) Hor=0;

  return(Sol[Hor].Croiss);
} /* Fonction CroissSol */
/****************************************************************************/
/****************************************************************************/
double RamiFFSol(TSol Sol, double Profondeur)
/* Renvoie le coefficient de ramification du sol ? la Profondeur donn?e */
{
int Hor;

  Hor=(int) floor(Profondeur/EpaissHor);
  if (Hor>=NBHORMAX) Hor=NBHORMAX-1;
  if (Hor<0) Hor=0;

  return(Sol[Hor].Ramif);
} /* Fonction RamiFFSol */
/****************************************************************************/
/****************************************************************************/
double ICMecaSol(double Profondeur)
/* Renvoie l'intensit? de la contraine m?ca du sol ? la Profondeur donn?e */
{
int Hor;

  Hor=(int) floor(Profondeur/EpaissHor);
  if (Hor>=NBHORMAX) Hor=NBHORMAX-1;
  if (Hor<0) Hor=0;

  return(Sol[Hor].ICMeca);
} /* Fonction ICMecaSol */
/****************************************************************************/
/****************************************************************************/
int OCMecaSol(double Profondeur)
/* Renvoie l'indice de la direction de contrainte : 0 pour iso, 1 pour verti */
{
int Hor;

  Hor=(int) floor(Profondeur/EpaissHor);
  if (Hor>=NBHORMAX) Hor=NBHORMAX-1;
  if (Hor<0) Hor=0;

  return(Sol[Hor].OCMeca);
} /* Fonction OCMecaSol */
/****************************************************************************/
/****************************************************************************/
double TireAngI(int TypeFils)
{  /* Tire l'angle d'insertion d'une ramification sur sa m?re */
double TireAngI,tire1,tire2;

tire1=ddRandUnif();
tire2=ddRandUnif();
TireAngI=P_AngIMoy[TypeFils]+(P_AngIEt[TypeFils]
                         *sqrt(-log(tire1))*cos(Pi*tire2)*1.414);
return(TireAngI);
} /* Fonction TireAngI */
/****************************************************************************/
/****************************************************************************/
double TireAngIReit(int Type)
{    /* Tire l'angle d'insertion d'une r?it?ration sur sa m?re */
double TireAngIReit,tire1,tire2;

tire1=ddRandUnif();
tire2=ddRandUnif();
TireAngIReit=P_AngIReitMoy[Type]+(P_AngIReitEt[Type]*sqrt(-log(tire1))*cos(Pi*tire2)*1.414);
return(TireAngIReit);
} /* Fonction TireAngIReit */
/****************************************************************************/
/****************************************************************************/
double TireAngRad(void)
{   /* Tire l'angle radial dans l'intervalle 0 - 2*Pi */

return (2.0*Pi*ddRandUnif());
} /* Fonction TireAngRad */
/****************************************************************************/
/****************************************************************************/
void IncreNbNoeudSR(PTSysRac SR)
/* Incr?mente le nombre de noeuds qui a ?t? form? dans ce syst?me SR */
{
  SR->NbNoeudForm++;
//printf("\n SR->NbNoeudForm = %li \n", SR->NbNoeudForm);
} /* Fonction IncreNbNoeudSR */
/****************************************************************************/
/****************************************************************************/
PTNoeud CreeNoeud(void)
/* Cette fonction retourne une nouvelle variable de type PTNoeud,
  c'est-?-dire un pointeur sur le type TNoeud */
{
PTNoeud Nd;
  Nd=(PTNoeud) malloc(sizeof(TNoeud));
  if (Nd==NULL)
  { printf("Probl?me m?moire allocation noeud \n"); exit(1); }

return Nd;
} /* Fonction CreeNoeud */
/****************************************************************************/
/****************************************************************************/
PTNoeud InitialiseNoeud(long int Num, r3 Position, double Diam, PTAxe Pere, PTAxe Fils)
/* Cette fonction retourne une nouvelle variable de type PTNoeud,
  dont une partie des valeurs est initialis?e */
{
PTNoeud Nd;

  Nd=CreeNoeud();

  Nd->Num=Num;
  Nd->JourForm=Temps;
  Nd->Necrose=0;

  Nd->Diametre=Diam;
  Nd->Pere=Pere;
  Nd->Fils=Fils;

  Nd->Pos[0]=Position[0];
  Nd->Pos[1]=Position[1];
  Nd->Pos[2]=Position[2];

  
  Nd->SuivSPere=NULL;
  Nd->SuivSFils=NULL;
  Nd->Prec=NULL;

return Nd;
} /* Fonction InitialiseNoeud */
/****************************************************************************/
/****************************************************************************/
void DetruitNoeud(PTNoeud NdADetruire)
/* Supprime un noeud en m?moire */
{
  free(NdADetruire);
} /* Fonction DetruitNoeud */
/****************************************************************************/
/****************************************************************************/
PTMeristeme CreeMeris(void)
/* Cette fonction retourne une nouvelle variable de type PTMeristeme,
  c'est-?-dire un pointeur sur le type TMeristeme */
{
PTMeristeme Meris;
  Meris=(PTMeristeme) malloc(sizeof(TMeristeme));
  if (Meris==NULL)
  { printf("Probl?me m?moire allocation Merist?me \n"); exit(1); }

return Meris;
} /* Fonction CreeMeris */
/****************************************************************************/
/****************************************************************************/
void TirePCroissMeris(PTMeristeme Meris)
{
double tire1,tire2;
const double PCroissMin=0.3;

tire1=ddRandUnif();
tire2=ddRandUnif();

/* On tire une asymptote suivant une loi normale et une vitesse initiale fixe */
Meris->PCroiss[0]=P_CroissMoy[0][Meris->Type]+(P_CroissEt[0][Meris->Type]*
            sqrt(-log(tire1))*cos(Pi*tire2)*1.414);  /* Asymptote */
if (Meris->PCroiss[0]<PCroissMin) { Meris->PCroiss[0]=PCroissMin; };

Meris->PCroiss[1]=P_CroissMoy[1][Meris->Type]; /* Vitesse initiale */
} /* Fonction TirePCroissMeris */
/****************************************************************************/
/****************************************************************************/
void TirePRamifMeris(PTMeristeme Meris)
{  /* Affectation de la distance inter-ramification */
if (Meris->PCroiss[0]>0.01) { Meris->PRamif=P_RamifMoy[Meris->Type]; }
                    else   { Meris->PRamif=1000.0; }
} /* Fonction TirePRamifMeristeme */
/****************************************************************************/
/****************************************************************************/
PTMeristeme InitialiseMeris(int Type, r3 Position, r3 Direction)
/* Cette fonction retourne une nouvelle variable de type PTMeristeme,
  dont les valeurs sont en partie initialis?es */
{
PTMeristeme Meris;

  Meris=CreeMeris();

  Meris->DistPrimInit=0.0;
  Meris->Age=0.0;
  Meris->Type=Type;
  Meris->Diametre=P_DiamPrim[Meris->Type];
  Meris->Senile=0;
  Meris->Mature=0;
  if (fabs(Position[0])>1000.0) printf("Probleme\n");
  Meris->Coord[0]=Position[0];
  Meris->Coord[1]=Position[1];
  Meris->Coord[2]=Position[2];

  Meris->DirCroiss[0]=Direction[0];
  Meris->DirCroiss[1]=Direction[1];
  Meris->DirCroiss[2]=Direction[2];

  Meris->DirInit[0]=Direction[0];
  Meris->DirInit[1]=Direction[1];
  Meris->DirInit[2]=Direction[2];

  TirePCroissMeris(Meris);
  TirePRamifMeris(Meris);

    //printf("\n INITMERIS : Meris->Age = %f \n", Meris->Age);


return Meris;
} /* Fonction InitialiseMeristeme */
/****************************************************************************/
/****************************************************************************/
void DeflecMecaMeris(PTMeristeme Meris, r3 DirApresMeca, double Elong)
{
const double Teta=15.0; /* Angle autour de G, en degres */

r3 VTire,VTireN,DirInt;
double Profondeur, Cont, Aa, Rr; /* Aa et Rr rajout?s suite interv. Xavier */

  Profondeur=Meris->Coord[2];
  Cont=ICMecaSol(Profondeur);
  if (OCMecaSol(Profondeur)==1)  /* Contrainte anisotrope verticale */
  {
    /* Tirage vecteur dans l'angle Teta autour de G */
    Aa=ddRandUnif()*2.0*Pi; /* Angle al?atoire */
    Rr=sqrt(ddRandUnif())*sin(Pi*Teta/180);
    VTireN[0]=Rr*cos(Aa);
    VTireN[1]=Rr*sin(Aa);
    VTireN[2]=sqrt(1.0-(VTireN[0]*VTireN[0])-(VTireN[1]*VTireN[1]));

    DirInt[0]=Meris->DirCroiss[0]+(Elong*VTireN[0]*Cont*P_SCMeca[Meris->Type]);
    DirInt[1]=Meris->DirCroiss[1]+(Elong*VTireN[1]*Cont*P_SCMeca[Meris->Type]);
    DirInt[2]=Meris->DirCroiss[2]+(Elong*VTireN[2]*Cont*P_SCMeca[Meris->Type]);
  }
  else    /* Contrainte isotrope [OCMecaSol(Profondeur)==0] */
  {
  VTire[0]=2.0*ddRandUnif()-1.0;
  VTire[1]=2.0*ddRandUnif()-1.0;
  VTire[2]=2.0*ddRandUnif()-1.0;
  Norme(VTire,VTireN);
  if (ProdScal(VTireN,Meris->DirCroiss)<0.0)
  {
  VTireN[0]=-VTireN[0];
  VTireN[1]=-VTireN[1];
  VTireN[2]=-VTireN[2];
  }
  DirInt[0]=Meris->DirCroiss[0]+(Elong*VTireN[0]*Cont*P_SCMeca[Meris->Type]);
  DirInt[1]=Meris->DirCroiss[1]+(Elong*VTireN[1]*Cont*P_SCMeca[Meris->Type]);
  DirInt[2]=Meris->DirCroiss[2]+(Elong*VTireN[2]*Cont*P_SCMeca[Meris->Type]);
  }
  Norme(DirInt,DirApresMeca);

} /* Fonction DeflecMecaMeris */
/****************************************************************************/
/****************************************************************************/
void DeflecGeoMeris(PTMeristeme Meris, r3 DirApresMeca, r3 DirApresGeo, double Elong)
/* Version avec plagiotropisme */
{
r3 DirInt,VGeoInt,VGeo;

  switch (P_TTrop[Meris->Type]) {
    case -1 : VGeo[0]=0.0;                  /* Gravitropisme n?gatif */
              VGeo[1]=0.0;
              VGeo[2]=-1.0;
              break;
    case 0 : VGeoInt[0]=Meris->DirInit[0]; /* Plagiotropisme */
             VGeoInt[1]=Meris->DirInit[1];
             VGeoInt[2]=0.0;
             Norme(VGeoInt,VGeo);
             break;
    case 1 : VGeo[0]=0.0;                  /* Gravitropisme positif */
             VGeo[1]=0.0;
             VGeo[2]=1.0;
              break;
    case 2 : VGeoInt[0]=Meris->DirInit[0]; /* Exotropisme */
             VGeoInt[1]=Meris->DirInit[1];
             VGeoInt[2]=Meris->DirInit[2];
             Norme(VGeoInt,VGeo);
             break;
    default : VGeo[0]=0.0;                 /* Gravitropisme positif */
              VGeo[1]=0.0;
              VGeo[2]=1.0;
              break;
  }

  DirInt[0]=DirApresMeca[0]+(VGeo[0]*P_ITrop[Meris->Type]*Elong);
  DirInt[1]=DirApresMeca[1]+(VGeo[1]*P_ITrop[Meris->Type]*Elong);
  DirInt[2]=DirApresMeca[2]+(VGeo[2]*P_ITrop[Meris->Type]*Elong);

  Norme(DirInt,DirApresGeo);
} /* Fonction DeflecGeoMeris */
/****************************************************************************/
/****************************************************************************/
void DeflecSurfMeris(PTMeristeme Meris, r3 DirApresGeo, r3 DirApresSurf)
{
const double ProfLim=3.0*ddRandUnif();
r3 DirInt;
  DirInt[0]=DirApresGeo[0];
  DirInt[1]=DirApresGeo[1];
  DirInt[2]=DirApresGeo[2];

  if ((DirInt[2]<0.0) && ((Meris->Coord[2])<ProfLim) && (Meris->Type>0))
                                                       DirInt[2]=DirInt[2]/8.0;
  Norme(DirInt,DirApresSurf);
} /* Fonction DeflecSurfMeris */
/****************************************************************************/
/****************************************************************************/
void ReorienteMeris(PTMeristeme Meris, double Elong)
{
r3 DirInt1, DirInt2, NouvDir;

  DeflecMecaMeris(Meris,DirInt1,Elong);
  DeflecGeoMeris(Meris,DirInt1,DirInt2,Elong);
  DeflecSurfMeris(Meris,DirInt2,NouvDir);

  Meris->DirCroiss[0]=NouvDir[0];
  Meris->DirCroiss[1]=NouvDir[1];
  Meris->DirCroiss[2]=NouvDir[2];


} /* Fonction ReorienteMeris */
/****************************************************************************/
/****************************************************************************/
double CalcElongationMeris(PTMeristeme Meris, float DeltaT) 

{
/* Calcul de l'?longation potentielle en subdivisant l'intervalle de temps,
et prise en compte du coefficient de croissance du sol ? la profondeur ad hoc */
int NbDiv=20;
double dt,t,A,b,ElongPot;
/* int i; */

  dt=(double) DeltaT/(double) NbDiv;
  t=Meris->Age; /* Age m?rist?me vrai */
  A=Meris->PCroiss[0];  /* Asymptote */
  b=Meris->PCroiss[1];  /* Vitesse initiale */
  ElongPot = -A * (exp(-b * (t + DeltaT) / A) - exp(-b * t / A));
/*  ElongPot=0.0;
  for (i=1; i<=NbDiv; i++)
  { ElongPot=ElongPot+(b*exp(-b*(t+(i*dt))/A)*dt); } */
  return(ElongPot*CroissSol(Sol,Meris->Coord[2]));

} /* Fonction CalcElongationMeris */
/****************************************************************************/
/****************************************************************************/
void VieillitMeris(PTMeristeme Meris, float DeltaT) //CBF
{ /* Incr?mente l'?ge du m?rist?me selon le pas de temps */

    //printf("\n VIEILLIT MERIS ENTER \n");

  Meris->Age=Meris->Age+DeltaT;

    //printf("\n Meris->Age = %f - DeltatT = %f\n", Meris->Age, DeltaT);


    //printf("\n VIEILLIT MERIS EXIT \n");


} /* Fonction VieillitMeris */
/****************************************************************************/
/****************************************************************************/
void MatureMeris(PTMeristeme Meris)
{ /* Assure l'?volution du primordium en m?rist?me si son ?ge est atteint */
  if ((!Meris->Mature)&&(Meris->Age>P_DurDevPrim[Meris->Type]))
  {
    Meris->Mature=1;  /* Le primordium devient m?rist?me vrai */
    Meris->Age=0.0;   /* Son ?ge est r?initialis? ? 0 en tant que m?rist?me */
  }
} /* Fonction MatureMeris */
/****************************************************************************/
/****************************************************************************/
void SenesceMeris(PTMeristeme Meris)
{ /* Rend s?nescent le m?rist?me qui ne s'allonge plus ou qui a r?it?r? */
  Meris->Senile=1;
} /* Fonction SenesceMeris */
/****************************************************************************/
/****************************************************************************/
void TransformeMeris(PTMeristeme Meris)
{ /* R?alise ?ventuellement la transformation du type du m?rist?me */
double ProbaDeTransformation;


  /* Calcule la probabilit? de transformation */
  ProbaDeTransformation=(double (Meris->Mature))*(double (Meris->Age>P_AgeTransf[Meris->Type]))*P_ProbTransf[Meris->Type];
/*  printf("Proba de transformation : %7.3d", ProbaDeTransformation, "\n"); */

  if (ddRandUnif()<ProbaDeTransformation) /* transformation */
  {
    Meris->Type=Meris->Type+P_SensTransf[Meris->Type];
    Meris->Age=0.0;  /* Age r?initialis? suite ? transformation */
    TirePCroissMeris(Meris);
    TirePRamifMeris(Meris);
    Meris->DirInit[0]=Meris->DirCroiss[0];
    Meris->DirInit[1]=Meris->DirCroiss[1];
    Meris->DirInit[2]=Meris->DirCroiss[2];
  }
  if ((Meris->Type<0)||(Meris->Type>NBTYPMAX))
  {
    printf("Probl?me dans TransformeMeris, Type du m?rist?me non conforme \n");
    exit(1);
  }
} /* Fonction TransformeMeris */
/****************************************************************************/
/****************************************************************************/
void DeveloppeMeris(PTMeristeme Meris, float DeltaT)//CBF
{ /* Assure l'?volution du m?rist?me */
  VieillitMeris(Meris, DeltaT);//CBF
  MatureMeris(Meris);
  TransformeMeris(Meris);
} /* Fonction DeveloppeMeris */
/****************************************************************************/
/****************************************************************************/
void DeplaceMeris(PTMeristeme Meris, double Elong)
{ /* Assure le d?placement du m?rist?me suite ? croissance axiale */

  /* Sa position est modifi?e */
  Meris->Coord[0]=Meris->Coord[0]+(Elong*Meris->DirCroiss[0]);
  Meris->Coord[1]=Meris->Coord[1]+(Elong*Meris->DirCroiss[1]);
  Meris->Coord[2]=Meris->Coord[2]+(Elong*Meris->DirCroiss[2]);

  /* Son attribut DistPrimInit est modifi? */
  Meris->DistPrimInit=Meris->DistPrimInit+Elong;

} /* Fonction DeplaceMeris */
/****************************************************************************/
/****************************************************************************/
double DistInterRamifMeris(PTMeristeme Meris, TSol Sol)
{ /* Renvoie la valeur locale de la distance inter-ramification du m?rist?me */

  return (Meris->PRamif*RamiFFSol(Sol,Meris->Coord[2]));

} /* Fonction DistInterRamifMeris */
/****************************************************************************/
/****************************************************************************/
void DetruitMeris(PTMeristeme MerisADetruire)
/* Supprime un m?rist?me */
{
  free(MerisADetruire);
} /* Fonction DetruitMeris
//****************************************************************************/
/****************************************************************************/
PTAxe CreeAxe(void)
/* Cette fonction retourne une nouvelle variable de type PTAxe,
  c'est-?-dire un pointeur sur le type TAxe */
{
PTAxe Axe;
  Axe=(PTAxe) malloc(sizeof(TAxe));
  if (Axe==NULL)
  { printf("Probl?me m?moire allocation dans CreeAxe \n"); exit(1); }

return Axe;
} /* Fonction CreeAxe */
/****************************************************************************/
/****************************************************************************/
PTAxe InitialiseAxe(long int NumAxe, int TypeMeris, r3 Origine, r3 DirInit, PTAxe AxePere)
/* Cette fonction retourne une nouvelle variable de type PTAxe,
  c'est-?-dire un pointeur sur le type TAxe */
{
PTAxe NouvAxe;
PTMeristeme Meris;
PTNoeud PremierNoeud;

  NouvAxe=CreeAxe();
  Meris=InitialiseMeris(TypeMeris,Origine,DirInit);
  PremierNoeud=InitialiseNoeud(SR->NbNoeudForm+1,Origine,P_DiamPrim[TypeMeris],AxePere,NouvAxe);
  NouvAxe->Meris=Meris;
  NouvAxe->PremNoeud=PremierNoeud;
  NouvAxe->DernNoeud=PremierNoeud;
  NouvAxe->NbNoeud=1;
  NouvAxe->Num=NumAxe;
  NouvAxe->Pere=AxePere;

  NouvAxe->Suivant=NULL;
  NouvAxe->Precedent=NULL;

  

return NouvAxe;
} /* Fonction InitialiseAxe */
/****************************************************************************/
/****************************************************************************/
PTNoeud AvtDernNoeudAxe(PTAxe Axe)
/* Cette fonction retourne une variable de type PTNoeud,
  qui pointe sur l'avant dernier noeud de l'axe, s'il existe */
{
PTNoeud NdCour,NdPrec;
  NdCour=Axe->PremNoeud;
  if (NdCour->SuivSFils==NULL)
  { printf("Pas d'avant-dernier noeud, car un seul noeud \n"); exit(1); }
  else
  {
    NdPrec=NdCour;
    NdCour=NdCour->SuivSFils;
    while ((NdCour->SuivSFils!=NULL)||(NdCour->SuivSPere!=NULL))
      {
      NdPrec=NdCour;
      if (NdCour->SuivSPere!=NULL) NdCour=NdCour->SuivSPere;
        else NdCour=NdCour->SuivSFils;
      }
  }
  return NdPrec;
} /* Fonction AvtDernNoeudAxe */
/****************************************************************************/
/****************************************************************************/
void AjouteNoeudTermAxe(PTAxe Axe, PTNoeud NdAAjouter)
/* Cette fonction ajoute un noeud en position terminale (apicale)
? l'axe concern?, et incr?mente son compteur de noeuds */
{
PTNoeud AncienNdTerm;

  AncienNdTerm=Axe->DernNoeud;

  AncienNdTerm->SuivSFils=NdAAjouter;

  NdAAjouter->Prec=AncienNdTerm;
  NdAAjouter->Pere=NULL;
  Axe->DernNoeud=NdAAjouter;
  Axe->NbNoeud++;

} /* Fonction AjouteNoeudTermAxe */
/****************************************************************************/
/****************************************************************************/
void AjouteNoeudLatAxe(PTAxe Axe, PTNoeud NdAAjouter)
/* Cette fonction ajoute un noeud en position lat?rale (lors d'une ramification
ou d'une r?it?ration) de l'axe concern?, et incr?mente son compteur de noeuds */
{
PTNoeud NdPrec;

  NdPrec=Axe->DernNoeud->Prec;
  NdAAjouter->Prec=NdPrec;
  NdAAjouter->SuivSPere=Axe->DernNoeud;
  NdAAjouter->SuivSFils=NULL;
  NdAAjouter->Pere=Axe;
  Axe->NbNoeud++;

  /* Reaffecter la succession du precedent */
  /* Si le pr?c?dent noeud ?tait un noeud de branchement */
  if (NdPrec->SuivSPere == NdAAjouter->SuivSPere) NdPrec->SuivSPere=NdAAjouter;

  /* Si le pr?c?dent noeud n'?tait pas un noeud de branchement */
  if (NdPrec->SuivSFils == NdAAjouter->SuivSPere) NdPrec->SuivSFils=NdAAjouter;

  Axe->DernNoeud->Prec=NdAAjouter;

 } /* Fonction AjouteNoeudLatAxe */
/****************************************************************************/
/****************************************************************************/
PTNoeud DernNoeudAxe(PTAxe Axe)
/* Cette fonction retourne une variable de type PTNoeud,
  qui pointe sur le dernier noeud de l'axe, le plus distal */
{
PTNoeud NdCour;
  NdCour=Axe->PremNoeud;
  if (NdCour->SuivSFils!=NULL)
  {
  NdCour=NdCour->SuivSFils;
  while ((NdCour->SuivSFils!=NULL)||(NdCour->SuivSPere!=NULL))
    {
    if (NdCour->SuivSPere!=NULL) NdCour=NdCour->SuivSPere;
      else NdCour=NdCour->SuivSFils;
    }
  }
  return NdCour;
} /* Fonction DernNoeudAxe */
/****************************************************************************/
/****************************************************************************/
void DeveloppeAxe(PTAxe Axe, float DeltaT)//CBF
{

    //printf("\nDEVELOPPE AXE ENTER\n");

  DeveloppeMeris(Axe->Meris, DeltaT);//CBF

    //printf("\nDEVELOPPE AXE EXIT\n");

} /* Fonction DeveloppeAxe */
/****************************************************************************/
/****************************************************************************/
void AllongeAxe(PTAxe Axe, float DeltaT)//CBF
{
const double LongSeuilCroiss=1.0e-2;
double Elongation;
PTNoeud NouvNd;

    //printf("\nALLONGE AXE ENTER\n");
  //printf("\n Axe->Meris->Senile=%d \n", Axe->Meris->Senile);
  //printf("\n Axe->Meris->Mature=%d \n", Axe->Meris->Mature);

  if ((!Axe->Meris->Senile) && (Axe->Meris->Mature))
  {

    Elongation=CalcElongationMeris(Axe->Meris, DeltaT);//CBF
    if (Elongation<LongSeuilCroiss) { SenesceMeris(Axe->Meris); }



    else
    {
      /* Calcule et affecte la nouvelle direction de croissance du m?rist?me */
      ReorienteMeris(Axe->Meris,Elongation);

      /* Le m?rist?me se d?place */
      DeplaceMeris(Axe->Meris,Elongation);

      /* Il g?n?re un nouveau noeud sur cet axe ? sa nouvelle position */
      IncreNbNoeudSR(SR);

      NouvNd=InitialiseNoeud(SR->NbNoeudForm,Axe->Meris->Coord,Axe->Meris->Diametre,NULL,Axe);

      AjouteNoeudTermAxe(Axe,NouvNd);

    }
  }


} /* Fonction AllongeAxe */
/****************************************************************************/
/****************************************************************************/
void DetruitAxe(PTAxe AxeADetruire)
/* Supprime un axe en supprimant ses noeuds, puis l'axe lui-m?me */
{
PTNoeud NdCour, NdAEnlever;

  /* Liberer tous les noeuds de cet axe */
  NdCour=AxeADetruire->PremNoeud;
  while (NdCour->SuivSFils!=NULL)
  {
    NdAEnlever=NdCour;
    NdCour=NdCour->SuivSFils;
    if (NdCour->SuivSPere!=NULL) { printf("Probl?me : Axe ramifie a enlever\n"); exit(1); }
    DetruitNoeud(NdAEnlever);
  }
  DetruitNoeud(NdCour); /* Enleve le noeud apical */

  DetruitMeris(AxeADetruire->Meris);

  /* Enlever l'axe en m?moire */
  free(AxeADetruire);

} /* Fonction DetruitAxe */
/****************************************************************************/
/****************************************************************************/
int AxeToutNecrose(PTAxe Axe)
/* Cette fonction retourne la valeur 1 si l'axe
   a tous ses noeuds necroses et 0 sinon */
{
PTNoeud NdCour;
int Resu;

  NdCour=Axe->PremNoeud;
  if (NdCour->SuivSFils==NULL) { Resu=NdCour->Necrose; }   /* Un seul noeud */
  else
  {
    NdCour=NdCour->SuivSFils;
    Resu=NdCour->Necrose;
  }
  return Resu;
} /* Fonction AxeToutNecrose */
/****************************************************************************/
/****************************************************************************/
int AxeToutNecroseAncien(PTAxe Axe)
/* Cette fonction retourne la valeur 1 si l'axe
   a tous ses noeuds necroses et 0 sinon */
{
PTNoeud NdCour;
int Resu=1;   /* Initialisation a la valeur OUI */

  NdCour=Axe->PremNoeud;
  if (NdCour->SuivSFils==NULL) { Resu=NdCour->Necrose; }
  else
  {
  NdCour=NdCour->SuivSFils;
  if (NdCour->Necrose==0) Resu=0;
  while ((NdCour->SuivSFils!=NULL)||(NdCour->SuivSPere!=NULL))
    {
    if (NdCour->SuivSPere!=NULL) NdCour=NdCour->SuivSPere;
      else NdCour=NdCour->SuivSFils;
    if (NdCour->Necrose==0) Resu=0;
    }
  }
  return Resu;
} /* Fonction AxeToutNecroseAncien */
/****************************************************************************/
/****************************************************************************/
void AffecValNecroseAxe(PTAxe Axe, int ValNecrose)
/* Cette fonction affecte a chacun des noeuds de l'axe
   la valeur de necrose (0 ou 1) */
{
PTNoeud NdCour;

  NdCour=Axe->PremNoeud;
  NdCour->Necrose=ValNecrose;
  if (NdCour->SuivSFils!=NULL)
  {
  NdCour=NdCour->SuivSFils;
  NdCour->Necrose=ValNecrose;
  while ((NdCour->SuivSFils!=NULL)||(NdCour->SuivSPere!=NULL))
    {
    if (NdCour->SuivSPere!=NULL) NdCour=NdCour->SuivSPere;
      else NdCour=NdCour->SuivSFils;
    NdCour->Necrose=ValNecrose;
    }
  }
} /* Fonction AffecValNecroseAxe */
/****************************************************************************/
/****************************************************************************/
void AffecValNecroseAmont(PTAxe Axe, int ValNecrose)
/* Cette fonction affecte a chacun des noeuds en amont de l'axe
la valeur de necrose (0 ou 1) */
{
PTNoeud NdCour;

  NdCour=Axe->PremNoeud->Prec;
  while (NdCour!=NULL)
  {
    NdCour->Necrose=ValNecrose;
    NdCour=NdCour->Prec;
  }

} /* Fonction AffecValNecroseAmont */
/****************************************************************************/
/****************************************************************************/
void AffecValDiamAxe(PTAxe Axe, float Diam)
/* Cette fonction affecte a chacun des noeuds de l'axe
la valeur de diametre Diam */
{
PTNoeud NdCour;
  NdCour=Axe->PremNoeud;
  NdCour->Diametre=Diam;
  if (NdCour->SuivSFils!=NULL)
  {
  NdCour=NdCour->SuivSFils;
  NdCour->Diametre=Diam;
  while ((NdCour->SuivSFils!=NULL)||(NdCour->SuivSPere!=NULL))
    {
    if (NdCour->SuivSPere!=NULL) NdCour=NdCour->SuivSPere;
      else NdCour=NdCour->SuivSFils;
    NdCour->Diametre=Diam;
    }
  }
} /* Fonction AffecValDiamAxe */
/****************************************************************************/
/****************************************************************************/
void IncreValDiamAmont(PTAxe Axe, double Diam, double Coeff)
/* Cette fonction incremente le diametre de chacun des noeuds en amont
de l'axe, en incluant son premier noeud */
{
PTNoeud NdCour;
double Section,DiamInit;

  NdCour=Axe->PremNoeud;
  while (NdCour!=NULL)
  {
    DiamInit=NdCour->Diametre;
    Section=(Pi*DiamInit*DiamInit/4.0)+(Pi*Coeff*Diam*Diam/4.0);
    NdCour->Diametre=sqrt(4.0*Section/Pi);
    NdCour=NdCour->Prec;
  }

} /* Fonction IncreValDiamAmont */
/****************************************************************************/
/****************************************************************************/
PTSysRac CreeSR(void)
/* Cette fonction retourne une nouvelle variable de type PTSysRac,
  c'est-?-dire un pointeur sur le type TSysRac */
{
PTSysRac SR;
  SR=(PTSysRac) malloc(sizeof(TSysRac));
  if (SR==NULL)
  { printf("Probl?me m?moire allocation dans CreeSR \n"); exit(1); }

return SR;
} /* Fonction CreeSR */
/****************************************************************************/
/****************************************************************************/
void AjouteAxeSR(PTSysRac SR, PTAxe AxeAAjouter)
/* Cette fonction ins?re un axe dans la cha?ne des axes du syst?me racinaire,
elle incr?mente en m?me temps le compteur d'axes et de noeuds */
{
  if ((SR->NbAxeForm - SR->NbAxeSup)==0)  /* Le syst?me racinaire est vide */
  {
    AxeAAjouter->Suivant=NULL;
    AxeAAjouter->Precedent=NULL;
    SR->PremAxe=AxeAAjouter;
    SR->DernAxe=AxeAAjouter;
  }
  else /* Le syst?me contient d?j? des axes, cha?nage double */
  {
    AxeAAjouter->Suivant=NULL;
    AxeAAjouter->Precedent=SR->DernAxe;
    SR->DernAxe->Suivant=AxeAAjouter;
    SR->DernAxe=AxeAAjouter;
  }
  SR->NbAxeForm++;

  
  IncreNbNoeudSR(SR);

  

} /* Fonction AjouteAxeSR */
/****************************************************************************/
/****************************************************************************/
void EnleveAxeSR(PTSysRac SR, PTAxe AxeAEnlever)
/* Cette fonction enl?ve un axe dans la cha?ne des axes et lib?re la memoire */
{
PTNoeud PrecSPere;

if ((SR->NbAxeForm - SR->NbAxeSup)==0)  /* Le syst?me racinaire est vide */
  {
    printf("ATTENTION, probleme dans EnleveAxeSR, SR vide \n");
    exit(1);
  }
else
  {
    SR->NbAxeSup++;

    /* Refaire les chainages dans la liste */
    AxeAEnlever->Precedent->Suivant=AxeAEnlever->Suivant;
    AxeAEnlever->Suivant->Precedent=AxeAEnlever->Precedent;

    /* Refaire les connexions, brancher le noeud precedent sur Pere, au noeud suivant sur Pere */
    if (AxeAEnlever->Pere!=NULL)
    {
      PrecSPere=AxeAEnlever->PremNoeud->Prec;
      if (PrecSPere->SuivSPere!=NULL) /* le precedent est un noeud de branchement */
      {
        if (PrecSPere->Pere==AxeAEnlever->Pere) /* le precedent est une ramif du meme axe */
          PrecSPere->SuivSPere=AxeAEnlever->PremNoeud->SuivSPere;
        else /* le precedent est branche sur un autre axe */
          PrecSPere->SuivSFils=AxeAEnlever->PremNoeud->SuivSPere;
      }
      else PrecSPere->SuivSFils=AxeAEnlever->PremNoeud->SuivSPere;  /* precedent n'est pas noeud branchement */

      AxeAEnlever->PremNoeud->SuivSPere->Prec=PrecSPere;
    }

    DetruitAxe(AxeAEnlever);  /* Detruit ses noeuds, son m?rist?me, et lui-m?me */
  }
} /* Fonction EnleveAxeSR */
/****************************************************************************/
/****************************************************************************/
PTSysRac InitialiseSR(r3 Origine)
{
/* Initialisation du syst?me racinaire */

PTSysRac SR;

  SR=CreeSR();  /* Cr?ation d'un syst?me racinaire */

  SR->NbAxeForm=0;  /* Initialisation des variables */
  SR->NbAxeSup=0;
  SR->NbNoeudForm=0;
  SR->PremAxe=NULL;
  SR->DernAxe=NULL;
  SR->NumVReitCourante=0;
  SR->ReitPossible=0;

  SR->Origine[0]=Origine[0];  /* Origine du syst?me racinaire */
  SR->Origine[1]=Origine[1];
  SR->Origine[2]=Origine[2];

  SR->AngDep=2.0*Pi*ddRandUnif();  /* Orientation */


  return(SR);
}  /* Fonction InitialiseSR */
/****************************************************************************/
/****************************************************************************/
void InstalleSR(PTSysRac SR)
{
/* Installation du syst?me racinaire, c'est ? dire ?mission des premiers axes
de type 0 */

  PTAxe NouvAxe;
  int NumAxeOG;
  r3 VInit, DirInit;
  double AngRot,AngI;

  for ((NumAxeOG=1); (NumAxeOG<=P_NbAxeOG); (NumAxeOG++)) /* Pour tous axes de l'OG */
  {
    /* Calcul de la direction initiale de l'axe */
    AngI=TireAngI(0);
    VInit[0]=sin(AngI);
    VInit[1]=0.0;
    VInit[2]=cos(AngI);
    AngRot=SR->AngDep+(2*Pi*NumAxeOG/P_NbAxeOG);
    RotZ(VInit,DirInit,AngRot);

    /* G?n?ration de l'axe et int?gration dans le syst?me racinaire */
    NouvAxe=InitialiseAxe(SR->NbAxeForm+1,0,SR->Origine,DirInit,NULL);
    AjouteAxeSR(SR,NouvAxe);


  }

  }  /* Fonction InstalleSR */
/****************************************************************************/
/****************************************************************************/
void EtatReiterationSR(PTSysRac SR)
{
/* Definit l'?tat du syst?me racinaire en terme de r?it?ration */

    //printf("\n ETAT REITERATION ENTER\n");

  if (Temps>=P_TpsReitVag[SR->NumVReitCourante+1])
  {
    SR->NumVReitCourante++;
    SR->ReitPossible=1;
  }
  else
  {
    SR->ReitPossible=0;
  }

    //    printf("\n ETAT REITERATION EXIT\n");

}  /* Fonction EtatReiterationSR */
/****************************************************************************/
/****************************************************************************/
void LitParam(double xmax, double ymax, double zmax)

/* Fonction de lecture des parametres de la simulation */
{
int i,typ;
char bid[MAXLINE];

  for (i=1; i<=4; i++) { fgets(bid,MAXLINE-1,FFPar); }

 // Seed Position X
  fscanf(FFPar,"%lf",&Orig[1]);// CBF reverse X (Orig[0]) and Y (Orig[1]) to be in agreement with ArchiSimple axis
  fgets(bid,MAXLINE-1,FFPar); 

  // Translate MIN3P frame for seed position in Root Typ frame :
  //printf("\n Orig[0]=%lf ", Orig[0]);    
  Orig[1]=100*(Orig[1]-(ymax/2));// *100 to swich from meters (MIN3P) to centimeters (RootTyp)
  //printf("\n Orig[0]=%lf \n", Orig[0]);

  // Seed position Y
  fscanf(FFPar,"%lf",&Orig[0]);// CBF reverse X (Orig[0]) and Y (Orig[1]) to be in agreement with ArchiSimple axis
  fgets(bid,MAXLINE-1,FFPar); 

  // Translate MIN3P frame for seed position in Root Typ frame :
  Orig[0]=100*(Orig[0]-(xmax/2));// *100 to swich from meters (MIN3P) to centimeters (RootTyp)    


  // Seed position Z
  fscanf(FFPar,"%lf",&Orig[2]);
  fgets(bid,MAXLINE-1,FFPar); 

  // Translate MIN3P frame for seed position in Root Typ frame :   
  Orig[2]=100*(zmax-Orig[2]);// *100 to swich from meters (MIN3P) to centimeters (RootTyp)

   //printf("\n Orig[0]=%lf \n", Orig[0]);
   //printf("\n Orig[1]=%lf \n", Orig[1]);
   //printf("\n Orig[2]=%lf \n", Orig[2]);

   // CBF removed because the simulation time is obtained from MIN3P :
 //Duree de simulation, en pas (entier) sachant que 1 pas de calcul = 2 jours(ici):
  //for (i=1; i<=4; i++) { fgets(bid,MAXLINE-1,FFPar); }
  //fscanf(FFPar,"%d",&P_Duree);

  //fgets(bid,MAXLINE-1,FFPar);
  fscanf(FFPar,"%d",&P_NbAxeOG);
  // printf("\n &P_NbAxeOG= %d ", P_NbAxeOG);
  fgets(bid,MAXLINE-1,FFPar);
  fscanf(FFPar,"%d",&P_NbVag);
  // printf("\n &P_NbVag= %d ", P_NbVag);
  fgets(bid,MAXLINE-1,FFPar);
  for (i=1; (i<=P_NbVag); i++) { fscanf(FFPar,"%d",&P_TpsReitVag[i]); }
  //printf("\n &P_TpsReitVag[i]= %d ", P_TpsReitVag[i]);
  fgets(bid,MAXLINE-1,FFPar);
  fscanf(FFPar,"%f",&P_CoeffCroissRad);
 //printf("\n &P_CoeffCroissRad= %f ", P_CoeffCroissRad);
  fgets(bid,MAXLINE-1,FFPar);


  for (typ=0; typ<NBTYPMAX; typ++)
  {
  //  printf("\n TYP = %d ", typ);
    fgets(bid,MAXLINE-1,FFPar);
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%f %f",&P_AngIMoy[typ],&P_AngIEt[typ]);
  //printf("\n %f %f ",P_AngIMoy[typ],P_AngIEt[typ]);
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%f %f",&P_AngIReitMoy[typ],&P_AngIReitEt[typ]);
  //printf("\n %f %f", P_AngIReitMoy[typ],P_AngIReitEt[typ]);
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%f",&P_DurDevPrim[typ]);
 //printf("\n %f", P_DurDevPrim[typ]);
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%f %f",&P_CroissMoy[0][typ],&P_CroissMoy[1][typ]);
 //printf("\n %f %f", P_CroissMoy[0][typ],P_CroissMoy[1][typ]);
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%f %f",&P_CroissEt[0][typ],&P_CroissEt[1][typ]);
 //printf("\n %f %f", P_CroissEt[0][typ],P_CroissEt[1][typ]);
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%f %f",&P_RamifMoy[typ],&P_RamifEt[typ]);
 //printf("\n %f %f ", P_RamifMoy[typ],P_RamifEt[typ]);
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%d",&P_TTrop[typ]);
 //printf("\n %d ", P_TTrop[typ]);
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%f",&P_ITrop[typ]);
 //printf("\n %f ", P_ITrop[typ]);
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%f",&P_SCMeca[typ]);
 //printf("\n %f ", P_SCMeca[typ]);
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%f",&P_DiamPrim[typ]);
 //printf("\n %f ", P_DiamPrim[typ]); 
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%f",&P_DureeNecrose[typ]);
 //printf("\n %f ", P_DureeNecrose[typ]); 
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%f",&P_ProbReit[typ]);
 //printf("\n %f ", P_ProbReit[typ]); 
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%d %d",&P_NbReitMin[typ],&P_NbReitMax[typ]);
 //printf("\n %d %d ",P_NbReitMin[typ],P_NbReitMax[typ]);
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%f",&P_AgeTransf[typ]);
 //printf("\n %f ", P_AgeTransf[typ]);
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%f",&P_ProbTransf[typ]);
 //printf("\n %f ", P_ProbTransf[typ]);
    fgets(bid,MAXLINE-1,FFPar);
    fscanf(FFPar,"%d",&P_SensTransf[typ]);
 //printf("\n %d ", P_SensTransf[typ]);
    fgets(bid,MAXLINE-1,FFPar);
    for (i=0; (i<NBTYPMAX); i++) { fscanf(FFPar,"%f",&P_PropTypRamif[typ][i]); }
 //printf("\n %f ",P_PropTypRamif[typ][i]);

    fgets(bid,MAXLINE-1,FFPar);
  }

} /* Fonction LitParam */
/****************************************************************************/
/****************************************************************************/
int AxeRamifiable(PTAxe Axe)
{   /* Renvoie 1 ou 0 suivant que l'axe est ramifiable ou non */

return(Axe->Meris->DistPrimInit>DistInterRamifMeris(Axe->Meris, Sol));

} /* Fonction AxeRamifiable */
/****************************************************************************/
/****************************************************************************/
int TireTypeFils(PTAxe AxePere)
{   /* Tire le type de la ramification suivant proportions */
int CompteType;
double tirage,PropCum;

tirage=ddRandUnif();
PropCum=0.0;
CompteType=0;

while ((tirage>PropCum) && (CompteType<NBTYPMAX))
{
  CompteType++;
  PropCum=PropCum+P_PropTypRamif[AxePere->Meris->Type][CompteType-1];
}
if ((CompteType-1<0)||(CompteType>NBTYPMAX))
{
  printf("Probleme dans TireTypeFils\n");
  exit(1);
}
return(CompteType-1);

} /* Fonction TireTypeFils */
/****************************************************************************/
/****************************************************************************/
void OrigineRamif(PTAxe AxePere, r3 OrigineFils)
{   /* Calcule la position du point d'origine d'une ramification */
OrigineFils[0]=AxePere->Meris->Coord[0]-
                  (AxePere->Meris->DistPrimInit*AxePere->Meris->DirCroiss[0]);
OrigineFils[1]=AxePere->Meris->Coord[1]-
                  (AxePere->Meris->DistPrimInit*AxePere->Meris->DirCroiss[1]);
OrigineFils[2]=AxePere->Meris->Coord[2]-
                  (AxePere->Meris->DistPrimInit*AxePere->Meris->DirCroiss[2]);
} /* Fonction OrigineRamif */
/****************************************************************************/
/****************************************************************************/
void OrienteRamif(PTAxe AxePere, int TypeFils, r3 DirFils)
{   /* Calcule la direction d'une Axe Fils */
r3 VAxeRot,RotDirCroiss;
double NorVProjHor,AngRot;

/* Calcul de la norme de la projection direction sur plan horizontal */
NorVProjHor=sqrt((AxePere->Meris->DirCroiss[0]*AxePere->Meris->DirCroiss[0])+
                 (AxePere->Meris->DirCroiss[1]*AxePere->Meris->DirCroiss[1]));
if (NorVProjHor<Epsilon)
{
  VAxeRot[0]=1.0; /* Vecteur initial vertical */
  VAxeRot[1]=0.0;
  VAxeRot[2]=0.0; /* Vecteur (1,0,0) choisi pour axe de rotation */
}
else
{
  VAxeRot[0]=AxePere->Meris->DirCroiss[1]/NorVProjHor;
  VAxeRot[1]=-AxePere->Meris->DirCroiss[0]/NorVProjHor;
  VAxeRot[2]=0.0;
}
/* On fait tourner DirCroiss autour de VAxeRot d'un angle d'insertion */
AngRot=TireAngI(TypeFils);
RotVect(AngRot,VAxeRot,AxePere->Meris->DirCroiss,RotDirCroiss);

/* On fait tourner RotDirCroiss autour de DirCroiss d'un angle radial */
AngRot=TireAngRad();
RotVect(AngRot,AxePere->Meris->DirCroiss,RotDirCroiss,DirFils);
} /* Fonction OrienteRamif */
/****************************************************************************/
/****************************************************************************/
void RamifieAxe(PTAxe AxePere)
{
PTAxe NouvAxe;
int TypeRamif;
r3 OrigRamif, DirRamif;

  /* D?cremente la distance au dernier primordium initi? */
  AxePere->Meris->DistPrimInit=(AxePere->Meris->DistPrimInit)
                               -DistInterRamifMeris(AxePere->Meris,Sol);

  /* Calcul de attributs d'une ramification */
  TypeRamif=TireTypeFils(AxePere);             /* Le type de son m?rist?me */
  OrigineRamif(AxePere,OrigRamif);             /* Sa position */
  OrienteRamif(AxePere,TypeRamif,DirRamif);    /* Sa direction */

  NouvAxe=InitialiseAxe(SR->NbAxeForm+1,TypeRamif,OrigRamif,DirRamif,AxePere);

  AjouteAxeSR(SR,NouvAxe);

  AjouteNoeudLatAxe(AxePere,NouvAxe->PremNoeud);

} /* Fonction RamifieAxe */
/****************************************************************************/
/****************************************************************************/
void OrigineReit(PTAxe AxePere, r3 OrigineFils)
{
/* Calcule le point d'origine d'une reiteration */
OrigineFils[0]=AxePere->Meris->Coord[0];
OrigineFils[1]=AxePere->Meris->Coord[1];
OrigineFils[2]=AxePere->Meris->Coord[2];
} /* Fonction OrigineReit */
/****************************************************************************/
/****************************************************************************/
void OrienteReit(PTAxe AxePere, int TypeFils, r3 DirFils)
{
/* Calcule la direction d'une reiteration */
r3 VAxeRot,RotDirCroiss;
double NorVProjHor,AngRot;

/* Calcul de la norme de la projection direction sur plan horizontal */
NorVProjHor=sqrt((AxePere->Meris->DirCroiss[0]*AxePere->Meris->DirCroiss[0])+
                 (AxePere->Meris->DirCroiss[1]*AxePere->Meris->DirCroiss[1]));
if (NorVProjHor<Epsilon)
{
  VAxeRot[0]=1.0; /* Vecteur initial vertical */
  VAxeRot[1]=0.0;
  VAxeRot[2]=0.0; /* Vecteur (1,0,0) choisi pour axe de rotation */
}
else
{
  VAxeRot[0]=AxePere->Meris->DirCroiss[1]/NorVProjHor;
  VAxeRot[1]=-AxePere->Meris->DirCroiss[0]/NorVProjHor;
  VAxeRot[2]=0.0;
}

/* On fait tourner DirCroiss autour de VAxeRot d'un angle d'insertion */
AngRot=TireAngIReit(TypeFils);
RotVect(AngRot,VAxeRot,AxePere->Meris->DirCroiss,RotDirCroiss);

/* On fait tourner RotDirCroiss autour de DirCroiss d'un angle generatrice */
AngRot=TireAngRad();
RotVect(AngRot,AxePere->Meris->DirCroiss,RotDirCroiss,DirFils);
} /* Fonction OrienteReit */
/****************************************************************************/
/****************************************************************************/
void ReitereAxe(PTAxe AxePere)
{
PTAxe NouvAxe;
int NumReit, NbReit;
r3 Origine, DirReit;

  if ((AxePere->Meris->Mature)&&(!AxePere->Meris->Senile)&&
                           (ddRandUnif()<P_ProbReit[AxePere->Meris->Type]))
  {
    NbReit=IRandUnif(P_NbReitMax[AxePere->Meris->Type]-P_NbReitMin[AxePere->Meris->Type])
                      +P_NbReitMin[AxePere->Meris->Type];
    for (NumReit=1; NumReit<=NbReit; NumReit++)
    {
      OrigineReit(AxePere,Origine);
      OrienteReit(AxePere,AxePere->Meris->Type,DirReit);
      NouvAxe=InitialiseAxe(SR->NbAxeForm+1,AxePere->Meris->Type,Origine,DirReit,AxePere);
      AjouteAxeSR(SR,NouvAxe);
      AjouteNoeudLatAxe(AxePere,NouvAxe->PremNoeud);
    }
    AxePere->Meris->DistPrimInit=0.0;
    SenesceMeris(AxePere->Meris);
  }

} /* Fonction ReitereAxe */
/****************************************************************************/
/****************************************************************************/
void OrigineEmission(PTAxe NouvAxe)
{
NouvAxe->Meris->Coord[0]=SR->Origine[0];
NouvAxe->Meris->Coord[1]=SR->Origine[1];
NouvAxe->Meris->Coord[2]=SR->Origine[2];
} /* Fonction OrigineEmission */
/****************************************************************************/
/****************************************************************************/
void OrienteEmission(PTAxe NouvAxe, int Num)
{
double AngRot,AngI;
r3 VInit;

AngI=TireAngI(NouvAxe->Meris->Type);
VInit[0]=sin(AngI);
VInit[1]=0.0;
VInit[2]=cos(AngI);

AngRot=SR->AngDep+(2*Pi*Num/P_NbAxeOG);
RotZ(VInit,NouvAxe->Meris->DirCroiss,AngRot);
} /* Fonction OrienteEmission */
/****************************************************************************/
/****************************************************************************/
void DeveloppeSR(PTSysRac SR, float DeltaT) //CBF
{
/* D?veloppement, croissance, ramification et r?it?ration de chaque axe */
PTAxe AxeCour;

  AxeCour=SR->PremAxe;
  while (AxeCour!=NULL)
  {
    DeveloppeAxe(AxeCour, DeltaT);
    AllongeAxe(AxeCour, DeltaT);
    while (AxeRamifiable(AxeCour)) RamifieAxe(AxeCour);
    if (SR->ReitPossible) ReitereAxe(AxeCour);
    AxeCour=AxeCour->Suivant;
  }

}  /* Fonction DeveloppeSR */
/****************************************************************************/
/****************************************************************************/
void MortaliteSR(PTSysRac SR)
{
PTAxe AxeCour, AxeAEnlever;
int Necrose;

  /* Calcul de la mortalite sur l'ensemble des Axes */
  AxeCour=SR->DernAxe;
  while (AxeCour!=NULL)
  {
    if ((AxeCour->Meris->Senile)&&
        (Temps-(DernNoeudAxe(AxeCour)->JourForm))>P_DureeNecrose[AxeCour->Meris->Type])
    { /* L'axe est necrose */
      Necrose=1;
      AffecValNecroseAxe(AxeCour, Necrose);
    }
    else
    {  /* L'axe concerne n'est pas necrose */
      Necrose=0;
      AffecValNecroseAxe(AxeCour, Necrose);
      /* Et les noeuds en amont ne sont pas necroses non plus */
      AffecValNecroseAmont(AxeCour, Necrose);
    }

    AxeCour=AxeCour->Precedent;
  }

  /* Deuxieme passage pour re-specifier l'ensemble non necrose */
  AxeCour=SR->DernAxe;
  while (AxeCour!=NULL)
  {
    if ((AxeCour->Meris->Senile)&&
        (Temps-(DernNoeudAxe(AxeCour)->JourForm))>P_DureeNecrose[AxeCour->Meris->Type])
    { /* L'axe est necrose */
    }
    else
    {  /* L'axe concerne n'est pas necrose */
      Necrose=0;
      AffecValNecroseAxe(AxeCour, Necrose);
      /* Et les noeuds en amont ne sont pas necroses non plus */
      AffecValNecroseAmont(AxeCour, Necrose);
    }

    AxeCour=AxeCour->Precedent;
  }

  /* Calcul de l'elagage, enlevement des axes necroses */
  AxeCour=SR->DernAxe;
  while (AxeCour!=NULL)
  {
    if (AxeToutNecrose(AxeCour))
    {
      AxeAEnlever=AxeCour;
      AxeCour=AxeCour->Precedent;
      EnleveAxeSR(SR,AxeAEnlever);
    }
    else AxeCour=AxeCour->Precedent;
  }

}  /* Fonction MortaliteSR */
/****************************************************************************/
/****************************************************************************/
void CroissanceRadialeSR(PTSysRac SR, float CoeffCroiss)
{
PTAxe AxeCour;
float Diam;

    //printf("\n CROISSANCE RADIALE SR \n");

  /* Premier passage, initialisation aux diametres primaires */
  AxeCour=SR->DernAxe;
  while (AxeCour!=NULL)
  {
    Diam=P_DiamPrim[AxeCour->Meris->Type];
    AffecValDiamAxe(AxeCour, Diam);
    AxeCour=AxeCour->Precedent;
  }

  /* Deuxieme passage, avec increment des diametres */
  AxeCour=SR->DernAxe;
  while (AxeCour!=NULL)
  {
    /* les noeuds en amont sont incrementes */
    Diam=P_DiamPrim[AxeCour->Meris->Type];
    IncreValDiamAmont(AxeCour, Diam, CoeffCroiss);
    AxeCour=AxeCour->Precedent;
  }

}  /* Fonction CroissanceRadialeSR */
/****************************************************************************/
/****************************************************************************/
void ImprimeNd(PTNoeud Nd, long int NumAxe)
{
long int SuivSF,SuivSP,Pere;

  if (Nd->SuivSFils==NULL) SuivSF=0;
  else SuivSF=Nd->SuivSFils->Num;

  if (Nd->SuivSPere==NULL) SuivSP=0;
  else SuivSP=Nd->SuivSPere->Num;

  if (Nd->Pere==NULL) Pere=-9;
  else Pere=Nd->Pere->Num;


  //// CBF removed for coupling to MIN3P :
  //fprintf(FNoeud,"%5li %5i %2i %2i %5li %5li %5li %5li %7.2f %lf %lf %lf\n",
  //       Nd->Num,Nd->JourForm,Nd->Fils->Meris->Type,Nd->Necrose,NumAxe,SuivSF,SuivSP,Pere,
  //       Nd->Diametre,Nd->Pos[0],Nd->Pos[1],Nd->Pos[2]);

 
       
}  /* Fonction ImprimeNd */
/****************************************************************************/
/****************************************************************************/
// CBF removed for coupling to MIN3P :
//void ImprimeMeris(PTAxe Axe)
//{
//  fprintf(FMeris,"%5li %5i %5i %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f\n",
//         Axe->Num,Axe->Meris->Type,Axe->PremNoeud->JourForm,
//         Axe->Meris->Coord[0],Axe->Meris->Coord[1],Axe->Meris->Coord[2],
//         Axe->Meris->DirCroiss[0],Axe->Meris->DirCroiss[1],Axe->Meris->DirCroiss[2],
//                 Axe->Meris->PCroiss[0],Axe->Meris->PCroiss[1]);      
//}  /* Fonction ImprimeMeris */
/****************************************************************************/
/****************************************************************************/
// CBF removed for coupling to MIN3P :
//void ImprimeEnteteSR(void)
//{   /* Imprime l'ent?te du fichier contenant le syst?me racinaire */
//  fprintf(FNoeud,"NumNd Jour Type Nec NumAxe SuivSF SuivSP Pere Diam     X       Y       Z\n");
//}  /* Fonction ImprimeEnteteSR */
/****************************************************************************/
/****************************************************************************/
// CBF removed for coupling to MIN3P :
//void ImprimeEnteteMerisSR(void)
//{   /* Imprime l'ent?te du fichier contenant les m?rist?mes du syst?me racinaire */
//  fprintf(FMeris,"NumAxe Type  Jour    posX    posY    posZ    dirX    dirY    dirZ  crois0  crois1\n");
//}  /* Fonction ImprimeEnteteMerisSR */
/****************************************************************************/
/****************************************************************************/
void ImprimeAvecRamifAxe(PTAxe Axe) 
{   /* Imprime les noeuds d'un axe en incluant les noeuds de branchement */

PTNoeud NdCour;


  NdCour=Axe->PremNoeud;



  ImprimeNd(NdCour,Axe->Num);
  

  if (NdCour->SuivSFils!=NULL)
  {
    NdCour=NdCour->SuivSFils;
    

    ImprimeNd(NdCour,Axe->Num);
    while ((NdCour->SuivSFils!=NULL)||(NdCour->SuivSPere!=NULL))
    {
      if (NdCour->SuivSPere!=NULL) NdCour=NdCour->SuivSPere;
        else NdCour=NdCour->SuivSFils;

      ImprimeNd(NdCour,Axe->Num);
    }
  }

    

}  /* Fonction ImprimeAvecRamifAxe */
/****************************************************************************/
/****************************************************************************/



//*********************************************************************************************
//*********************************************************************************************
//CBF removed for coupling TO MIN3P :
//void ImprimeAvecRamifSR(PTSysRac SR)

//{   /* Imprime le syst?me racinaire avec les noeuds de ramification */

//PTAxe AxeCour;

//  ImprimeEnteteSR();

//  AxeCour=SR->PremAxe;
//  while (AxeCour!=NULL)
//  {
//    ImprimeAvecRamifAxe(AxeCour);
//    AxeCour=AxeCour->Suivant;
//  }
//}  /* Fonction ImprimeAvecRamifSR */
/****************************************************************************/
/****************************************************************************/
void ImprNd(PTNoeud Nd, long int NumAxe, float *x, float *y, float *z, int nvx, int nvz, \
            double *RSD,float volume_cube, double xmax, double ymax, double zmax)
{
  
  long int SuivSF,SuivSP,Pere;

  double xx, yy, zz, xS, yS, zS, xM, yM, zM, rs, distance;
  int ivol;
  
  if (Nd->SuivSFils==NULL) SuivSF=0;
  else SuivSF=Nd->SuivSFils->Num;
  
  if (Nd->SuivSPere==NULL) SuivSP=0;
  else SuivSP=Nd->SuivSPere->Num;

  if (Nd->Pere==NULL) Pere=-9;
  else Pere=Nd->Pere->Num;

  // Noeud courant : 
    xx=Nd->Pos[0]; // On ne peux pas faire d'operation sur Nd->Pos[] donc on le mets dans une variable type double xx, yy, zz
    yy=Nd->Pos[1];
    zz=Nd->Pos[2];
    
    xS=0;
    yS=0;
    zS=0;
 
 // Definition du point suivant (sur pere ou sur fils) qui forme le segment avec les noeud courant xx,yy,zz : 
    if(SuivSF>0)
    {    
      xS=Nd->SuivSFils->Pos[0]; // On ne peux pas faire d'operation sur Nd->SuivSFils->Pos[] donc on le mets dans une variable type double xx, yy, zz
      yS=Nd->SuivSFils->Pos[1];
      zS=Nd->SuivSFils->Pos[2];
    }    
    
    if(SuivSP>0)
    {    
      xS=Nd->SuivSPere->Pos[0]; // On ne peux pas faire d'operation sur Nd->SuivSFils->Pos[] donc on le mets dans une variable type double xx, yy, zz
      yS=Nd->SuivSPere->Pos[1];
      zS=Nd->SuivSPere->Pos[2];
    }    
    
    //printf("\n RT xx yy zz = %lf %lf %lf", xx, yy, zz);
    //printf("\n RT xS yS zS = %lf %lf %lf", xS, yS, zS);
    
    // Calcul du milieu du segment entre le noeud courand (xx,yy,zz) et le noeud suivant (soit sur fils soit sur pere puisqu'on evite les ramifs)
    xM=(xx+xS)/2;
    yM=(yy+yS)/2;
    zM=(zz+zS)/2;
    
    //printf("\n RT xM yM zM = %lf %lf %lf", xM, yM, zM);
    
  // Passage du point milieu du segment en coordonées MIN3P (donc en metres) :     
    xM=(xmax/2)+0.01*xM; // CBF cgt de repere en X : +0.5 -> callage au centre du domaine MIN3P et *0.01 -> passage des cm de Root Typ aux metres de MIN3P
    yM=(ymax/2)+0.01*yM; // CBF cgt de repere en Y : 0.01* -> passage des cm de Root Typ aux metres de MIN3P
    zM=zmax-(0.01*zM); // CBF cgt de repere en Z : 0.01* -> passage des cm de Root Typ aux metres de MIN3P et 1- -> pour mettre la racine dans le bon sens



  // Recherche du control volume IVOL qui englobe le milieu du segment xM,yM,zM et association de la valeur rs au tableau RSD :
  
  if(zM<=1) // on considere que les racines qui poussent ds le sol (pas les type 0 hors sol)
  {
  
    // Calcul de la RSD 'rs' donnée par le segment de longueur 'distance' :
    // On calcul une distance dans le repere Root Typ en centimetres qu'on transcrit ensuite en metres
    
    distance=sqrt(((xx-xS)*(xx-xS))+((yy-yS)*(yy-yS))+((zz-zS)*(zz-zS)));// (cm, echelle Root Typ)
    distance=distance*0.01; // passage en metres
   // printf("\n distance = %lf", distance);


    if(SuivSF>0)
    {    
      aire=Pi*(0.01*Nd->SuivSFils->Diametre+0.01*Nd->Diametre)*(distance/2);// (m2) aire = pi*(longueur coté1 + longueur coté2 )*(hauteur/2)
     // printf("\n aire SF = %lf", aire);
    //  printf("\n volumle_cube = %lf", volume_cube);
    //printf("\n ImprNd SuivSF : Nd->Diametre = %lf \n", Nd->Diametre);
    }    
    
    if(SuivSP>0)
    {    
      // aire : on x les diametres et distance par 0.01 pour le passage de cm de Root Typ à metres de MIN3P
      aire=Pi*(0.01*Nd->SuivSPere->Diametre+0.01*Nd->Diametre)*(distance/2);// (m2) aire = pi*(longueur coté1 + longueur coté2 )*(hauteur/2)
      //printf("\n aire SP = %lf", aire);
    //printf("\n ImprNd SuivSP : Nd->Diametre = %lf \n", Nd->Diametre);

  
    }    
    
    rs=aire/(volume_cube); // m2/m3
   // printf("\nSuivSP = %d - suivSF = %d - aire = %lf ", SuivSP, SuivSF, aire);
    
    
    for(ivol=0;ivol<=(nvx*nvz-1);ivol++) // For each control volume : 
    {

            if(x[ivol]<=yM) // X: Nd->Pos[0]
                if(yM<x[ivol+1]) // X: Nd->Pos[0] 
                    if(z[ivol]<=zM) // Z: Nd->Pos[2]
                         if(zM<z[nvz+1+ivol]) // Z: Nd->Pos[2]
                          // if(xx>=-0.1)
                          //  if(xx<0.1)
                            {
            
                              RSD[ivol+nvz]+=rs;
                         
                          //printf("\n absx = %lf - absy = %lf - absz = %lf", absx, absy, absz);    
                          //printf("\n xxx = %lf - yyy = %lf - zzz = %lf", xxx, yyy, zzz);
                          //printf("\n distance = %lf - aire = %lf - volume_cube = %lf", distance, aire, volume_cube);
                            //printf("\n ImprNd : RSD[%d] = %lf ", ivol+nvz, RSD[ivol+nvz]); 
                      
                    }// tout les if
            }//for
  } //if(zM<=1)
  


  
}  /* Fonction ImprNd */

//********************************************************************************/
/*********************************************************************************/

void ImprimeSansRamifAxe(PTAxe Axe)
{   /* Imprime les noeuds d'un axe sans y inclure les noeuds de branchement */

PTNoeud NdCour;

  NdCour=Axe->PremNoeud;
  if (NdCour->SuivSFils!=NULL)
  { 
    ImprimeNd(NdCour,Axe->Num); /* Ecriture du premier noeud d?s lors qu'il a un suivant */
    NdCour=NdCour->SuivSFils;
    while ((NdCour->SuivSFils!=NULL)||(NdCour->SuivSPere!=NULL))
    { 
      if (NdCour->SuivSPere!=NULL) { NdCour=NdCour->SuivSPere; } /* On passe, c'est ramif */
      else
      { 
        ImprimeNd(NdCour,Axe->Num); /* Ecriture des noeuds interm?diaires non branch?s */
        NdCour=NdCour->SuivSFils;
      }
    }
    ImprimeNd(NdCour,Axe->Num); /* Ecriture du dernier noeud */
  }

}  /* Fonction ImprimeSansRamifAxe */

/*****************************************************************************/
/*****************************************************************************/

void ISansRamifAxe(PTAxe Axe, float *x, float *y, float *z, int nvx, int nvz, double *RSD, \
                   float volume_cube, double xmax, double ymax, double zmax)
  {   /* Imprime les noeuds d'un axe sans y inclure les noeuds de branchement */

PTNoeud NdCour;
  
  NdCour=Axe->PremNoeud;
  if (NdCour->SuivSFils!=NULL)
  { 
    ImprNd(NdCour,Axe->Num,x,y,z,nvx,nvz,RSD,volume_cube,xmax,ymax,zmax); /* Ecriture du premier noeud d?s lors qu'il a un suivant */
  NdCour=NdCour->SuivSFils;
  while ((NdCour->SuivSFils!=NULL)||(NdCour->SuivSPere!=NULL))
  { 
  if (NdCour->SuivSPere!=NULL) { NdCour=NdCour->SuivSPere; } /* On passe, c'est ramif */
else
{ 
  ImprNd(NdCour,Axe->Num,x,y,z,nvx,nvz,RSD,volume_cube,xmax,ymax,zmax);
  NdCour=NdCour->SuivSFils;
}
}
  ImprNd(NdCour,Axe->Num,x,y,z,nvx,nvz,RSD,volume_cube,xmax,ymax,zmax); /* Ecriture du dernier noeud */
  }
  
}  /* Fonction ISansRamifAxe */



/****************************************************************************/
/****************************************************************************/
// CBF removed for coupling to MIN3P :
//void ImprimeSansRamifSR(PTSysRac SR)
//{   /* Imprime le syst?me racinaire sansles noeuds de ramification */

//PTAxe AxeCour;
  
//    ImprimeEnteteSR();
  
//  AxeCour=SR->PremAxe;
//  while (AxeCour!=NULL)
//  {
//    ImprimeSansRamifAxe(AxeCour);
//    AxeCour=AxeCour->Suivant;
//  }
//}  /* Fonction ImprimeSansRamifSR */
/****************************************************************************/
/****************************************************************************/
void ISansRamifSR(PTSysRac SR, float *x, float *y, float *z, int nvx, int nvz, double *RSD,\
                  float volume_cube, double xmax, double ymax, double zmax)
{   /* Imprime le syst?me racinaire sansles noeuds de ramification */

PTAxe AxeCour;

//  ImprimeEnteteSR();

  AxeCour=SR->PremAxe;
  while (AxeCour!=NULL)
  {
    ISansRamifAxe(AxeCour,x,y,z,nvx,nvz,RSD,volume_cube, xmax, ymax, zmax);
 
    AxeCour=AxeCour->Suivant;
  }
}  /* Fonction ISansRamifSR */
/****************************************************************************/
/****************************************************************************/
// CBF removed for coupling to MIN3P :
//void ImprimeMerisSR(PTSysRac SR)
//{   /* Imprime les m?rist?me du syst?me racinaire */

//PTAxe AxeCour;

//  ImprimeEnteteMerisSR();

//  AxeCour=SR->PremAxe;
//  while (AxeCour!=NULL)
//  {
//    ImprimeMeris(AxeCour);
//    AxeCour=AxeCour->Suivant;
//  }
//}  /* Fonction ImprimeAvecRamifSR */
/****************************************************************************/
/****************************************************************************/
void FermeFichiers(void)
{
  fclose(FFPar);

  fclose(FFSol);

// CBF removed for coupling to MIN3P :
  //fclose(FNoeud);
// CBF removed for coupling to MIN3P :
  //fclose(FMeris);
 

}  /* Fonction FermeFichiers */
/****************************************************************************/
/****************************************************************************/

void INIT_ROOTTYP(char *directory, int seed, int pos, double xmax, double ymax, double zmax)
{
	// DSU Add flag to control rand generator, by default, seed is negative and time(NULL) is used
	if (seed >= 0) {
		srand( (unsigned) seed);    //Generate same random series, same results in different runnings
	}
	else {
		srand( (unsigned) time(NULL) ); //Generate different random series, different results in different runnings
	}

    OuvreFichiers(directory,pos);

    LitParam(xmax,ymax,zmax);

    LitSol();

    SR=InitialiseSR(Orig);

    InstalleSR(SR);
}



void COMP_RT(double time_MIN3P, float DeltaT, int nvx_gbl, int nvy_gbl, int nvz_gbl, \
             int nvx, int nvy, int nvz, float *xg, float *yg, float *zg, double *RSD, \
             double xmax, double ymax, double zmax)

{
// CBF : nvx, nvy, nvz, xmax, ymax, zmax : besoin pour le calcul du volume d'un element du maillage pour calculer les RSD
int ivol;

//  DSU : xmax and ymax can be zero for 1 D simulation, use 1.0 instead for 1D simulation.
double xmax_loc, ymax_loc, zmax_loc;

xmax_loc = (nvx_gbl > 1) ? xmax : 1.0;
ymax_loc = (nvy_gbl > 1) ? ymax : 1.0;
zmax_loc = (nvz_gbl > 1) ? zmax : 1.0;

float volume_cube=(zmax_loc/nvz_gbl)*(xmax_loc/nvx_gbl)*(ymax_loc/nvy_gbl);// CBF pour calcul des RSD en 3D (surface racinaire/volume d'1 cube de MIN3P)
//A passer à ImprSR - Rq : pour RSD 2D, diviser par volume_carre et *100 car passage des metres de MIN3P aux cm de Root Typ

PTAxe AxeCour;
PTNoeud NdCour;
Temps=time_MIN3P; // CBF : ATTENTION il faut que time_MIN3P ait des valeurs entieres car ds archisimple temps est un entier    


  /* Calcule l'?tat du syst?me racinaire en terme de r?it?ration */
  EtatReiterationSR(SR);
    

  /* D?veloppement du syst?me racinaire */
  DeveloppeSR(SR, DeltaT);


  /* Croissance radiale du syst?me racinaire */
  CroissanceRadialeSR(SR, P_CoeffCroissRad);
    

  /* Mortalite du syst?me racinaire */
  MortaliteSR(SR);
    
  ISansRamifSR(SR,xg,yg,zg,nvx,nvz,RSD,volume_cube,xmax_loc,ymax_loc,zmax_loc); // CBF ADDED

// CBF : uncomment for debug
// for(ivol=0;ivol<=((nvx*nvz)-1);ivol++)    
//    {
//        printf("\n %d %f %f ", ivol, xg[ivol], zg[ivol]); //, RSD[ivol]);
//    }


}

void END_ROOTTYP()
{
    FermeFichiers();
}

} // CBF extern "C"



















