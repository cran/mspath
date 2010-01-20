

#include <math.h> 
#include <stdlib.h> 
#if 0
#include <R.h> 
#include <R_ext/Applic.h> 
#endif


extern "C"{

#define MI(i, j, ncols) ( (int) ((i)*(ncols) + (j)) ) 
#define logit(x) (log ( (x) / (1 - (x))))
#define expit(x) (exp (x) / (1 + exp(x)))

/* 302: */
#line 10746 "mspath.web"

typedef double*Matrix;
typedef int*iMatrix;
typedef double*vector;
typedef int*ivector;


/* :302 */
#line 10739 "mspath.web"

/* 303: */
#line 10754 "mspath.web"

void mspathCEntry(
/* 304: */
#line 10768 "mspath.web"

int*do_what,


double*params,

double*allinits,

int*misc,


int*p,

int*subjvec,
double*timevec,
int*statevec,
int*qvector,
int*evector,
double*covvec,
int*constraint,
double*misccovvec,
int*miscconstraint,
int*baseconstraint,
int*basemiscconstraint,


const char**history,
double*initialOffset,
int*pathconstraint,
int*pathmiscconstraint,

double*initprobs,
int*nst,
int*nms,
int*nintens,
int*nintenseffs,
int*nmisc,
int*nmisceffs,
int*nobs,
int*npts,
int*ncovs,
int*ncoveffs,
int*nmisccovs,
int*nmisccoveffs,


int*nhistory,
int*npatheffs,
int*npathmisceffs,


int*isexact,

int*nfix,
int*fixedpars,


int*stepNumerator,
int*stepDenominator,
double*returned

/* :304 */
#line 10756 "mspath.web"

);
/* :303 */
#line 10740 "mspath.web"

/* 305: */
#line 10831 "mspath.web"


/* :305 */
#line 10741 "mspath.web"

/* 306: */
#line 10834 "mspath.web"

#if 0
void msmLikelihood(data*d,model*m,int misc,double*returned);

double likmisc(int pt,data*d,model*m);

void AddCovs(int obs,data*d,model*m,double*newintens);
void AddMiscCovs(int obs,data*d,model*m,double*newp);
double PObsTrue(int obst,
int tst,
double*miscprobs,
model*m
);
double liksimple(data*d,model*m);

void Viterbi(data*d,model*m,double*fitted);


double pijt(int i,int j,double t,vector intens,int*qvector,int nstates,int exacttimes);
void Pmat(Matrix pmat,double t,vector intens,int*qvector,int nstates,int exacttimes);
void FillQmatrix(int*qvector,vector intens,Matrix qmat,int nstates);
void MatrixExp(Matrix mat,int n,Matrix expmat,double t);
int repeated_entries(vector vec,int n);
void MatrixExpSeries(Matrix mat,int n,Matrix expmat,double t);
void MatTranspose(Matrix A,Matrix AT,int n);
void MatInv(Matrix A,Matrix Ainv,int n);
void MultMat(Matrix A,Matrix B,int arows,int acols,int bcols,Matrix AB);
void MultMatDiag(Matrix A,vector diag,int n,Matrix AB);
void FormIdentity(Matrix A,int n);
#endif

/* :306 */
#line 10742 "mspath.web"

}

/* :301 */
#line 13422 "mspath.web"

/* 308: */
#line 10868 "mspath.web"

