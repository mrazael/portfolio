# -----------------------------------------------------------------------------------------------------------
# Script: 2ACE_Chol.R  

# Fits a MV Cholesky ACE Decomposition
# Data File:		MVasb.dat (E-Risk Study)
# Phenotypes:		mothers-, teachers-, examiners-, and  children's self-report on antisocial behavior
# Type of data:		Raw continuous data
# ZYG: 			1=MZ 2=DZ
# Assumptions: 		means and variances can be equated across birth order & zygosity groups
# ------------------------------------------------------------------------------------------------------------
#Load libraries and functions
require(OpenMx)
require(psych)
source("miFunctions.R")
source("GenEpiHelperFunctions.R")
mxOption(NULL,"Default optimizer","SLSQP")
mxOption(NULL,"Default optimizer","CSOLNP")

for(i in 1){
# Select Variables for Analysis
vars <- c('PIQ','PercSpd','WM','VSTM','PSTM','Phon') # list of variables names
nv <- length(vars) # number of variables
fm <- 3 # family members, defined in dissertation.R file
ntv <- nv*fm # number of total variables
selVars <- paste0(rep(vars, fm), rep(1:fm, each = nv))

# Select Data for Analysis
mzData <- subset(subData, zyg=='MZ', selVars)
dzData <- subset(subData, zyg=='DZ', selVars)

mzcov <- cov(mzData,use="pairwise.complete")
dzcov <- cov(dzData,use="pairwise.complete")

# Set Starting Values
svPa <- sqrt(2*abs(mzcov[1,2]-dzcov[1,2]))
svPct <- sqrt(abs(2*dzcov[1,2]-mzcov[1,2]))
svPe <- sqrt(abs(mzcov[2,2]-mzcov[1,2]))
Stmean <- colMeans(mzData[,c(1:6,1:6,13:18)],na.rm=T)
lbPa <- 0.0001 # lower bound for path coefficient
lbPaD <- valDiagLU(lbPa,-10,NA,nv) # lower bound for diagonal, lower & upper elements of covariance matrix
mLabs	<- paste(paste0("m",selVars[c(1:6,1:6,13:18)]),sep="")

# -------------------------------------------------------------------------------------------------
# (model 2) Specify Cholesky ACTE Decomposition 
# -------------------------------------------------------------------------------------------------

nvalues <- (nv^2+nv)/2

# Matrices declared to store a, c, and e Path Coefficients
pathA  <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPa, nv), labels=labLower("a", nv), name="a")
pathC	<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPct, nv), labels=labLower("c", nv), name="c")
pathTw <- mxMatrix(type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPct, nv), labels=labLower("tw", nv), name="tw")
pathE	<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPe, nv), labels=labLower("e", nv), name="e")

# Matrices generated to hold A, C, T, and E computed Variance Components
covA	<- mxAlgebra( expression=a %*% t(a), name="A" )
covC	<- mxAlgebra( expression=c %*% t(c), name="C" )
covTw <- mxAlgebra( expression=tw %*% t(tw), name="Tw" )
covE	<- mxAlgebra( expression=e %*% t(e), name="E" )
covP	<- mxAlgebra( expression=A+C+Tw+E, name="V" )
StA	<- mxAlgebra( expression=A/V, name="h2" )
StC	<- mxAlgebra( expression=C/V, name="c2" )
StE	<- mxAlgebra( expression=E/V, name="e2" )
StTw <- mxAlgebra( expression=Tw/V, name="tw2")

# Algebra to compute Correlations
matI	<- mxMatrix( type="Iden", nrow=nv, ncol=nv, name="I")
invSD_chol <- mxAlgebra( expression=solve(sqrt(I*V)), name="iSD")
a_std <- mxAlgebra(expression = iSD %*% a, name = "std_a")
c_std <- mxAlgebra(expression = iSD %*% c, name = "std_c")
tw_std <- mxAlgebra(expression = iSD %*% tw, name = "std_tw")
e_std <- mxAlgebra(expression = iSD %*% e, name = "std_e")
CI_std <- mxCI(c("std_a", "std_c", "std_tw", "std_e" ))

# Calculate genetic and environmental correlations
Rph <- mxAlgebra( expression=solve(sqrt(I*V))%&%V, name ="Phcor") #rP?
Rg <- mxAlgebra( expression=solve(sqrt(I*A))%&%A, name ="Acor" ) #cov2cor() #tää on rG
Rc <- mxAlgebra( expression=solve(sqrt(I*C))%&%C, name ="Ccor" )
Rt <- mxAlgebra( expression=solve(sqrt(I*Tw))%&%Tw, name ="Twcor" )
Re <- mxAlgebra( expression=solve(sqrt(I*E))%&%E, name ="Ecor" )

# Algebra for expected Mean and Variance/Covariance Matrices in MZ & DZ twins

MeanMZ	<- mxMatrix( type="Full", nrow=1, ncol=ntv, free=T, values=Stmean, labels=mLabs, name="ExpMeanMZ" ) ### labels ottaa sisälleen yhtä monta kuin mitä perheenjäseniä on

covMZ	<- mxAlgebra( expression= rbind( cbind(A+C+Tw+E , A+C+Tw, 0.5%x%A+C),
                                       cbind(A+C+Tw , A+C+Tw+E, 0.5%x%A+C),
                                       cbind(0.5%x%A+C, 0.5%x%A+C, A+C+E)),		name="ExpCovMZ" )

covDZ	<- mxAlgebra( expression= rbind( cbind(A+C+Tw+E , 0.5%x%A+C+Tw, 0.5%x%A+C),
                                       cbind(0.5%x%A+C+Tw , A+C+Tw+E, 0.5%x%A+C),
                                       cbind(0.5%x%A+C, 0.5%x%A+C, A+C+E)),	name="ExpCovDZ" )

# Data objects for Multiple Groups
dataMZ	<- mxData( observed=mzData, type="raw" )
dataDZ	<- mxData( observed=dzData, type="raw" )

# Objective objects for Multiple Groups
objMZ	<- mxExpectationNormal( covariance="ExpCovMZ", means="ExpMeanMZ", dimnames=selVars )
objDZ	<- mxExpectationNormal( covariance="ExpCovDZ", means="ExpMeanMZ", dimnames=selVars )

fitFunction <- mxFitFunctionML()

# Create Confidence Interval Objects

modelCIs <- mxCI(c("h2","Acor"))

# Combine Groups
pars	<- list( pathA, pathC, pathTw, pathE, covA, covC, covE, covP, covTw, matI, invSD_chol, StA, StC, StE, StTw, a_std, c_std, tw_std, e_std, Rph, Rg, Rc, Re, Rt) 
modelMZ	<- mxModel( pars, covMZ, MeanMZ, dataMZ, objMZ, fitFunction, name="MZ" )
modelDZ	<- mxModel( pars, covDZ, MeanMZ, dataDZ, objDZ, fitFunction, name="DZ" )
multi <- mxFitFunctionMultigroup( c("MZ","DZ") )
minus2ll<- mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
obj	<- mxFitFunctionAlgebra( "m2LL" )
CholACTEModel<- mxModel( "CholACTE", pars, modelMZ, modelDZ, multi, minus2ll, obj, modelCIs, CI_std)
#---------------------------------------------------------------------------------------------------------------------
# (model 2) RUN Cholesky Decomposition ACTE Model
#CholACTEModel <- mxOption(CholACTEModel, "Standard Errors", "Yes")
#CholACTEModel <- mxOption(CholACTEModel, "Calculate Hessian", "Yes")
CholACTEFit	<- mxTryHard(CholACTEModel, intervals=F, extraTries = 5)
(CholACTESum	<- summary(CholACTEFit))

# RUN SUBMODELS
# Run ATE model
# modelATE <- mxModel( CholACTEFit, name="ATE" )
# modelATE <- omxSetParameters( modelATE, labels=labLower("c", nv), free=FALSE, values=0 )
# CholATEFit <- mxTryHard( modelATE, intervals=F, extraTries = 5 )
# 
# # Run ACE model
# modelACE <- mxModel( CholACTEFit, name="ACE" )
# modelACE <- omxSetParameters( modelACE, labels=labLower("tw", nv), free=FALSE, values=0 )
# CholACEFit <- mxTryHard( modelACE, intervals=F, extraTries = 5 )

# Run ACTE model with a Common Environmental Factor and a Common Twin Factor
ts <- labLower("tw", nv)[-c(1:nv)]
cs <- labLower("c", nv)[-c(1:nv)]
es <- labLower("e", nv)[!labLower("e", nv) %in% labDiag("e", nv)]

# # Run AE model
modelAE <- mxModel( CholACTEFit, name="AE" )
modelAE <- omxSetParameters( modelAE, labels=c(labLower("c", nv),labLower("tw", nv)), free=FALSE, values=0 )
CholAEFit <- mxTryHard( modelAE, intervals=F, extraTries = 5 )

modelACTEhypo <- mxModel( CholACTEFit, name="ACTEhypo" )
modelACTEhypo <- omxSetParameters( modelACTEhypo, labels=c("a32","a42","a52",cs, ts), free=FALSE, values=0 )
CholACTEhypoFit <- mxTryHard( modelACTEhypo, intervals=T, extraTries = 5)
}

#print(mxCompare( CholACTEFit, subs <- list(CholACTEhypoFit, CholAEFit)))
#print(summary(CholACTEhypoFit)$parameters)
#print(omxAkaikeWeights(models = c(CholACTEhypoFit, CholAEFit), type = c("AIC", "AICc"), conf.level = 0.95))

# Print the h2, c2, t2, e2, rG, rC, rTw, rE matrices (note: in the summary output you will find them with 95% CI)
h2 <- mxEval(ACTEhypo.h2, CholACTEhypoFit)
c2 <- mxEval(ACTEhypo.c2, CholACTEhypoFit)
t2 <- mxEval(ACTEhypo.tw2, CholACTEhypoFit)
e2 <- mxEval(ACTEhypo.e2, CholACTEhypoFit)
# 
# h2SE <- mxSE("h2", CholACTEhypoFit)
# c2SE <- mxSE("c2", CholACTEhypoFit)
# tw2SE <- mxSE("tw2", CholACTEhypoFit)
# e2SE <- mxSE("e2", CholACTEhypoFit)
# 
# herInts <- summary(CholACTEhypoFit, verbose = T)$CIdetail[1:(2*4*nv),1:3][3]

set.seed(666)
ACTEbootPoor <- mxBootstrap(CholACTEhypoFit, replications = 10000, verbose = 2L)
# set.seed(666)
# ACTEbootGood <- mxBootstrap(CholACTEhypoFit, replications = 2000, verbose = 2L)

ACTEboot <- mxBootstrap(CholACTEhypoFit, replications = 2000, verbose = 2L)

set.seed(666)
ACTEevalstdaB <- mxBootstrapEvalByName(name = "std_a", model = ACTEbootPoor, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
set.seed(666)
ACTEevalstdcB <- mxBootstrapEvalByName(name = "std_c", model = ACTEbootPoor, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
set.seed(666)
ACTEevalstdtB <- mxBootstrapEvalByName(name = "std_tw", model = ACTEbootPoor, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
set.seed(666)
ACTEevalstdeB <- mxBootstrapEvalByName(name = "std_e", model = ACTEbootPoor, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
set.seed(666)
ACTEevalAcorB <- mxBootstrapEvalByName(name = "Acor", model = ACTEbootPoor, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)

for(i in 1){
set.seed(666)
ACTEevalh2B <- mxBootstrapEvalByName(name = "h2", model = ACTEbootPoor, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
ACTEevalc2B <- mxBootstrapEvalByName(name = "c2", model = ACTEbootPoor, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
ACTEevalt2B <- mxBootstrapEvalByName(name = "t2", model = ACTEbootPoor, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
ACTEevale2B <- mxBootstrapEvalByName(name = "e2", model = ACTEbootPoor, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
}
# set.seed(666)
# ACTEevalstdaG <- mxBootstrapEvalByName(name = "std_a", model = ACTEbootGood, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
# set.seed(666)
# ACTEevalstdcG <- mxBootstrapEvalByName(name = "std_c", model = ACTEbootGood, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
# set.seed(666)
# ACTEevalstdtG <- mxBootstrapEvalByName(name = "std_tw", model = ACTEbootGood, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
# set.seed(666)
# ACTEevalstdeG <- mxBootstrapEvalByName(name = "std_e", model = ACTEbootGood, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
# set.seed(666)
# ACTEevalAcor <- mxBootstrapEvalByName(name = "Acor", model = ACTEbootGood, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
# set.seed(666)
# ACTEevalEcor <- mxBootstrapEvalByName(name = "Ecor", model = ACTEbootGood, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)

ACTEevalh2 <- mxBootstrapEvalByName(name = "h2", model = ACTEbootGood, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
ACTEevalc2 <- mxBootstrapEvalByName(name = "c2", model = ACTEbootGood, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
ACTEevalt2 <- mxBootstrapEvalByName(name = "t2", model = ACTEbootGood, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)
ACTEevale2 <- mxBootstrapEvalByName(name = "e2", model = ACTEbootGood, bq = c(0.05, 0.95), replications = 10000, verbose = 2L)



for(i in 1:nv){
  print(paste(sprintf("h2 %.3f", round(h2[i,i], 3)),
              sprintf("c2 %.3f", round(c2[i,i], 3)),
              sprintf("t2 %.3f", round(t2[i,i], 3)),
              sprintf("e2 %.3f", round(e2[i,i], 3))))
}
